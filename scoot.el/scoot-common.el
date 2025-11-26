;;; scoot-common.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2025 Sven Johansson

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Contains common functions backing scoot.el functionality and piping,
;; such as resolver infrastructure allowing the various scoot modes to
;; adjust the behavior of common commands and functions.

;;; Code:

(require 'dash)
(require 'cl-lib)


;; Variables

(defvar-local scoot-local--table-name-resolvers nil
  "Buffer local list of resolver functions for table-name.

Used by various common scoot operations that require a table-name to resolve
it in a way that suits the current buffer's mode.

If this variable is not defined locally, the global variable
`scoot-default--table-name-resolvers` will be used.

See `scoot--try-resolvers`.")

(defvar-local scoot-local--context-name-resolvers nil
  "Buffer local list of resolver functions for context-name.

Used by various common scoot operations that require a context-name to
resolve it in a way that suits the current buffer's mode.

If this variable is not defined locally, the global variable
`scoot-default--context-name-resolvers` will be used.

See `scoot--try-resolvers`.")


(defvar-local scoot-local--connection-resolvers nil
  "Buffer local list of resolver functions for resolving local connection.

Used by various common scoot operations that require a connection, and
resolving it in a way that suits the current mode of the buffer.

If this variable is not defined locally, the global variable
`scoot-default--connection-resolvers` will be used.

See `scoot--try-resolvers`.")

(defvar scoot-default--table-name-resolvers (list 'scoot-connection--table-prompt)
  "Global default list of resolver functions for table-name.

See `scoot-local--table-name-resolvers`")

(defvar scoot-default--connection-resolvers (list 'scoot-connection--connection-prompt)
  "Global default list of resolver functions for connection.

See `scoot-local--connection-resolvers`")

(defvar scoot-default--context-name-resolvers (list 'scoot-connection--context-prompt)
    "Global default list of resolver functions for context-name.

See `scoot-local--context-name-resolvers`")

(defvar-local scoot--saved-cursor-pos nil
  "Temporary storage of cursor position during buffer modification.")

(defvar scoot-connections)
(declare-function scoot-connection--list-objects "scoot-connection")
(declare-function scoot--open-resultet "scoot-resultset")
(declare-function helm-build-sync-source nil)
(declare-function helm nil)


;; Variable and elisp type utilities

(defun scoot--wrap-string (str width)
  "Wrap STR to WIDTH, preserving whitespace and \n boundaries."
  (let ((lines (split-string str "\n"))
        (result '()))
    (dolist (line lines)
      (while (> (scoot--visible-width line) width)
        (let ((visible-substr (scoot--visible-substring line width)))
          (push visible-substr result)
          (setq line (substring line (length visible-substr)))))
      (push line result))
    (apply #'list (nreverse result))))

(defun scoot--visible-width (s &optional tabwidth)
  "Return the visual width of the string S, expanding tabs.

TABWIDTH may be passed in, otherwise the buffer-local `tab-width`
will be used to compute the column-width of tabs"
  (let ((tabwidth (or tabwidth tab-width))
        (col 0))
    (seq-do (lambda (ch)
              (if (eq ch ?\t)
                  (setq col (+ col (- tabwidth (% col tabwidth))))
                (setq col (1+ col))))
            s)
    col))

(defun scoot--visible-substring (s width)
  "Return the substring of S that fits in WIDTH visual columns."
  (let ((col 0)
        (idx 0))
    (while (and (< idx (length s))
                (< col width))
      (let* ((ch (aref s idx))
             (step (if (eq ch ?\t)
                       (- tab-width (% col tab-width))
                     1)))
        (setq col (+ col step))
        (when (<= col width)
          (setq idx (1+ idx)))))
    (substring s 0 idx)))


(defun scoot--plist-merge (plist1 plist2)
  "Merge PLIST1 and PLIST2. Values from PLIST2 take precedence."
  (let ((result plist1))
    (while plist2
      (setq result (plist-put result (pop plist2) (pop plist2))))
    result))

(defun scoot--plist-remove! (plist key)
  "Destructively remove KEY and its value from PLIST."
  (let ((cell plist)
        prev)
    (while cell
      (if (eq (car cell) key)
          (progn
            (setcdr cell (cddr cell))
            (when prev
              (setcdr prev cell))
            (setq cell nil)) ;; stop after first match
        (setq prev cell
              cell (cddr cell))))
    plist))

(defun scoot--plist-select-keys (plist keys)
  "Return a new plist created from PLIST containing only the keys in KEYS."
  (let (result)
    (dolist (key keys result)
      (when (plist-member plist key)
        (setq result (plist-put result key (plist-get plist key)))))))

(defun scoot--alist-to-plist (alist)
  "Convert ALIST to plist, transforming `key to :key."
  (cl-loop for (key . value) in alist append (list (intern (format ":%s" key)) value)))

(defun scoot--valid-identifier (str)
  "Test STR for a valid SQL identifier and return it."
  (when (and (stringp str)
             (string-match "\\`\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\([.;,]\\)?\\'" str))
    (let ((name (match-string 1 str)))
      name)))


;; Buffer inspection and navigation utilities

(defun scoot--props-at (pt)
  "Return `text-properties` at PT as an alist."
  (let* ((text-props (text-properties-at pt)))
    (cl-loop for (prop val) on text-props by #'cddr collect (cons prop val))))

(defun scoot--props-at-point ()
  "Return `text-properties` at point as an alist."
  (scoot--props-at (point)))

(defun scoot--object-name-at-point ()
  "Test the `thing-at-point`, if any, for a valid SQL identifier and return it."
  (scoot--valid-identifier (cond
                            ((thing-at-point 'symbol t)
                             (thing-at-point 'symbol t)))))

(defun scoot--thing-at-p (point thing)
  "Test the scoot `thing` prop at POINT for equality withn THING."
  (if (sequencep thing)
      (member (alist-get 'thing (scoot--props-at point)) thing)
    (equal thing (alist-get 'thing (scoot--props-at point)))))

(defun scoot--scan-property-with-value (forward-p opt prop value limit)
  "Scan for the next position where PROP has VALUE.

Returns nil if not found.

Base implementation for scoot--next/previous-property-with-value variations.

FORWARD-P determines forward or backward scan direction.
OPT determines if value should be
treat as a list of options or an exact value.

Scans until the beginning/end of the buffer or LIMIT, if required."
  (let* ((scan-fn (if forward-p
                      #'next-single-property-change
                    #'previous-single-property-change))
         (cmp-fn (lambda (pos)
                   (let ((val (get-text-property pos prop)))
                     (if opt
                         (member val value)
                       (equal val value)))))
         (limit-fn (if forward-p #'< #'>))
         (pos (point))
         (limit (or limit (if forward-p (point-max) (point-min))))
         (first-match-is-false (and (not forward-p)
                                    (funcall cmp-fn (1- pos))))
         result)
    (while (and (funcall limit-fn pos limit) (not result))
      (setq pos (funcall scan-fn pos prop nil limit))
      (when (and pos (funcall cmp-fn pos))
        (if first-match-is-false
            (setq first-match-is-false nil)
          (setq result pos))))
    result))

(defun scoot--next-property-with-value (prop value &optional limit)
  "Find the next position where PROP has VALUE.  Returns nil if not found.

Scans until the end of the buffer or LIMIT, if required."
  (scoot--scan-property-with-value t nil prop value limit))

(defun scoot--next-property-with-value-in (prop seq &optional limit)
  "Find the next position where PROP has value that is a member of SEQ.

Returns nil if not found.

Scans until the end of the buffer or LIMIT, if required."
  (scoot--scan-property-with-value t t prop seq limit))

(defun scoot--previous-property-with-value (prop value &optional limit)
  "Find the previous position where PROP has VALUE.  Returns nil if not found.

Scans until the end of the buffer or LIMIT, if required."
  (scoot--scan-property-with-value nil nil prop value limit))

(defun scoot--previous-property-with-value-in (prop seq &optional limit)
  "Find the previous position where PROP has value that is a member of SEQ.

Returns nil if not found.

Scans until the end of the buffer or LIMIT, if required."
  (scoot--scan-property-with-value nil t prop seq limit))



(defun scoot--object-type-name (object-type &optional plural)
  "Get human readable name of OBJECT-TYPE.

If PLURAL is non-nil, return the plural form."
  (pcase object-type
    ('table (if plural "Tables" "Table"))
    ('tables (if plural "Tables" "Table"))
    ('schema (if plural "Schemas" "Schema"))
    ('schemas (if plural "Schemas" "Schema"))
    ('database (if plural "Databases" "Database"))
    ('databases (if plural "Databases" "Database"))
    (_ (if plural "Objects" "Object"))))

(defun scoot--pos-to-point (pos)
  "Translate position POS to point in the current buffer.

The term \"position\" refers to the plist format of:
\(:line <line number>
 :col <column number>)"
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (plist-get pos :line)))
    (move-to-column (min (plist-get pos :col) (- (line-end-position) (line-beginning-position))))
    (point)))

(defun scoot--save-cursor (&optional var-name)
  "Save the current cursor position.

Stores the position to the buffer-local variable `scoot--saved-cursor-pos`,
unless optionally overriden by VAR-NAME."
  (let ((target-var-name (or var-name 'scoot--saved-cursor-pos)))
    (set (make-local-variable target-var-name)
         (list :line (line-number-at-pos)
               :col (current-column)))))

(defun scoot--restore-cursor ()
  "Restore the cursor to the saved position in `scoot--saved-cursor-pos`."
  (goto-char (scoot--pos-to-point scoot--saved-cursor-pos)))

(defun scoot--object-name-in-region ()
  "Test the active region, if any, for a valid SQL identifier and return it."
  (when (use-region-p)
    (scoot--valid-identifier
     (-> (buffer-substring-no-properties (region-beginning) (region-end))
         (string-trim)))))


;; Elisp function utilities

(defun scoot--await (fn)
  "Run FN, which must accept a callback argument, and wait for the callback.
Returns the callback’s value.

Example:
  (scoot-await (lambda (cb) (my-async-op cb)))."
  (let ((done nil)
        (result nil))
    (funcall fn
             (lambda (value)
               (setq result value
                     done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    result))

(cl-defun scoot--resolve-fn (&key fn-obj fallback-fn)
  "Helper-function that resolves a function reference.

FN-OBJ is a function-object or a symbol to which a function is bound.
May be nil, in which case fallback-fn will be tried.

FALLBACK-FN is a function-object or a symbol to which a function is bound."
  (cond
   ((functionp fn-obj)
    fn-obj)
   ((and (symbolp fn-obj) (fboundp fn-obj))
    (symbol-function fn-obj))
   (fallback-fn
    (scoot--resolve-fn :fn-obj fallback-fn))))

(cl-defun scoot--apply-fn (&key fn-obj args slice-args fallback-value fallback-fn)
  "Helper-function to invoke a function-object or symbol-bound function.

FN-OBJ is a function object or a symbol to which a function is bound.
May be nil, in which case fallback-fn and fallback-value will be
tried (in that order).

ARGS is a list of arguments to apply to the selected function.

SLICE-ARGS determines if the ARGS list should be sliced to fit the arity of
the selected function.

FALLBACK-FN is a function object, symbol or nil to attempt to invoke in case
FN-OBJ cannot be resolved to a function.

FALLBACK-VALUE is a value to simply return if all else fails."
  (if-let (fn (scoot--resolve-fn :fn-obj fn-obj :fallback-fn fallback-fn))
      (apply fn (if slice-args (scoot--slice-arg-list fn args) args))
    fallback-value))

(defun scoot--resolver-fn (obj)
  "Return a function object/callable object from OBJ if possible."
  (cond
   ((functionp obj) obj)
   ((and (symbolp obj) (fboundp obj)) (symbol-function obj))
   (t (progn
        (message "Warning: Invalid resolver '%s' in buffer %s" obj (buffer-name))))))

(defun scoot--slice-arg-list (fn arg-list)
  "Slice ARG-LIST to match the arity of FN."
  (let ((max (cdr (func-arity fn))))
    (cond

     ((null arg-list)
      '())

     ((or (eq 'many max)
          (>= max (length arg-list)))
      arg-list)

     ((> (length arg-list) max)
      (cl-subseq arg-list 0 max)))))



;; Rendering utilities

(defun scoot--propertize-sql (sql-string)
  "Propertize SQL-STRING with syntax highlighting via font-lock."
  (with-temp-buffer
    (erase-buffer)
    (sql-mode)
    (insert sql-string)
    (font-lock-ensure)
    (buffer-string)))

(defun scoot--insert-propertized-string (s)
  "Insert propertized string S into current buffer, preserving its text properties.

This feels like a nasty hack, but ensures that the inserted text retains its
font-lock properties."
  (let ((start (point)))
    (insert s)
    (let ((i 0)
          (len (length s)))
      (while (< i len)
        (let ((props (text-properties-at i s)))
          (when props
            (add-text-properties (+ start i) (+ start i 1) props)))
        (setq i (1+ i))))))



;; Scoot context/connection resolver utilities

(defun scoot--try-resolvers (resolvers &optional default-resolvers arg-list)
  "Try each function or symbol in RESOLVERS until a non-nil result is returned.

If no non-nil value can be resolved, the function returns nil.

RESOLVERS can be a list or a function returning a list, or a symbol whose value
resolves to either of those two types.

If RESOLVERS is nil, the optional DEFAULT-RESOLVERS will be attempted instead.

An argument-list can be provided in ARG-LIST."
  (let ((resolver-list (if (or (null resolvers)
                               (null (symbol-value resolvers)))
                           default-resolvers
                         resolvers)))
    (seq-some (lambda (entry)
                (let ((fn (scoot--resolver-fn entry)))
                  (when (functionp fn)
                    (apply fn (scoot--slice-arg-list fn arg-list)))))
              (cond
               ((listp resolver-list)
                resolver-list)

               ((and (symbolp resolver-list) (listp (symbol-value resolver-list)))
                (symbol-value resolver-list))

               ((functionp resolver-list)
                (funcall resolver-list))

               ((and (symbolp resolver-list) (fboundp resolver-list))
                (symbol-function resolver-list))

               (t '())))))

(cl-defun scoot--completing-read (&key name
                                       prompt
                                       candidates
                                       default-value
                                       sort-fn
                                       annotation-fn
                                       fuzzy-match)
  "Perform a completing read, enhanced by `helm` if available.

NAME - The name/title of the completing read modal.
PROMPT - The prompt to show before the input.
CANDIDATES - A list of options to match against.
DEFAULT-VALUE - The pre-selected default-value, if any.
SORT-FN - Comparator to use for sorting CANDIDATES.
ANNOTATION-FN - Function used to decorate options for display.
FUZZY-MATCH - Allow fuzzy match.

Features implied by the arguments are available/unavailable depending on the
available \"completing-read backend\"."
  (let ((sorted-candidates (if-let (sorter (scoot--resolve-fn :fn-obj sort-fn))
                              (sort candidates sorter)
                            candidates)))
    (if (and (featurep 'helm)
             (boundp 'helm-build-sync-source))
        (helm :sources (helm-build-sync-source name
                         :fuzzy-match fuzzy-match
                         :candidates sorted-candidates
                         :candidate-transformer
                         (lambda (cds)
                           (mapcar
                            (lambda (c)
                              (propertize
                               c 'display
                               (concat c (scoot--apply-fn :fn-obj annotation-fn
                                                          :args (list c)
                                                          :slice-args t
                                                          :fallback-value ""))))
                            cds)))
              :buffer (concat "*helm " (downcase name) "*"))
      (completing-read prompt candidates nil nil default-value))))

(defun scoot--interactive-resolve-context-name ()
  "Interactively resolve context-name using configured resolvers."
  (scoot--try-resolvers
   'scoot-local--context-name-resolvers
   'scoot-default--context-name-resolvers
   (list :context-name "default"
         :interactive t)))

(defun scoot--interactive-resolve-connection (&optional context-name)
  "Interactively resolve connection using configured resolvers.

If CONTEXT-NAME is not given an attempt will be made to resolve it using
`scoot--interactive-resolve-context-name'."
  (unless context-name
    (setq context-name (scoot--interactive-resolve-context-name)))
  (scoot--try-resolvers
   'scoot-local--connection-resolvers
   'scoot-default--connection-resolvers
   (list :connection-name "default"
         :interactive t)))

(defun scoot--list-objects (object-type connection callback)
  "List objects visible to the user of CONNECTION or the
current contextually available connection.

Valid object types are:
\\='databases
\\='schemas
\\='tables

OBJECT-TYPE is the type of the object to list.
CONNECTION (Optional)
CALLBACK The function to call with the object result"
  (let* ((connection (or connection (scoot--interactive-resolve-connection))))
    (scoot-connection--list-objects
     connection
     object-type
     callback)))

(provide 'scoot-common)

;;; scoot-common.el ends here

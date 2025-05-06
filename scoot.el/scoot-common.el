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
(require 'scoot-connection)

(defvar-local scoot-local--table-name-resolvers nil
  "Buffer local list of resolver functions for table-name.

Used by various common scoot operations that require a table-name to resolve
it in a way that suits the current buffer's mode.

If this variable is not defined locally, the global variable
`scoot-default--table-name-resolvers` will be used.

See `scoot--try-resolvers`.")

(defvar-local scoot-local--connection-name-resolvers nil
  "Buffer local list of resolver functions for connection-name.

Used by various common scoot operations that require a connection-name to
resolve it in a way that suits the current buffer's mode.

If this variable is not defined locally, the global variable
`scoot-default--connection-name-resolvers` will be used.

See `scoot--try-resolvers`.")

(defvar scoot-default--table-name-resolvers (list 'scoot-connection--table-prompt)
  "Global default list of resolver functions for table-name.

See `scoot-local--table-name-resolvers`")
(defvar scoot-default--connection-name-resolvers (list 'scoot-connection--connection-prompt)
  "Global default list of resolver functions for connection-name.

See `scoot-local--connection-name-resolvers`")

(defun scoot--valid-identifier (str)
  "Test STR for a valid SQL identifier and return it."
  (when (and (stringp str)
             (string-match "\\`\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\([.;,]\\)?\\'" str))
    (let ((name (match-string 1 str)))
      name)))

(defun scoot--object-name-at-point ()
  "Test the `thing-at-point`, if any, for a valid SQL identifier and return it."
  (scoot--valid-identifier (cond
                            ((thing-at-point 'symbol t)
                             (thing-at-point 'symbol t)))))

(defun scoot--object-name-in-region ()
  "Test the active region, if any, for a valid SQL identifier and return it."
  (when (use-region-p)
    (scoot--valid-identifier
     (-> (buffer-substring-no-properties (region-beginning) (region-end))
         (string-trim)))))

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
  (if-let (fn (scoot--resolve-fn :fn-obj fn-obj :fallback-fn))
      (apply fn (if slice-args (scoot--slice-arg-list fn args) args))
    fallback-value))

(defun scoot--resolver-fn (obj)
  "Return a function object/callable object from OBJ if possible."
  (cond
   ((functionp obj) obj)
   ((and (symbolp obj) (fboundp entry)) ((symbol-function entry))
    (t (progn
         (message "Warning: Invalid resolver '%s' in buffer %s" obj (buffer-name)))))))

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
                            sorted-candidates)))
              :buffer (concat "*helm " (downcase name) "*"))
      (completing-read prompt candidates))))

(defun scoot--interactive-resolve-connection-name ()
  "Interactively resolve connection-name using configured resolvers."
  (scoot--try-resolvers
   'scoot-local--connection-name-resolvers
   'scoot-default--connection-name-resolvers
   (list :connection-name "default"
         :interactive t)))

(defun scoot--list-objects (object-type title)
  "Interactively list objects visible to the user of the current connection.

OBJECT-TYPE is the type of the object to list.
TITLE is the resultset header to be used."
  (let ((connection-name (scoot--interactive-resolve-connection-name)))
    (scoot-connection--list-objects
     connection-name
     object-type
     (lambda (object-result)
       (scoot-result--open-result-buffer
        (list :type 'objects
              :object-type object-type
              :result
              `((columns . [,title])
                (rows . ,(mapcar #'vector
                                 (seq-into
                                  (alist-get object-type object-result)
                                  'vector)))
                (metadata . ((columns . [((name . ,title)
                                          (type . "OBJECT-NAME"))]))))
              :connection connection-name))))))


(provide 'scoot-common)

;;; scoot-common.el ends here

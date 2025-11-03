((python-mode
  . ((lsp-pylsp-plugins-pylint-enabled . nil)
     (lsp-pylsp-plugins-flake8-enabled . nil)
     (lsp-pylsp-plugins-autopep8-enabled . nil)
     (python-shell-extra-pythonpaths . ("core" "cli" "server"))
     (lsp-pyright-extra-paths . ("core cli") "server"))))

;; (("system_test" . ((python-ts-mode
;;                     (eval . (setq lsp-pyright-extra-paths (list "../core")))))))

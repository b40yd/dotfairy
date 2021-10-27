;;; Code:

;; Rust
(use-package rustic
  :init
  ;; We use the superior default client provided by `lsp-mode', not the
  ;; one rustic-mode sets up for us
  (setq rustic-lsp-server 'rust-analyzer)
  ;; disable rustic flycheck error display in modeline. Its redundant
  (setq rustic-flycheck-setup-mode-line-p nil)

  :hook ((rustic-mode . (lambda ()
                          (dotfairy-set-prettify '(("fn" . ?ƒ)
                                                   ("string" . ?𝕊)))
                          )))
  :config
  (setq rust-indent-method-chain t)

  ;; format using rustfmt on save
  (setq rustic-format-on-save t)

  (defun my-rustic-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rustic-mode-hook))
(use-package rust-playground)

(provide 'init-rust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here

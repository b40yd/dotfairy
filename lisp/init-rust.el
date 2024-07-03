;;; Code:
(require 'init-funcs)

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(use-package rust-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :init (setq rust-format-on-save t
              rust-mode-treesitter-derive t)
  :config

  (set-ligatures! 'rust-mode
    ;; Functional
    :def "fn"
    ;; Types
    :null "None"
    :true "true" :false "false"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    ;; Flow
    :in "in"
    :and "&&" :or "||"
    :for "for"
    :return "return")

  ;; HACK: `global-treesit-auto-mode' will override `rust-mode'.
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code.

\\{rust-mode-map}")

  (when (centaur-treesit-available-p)
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist))))

(use-package rust-playground)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here

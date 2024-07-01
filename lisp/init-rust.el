;;; Code:
(require 'init-funcs)

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
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
    :return "return"))

(use-package rust-playground)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here

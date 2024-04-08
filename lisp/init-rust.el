;;; Code:
(require 'init-funcs)
(require 'init-keybinds)

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
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
;; Rust
(when (memq dotfairy-lsp '(lsp-mode eglot))
  (use-package rustic
    :init
    ;; We use the superior default client provided by `lsp-mode', not the
    ;; one rustic-mode sets up for us
    (setq rustic-lsp-server 'rust-analyzer)
    ;; disable rustic flycheck error display in modeline. Its redundant
    (setq rustic-flycheck-setup-mode-line-p nil)

    :config
    (setq rustic-indent-method-chain t)

    ;; Conflicts with (and is redundant with) :ui ligatures
    (setq rust-prettify-symbols-alist nil)

    ;; Leave automatic reformatting to the :editor format module.
    (setq rustic-babel-format-src-block nil
          rustic-format-trigger nil)

    (setq rustic-lsp-client
          (cond ((memq dotfairy-lsp '(eglot))
                 'eglot)
                ((memq dotfairy-lsp '(lsp-mode))
                 'lsp-mode)
                (t
                 nil)))
    (add-hook 'rustic-mode-local-vars-hook #'rustic-setup-lsp 'append)

    ;; HACK If lsp/eglot isn't available, it attempts to install lsp-mode via
    ;;   package.el. Doom manages its own dependencies through straight so disable
    ;;   this behavior to avoid package-not-initialized errors.
    (defadvice! +rust--dont-install-packages-a (&rest _)
      :override #'rustic-install-lsp-client-p
      (message "No LSP server running"))
    ;;; Custom Cargo commands
    (autoload 'rustic-run-cargo-command "rustic-cargo")
    ;;;###autoload
    (defun +rust/cargo-audit ()
      "Run 'cargo audit' for the current project."
      (interactive)
      (rustic-run-cargo-command "cargo audit"))

    (setq rust-indent-method-chain t
          ;; format using rustfmt on save
          rustic-format-on-save t)


    (map! :map rustic-mode-map
          :localleader
          :desc "cargo compile"        "C-c" #'rustic-compile
          :desc "cargo recompile"      "C-r" #'rustic-recompile
          :desc "racer describe"       "C-d" #'rustic-racer-describe
          :desc "docstring dwim"       "C-," #'rustic-docstring-dwim
          :desc "cargo add"            "a" #'rustic-cargo-add
          :desc "cargo remove"         "r" #'rustic-cargo-rm
          :desc "cargo run"            "R" #'rustic-cargo-run
          :desc "cargo fmt"            "f" #'rustic-cargo-fmt
          :desc "cargo format buffer"  "F" #'rustic-format-buffer
          :desc "cargo init"           "i" #'rustic-cargo-init
          :desc "cargo new"            "n" #'rustic-cargo-new
          :desc "cargo doc --open"     "D" #'rustic-cargo-doc
          :desc "cargo doc"            "d" #'rustic-cargo-build-doc
          (:prefix ("b" . "build")
           :desc "cargo audit"      "a" #'+rust/cargo-audit
           :desc "cargo build"      "b" #'rustic-cargo-build
           :desc "cargo check"      "c" #'rustic-cargo-check
           :desc "cargo clippy"     "k" #'rustic-cargo-clippy
           :desc "cargo clippy fix" "K" #'rustic-cargo-clippy-fix
           :desc "cargo outdated"   "o" #'rustic-cargo-outdated)
          (:prefix ("t" . "cargo test")
           :desc "all"          "a" #'rustic-cargo-test
           :desc "cargo bench"  "b" #'rustic-cargo-bench
           :desc "current test" "t" #'rustic-cargo-current-test))))

(use-package rust-playground)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here

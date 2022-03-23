;;; Code:
(require 'init-funcs)


(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

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
                                                   ("None" . ?∅)
                                                   ("<<=" . (?≪ (Br . Bl) ?=))
                                                   ("||=" . ?⊫)
                                                   ("|=" . ?⊨)
                                                   (".." . ?‥)
                                                   ("..." . ?…)
                                                   ("::" . ?∷))))))
  :config

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

  (after! flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy))

  (defun my-rustic-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rustic-mode-hook)

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
         :desc "current test" "t" #'rustic-cargo-current-test)))

(use-package rust-playground)

(use-package racer)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here

;;; init-go.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  7ym0n.q6e

;; Author: 7ym0n.q6e <bb.qnyd@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:
(require 'init-basic)
(require 'init-funcs)
(require 'init-keybinds)

(use-package go-mode
  :ensure t
  :functions go-update-tools
  :commands godoc-gogetdoc
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (dotfairy-set-prettify '(("func()" . ?λ)
                                               ("func" . ?ƒ)
                                               ("map" . ?↦)
                                               (":=" . ?≔)
                                               ("string" . ?𝕊)
                                               ("nil" . ?∅)))
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))
    (if (not (executable-find "go"))
        (exec-path-from-shell-setenv "PATH" (format "%s/bin" (cdr (dotfairy-call-process "go" "env" "GOPATH")))))
    (exec-path-from-shell-initialize))

  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/gopls"
                      "golang.org/x/tools/cmd/goimports"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/josharian/impl"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct"
                      "github.com/110y/go-expr-completion"
                      "github.com/golangci/golangci-lint/cmd/golangci-lint")
    "All necessary go tools.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")

    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools))

  ;; Misc
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package gotest)
  (use-package go-expr-completion)

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :hook (go-mode . (lambda ()
                       "Enable golangci-lint."
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-build
                                                          go-test))
                       (flycheck-golangci-lint-setup))))

  (use-package go-tag
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (after! go-mode
    (map! :localleader
          :map go-mode-map
          :desc "add tag"                           "a" #'go-tag-add
          :desc "remove tag"                        "d" #'go-tag-remove
          :desc "doc at point"                      "p" #'godoc-at-point
          (:prefix ("i" . "imports")
           :desc "import add"                       "a" #'go-import-add
           :desc "expr completion"                  "e" #'go-expr-completion
           :desc "fill struct"                      "f" #'go-fill-struct
           :desc "goto imports"                     "i" #'go-goto-imports      ; Go to imports
           :desc "remove unused imports"            "r" #'go-remove-unused-imports)
          (:prefix ("b" . "build")
           :desc "go run ."                         "r" (cmd! (compile "go run ."))
           :desc "go build"                         "b" (cmd! (compile "go build"))
           :desc "go clean"                         "c" (cmd! (compile "go clean")))
          (:prefix ("t" . "test")
           :desc "test current project"             "a" #'go-test-current-project
           :desc "test current coverage"            "c" #'go-test-current-coverage
           :desc "test current test"                "s" #'go-test-current-test
           :desc "test current file"                "t" #'go-test-current-file
           :desc "gen test dwim"                    "g" #'go-gen-test-dwim
           :desc "gen test all"                     "G" #'go-gen-test-all
           :desc "gen test exported"                "e" #'go-gen-test-exported
           (:prefix ("b" . "bench")
            :desc "test current project benchmarks" "a" #'go-test-current-project-benchmarks
            :desc "test current benchmark"          "s" #'go-test-current-benchmark
            :desc "test current file benchmarks"    "t" #'go-test-current-file-benchmarks)))))
(provide 'init-go)
;;; init-go.el ends here

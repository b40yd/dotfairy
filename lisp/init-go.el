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



(use-package go-mode
  :ensure t
  :functions go-update-tools
  :commands godoc-gogetdoc
  :mode (("\\.go\\'" . go-mode))
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (dotfairy-set-prettify '(("func()" . ?λ)
                                               ("func" . ?ƒ)
                                               ("map" . ?↦)
                                               ("string" . ?𝕊)
                                               ("nil" . ?∅)))
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

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
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
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
                                                          go-test
                                                          go-errcheck))
                       (flycheck-golangci-lint-setup))))

  (use-package go-tag
    :init (setq go-tag-args (list "-transform" "camelcase")))
  (use-package go-gen-test)
  (use-package gotest)
  (use-package go-expr-completion)
  (map! :map go-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "p" #'godoc-at-point
        (:prefix ("l" . "imports")
         "a" #'go-import-add
         "e" #'go-expr-completion
         "f" #'go-fill-struct
         "i" #'go-goto-imports      ; Go to imports
         "r" #'go-remove-unused-imports)
        (:prefix ("b" . "build")
         :desc "go run ." "r" (cmd! (compile "go run ."))
         :desc "go build" "b" (cmd! (compile "go build"))
         :desc "go clean" "c" (cmd! (compile "go clean")))
        (:prefix ("t" . "test")
         "a" #'go-test-current-project
         "c" #'go-test-current-coverage
         "s" #'go-test-current-test
         "t" #'go-test-current-file
         "g" #'go-gen-test-dwim
         "G" #'go-gen-test-all
         "e" #'go-gen-test-exported
         (:prefix ("b" . "bench")
          "a" #'go-test-current-project-benchmarks
          "s" #'go-test-current-benchmark
          "t" #'go-test-current-file-benchmarks
          )))
  )


(provide 'init-go)
;;; init-go.el ends here

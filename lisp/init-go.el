;;; init-go.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  7ym0n.q6e

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
  :functions (go-packages-gopkgs go-update-tools)
  :mode (("\\.go\\'" . go-mode))
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :config
  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook (lambda ()
                              (add-to-list (make-local-variable 'company-backends)
                                           '(company-go company-files company-yasnippet company-capf)))))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/cmd/goimports"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/josharian/impl"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct"
                      "github.com/golangci/golangci-lint/cmd/golangci-lint@latest")
    "All necessary go tools.")

  ;; Do not use the -u flag for gopls, as it will update the dependencies to incompatible versions
  ;; https://github.com/golang/tools/blob/master/gopls/doc/user.md#installation
  (defvar go--tools-no-update '("golang.org/x/tools/gopls@latest")
    "All necessary go tools without update the dependencies.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (let ((proc-name "go-tools")
          (proc-buffer "*Go Tools*"))
      (dolist (pkg go--tools-no-update)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))

      (dolist (pkg go--tools)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))))

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
  (map! :map go-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("ri" . "imports")
                 "a" #'go-import-add
                 "r" #'go-remove-unused-imports)
        (:prefix ("b" . "build")
                 :desc "go run ." "r" (cmd! (compile "go run ."))
                 :desc "go build" "b" (cmd! (compile "go build"))
                 :desc "go clean" "c" (cmd! (compile "go clean")))
        (:prefix ("t" . "test")
                 "t" #'+go/test-rerun
                 "a" #'+go/test-all
                 "s" #'+go/test-single
                 "n" #'+go/test-nested
                 "g" #'go-gen-test-dwim
                 "G" #'go-gen-test-all
                 "e" #'go-gen-test-exported
                 (:prefix ("b" . "bench")
                          "s" #'+go/bench-single
                          "a" #'+go/bench-all)))
  )

(defvar +go-test-last nil
  "The last test run.")

(defun +go--spawn (cmd)
  (save-selected-window
    (compile cmd)))

(defun +go--run-tests (args)
  (let ((cmd (concat "go test " args)))
    (setq +go-test-last (concat "cd " default-directory ";" cmd))
    (+go--spawn cmd)))

;;;###autoload
(defun +go/test-rerun ()
  (interactive)
  (if +go-test-last
      (+go--spawn +go-test-last)
    (+go/test-all)))

;;;###autoload
(defun +go/test-all ()
  (interactive)
  (+go--run-tests ""))

;;;###autoload
(defun +go/test-nested ()
  (interactive)
  (+go--run-tests "./..."))

;;;###autoload
(defun +go/test-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-run" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/bench-all ()
  (interactive)
  (+go--run-tests "-test.run=NONE -test.bench=\".*\""))

;;;###autoload
(defun +go/bench-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Benchmark[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-test.run=NONE -test.bench" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

(provide 'init-go)
;;; init-go.el ends here

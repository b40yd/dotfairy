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
  :autoload godoc-gogetdoc
  :hook ((before-save . (lambda ()
                          (unless dotfairy-lsp-format-disable-on-save
                            (gofmt-before-save))))
         (go-mode . (lambda ()
                      (dotfairy-set-prettify '(("func()" . ?λ)
                                               ("func" . ?ƒ)
                                               ("map" . ?↦)
                                               (":=" . ?≔)
                                               ("string" . ?𝕊)
                                               ("nil" . ?∅))))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))
    (if (not (executable-find "go"))
        (exec-path-from-shell-setenv "PATH" (format "%s/bin" (cdr (dotfairy-call-process "go" "env" "GOPATH")))))
    (exec-path-from-shell-initialize))

  ;; Install or update tools
  (defconst go--tools '("golang.org/x/tools/gopls"
                        "golang.org/x/tools/cmd/goimports"
                        "github.com/go-delve/delve/cmd/dlv"
                        "github.com/josharian/impl"
                        "github.com/cweill/gotests/gotests"
                        "github.com/fatih/gomodifytags"
                        "github.com/110y/go-expr-completion"
                        "golang.org/x/tools/cmd/guru"
                        "golang.org/x/tools/cmd/gorename"
                        "golang.org/x/tools/cmd/godoc"
                        "github.com/josharian/impl"
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
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package gotest)
  (use-package go-expr-completion)
  (use-package go-guru)
  (use-package go-rename)

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

  (defvar +go-test-last nil
    "The last test run.")

  (defun +go--spawn (cmd)
    (save-selected-window
      (compile cmd)))

  (defun +go--run-tests (args)
    (let ((cmd (concat "go test -test.v " args)))
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
          (+go--run-tests (concat "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
      (error "Must be in a _test.go file")))

  ;;;###autoload
  (defun +go/test-file ()
    (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
        (save-excursion
          (goto-char (point-min))
          (let ((func-list))
            (while (re-search-forward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)" nil t)
              (push (match-string-no-properties 2) func-list))
            (+go--run-tests (concat "-run" "='^(" (string-join func-list "|")  ")$'"))))
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
          (+go--run-tests (concat "-test.run=NONE -test.bench" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
      (error "Must be in a _test.go file")))


  (after! go-mode
    (map! :localleader
          :map go-mode-map
          :desc "add tag"                           "a" #'go-tag-add
          :desc "remove tag"                        "d" #'go-tag-remove
          :desc "doc at point"                      "p" #'godoc-at-point
          (:prefix ("h" . "help")
           "." #'godoc-at-point     ; Lookup in godoc
           "d" #'go-guru-describe   ; Describe this

           "i" #'go-guru-implements ; Implements relations for package types
           "p" #'go-guru-peers      ; List peers for channel
           "P" #'go-guru-pointsto   ; What does this point to
           "r" #'go-guru-referrers  ; List references to object
           "e" #'go-guru-whicherrs  ; Which errors
           "w" #'go-guru-what       ; What query
           "c" #'go-guru-callers    ; Show callers of this function
           "C" #'go-guru-callees)   ; Show callees of this function
          (:prefix ("i" . "imports")
           :desc "import add"                       "a" #'go-import-add
           :desc "expr completion"                  "e" #'go-expr-completion
           :desc "goto imports"                     "i" #'go-goto-imports      ; Go to imports
           :desc "remove unused imports"            "r" #'go-remove-unused-imports)
          (:prefix ("b" . "build")
           :desc "go run ."                         "r" (cmd! (compile "go run ."))
           :desc "go build"                         "b" (cmd! (compile "go build"))
           :desc "go clean"                         "c" (cmd! (compile "go clean")))
          (:prefix ("t" . "test")
           "t" #'+go/test-rerun
           "a" #'+go/test-all
           "A" #'go-test-current-project
           "c" #'go-test-current-coverage
           "s" #'+go/test-single
           "S" #'go-test-current-test
           "n" #'+go/test-nested
           "f" #'+go/test-file
           "t" #'go-test-current-file
           "g" #'go-gen-test-dwim
           "G" #'go-gen-test-all
           "e" #'go-gen-test-exported
           (:prefix ("b" . "bench")
            "a" #'+go/bench-all
            "A" #'go-test-current-project-benchmarks
            "s" #'+go/bench-single
            "S" #'go-test-current-benchmark
            "t" #'go-test-current-file-benchmarks)))))

(provide 'init-go)
;;; init-go.el ends here

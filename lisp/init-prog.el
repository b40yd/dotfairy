;;; init-prog.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n.q6e, all rights reserved.

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
(require 'init-custom)
(require 'init-const)
;;; Code:

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist dotfairy-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

;; https://gitlab.com/jgkamat/rmsbolt
;; rmsbolt:A godbolt embedded in Emacs
(use-package rmsbolt
  :defer t)

;; quickrun - Execute editing buffer and show its output quickly.
;; https://github.com/syohex/emacs-quickrun
(use-package quickrun
  :config
  ;; hydra for quickrun
  ;; :bind (("C-c r" . quickrun-hydra/body))

  :pretty-hydra
  ((:title (pretty-hydra-title "QuickRun" 'faicon "linux")
           :color amaranth :quit-key "q")
   ("CMD"
    (("u" quickrun)
     ("r" quickrun-region)
     ("e" quickrun-replace-region)
     ("c" quickrun-compile-only)
     ("a" quickrun-with-arg)
     ("s" quickrun-shell)
     ("q" nil "Quit" :color blue)))))

;; Search
(use-package webjump
  :ensure nil
  :init (setq webjump-sites
              '(;; Emacs
                ("Emacs Home Page" .
                 "www.gnu.org/software/emacs/emacs.html")
                ("Xah Emacs Site" . "ergoemacs.org/index.html")
                ("(or emacs irrelevant)" . "oremacs.com")
                ("Mastering Emacs" .
                 "https://www.masteringemacs.org/")

                ;; Search engines.
                ("DuckDuckGo" .
                 [simple-query "duckduckgo.com"
                               "duckduckgo.com/?q=" ""])
                ("Google" .
                 [simple-query "www.google.com"
                               "www.google.com/search?q=" ""])
                ("Bing" .
                 [simple-query "www.bing.com"
                               "www.bing.com/search?q=" ""])

                ("Baidu" .
                 [simple-query "www.baidu.com"
                               "www.baidu.com/s?wd=" ""])
                ("Wikipedia" .
                 [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

;; Nice writing
(use-package olivetti
  :diminish
  ;; :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; Music player
(use-package bongo
  ;; :bind ("C-<f9>" . bongo)
  :config
  (with-eval-after-load 'dired
    (with-no-warnings
      (defun bongo-add-dired-files ()
        "Add marked files to the Bongo library."
        (interactive)
        (bongo-buffer)
        (let (file (files nil))
          (dired-map-over-marks
           (setq file (dired-get-filename)
                 files (append files (list file)))
           nil t)
          (with-bongo-library-buffer
           (mapc 'bongo-insert-file files)))
        (bongo-switch-buffers))
      (bind-key "b" #'bongo-add-dired-files dired-mode-map))))

(use-package ag
  :ensure t)

;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu)
         :map rg-mode-map
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep #'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (with-eval-after-load 'counsel
    (bind-keys
     :map rg-global-map
     ("R" . counsel-rg)
     ("F" . counsel-fzf))))

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment)
(use-package memory-usage)
(use-package tldr)
(use-package command-log-mode)
(use-package mermaid-mode)
(use-package plantuml-mode
  :config
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" dotfairy-local-dir)))

(use-package devdocs
  :bind (:map prog-mode-map
         ("C-c <f1>" . devdocs-dwim))
  :init
  (defvar devdocs-major-mode-docs-alist
    '((c-mode . ("C"))
      (emacs-lisp-mode . ("Elisp"))
      (c++-mode . ("C++"))
      (python-mode . ("Python 3.9" "Python 3.8"))
      (ruby-mode . ("Ruby 3"))
      (go-mode . ("Go"))
      (rustic-mode . ("Rust"))
      (java-mode . ("OpenJDK 17" "OpenJDK 11" "OpenJDK 8"))
      (css-mode . ("CSS"))
      (html-mode . ("HTML"))
      (js-mode . ("JavaScript" "JQuery"))
      (js2-mode . ("JavaScript" "JQuery"))
      (emacs-lisp-mode . ("Elisp")))
    "Alist of MAJOR-MODE and list of docset names.")

  (mapc
   (lambda (e)
     (add-hook (intern (format "%s-hook" (car e)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr e)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" dotfairy-local-dir))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (str)
       (let* ((docs (split-string str " "))
              (doc (if (length= docs 1)
                       (downcase (car docs))
                     (concat (downcase (car docs)) "~" (downcase (cdr docs))))))
         (unless (and (file-directory-p devdocs-data-dir)
                      (directory-files devdocs-data-dir nil "^[^.]"))
           (message "Installing %s..." str)
           (devdocs-install doc))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))

  (with-no-warnings
    (if (>= emacs-major-version 28)
        (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
              xref-show-definitions-function #'xref-show-definitions-completing-read)
      ;; Select from xref candidates with Ivy
      (use-package ivy-xref
        :after ivy
        :init
        (when (>= emacs-major-version 27)
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
        (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))))


;; align table
(use-package valign
  :hook ((org-mode markdown-mode) . valign-mode))

;; Batch Mode eXtras
(use-package bmx-mode
  :after company
  :diminish
  :hook (after-init . bmx-mode-setup-defaults))

;; Tree-sitter
(when (functionp 'module-load)
  (use-package tree-sitter
    :ensure tree-sitter-langs
    :diminish
    :hook ((after-init . global-tree-sitter-mode)
           (tree-sitter-after-on . tree-sitter-hl-mode))))

;; Use quelpa install turbo-log
(use-package turbo-log
  :ensure nil
  :quelpa (turbo-log :fetcher github :repo "artawower/turbo-log.el")
  :config
  (setq turbo-log-msg-format-template "\"🚀: %s\""
        turbo-log-allow-insert-without-tree-sitter-p t)
  (turbo-log-configure
   :modes (typescript-mode js2-mode web-mode rjsx-mode)
   :strategy merge
   :msg-format-template "\"Default: %s\"")
  :hook (after-init . (lambda ()
                        (map! :localleader
                              :map (python-mode-map rjsx-mode-map js-mode-map go-mode-map rust-mode-map java-mode-map typescript-mode-map)
                              (:prefix ("SPC" . "Print")
                               "l" #'turbo-log-print
                               "i" #'turbo-log-print-immediately
                               "h" #'turbo-log-comment-all-logs
                               "s" #'turbo-log-uncomment-all-logs
                               "[" #'turbo-log-paste-as-logger
                               "]" #'turbo-log-paste-as-logger-immediately
                               "d" #'turbo-log-delete-all-logs)))))

(provide 'init-prog)
;;; init-prog.el ends here

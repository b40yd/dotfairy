;;; init-prog.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2020-2024 b40yd

;; Author: b40yd <bb.qnyd@gmail.com>
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
(require 'init-custom)
(require 'init-const)
(require 'init-keybinds)

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
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

(use-package csv-mode)

;; quickrun - Execute editing buffer and show its output quickly.
;; https://github.com/syohex/emacs-quickrun
(use-package quickrun
  :config
  :pretty-hydra
  ((:title (pretty-hydra-title "QuickRun" 'faicon "linux")
    :color amaranth :quit-key ("q" "C-g"))
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
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

(use-package swift-mode) ;; swift language

;; Misc
(use-package diffview)                  ; side-by-side diff view
(use-package disk-usage)
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))
(use-package memory-usage)
(use-package tldr)
(use-package command-log-mode)
(use-package mermaid-mode)
(use-package plantuml-mode
  :config
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" dotfairy-local-dir)))

(use-package devdocs
  :autoload (devdocs--installed-docs devdocs--available-docs)
  :bind (:map prog-mode-map
         ("M-<f1>" . devdocs-dwim)
         ("C-h D"  . devdocs-dwim))
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.10" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))

      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (js-mode         . ("javascript" "jquery"))
      (js2-mode        . ("javascript" "jquery"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))


;; align table
(use-package valign
  :hook ((org-mode markdown-mode) . valign-mode))

(provide 'init-prog)
;;; init-prog.el ends here

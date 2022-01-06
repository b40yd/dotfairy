;;; init-org.el ---                                  -*- lexical-binding: t; -*-

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
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package org
  :init (setq org-startup-indented t)
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist '(("#+BEGIN_SRC" . ?»)
                                                      ("#+END_SRC" . ?«)
                                                      ("#+begin_src" . ?»)
                                                      ("#+end_src" . ?«)
                                                      ("#+BEGIN_COMMENT" . ?»)
                                                      ("#+END_COMMENT" . ?«)
                                                      ("#+begin_comment" . ?»)
                                                      ("#+end_comment" . ?«)
                                                      ("#+BEGIN_QUOTE" . ?»)
                                                      ("#+END_QUOTE" . ?«)
                                                      ("#+begin_quote" . ?»)
                                                      ("#+end_quote" . ?«)
                                                      ("#+HEADERS" . ?☰)
                                                      ("#+RESULTS:" . ?💻)
                                                      ("#+ARCHIVE:" . ?📦)
                                                      ("#+AUTHOR:" . ?👤)
                                                      ("#+CREATOR:" . ?💁)
                                                      ("#+DATE:" . ?📆)
                                                      ("#+DESCRIPTION:" . ?⸙)
                                                      ("#+EMAIL:" . ?📧)
                                                      ("#+OPTIONS:" . ?⛭)
                                                      ("#+SETUPFILE:" . ?⛮)
                                                      ("#+TAGS:" . ?🏷)
                                                      ("#+TITLE:" . ?📓)
                                                      ("[ ]" . ?☐)
                                                      ("[X]" . ?☑)
                                                      ("[-]" . ?❍)
                                                      (">=" . "≥")
                                                      ("=>" . "⇨")))
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c n b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :config
  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (with-eval-after-load 'ox
    (require 'ox-hugo))
  (setq org-startup-indented t
        org-journal-file-format "%Y-%m-%d"
        org-journal-date-prefix "#+TITLE: "
        org-journal-date-format "%A, %B %d %Y"
        org-journal-time-prefix "* "
        org-journal-time-format "%Y-%m-%D"
        org-enable-hugo-support t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-duration-format '((special . h:mm))
        org-directory dotfairy-org-dir ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-agenda-files `(,dotfairy-org-dir)
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-hide-leading-stars t
        org-log-done 'time
        org-log-reschedule 'note
        org-log-redeadline 'note
        org-log-into-drawer "LOGBOOK"
        org-return-follows-link t
        org-special-ctrl-a/e t
        org-use-fast-todo-selection t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-pretty-entities t
        org-odd-levels-only t
        org-list-allow-alphabetical t
        org-capture-templates
        `(("b" "book" entry (file ,(concat org-directory "/book.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/todo.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/notes.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (,(if (>= emacs-major-version 26) 'file+olp+datetree 'file+datetree)
                                ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t))
        ;; TODO sequences
        org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "NEXT(n)" "HOLD(h)" "CANCELLED(c@/!)" "WAITING(W@/!)" "DONE(d)"))
        ;; Targets include this file and any file contributing to the agenda -
        ;; up to 5 levels deep
        org-refile-targets '((org-agenda-files :maxlevel . 5)
                             (nil :maxlevel . 5)))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (char-displayable-p ?🟈)
                    '("🟔" "🟑" "🟍" "✯" "🟈") ;;"🟔" "🟑" "🟍" "✯" "🟈"
                  '("HIGH" "MEDIUM" "LOW" "WARN" "OPTIONAL"))))

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-prettify-item-bullets t
          org-superstar-headline-bullets-list'("🐀" "🐁" "🐂" "🐃" "🐄" "🐅" "🐆" "🐇" "🐈" "🐉" "🐊" "🐋"
                                               "🐌" "🐍" "🐎" "🐏" "🐐" "🐑" "🐒" "🐓" "🐔" "🐕" "🐖" "🐗"
                                               "🐘" "🐙" "🐚" "🐛" "🐜" "🐝" "🐞" "🐟" "🐠" "🐡" "🐢" "🐣"
                                               "🐤" "🐥" "🐦" "🐧" "🐨" "🐩" "🐪" "🐫" "🐬" "🐭" "🐮" "🐯"
                                               "🐰" "🐱" "🐲" "🐳" "🐴" "🐵" "🐶" "🐷" "🐸" "🐹" "🐺" "🐻"
                                               "🐼" "𝍖" "🩠" "🩡" "🩢" "🩣" "🩤" "🩥" "🩦")
          org-superstar-leading-bullet ?\s
          org-hide-leading-stars t
          org-superstar-item-bullet-alist
          '((?* . ?🞿)
            (?+ . ?⮚)
            (?- . ?•)
            )
          ;; Enable custom bullets for TODO items
          org-superstar-special-todo-items t
          org-superstar-todo-bullet-alist
          '(("TODO" "⚐")
            ("NEXT" "⚛")
            ("SOMEDAY" "🌅")
            ("HOLD" "✰")
            ("WAITING" "☕")
            ("CANCELLED" "✘")
            ("DONE" "✔"))))

  (use-package org-super-agenda
    :hook ((org-agenda-mode org-mode) . org-super-agenda-mode)
    :config
    (setq org-agenda-custom-commands
          '(("z" "Super view"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                              :time-grid t
                              :date today
                              :scheduled today
                              :order 1)
                             (:name "Due Today"
                              :deadline today
                              :order 2)
                             (:name "Next Work"
                              :deadline future
                              :order 2)
                             (:name "Overdue"
                              :deadline past
                              :order 3)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '((:name "Passed Deadline"
                               :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT"))
                               :face (:background "#7f1b19"))
                              (:name "Important"
                               :tag "Important"
                               :priority "A"
                               :order 1)
                              (:name "Issues"
                               :and (:tag "Issue" :todo ("TODO" "WAITING" "HOLD" "NEXT"))
                               :order 1)
                              (:name "Next to do"
                               :todo ("TODO" "NEXT")
                               :order 2)
                              (:name "Waiting"
                               :todo "WAITING"
                               :order 3)
                              (:name "On Hold"
                               :todo ("SOMEDAY" "HOLD")
                               :order 4)
                              (:name "Projects"
                               :tag "Project"
                               :order 14)
                              (:name "Research"
                               :tag "Research"
                               :order 15)
                              (:name "To read"
                               :tag "Book"
                               :order 30)))))))))
    (setq org-agenda-breadcrumbs-separator " ❯ "))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (sql . t)
                               (java . t)
                               (plantuml . t)))

  (when (featurep 'plantuml-mode)
    (setq org-plantuml-jar-path plantuml-jar-path))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if (>= emacs-major-version 26)
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-http
    :init (cl-pushnew '(http . t) load-language-list))

  ;; Use mermadi-cli: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("C-<tab>" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 4))

  (use-package org-roam
    :custom (org-roam-directory (concat dotfairy-org-dir "roam/"))
    :hook (after-init . org-roam-mode)
    :init
    (setq org-roam-v2-ack t)
    :config
    (org-roam-db-autosync-mode)
    (require 'org-roam-protocol)
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
    (use-package org-roam-ui
      :init
      (when (featurep 'xwidget-internal)
        (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  (use-package org-re-reveal
    :after ox
    :config
    (setq org-re-reveal-revealjs-version "4"))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  (use-package org-journal
    :ensure t
    :defer t
    :init
    ;; Change default prefix key; needs to be set before loading org-journal
    (setq org-journal-prefix-key "C-c j")
    :config
    (setq org-journal-dir (concat dotfairy-local-dir "journal/")
          org-journal-date-format "%A, %d %B %Y"))

  (use-package ox-hugo
    :ensure t            ;Auto-install the package from Melpa (optional)
    :after ox)

  (defadvice! +chinese--org-html-paragraph-a (args)
    "Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to '(html hugo md odt)."
    :filter-args #'org-html-paragraph
    :filter-args #'org-hugo-paragraph
    :filter-args #'org-md-paragraph
    :filter-args #'org-odt-paragraph
    (cl-destructuring-bind (paragraph contents info) args
      (let* ((fix-regexp "[[:multibyte:]]")
             (fixed-contents
              (replace-regexp-in-string
               (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
               "\\1\\2"
               contents)))
        (list paragraph fixed-contents info)))))

(provide 'init-org)
;;; init-org.el ends here

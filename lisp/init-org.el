;;; init-org.el ---                                  -*- lexical-binding: t; -*-

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
                                                      ("[-]" . ?⛝)
                                                      (">=" . "≥")
                                                      ("=>" . "⇨")))
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
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
        org-default-notes-file (expand-file-name "notes.org" org-directory))

  (setq org-enforce-todo-checkbox-dependencies t
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
        org-agenda-files `(,dotfairy-org-dir)
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
                             (nil :maxlevel . 5))
        )

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
          org-hide-leading-stars t)
    (setq org-superstar-item-bullet-alist
          '((?* . ?🞿)
            (?+ . ?⮚)
            (?- . ?•)
            ))
    ;; Enable custom bullets for TODO items
    (setq org-superstar-special-todo-items t)
    (setq org-superstar-todo-bullet-alist
          '(("TODO" "⚐")
            ("NEXT" "⚛")
            ("SOMEDAY" "🌅")
            ("HOLD" "✰")
            ("WAITING" "☕")
            ("CANCELLED" "✘")
            ("DONE" "✔")))

    )

  (use-package org-super-agenda
    :hook (org-mode . org-super-agenda-mode)
    :config
    (setq org-agenda-custom-commands
          '(("z" "Next View"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                                    :time-grid t
                                    :todo "TODAY"
                                    :date today
                                    :scheduled today
                                    :order 0)
                             (:habit t)
                             (:name "Due Today"
                                    :deadline today
                                    :order 2)
                             (:name "Due Soon"
                                    :deadline future
                                    :order 8)
                             (:name "Overdue"
                                    :deadline past
                                    :order 7)
                             ))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '((:name "Passed Deadline"
                                     :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT"))
                                     :face (:background "#7f1b19"))
                              (:name "Important"
                                     :priority "A")
                              (:priority<= "B"
                                           ;; Show this section after "Today" and "Important", because
                                           ;; their order is unspecified, defaulting to 0. Sections
                                           ;; are displayed lowest-number-first.
                                           :order 1)
                              (:name "Waiting"
                                     :todo "WAITING"
                                     :order 9)
                              (:name "On hold"
                                     :todo "HOLD"
                                     :order 10)
                              (:name "On Working"
                                     :todo "NEXT"
                                     :order 9)
                              (:name "To Read"
                                     :tag ("Read" "Book" "Books")
                                     :todo "SOMEDAY")
                              ))))
              ))))
    (setq org-agenda-breadcrumbs-separator " ❯ ")
    )

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
    :diminish
    :custom (org-roam-directory (file-truename dotfairy-org-dir))
    :hook (after-init . org-roam-mode)
    :config
    (require 'org-roam-protocol)
    (setq org-roam-directory (concat dotfairy-org-dir "roam/")
          org-roam-capture-templates
          '(
            ("d" "default" plain (function org-roam-capture--get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+roam_alias:\n\n")
            ("g" "group")
            ("ga" "Group A" plain (function org-roam-capture--get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+roam_alias:\n\n")
            ("gb" "Group B" plain (function org-roam-capture--get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+roam_alias:\n\n"))
          org-roam-capture-immediate-template
          '("d" "default" plain (function org-roam-capture--get-point)
            "%?"
            :file-name "%<%Y%m%d%H%M%S>-${slug}"
            :head "#+title: ${title}\n"
            :unnarrowed t)
          org-roam-capture-ref-templates
          '(("r" "ref" plain (function org-roam-capture--get-point)
             ""
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_key: ${ref}\n"
             :unnarrowed t)
            ("a" "Annotation" plain (function org-roam-capture--get-point)
             "%U ${body}\n"
             :file-name "${slug}"
             :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
             :immediate-finish t
             :unnarrowed t)
            ))
    )

  (use-package org-roam-server
    :ensure t
    :hook (org-roam-server-mode . org-roam-server-browse)
    :config
    (defun org-roam-server-browse ()
      (when org-roam-server-mode
        (let ((url (format "http://%s:%d" org-roam-server-host org-roam-server-port)))
          (browse-url url))))
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8080
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          org-roam-server-serve-files nil
          org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20))

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
        (list paragraph fixed-contents info))))
  )

(provide 'init-org)
;;; init-org.el ends here

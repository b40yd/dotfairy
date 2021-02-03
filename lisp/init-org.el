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

;; (use-package ox-pandoc
;;   :after ox
;;   :init
;;   (require 'ox-pandoc)
;;   (add-to-list 'org-export-backends 'pandoc)
;;   (setq org-pandoc-options
;;         '((standalone . t)
;;           (mathjax . t))))

(use-package org
  :init (setq org-startup-indented t)
  :bind
  (("C-c o a" . org-agenda)
   ("C-c a" . org-agenda)
   ("C-c o c" . org-capture))
  :config
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
  ;; settings for org-refile
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(((concat dotfairy-org-dir "notes.org") :maxlevel . 3)
                             ((concat dotfairy-org-dir "gtd.org") :maxlevel . 3)
                             ((concat dotfairy-org-dir "projects.org") :maxlevel . 3)))

  (defconst dotfairy-org-todo-file
    (expand-file-name "notes.org" dotfairy-org-dir))
  (defconst dotfairy-org-tickler-file
    (expand-file-name "tickler.org" dotfairy-org-dir))
  (defconst dotfairy-org-books-file
    (expand-file-name "books.org" dotfairy-org-dir))
  (defconst dotfairy-org-projects-file
    (expand-file-name "projects.org" dotfairy-org-dir))
  (defconst dotfairy-org-references-file
    (expand-file-name "references.org" dotfairy-org-dir))
  (defconst dotfairy-org-examples-file
    (expand-file-name "examples.org" dotfairy-org-dir))
  (defconst dotfairy-org-gtd-file
    (expand-file-name "gtd.org" dotfairy-org-dir))
  (setq org-agenda-files `(,dotfairy-org-todo-file
                           ,dotfairy-org-gtd-file
                           ,dotfairy-org-tickler-file
                           ,dotfairy-org-projects-file))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline dotfairy-org-todo-file "Tasks")
                                 (file "~/.emacs.d/private/org/todo.tmpl"))
                                ("r" "Tickler" entry
                                 (file+headline dotfairy-org-tickler-file "Tickler")
                                 (file "~/.emacs.d/private/org/ticker.tmpl"))
                                ("b" "Add a book to read list" entry
                                 (file+headline dotfairy-org-books-file "Read list")
                                 (file "~/.emacs.d/private/org/books.tmpl"))
                                ("p" "Add a new project" entry
                                 (file+headline dotfairy-org-projects-file "Projects")
                                 (file "~/.emacs.d/private/org/projects.tmpl"))
                                ("R" "Add a new reference" entry
                                 (file+headline dotfairy-org-references-file "References")
                                 (file "~/.emacs.d/private/org/references.tmpl"))
                                ("e" "Add a new code example" entry
                                 (file+headline dotfairy-org-examples-file "Examples")
                                 (file "~/.emacs.d/private/org/examples.tmpl"))))

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
        ;; TODO sequences
        org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "NEXT(n)" "HOLD(h)" "CANCELLED(c@/!)" "WAITING(W@/!)" "DONE(d)"))
        ;; Targets include this file and any file contributing to the agenda -
        ;; up to 5 levels deep
        org-refile-targets '((org-agenda-files :maxlevel . 5)
                             (nil :maxlevel . 5))
        )


  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "｛")
                                         ("#+END_SRC" . "｝")
                                         ("#+begin_src" . "｛")
                                         ("#+end_src" . "｝")
                                         ("#+BEGIN_COMMENT" . "｛")
                                         ("#+END_COMMENT" . "｝")
                                         ("#+begin_comment" . "｛")
                                         ("#+end_comment" . "｝")
                                         (">=" . "≥")
                                         ("=>" . "⇨")))

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
    :init
    (org-super-agenda-mode)
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

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
                ("C-c o P" . org-pomodoro)))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("C-c o p" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
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
    :hook (after-init . org-roam-mode)
    :bind (:map org-roam-mode-map
                (("C-c o r l" . org-roam)
                 ("C-c o r f" . org-roam-find-file)
                 ("C-c o r g" . org-roam-graph))
                :map org-mode-map
                (("C-c o r i" . org-roam-insert))
                (("C-c o r I" . org-roam-insert-immediate)))
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
    :config
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
  )

(provide 'init-org)
;;; init-org.el ends here

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

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
(with-eval-after-load 'ox
  (require 'ox-hugo))
(use-package ox-reveal)
(with-eval-after-load 'ox
  (require 'ox-reveal))

(use-package org
  :init (setq org-startup-indented t)
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

  (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-hide-leading-stars t
        org-log-done t
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
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                            (sequence "WAITING(w@/!)" "|" "HOLD(h@/!)"
                                      "CANCELLED(c@/!)" "PHONE" "MEETING")
                            (sequence "QUOTE(q!)" "QUOTED(Q!)" "|"
                                      "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                            (sequence "OPEN(O)" "|" "CLOSED(C)")
                            (type "PERIODIC(P)"))
        ;; org-todo-keyword-faces '(("TODO"      :foreground "red"          :weight bold)
        ;;                          ("PERIODIC"  :foreground "magenta"      :weight bold)
        ;;                          ("NEXT"      :foreground "blue"         :weight bold)
        ;;                          ("DONE"      :foreground "forest green" :weight bold)
        ;;                          ("WAITING"   :foreground "yellow"       :weight bold)
        ;;                          ("HOLD"      :foreground "goldenrod"    :weight bold)
        ;;                          ("CANCELLED" :foreground "orangered"    :weight bold)
        ;;                          ("PHONE"     :foreground "forest green" :weight bold)
        ;;                          ("MEETING"   :foreground "forest green" :weight bold)
        ;;                          ("QUOTE"     :foreground "hotpink"      :weight bold)
        ;;                          ("QUOTED"    :foreground "indianred1"   :weight bold)
        ;;                          ("APPROVED"  :foreground "forest green" :weight bold)
        ;;                          ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
        ;;                          ("REJECTED"  :foreground "olivedrab"    :weight bold)
        ;;                          ("OPEN"      :foreground "magenta"      :weight bold)
        ;;                          ("CLOSED"    :foreground "forest green" :weight bold))
        org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       ("HOLD" ("WAITING" . t) ("HOLD" . t))
                                       (done ("WAITING") ("HOLD"))
                                       ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))
        org-treat-S-cursor-todo-selection-as-state-change nil
        ;; Targets include this file and any file contributing to the agenda -
        ;; up to 5 levels deep
        org-refile-targets '((org-agenda-files :maxlevel . 5)
                             (nil :maxlevel . 5))
        ;; Targets start with the file name - allows creating level 1 tasks
        ;; !!!!!!!!!!!!!!!!!!! REMOVED. It doesn't work well with ivy/org
        ;;;;; org-refile-use-outline-path 'file
        ;; Targets complete in steps so we start with filename, TAB shows the
        ;; next level of targets etc
        ;; !!!!!!!!!!!!!!!!!!! REMOVED. It breaks ivy/org
        ;;;;; org-outline-path-complete-in-steps t
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm
        ;; Column view and estimates
        org-columns-default-format "%80ITEM(Task) %7TODO(To Do) %10Effort(Estim){:} %10CLOCKSUM{Total}"
        org-global-properties '(("Effort_ALL" . "0:0 0:10 0:30 1:00 2:00 3:00 4:00 8:00"))
        org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%0d" :require-minutes t)
        ;; Mark a task as DONE when archiving
        org-archive-mark-done nil
        org-src-fontify-natively t
        org-time-clocksum-use-effort-durations t)

  (setq org-agenda-block-separator (string-to-char " "))
  (setq org-agenda-custom-commands
        '(("a" "My Agenda"
           ((todo "TODO" (
                          (org-agenda-overriding-header "☆ TODAY:\n")
                          (org-agenda-remove-tags t)
                          (org-agenda-prefix-format "  %-2i  %b")
                          (org-agenda-todo-keyword-format "")))
            (agenda "" (
                        (org-agenda-skip-scheduled-if-done t)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-start-day "+0d")
                        (org-agenda-span 5)
                        (org-agenda-overriding-header "㊠ SCHEDULE:\n")
                        (org-agenda-repeating-timestamp-show-all nil)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  %-12:c%?-12t% s")
                        ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                        ;; (org-agenda-todo-keyword-format "")
                        (org-agenda-time)
                        (org-agenda-current-time-string "☟ ┈┈┈┈┈┈┈ now")
                        (org-agenda-scheduled-leaders '("" ""))
                        (org-agenda-deadline-leaders '("" ""))
                        (org-agenda-time-grid (quote ((today require-timed remove-match) (0900 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

            (todo "NEXT" (
                          (org-agenda-overriding-header "🌟 THIS WEEK:\n")
                          (org-agenda-remove-tags t)
                          (org-agenda-prefix-format "  %-2i  %b")
                          (org-agenda-todo-keyword-format "")))
            ))))

  (setq org-list-demote-modify-bullet
        (quote (("+" . "⮙")
                ("-" . "⮚")
                ("*" . "🞿")
                ("1." . "⯁")
                ("1)" . "⯀")
                ("A)" . "⬥")
                ("B)" . "⬦")
                ("a)" . "⬧")
                ("b)" . "⬨")
                ("A." . "⬝")
                ("B." . "⬞")
                ("a." . "⬖")
                ("b." . "⬗"))))

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
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (add-hook 'org-mode-hook 'prettify-symbols-mode)

  (font-lock-add-keywords 'org-mode
                          '(("^ *\([+]\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\([-]\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))
  ;; Prettify UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :init
    ;;  𝋠 𝋡 𝋢 𝋣 𝋤 𝋥 𝋦 𝋧 𝋨 𝋩 𝋪 𝋫 𝋬 𝋭 𝋮 𝋯 𝋰 𝋱 𝋲
    ;;
    ;; "🐀" "🐁" "🐂" "🐃" "🐄" "🐅" "🐆" "🐇" "🐈" "🐉" "🐊" "🐋" "🐌" "🐍" "🐎" "🐏" "🐐" "🐑" "🐒" "🐓" "🐔" "🐕" "🐖" "🐗" "🐘" "🐙" "🐚" "🐛" "🐜" "🐝" "🐞" "🐟" "🐠" "🐡" "🐢" "🐣" "🐤" "🐥" "🐦" "🐧" "🐨" "🐩" "🐪" "🐫" "🐬" "🐭" "🐮" "🐯" "🐰" "🐱" "🐲" "🐳" "🐴" "🐵" "🐶" "🐷" "🐸" "🐹" "🐺" "🐻" "🐼"
    (setq org-bullets-bullet-list '("🐀" "🐁" "🐂" "🐃" "🐄" "🐅" "🐆" "🐇" "🐈" "🐉" "🐊" "🐋"
                                    "🐌" "🐍" "🐎" "🐏" "🐐" "🐑" "🐒" "🐓" "🐔" "🐕" "🐖" "🐗"
                                    "🐘" "🐙" "🐚" "🐛" "🐜" "🐝" "🐞" "🐟" "🐠" "🐡" "🐢" "🐣"
                                    "🐤" "🐥" "🐦" "🐧" "🐨" "🐩" "🐪" "🐫" "🐬" "🐭" "🐮" "🐯"
                                    "🐰" "🐱" "🐲" "🐳" "🐴" "🐵" "🐶" "🐷" "🐸" "🐹" "🐺" "🐻"
                                    "🐼" "𝍖" "🩠" "🩡" "🩢" "🩣" "🩤" "🩥" "🩦"))
    (setq org-agenda-breadcrumbs-separator " ❯ "
          org-ellipsis (if (char-displayable-p ?⤵) " ⤵ " nil)))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (char-displayable-p ?🟈)
                    '("🟔" "🟑" "🟍" "✯" "🟈") ;;"🟔" "🟑" "🟍" "✯" "🟈"
                  '("HIGH" "MEDIUM" "LOW" "WARN" "OPTIONAL"))))
  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))

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

  :bind
  (("C-c o a" . org-agenda)
   ("C-c a" . org-agenda)
   ("C-c o c" . org-capture)))


(provide 'init-org)
;;; init-org.el ends here

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
        org-directory (concat dotfairy-local-dir "org/") ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory))

  (setq org-agenda-files `(,org-directory)
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "⚫(I)" "❓(H)" "|" "✔(D)" "✘(C)")))

  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))

  ;; Prettify UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :init
    (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
    (setq org-ellipsis (if (char-displayable-p ?▼) " ▼ " nil)))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (char-displayable-p ?☕)
                    '("⚡" "⬆" "⬇" ""❗"" "☕")
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
    :custom (org-roam-directory dotfairy-local-dir)
    :hook (after-init . org-roam-mode)
    :bind (:map org-roam-mode-map
                (("C-c o r l" . org-roam)
                 ("C-c o r f" . org-roam-find-file)
                 ("C-c o r g" . org-roam-graph))
                :map org-mode-map
                (("C-c o r i" . org-roam-insert))
                (("C-c o r I" . org-roam-insert-immediate))))

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
    (setq org-journal-dir dotfairy-local-dir
          org-journal-date-format "%A, %d %B %Y"))

  :bind
  (("C-c o a" . org-agenda)
   ("C-c a" . org-agenda)
   ("C-c o c" . org-capture)))


(provide 'init-org)
;;; init-org.el ends here

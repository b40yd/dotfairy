;;; init-org.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 b40yd

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
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)
(require 'init-keybinds)

(use-package org
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist '(("#+BEGIN_SRC" . ?»)
                                                      ("#+END_SRC" . ?«)
                                                      ("#+begin_src" . ?»)
                                                      ("#+end_src" . ?«)
                                                      ("#+BEGIN_COMMENT" . ?»)
                                                      ("#+END_COMMENT" . ?«)
                                                      ("#+begin_example" . ?»)
                                                      ("#+end_example" . ?«)
                                                      ("#+begin_export" . ?»)
                                                      ("#+end_export" . ?«)
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
                                                      ("[ ]" . ?)
                                                      ("[-]" . ?)
                                                      ("[X]" . ?)
                                                      (":END:" . ?🔚)
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
  ((:title (pretty-hydra-title "Org Template" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green)
    :color blue :quit-key ("q" "C-g"))
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
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (:map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :config
  (defvar org-attach-id-dir nil)
  (defun +org--relative-path (path root)
    (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
        (file-relative-name path)
      path))

  (defun +org--read-link-path (key dir &optional fn)
    (let ((file (funcall (or fn #'read-file-name) (format "%s: " (capitalize key)) dir)))
      (format "%s:%s" key (file-relative-name file dir))))

;;;###autoload
  (defun +org-read-link-description-at-point (&optional default context)
    "TODO"
    (if (and (stringp default) (not (string-empty-p default)))
        (string-trim default)
      (if-let* ((context (or context (org-element-context)))
                (context (org-element-lineage context '(link) t))
                (beg (org-element-property :contents-begin context))
                (end (org-element-property :contents-end context)))
          (unless (= beg end)
            (replace-regexp-in-string
             "[ \n]+" " " (string-trim (buffer-substring-no-properties beg end)))))))

;;;###autoload
  (defun +org-define-basic-link (key dir-var &rest plist)
    "Define a link with some basic completion & fontification.
KEY is the name of the link type. DIR-VAR is the directory variable to resolve
links relative to. PLIST is passed to `org-link-set-parameters' verbatim.
Links defined with this will be rendered in the `error' face if the file doesn't
exist, and `org-link' otherwise."
    (declare (indent 2))
    (let ((requires (plist-get plist :requires))
          (dir-fn (if (functionp dir-var)
                      dir-var
                    (lambda () (symbol-value dir-var)))))
      (apply #'org-link-set-parameters
             key
             :complete (lambda ()
                         (if requires (mapc #'require (dotfairy-enlist requires)))
                         (+org--relative-path (+org--read-link-path key (funcall dir-fn))
                                              (funcall dir-fn)))
             :follow   (lambda (link)
                         (org-link-open-as-file (expand-file-name link (funcall dir-fn)) nil))
             :face     (lambda (link)
                         (let* ((path (expand-file-name link (funcall dir-fn)))
                                (option-index (string-match-p "::\\(.*\\)\\'" path))
                                (file-name (substring path 0 option-index)))
                           (if (file-exists-p file-name)
                               'org-link
                             'error)))
             (dotfairy-plist-delete plist :requires))))

  (defun +org-init-attachments-h ()
    "Sets up org's attachment system."
    (setq org-attach-store-link-p 'attached     ; store link after attaching files
          org-attach-use-inheritance t) ; inherit properties from parent nodes

    ;; Autoload all these commands that org-attach doesn't autoload itself
    (require 'org-attach)

    (unless org-attach-id-dir
      ;; Centralized attachments directory by default
      (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
    (after! projectile
      (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))

  (defun +org-inline-image-data-fn (_protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (base64-decode-string link))
  ;; Add inline image previews for attachment links
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn)

  (add-hook! 'org-mode-hook
             #'+org-init-attachments-h)

  (use-package org-download
    :commands (org-download-dnd
               org-download-yank
               org-download-screenshot
               org-download-clipboard
               org-download-dnd-base64)
    :init
    ;; HACK We add these manually so that org-download is truly lazy-loaded
    (pushnew! dnd-protocol-alist
              '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
              '("^data:" . org-download-dnd-base64))
    (advice-add #'org-download-enable :override #'ignore)

    (after! org
      ;; A shorter link to attachments
      (+org-define-basic-link "download" (lambda () (or org-download-image-dir org-attach-id-dir "."))
                              :image-data-fun #'+org-image-file-data-fn
                              :requires 'org-download))
    :config
    (unless org-download-image-dir
      (setq org-download-image-dir org-attach-id-dir))
    (setq org-download-method 'attach
          org-download-timestamp "_%Y%m%d_%H%M%S"
          org-download-screenshot-method
          (cond (IS-MAC "screencapture -i %s")
                (IS-LINUX
                 (cond ((executable-find "maim")  "maim -s %s")
                       ((executable-find "scrot") "scrot -s %s")
                       ((executable-find "gnome-screenshot") "gnome-screenshot -a -f %s"))))

          org-download-heading-lvl nil
          org-download-link-format "[[download:%s]]\n"
          org-download-annotate-function (lambda (_link) "")
          org-download-link-format-function
          (lambda (filename)
            (if (eq org-download-method 'attach)
                (format "[[attachment:%s]]\n"
                        (org-link-escape
                         (file-relative-name filename (org-attach-dir))))
              ;; Handle non-image files a little differently. Images should be
              ;; inserted as normal with previews. Other files, like pdfs or zips,
              ;; should be linked to, with an icon indicating the type of file.
              (format (concat (unless (image-type-from-file-name filename)
                                (concat (+org-attach-icon-for filename)
                                        " "))
                              org-download-link-format)
                      (org-link-escape
                       (funcall org-download-abbreviate-filename-function filename)))))
          org-download-abbreviate-filename-function
          (lambda (path)
            (if (file-in-directory-p path org-download-image-dir)
                (file-relative-name path org-download-image-dir)
              path)))

    (defadvice! +org--fix-org-download-delete-a (fn beg end &optional times)
      "Fix `org-download-delete' for a non-standard `org-download-link-format'."
      :around #'org-download--delete
      (save-excursion
        (save-match-data
          (goto-char beg)
          (let ((times (or times most-positive-fixnum))
                (linkname
                 (or (and (string-match "\\[\\[\\(\\w+\\):" org-download-link-format)
                          (match-string 1 org-download-link-format))
                     "file")))
            (while (and (>= (cl-decf times) 0)
                        (re-search-forward (format "\\[\\[%s:\\([^]]*\\)\\]\\]"
                                                   (regexp-quote linkname))
                                           end t))
              (let ((str (match-string-no-properties 2)))
                (delete-region beg (match-end 0))
                (when (file-exists-p str)
                  (delete-file str))))))))

    (defadvice! +org--dragndrop-then-display-inline-images-a (_link filename)
      :after #'org-download-insert-link
      (when (image-type-from-file-name filename)
        (save-excursion
          (org-display-inline-images
           t t
           (progn (org-back-to-heading t) (point))
           (progn (org-end-of-subtree t t)
                  (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                  (point)))))))

  ;;;###autoload
  (defun +org-attach-icon-for (path)
    (char-to-string
     (pcase (downcase (file-name-extension path))
       ((or "jpg" "jpeg" "png" "gif") ?)
       ("pdf" ?)
       ((or "ppt" "pptx") ?)
       ((or "xls" "xlsx") ?)
       ((or "doc" "docx") ?)
       ((or "ogg" "mp3" "wav" "aiff" "flac") ?)
       ((or "mp4" "mov" "avi") ?)
       ((or "zip" "gz" "tar" "7z" "rar") ?)
       (_ ?))))

;;;###autoload
  (defun +org/open-gallery-from-attachments ()
    "TODO"
    (interactive)
    (require 'org-attach)
    (if-let (dir (org-attach-dir))
        (pop-to-buffer
         ;; Rather than opening dired *and* image-dired windows, suppress them
         ;; both and open only the image-dired window.
         (save-window-excursion
           (image-dired dir)
           (current-buffer)))
      (user-error "No attachments for this node")))

;;;###autoload
  (defun +org/find-file-in-attachments ()
    "Open a file from `org-attach-id-dir'."
    (interactive)
    (dotfairy-project-browse org-attach-id-dir))

;;;###autoload
  (defun +org/attach-file-and-insert-link (path)
    "Downloads the file at PATH and insert an org link at point.
PATH (a string) can be an url, a local file path, or a base64 encoded datauri."
    (interactive "sUri/file: ")
    (unless (eq major-mode 'org-mode)
      (user-error "Not in an org buffer"))
    (require 'org-download)
    (condition-case-unless-debug e
        (let ((raw-uri (url-unhex-string path)))
          (cond ((string-match-p "^data:image/png;base64," path)
                 (org-download-dnd-base64 path nil))
                ((image-type-from-file-name raw-uri)
                 (org-download-image raw-uri))
                ((let ((new-path (expand-file-name (org-download--fullname raw-uri))))
                   ;; Download the file
                   (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") path)
                       (url-copy-file raw-uri new-path)
                     (copy-file path new-path))
                   ;; insert the link
                   (org-download-insert-link raw-uri new-path)))))
      (error
       (user-error "Failed to attach file: %s" (error-message-string e)))))

  ;;;###autoload
  (defun +org/refile-to-current-file (arg &optional file)
    "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-targets `((,file :maxlevel . 10)))
          (org-refile-use-outline-path nil)
          (org-refile-keep arg)
          current-prefix-arg)
      (call-interactively #'org-refile)))

;;;###autoload
  (defun +org/refile-to-file (arg file)
    "Refile current heading to a particular org file.
If prefix ARG, copy instead of move."
    (interactive
     (list current-prefix-arg
           (read-file-name "Select file to refile to: "
                           default-directory
                           (buffer-file-name (buffer-base-buffer))
                           t nil
                           (lambda (f) (string-match-p "\\.org$" f)))))
    (+org/refile-to-current-file arg file))

;;;###autoload
  (defun +org/refile-to-other-window (arg)
    "Refile current heading to an org buffer visible in another window.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-keep arg)
          org-refile-targets
          current-prefix-arg)
      (dolist (win (delq (selected-window) (window-list)))
        (with-selected-window win
          (let ((file (buffer-file-name (buffer-base-buffer))))
            (and (eq major-mode 'org-mode)
                 file
                 (cl-pushnew (cons file (cons :maxlevel 10))
                             org-refile-targets)))))
      (call-interactively #'org-refile)))

;;;###autoload
  (defun +org/refile-to-other-buffer (arg)
    "Refile current heading to another, living org buffer.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-keep arg)
          org-refile-targets
          current-prefix-arg)
      (dolist (buf (delq (current-buffer) (dotfairy-buffers-in-mode 'org-mode)))
        (when-let (file (buffer-file-name (buffer-base-buffer buf)))
          (cl-pushnew (cons file (cons :maxlevel 10))
                      org-refile-targets)))
      (call-interactively #'org-refile)))

;;;###autoload
  (defun +org/refile-to-running-clock (arg)
    "Refile current heading to the currently clocked in task.
If prefix ARG, copy instead of move."
    (interactive "P")
    (unless (bound-and-true-p org-clock-current-task)
      (user-error "No active clock to refile to"))
    (let ((org-refile-keep arg))
      (org-refile 2)))

;;;###autoload
  (defun +org/refile-to-last-location (arg)
    "Refile current heading to the last node you refiled to.
If prefix ARG, copy instead of move."
    (interactive "P")
    (or (assoc (plist-get org-bookmark-names-plist :last-refile)
               bookmark-alist)
        (user-error "No saved location to refile to"))
    (let ((org-refile-keep arg)
          (completing-read-function
           (lambda (_p _coll _pred _rm _ii _h default &rest _)
             default)))
      (org-refile)))

  (defvar org-after-refile-insert-hook)
  ;; Inspired by org-teleport and alphapapa/alpha-org
;;;###autoload
  (defun +org/refile-to-visible ()
    "Refile current heading as first child of visible heading selected with Avy."
    (interactive)
    (when-let (marker (+org-headline-avy))
      (let* ((buffer (marker-buffer marker))
             (filename
              (buffer-file-name (or (buffer-base-buffer buffer)
                                    buffer)))
             (heading
              (org-with-point-at marker
                                 (org-get-heading 'no-tags 'no-todo)))
             ;; Won't work with target buffers whose filename is nil
             (rfloc (list heading filename nil marker))
             (org-after-refile-insert-hook (cons #'org-reveal org-after-refile-insert-hook)))
        (org-refile nil nil rfloc))))

  ;;;###autoload
  (defun +org/remove-link ()
    "Unlink the text at point."
    (interactive)
    (unless (org-in-regexp org-link-bracket-re 1)
      (user-error "No link at point"))
    (save-excursion
      (let ((label (if (match-end 2)
                       (match-string-no-properties 2)
                     (org-link-unescape (match-string-no-properties 1)))))
        (delete-region (match-beginning 0) (match-end 0))
        (insert label))))
  ;;;###autoload
  (defun +org-headline-avy ()
    "TODO"
    (require 'avy)
    (save-excursion
      (when-let* ((org-reverse-note-order t)
                  (pos (avy-with avy-goto-line (avy-jump (rx bol (1+ "*") (1+ blank))))))
        (when (integerp (car pos))
          ;; If avy is aborted with "C-g", it returns `t', so we know it was NOT
          ;; aborted when it returns an int. If it doesn't return an int, we
          ;; return nil.
          (copy-marker (car pos))))))

  ;;;###autoload
  (defun +org/goto-visible ()
    "TODO"
    (interactive)
    (goto-char (+org-headline-avy)))

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

  (setq org-startup-indented t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-babel-python-command "python3"
        org-duration-format '((special . h:mm))
        org-directory dotfairy-org-dir ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-agenda-files (list dotfairy-org-dir)
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
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
        org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f")
        org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("b" "Book" entry (file+olp+datetree
                             ,(concat org-directory "/book.org"))
	       "* Topic: %^{Description}  %^g %? Added: %U"))
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
          org-superstar-item-bullet-alist
          '((?* . ?🞿)
            (?+ . ?⮚)
            (?- . ?•))
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

  (defconst load-language-list '((emacs-lisp . t)
                                 (perl . t)
                                 (python . t)
                                 (ruby . t)
                                 (js . t)
                                 (css . t)
                                 (sass . t)
                                 (C . t)
                                 (sql . t)
                                 (java . t)
                                 (plantuml . t))
    "Alist of org ob languages.")

  (when (featurep 'plantuml-mode)
    (setq org-plantuml-jar-path plantuml-jar-path))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/26
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
    (setq org-roam-v2-ack t
          org-roam-graph-viewer (if (featurep 'xwidget-internal)
                                    #'xwidget-webkit-browse-url
                                  #'browse-url))
    :config
    (setq org-roam-db-location (concat dotfairy-org-dir "org-roam.db"))
    (org-roam-db-autosync-mode)
    (require 'org-roam-protocol)
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
    (add-to-list 'org-agenda-files org-roam-directory)
    (use-package websocket
      :after org-roam)
    (use-package org-roam-ui
      :init
      (when (featurep 'xwidget-internal)
        (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t)))

  ;; Auto-toggle Org LaTeX fragments
  (use-package org-fragtog
    :diminish
    :hook (org-mode . org-fragtog-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish
    :bind (:map org-mode-map
           ("C-c C-h" . org-preview-html-mode))
    :init (when (featurep 'xwidget-internal)
            (setq org-preview-html-viewer 'xwidget)))

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
          org-journal-date-format "%A, %d %B %Y"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-time-prefix "* "
          org-journal-time-format "%Y-%m-%D"))

  (use-package ox-hugo
    :ensure t            ;Auto-install the package from Melpa (optional)
    :after ox
    :init
    (with-eval-after-load 'ox
      (require 'ox-hugo))
    :config
    (setq org-enable-hugo-support t))

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

  (map! :map org-mode-map
        :localleader
        "#" #'org-update-statistics-cookies
        "'" #'org-edit-special
        "*" #'org-ctrl-c-star
        "+" #'org-ctrl-c-minus
        "," #'org-switchb
        "." #'org-goto
        (:when (featurep 'ivy)
         "." #'counsel-org-goto
         "/" #'counsel-org-goto-all)
        (:when (featurep 'vertico)
         "." #'consult-org-heading
         "/" #'consult-org-agenda)
        "@" #'org-mark-subtree
        "A" #'org-archive-subtree
        "b" #'org-switchb
        (:prefix ("a" . "attachments")
         "a" #'org-attach
         "d" #'org-attach-delete-one
         "D" #'org-attach-delete-all
         "f" #'+org/find-file-in-attachments
         "l" #'+org/attach-file-and-insert-link
         "n" #'org-attach-new
         "o" #'org-attach-open
         "O" #'org-attach-open-in-emacs
         "r" #'org-attach-reveal
         "R" #'org-attach-reveal-in-emacs
         "u" #'org-attach-url
         "s" #'org-attach-set-directory
         "S" #'org-attach-sync
         "c" #'org-download-screenshot
         "p" #'org-download-clipboard
         "P" #'org-download-yank)
        (:prefix ("c" . "clock")
         "c" #'org-clock-cancel
         "d" #'org-clock-mark-default-task
         "e" #'org-clock-modify-effort-estimate
         "E" #'org-set-effort
         "g" #'org-clock-goto
         "G" (cmd! (org-clock-goto 'select))
         "i" #'org-clock-in
         "I" #'org-clock-in-last
         "o" #'org-clock-out
         "r" #'org-resolve-clocks
         "R" #'org-clock-report
         "t" #'org-evaluate-time-range
         "=" #'org-clock-timestamps-up
         "-" #'org-clock-timestamps-down)
        (:prefix ("d" . "date/deadline")
         "d" #'org-deadline
         "s" #'org-schedule
         "t" #'org-time-stamp
         "T" #'org-time-stamp-inactive)
        (:prefix ("g" . "goto")
         "g" #'org-goto
         (:when (featurep 'ivy)
          "g" #'counsel-org-goto
          "G" #'counsel-org-goto-all)
         (:when (featurep 'vertico)
          "g" #'consult-org-heading
          "G" #'consult-org-agenda)
         "c" #'org-clock-goto
         "C" (cmd! (org-clock-goto 'select))
         "i" #'org-id-goto
         "r" #'org-refile-goto-last-stored
         "v" #'+org/goto-visible
         "x" #'org-capture-goto-last-stored)
        (:prefix ("j" . "journal")
         "j" #'org-journal-new-entry
         "J" #'org-journal-new-scheduled-entry
         "s" #'org-journal-search-forever)
        (:prefix ("l" . "links")
         "c" #'org-cliplink
         "d" #'+org/remove-link
         "i" #'org-id-store-link
         "l" #'org-insert-link
         "L" #'org-insert-all-links
         "s" #'org-store-link
         "S" #'org-insert-last-stored-link
         "t" #'org-toggle-link-display)
        (:prefix ("P" . "publish")
         "a" #'org-publish-all
         "f" #'org-publish-current-file
         "p" #'org-publish
         "P" #'org-publish-current-project
         "s" #'org-publish-sitemap)
        (:prefix ("r" . "refile")
         "." #'+org/refile-to-current-file
         "c" #'+org/refile-to-running-clock
         "l" #'+org/refile-to-last-location
         "f" #'+org/refile-to-file
         "o" #'+org/refile-to-other-window
         "O" #'+org/refile-to-other-buffer
         "v" #'+org/refile-to-visible
         "r" #'org-refile) ; to all `org-refile-targets'
        (:prefix ("R" . "roam")
         "b" #'org-roam-buffer-toggle
         "c" #'org-roam-capture
         "f" #'org-roam-node-find
         "g" #'org-roam-ui-mode
         "o" #'org-roam-ui-open
         "i" #'org-roam-node-insert
         "t" #'org-roam-tag-add
         "T" #'org-roam-tag-remove
         (:prefix ("d" . "by date")
          "d" #'org-roam-dailies-find-date
          "t" #'org-roam-dailies-find-today
          "m" #'org-roam-dailies-find-tomorrow
          "y" #'org-roam-dailies-find-yesterday))
        (:prefix ("s" . "tree/subtree")
         "a" #'org-toggle-archive-tag
         "b" #'org-tree-to-indirect-buffer
         "d" #'org-cut-subtree
         "h" #'org-promote-subtree
         "j" #'org-move-subtree-down
         "k" #'org-move-subtree-up
         "l" #'org-demote-subtree
         "n" #'org-narrow-to-subtree
         "r" #'org-refile
         "s" #'org-sparse-tree
         "A" #'org-archive-subtree
         "N" #'widen
         "S" #'org-sort)
        (:prefix ("p" . "priority")
         "d" #'org-priority-down
         "p" #'org-priority
         "u" #'org-priority-up)
        (:prefix ("t" . "tables")
         "=" #'org-table-eval-formula
         "+" #'org-table-sum
         "-" #'org-table-insert-hline
         "a" #'org-table-align
         "b" #'org-table-blank-field
         "c" #'org-table-create-or-convert-from-region
         "e" #'org-table-edit-field
         "f" #'org-table-edit-formulas
         "h" #'org-table-field-info
         "s" #'org-table-sort-lines
         "r" #'org-table-recalculate
         "R" #'org-table-recalculate-buffer-tables
         (:prefix ("d" . "delete")
          "c" #'org-table-delete-column
          "r" #'org-table-kill-row)
         (:prefix ("i" . "insert")
          "c" #'org-table-insert-column
          "h" #'org-table-insert-hline
          "r" #'org-table-insert-row
          "H" #'org-table-hline-and-move)
         (:prefix ("t" . "toggle")
          "f" #'org-table-toggle-formula-debugger
          "o" #'org-table-toggle-coordinate-overlays))
        (:prefix ("x" . "toggle")
         "e" #'org-export-dispatch
         "f" #'org-footnote-new
         "h" #'org-toggle-heading
         "i" #'org-toggle-item
         "I" #'org-id-get-create
         "n" #'org-store-link
         "o" #'org-set-property
         "q" #'org-set-tags-command
         "t" #'org-todo
         "T" #'org-todo-list
         "x" #'org-toggle-checkbox
         "S" #'org-tree-slide-mode
         "m" #'org-tags-view
         "v" #'org-display-inline-images))
  (map! :after org-agenda
        :map org-agenda-mode-map
        :m "M-<return>" #'org-agenda-show-and-scroll-up
        :localleader
        (:prefix ("d" . "date/deadline")
         "d" #'org-agenda-deadline
         "s" #'org-agenda-schedule)
        (:prefix ("c" . "clock")
         "c" #'org-agenda-clock-cancel
         "g" #'org-agenda-clock-goto
         "i" #'org-agenda-clock-in
         "o" #'org-agenda-clock-out
         "r" #'org-agenda-clockreport-mode
         "s" #'org-agenda-show-clocking-issues)
        (:prefix ("p" . "priority")
         "d" #'org-agenda-priority-down
         "p" #'org-agenda-priority
         "u" #'org-agenda-priority-up)
        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "t" #'org-agenda-todo))

(provide 'init-org)
;;; init-org.el ends here

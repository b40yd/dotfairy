;;; init-git.el ---                                  -*- lexical-binding: t; -*-

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

(defun my-transient-file (file-name)
  (expand-file-name (convert-standard-filename file-name) dotfairy-local-dir))
(setq transient-history-file (my-transient-file "transient/history.el")
      transient-values-file (my-transient-file "transient/values.el")
      transient-levels-file (my-transient-file "transient/levels.el"))

(use-package magit
  :ensure t
  :commands (+magit/quit +magit/quit-all)
  :init (setq magit-diff-refine-hunk t
              git-commit-major-mode 'git-commit-elisp-text-mode)
  :bind
  (("C-x g" . magit-status))
  :config
  (when IS-WINDOWS
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; modeline magit status update, But doing so isn't good for performance
  (setq auto-revert-check-vc-info t)
  (defvar +magit--stale-p nil)
  (defvar +magit-auto-revert 'local
    "If non-nil, revert associated buffers after Git operations with side-effects.

These buffers are auto-reverted immediately if they're visible or reverted next
time they're switched to. This is intended to be a much more efficient
replacement for `magit-auto-revert-mode' and `global-auto-revert-mode', and
should not be used together with them! Set this to `nil' if you plan to use the
above.

Accepts one of three values OR a predicate function:

t
  Revert any associated buffers.
local
  Same as `t', except remote (TRAMP) buffers are ignored.
nil
  Don't do any auto-reverting at all.
FUNCTION
  If given a function, it will be passed a buffer associated with the current
  Magit session and must return non-nil to signal this is buffer is safe to
  revert (now or later, when switched to).")

  (defun +magit--revertable-buffer-p (buffer)
    (when (buffer-live-p buffer)
      (pcase +magit-auto-revert
        (`t t)
        (`local
         (not (file-remote-p
               (or (buffer-file-name buffer)
                   (buffer-local-value 'default-directory buffer)))))
        ((pred functionp)
         (funcall +magit-auto-revert buffer)))))

  (defun +magit--revert-buffer (buffer)
    (with-current-buffer buffer
      (kill-local-variable '+magit--stale-p)
      (when (magit-auto-revert-repository-buffer-p buffer)
        (save-restriction
          (cl-incf magit-auto-revert-counter)
          (when (bound-and-true-p vc-mode)
            (let ((vc-follow-symlinks t))
              (vc-refresh-state)))
          (when (and (not (get-buffer-process buffer))
                     (funcall buffer-stale-function t))
            (revert-buffer t t t))
          (force-mode-line-update)))))

  ;;;###autoload
  (defun +magit-mark-stale-buffers-h ()
    "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
    (when +magit-auto-revert
      (let ((visible-buffers (dotfairy-visible-buffers nil t)))
        (dolist (buffer (buffer-list))
          (when (+magit--revertable-buffer-p buffer)
            (if (memq buffer visible-buffers)
                (progn
                  (+magit--revert-buffer buffer)
                  (cl-callf2 delq buffer visible-buffers)) ; hasten future lookups
              (with-current-buffer buffer
                (setq-local +magit--stale-p t))))))))

  ;;;###autoload
  (defun +magit/quit (&optional kill-buffer)
    "Bury the current magit buffer.
If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
    (interactive "P")
    (let ((topdir (magit-toplevel)))
      (funcall magit-bury-buffer-function kill-buffer)
      (or (cl-find-if (lambda (win)
                        (with-selected-window win
                          (and (derived-mode-p 'magit-mode)
                               (equal magit--default-directory topdir))))
                      (window-list))
          (+magit/quit-all))))

;;;###autoload
  (defun +magit/quit-all ()
    "Kill all magit buffers for the current repository."
    (interactive)
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
    (+magit-mark-stale-buffers-h))

  (defun +magit--kill-buffer (buf)
    "TODO"
    (when (and (bufferp buf) (buffer-live-p buf))
      (let ((process (get-buffer-process buf)))
        (if (not (processp process))
            (kill-buffer buf)
          (with-current-buffer buf
            (if (process-live-p process)
                (run-with-timer 5 nil #'+magit--kill-buffer buf)
              (kill-process process)
              (kill-buffer buf)))))))

  (after! vc-annotate
    ;; Clean up after itself
    (define-key vc-annotate-mode-map [remap quit-window] #'kill-current-buffer))


  ;; Show TODOs in magit
  (use-package magit-todos
    :after magit-status
    :commands magit-todos-mode
    :init
    (setq magit-todos-nice (if (executable-find "nice") t nil))
    (magit-todos-mode 1))

  ;; Access Git forges from Magit
  ;; see config: https://magit.vc/manual/ghub/Storing-a-Token.html#Storing-a-Token
  ;; writting like as gitlib.com:
  ;; echo "machine gitlab.com/api/v4 login $YOU_AUTH_NAME^forge password $YOU_AUTH_TOKEN" ~/.authinfo
  (use-package forge
    :demand t
    :defines forge-topic-list-columns
    :commands forge-create-pullreq forge-create-issue
    :init
    (setq forge-topic-list-columns
          '(("#" 5 t (:right-align t) number nil)
            ("Title" 60 t nil title  nil)
            ("State" 6 t nil state nil)
            ("Updated" 10 t nill updated nil)))
    (after! ghub-graphql
      ;; Killing recreating the status buffer prevents progress updates from being
      ;; relayed through the modeline. Use `message' instead.
      (setq ghub-graphql-message-progress t))
    :preface
    (setq forge-add-default-bindings (not (fboundp 'evil-mode)))
    :config
    (require 'emacsql-sqlite)
    (setq forge-database-file (concat dotfairy-cache-dir "forge/forge-database.sqlite"))
    ;; All forge list modes are derived from `forge-topic-list-mode'
    (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
    (when (not forge-add-default-bindings)
      (map! :map magit-mode-map [remap magit-browse-thing] #'forge-browse
            :map magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote
            :map magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch))))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode 'yaml-ts-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer")))))
  :config
  (after! git-timemachine
    ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
    ;; showing revision details in the minibuffer, show them in
    ;; `header-line-format', which has better visibility.
    (setq git-timemachine-show-minibuffer-details t)

    ;; TODO PR this to `git-timemachine'
    (defadvice! +vc-support-git-timemachine-a (fn)
      "Allow `browse-at-remote' commands in git-timemachine buffers to open that
file in your browser at the visited revision."
      :around #'browse-at-remote-get-url
      (if git-timemachine-mode
          (let* ((start-line (and (use-region-p) (line-number-at-pos
                                                  (min (region-beginning) (region-end)))))
                 (point-end (and (use-region-p) (max (region-beginning) (region-end))))
                 (end-line (and (use-region-p) (line-number-at-pos point-end)))
                 (remote-ref (browse-at-remote--remote-ref buffer-file-name))
                 (remote (car remote-ref))
                 (ref (car git-timemachine-revision))
                 (relname
                  (file-relative-name
                   buffer-file-name (expand-file-name (vc-git-root buffer-file-name))))
                 (target-repo (browse-at-remote--get-url-from-remote remote))
                 (remote-type (browse-at-remote--get-remote-type (plist-get target-repo :unresolved-host)))
                 (repo-url (plist-get target-repo :url))
                 (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
            (unless url-formatter
              (error (format "Origin repo parsing failed: %s" repo-url)))
            (funcall url-formatter repo-url ref relname
                     (if start-line start-line)
                     (when (and end-line (not (equal start-line end-line)))
                       (if (eq (char-before point-end) ?\n) (- end-line 1) end-line))))
        (funcall fn)))

    (defadvice! +vc-update-header-line-a (revision)
      "Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting revision
info in the `header-line-format' is a more visible indicator."
      :override #'git-timemachine--show-minibuffer-details
      (let* ((date-relative (nth 3 revision))
             (date-full (nth 4 revision))
             (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
             (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
        (setq header-line-format
              (format "%s%s [%s (%s)]"
                      (propertize author 'face 'git-timemachine-minibuffer-author-face)
                      (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                      date-full date-relative))))

    ;; HACK: `delay-mode-hooks' suppresses font-lock-mode in later versions of
    ;;   Emacs, so git-timemachine buffers end up unfontified.
    (add-hook 'git-timemachine-mode-hook #'font-lock-mode)

    (after! evil
      ;; Rehash evil keybindings so they are recognized
      (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

    (when (featurep 'magit-mode)
      (add-transient-hook! #'git-timemachine-blame (require 'magit-blame)))

    (map! :map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "gb"  #'git-timemachine-blame
          :n "gtc" #'git-timemachine-show-commit)))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (fn vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (funcall fn vcs commit-id author message)))
    (advice-add #'git-messenger:format-detail :around #'my-git-messenger:format-detail)

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name " *git-messenger*"))
                 (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                :internal-border-color (face-background 'posframe-border nil t)
                                :foreground-color (face-foreground 'tooltip nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure t
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "nf-oct-diff")
    :color pink :quit-key ("q" "C-g"))
   ("Move"
    (("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))) "recenter next")
     ("N" smerge-next "next")
     ("p" smerge-prev "previous")
     ("g" (progn (goto-char (point-min)) (smerge-next)) "goto first")
     ("G" (progn (goto-char (point-max)) (smerge-prev)) "goto last"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "mine")
     ("o" smerge-keep-lower "other")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "base/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :config (map! :map smerge-mode-map
                :localleader
                "n" #'smerge-next
                "p" #'smerge-prev
                "r" #'smerge-resolve
                "a" #'smerge-keep-all
                "b" #'smerge-keep-base
                "o" #'smerge-keep-lower
                "l" #'smerge-keep-lower
                "m" #'smerge-keep-upper
                "u" #'smerge-keep-upper
                "E" #'smerge-ediff
                "C" #'smerge-combine-with-next
                "R" #'smerge-refine
                "C-m" #'smerge-keep-current
                (:prefix "="
                 "<" #'smerge-diff-base-upper
                 ">" #'smerge-diff-base-lower
                 "=" #'smerge-diff-upper-lower)
                :m "v" #'smerge-mode-hydra/body)

  :hook ((find-file . (lambda ()
                        (unless (bound-and-true-p smerge-mode)
                          (save-excursion
                            (goto-char (point-min))
                            (when (re-search-forward "^<<<<<<< " nil t)
                              (smerge-mode 1))))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("." . browse-at-remote)))

;; Git related modes
(use-package git-modes)

(add-hook! 'git-commit-setup-hook
  (defun +vc-start-in-insert-state-maybe-h ()
    "Start git-commit-mode in insert state if in a blank commit message,
-otherwise in default state."
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p))
               (bobp) (eolp))
      (evil-insert-state))))

(provide 'init-git)
;;; init-git.el ends here

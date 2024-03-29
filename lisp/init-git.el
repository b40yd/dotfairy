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
  :bind
  (("C-x g" . magit-status))
  :config
  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all)
  ;; modeline magit status update, But doing so isn't good for performance
  (setq auto-revert-check-vc-info t)
  (defvar +magit--stale-p nil)

  (defun +magit--revert-buffer (buffer)
    (with-current-buffer buffer
      (kill-local-variable '+magit--stale-p)
      (when buffer-file-name
        (if (buffer-modified-p (current-buffer))
            (when (bound-and-true-p vc-mode)
              (vc-refresh-state)
              (force-mode-line-update))
          (revert-buffer t t t)))))

;;;###autoload
  (defun +magit-mark-stale-buffers-h ()
    "Revert all visible buffers and mark buried buffers as stale.
Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (if (get-buffer-window buffer)
            (+magit--revert-buffer buffer)
          (with-current-buffer buffer
            (setq-local +magit--stale-p t))))))
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

  ;; Unbind M-1, M-2, M-3, and M-4 shortcuts due to conflict with `ace-window'
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map)

  ;; Access Git forges from Magit
  ;; see config: https://magit.vc/manual/ghub/Storing-a-Token.html#Storing-a-Token
  ;; writting like as gitlib.com:
  ;; echo "machine gitlab.com/api/v4 login $YOU_AUTH_NAME^forge password $YOU_AUTH_TOKEN" ~/.authinfo
  (use-package forge
    :demand t
    :defines forge-topic-list-columns
    :commands forge-create-pullreq forge-create-issue
    :init (setq forge-topic-list-columns
                '(("#" 5 t (:right-align t) number nil)
                  ("Title" 60 t nil title  nil)
                  ("State" 6 t nil state nil)
                  ("Updated" 10 t nill updated nil)))
    :config
    (require 'emacsql-sqlite)
    (setq forge-database-file (concat dotfairy-cache-dir "forge/forge-database.sqlite"))
    ;; All forge list modes are derived from `forge-topic-list-mode'
    (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
    (when (not forge-add-default-bindings)
      (map! :map magit-mode-map [remap magit-browse-thing] #'forge-browse-dwim
            :map magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote
            :map magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch))

    (defadvice! +magit--forge-get-repository-lazily-a (&rest _)
      "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."
      :before-while #'forge-get-repository
      (file-executable-p emacsql-sqlite-executable))

    (defadvice! +magit--forge-build-binary-lazily-a (&rest _)
      "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
      :before #'forge-dispatch
      (unless (file-executable-p emacsql-sqlite-executable)
        (emacsql-sqlite-compile 2)
        (if (not (file-executable-p emacsql-sqlite-executable))
            (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                             "See *Compile-Log* buffer for details"))
          ;; HACK Due to changes upstream, forge doesn't initialize completely if
          ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
          ;;      manually after installing it.
          (setq forge--sqlite-available-p t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
          (after! forge-topic
            (dolist (hook forge-bug-reference-hooks)
              (add-hook hook #'forge-bug-reference-setup))))))
    (use-package code-review
      :after magit
      :init
      (setq code-review-db-database-file (concat dotfairy-cache-dir "code-review/code-review-db-file.sqlite")
            code-review-log-file (concat dotfairy-cache-dir "code-review/code-review-error.log")
            code-review-auth-login-marker 'forge
            code-review-log-raw-request-responses t
            code-review-download-dir (expand-file-name "code-review/" dotfairy-cache-dir))

      (defun +magit/start-code-review (arg)
        (interactive "P")
        (call-interactively
         (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
                (repo    (forge-get-repository pullreq))
                (githost (concat (oref repo githost) "/api")))
           (when (forge-gitlab-repository-p repo)
             (setq-default code-review-gitlab-host githost
                           code-review-gitlab-graphql-host githost))
           (if (or arg (not (featurep 'forge)))
               #'code-review-start
             #'code-review-forge-pr-at-point))))
      (transient-append-suffix 'magit-merge "i"
        '("y" "Review pull request" +magit/start-code-review))
      (after! forge
        (transient-append-suffix 'forge-dispatch "c u"
          '("c r" "Review pull request" +magit/start-code-review))))
    )


  ;; (use-package magit-todos
  ;;   :defines magit-todos-nice
  ;;   :commands magit-todos--scan-with-git-grep
  ;;   :init
  ;;   (setq magit-todos-nice (if (executable-find "nice") t nil))
  ;;   (setq magit-todos-scanner #'magit-todos--scan-with-git-grep)
  ;;   (let ((inhibit-message t))
  ;;     (magit-todos-mode 1))
  ;;   :config
  ;;   (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?")
  ;;   (with-eval-after-load 'magit-status
  ;;     (transient-append-suffix 'magit-status-jump '(0 0 -1)
  ;;       '("t " "Todos" magit-todos-jump-to-todos))))

  (use-package magit-gitflow
    :hook (magit-mode . turn-on-magit-gitflow)
    :config
    (define-key magit-mode-map "@" 'magit-gitflow-popup)
    (transient-replace-suffix 'magit-dispatch 'magit-worktree
      '("@" "Gitflow" magit-gitflow-popup))

    (transient-append-suffix 'magit-dispatch '(0 -1 -1)
      '("%" "Worktree" magit-worktree))))


;; Display transient in child frame
(when (childframe-completion-workable-p)
  (use-package transient-posframe
    :diminish
    :custom-face
    (transient-posframe ((t (:inherit tooltip))))
    (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
    :hook (after-init . transient-posframe-mode)
    :init
    (setq transient-posframe-border-width posframe-border-width
          transient-posframe-min-height nil
          transient-posframe-min-width 80
          transient-posframe-poshandler 'posframe-poshandler-frame-center
          transient-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    :config
    (with-no-warnings
      ;; FIXME:https://github.com/yanghaoxie/transient-posframe/issues/5#issuecomment-1974871665
      (defun my-transient-posframe--show-buffer (buffer _alist)
        "Show BUFFER in posframe and we do not use _ALIST at this period."
        (when (posframe-workable-p)
          (let* ((posframe
	              (posframe-show buffer
                                 :height (with-current-buffer buffer (1- (count-screen-lines (point-min) (point-max))))
			                     :font transient-posframe-font
			                     :position (point)
			                     :poshandler transient-posframe-poshandler
			                     :background-color (face-attribute 'transient-posframe :background nil t)
			                     :foreground-color (face-attribute 'transient-posframe :foreground nil t)
			                     :min-width transient-posframe-min-width
			                     :min-height transient-posframe-min-height
			                     :internal-border-width transient-posframe-border-width
			                     :internal-border-color (face-attribute 'transient-posframe-border :background nil t)
			                     :override-parameters transient-posframe-parameters)))
            (frame-selected-window posframe))))
      (advice-add #'transient-posframe--show-buffer :override #'my-transient-posframe--show-buffer)

      (defun my-transient-posframe--hide ()
        "Hide transient posframe."
        (posframe-hide transient--buffer-name))
      (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide))))


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
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

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

    (defun my-git-messenger:format-detail (vcs commit-id author message)
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
        (git-messenger:format-detail vcs commit-id author message)))

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
                                  (my-git-messenger:format-detail vcs commit-id author msg)
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
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                ;; :internal-border-color (face-background 'posframe-border nil t)
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
     (">" smerge-diff-base-lower "upper/lower")
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
  :bind (:map smerge-mode-map
         ("C-c v v" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("." . browse-at-remote)))

;; Git related modes
(use-package git-modes)

(provide 'init-git)
;;; init-git.el ends here

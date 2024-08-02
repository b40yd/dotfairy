;;; init-completion.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2024, b40yd all rights reserved.

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


(defvar +corfu-want-ret-to-confirm t
  "Configure how the user expects RET to behave.
Possible values are:
- t (default): Insert candidate if one is selected, pass-through otherwise;
- `minibuffer': Insert candidate if one is selected, pass-through otherwise,
              and immediatelly exit if in the minibuffer;
- nil: Pass-through without inserting.")

(defvar +corfu-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-dabbrev'.")

(defvar +corfu-want-minibuffer-completion t
  "Whether to enable Corfu in the minibuffer.
Setting this to `aggressive' will enable Corfu in more commands which
use the minibuffer such as `query-replace'.")

;; Auto completion
(use-package corfu
  :bind ("<backtab>" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :init
  (add-hook! 'minibuffer-setup-hook
    (defun +corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer."
      (when (pcase +corfu-want-minibuffer-completion
              ('aggressive
               (not (or (bound-and-true-p mct--active)
                        (bound-and-true-p vertico--input)
                        (eq (current-local-map) read-passwd-map)
                        (and (featurep 'ido) (ido-active))
                        (where-is-internal 'minibuffer-complete
                                           (list (current-local-map)))
                        (memq #'ivy--queue-exhibit post-command-hook))))
              ('nil nil)
              (_ (where-is-internal #'completion-at-point
                                    (list (current-local-map)))))
        (setq-local corfu-echo-delay nil)
        (corfu-mode +1))))
  :config

  ;;;###autoload
  (defun +corfu/move-to-minibuffer ()
    "Move the current list of candidates to your choice of minibuffer completion UI."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (cond ((and (bound-and-true-p vertico-mode)
                     (fboundp #'consult-completion-in-region))
                (consult-completion-in-region beg end table pred))
               ((and (bound-and-true-p ivy-mode)
                     (fboundp #'ivy-completion-in-region))
                (ivy-completion-in-region (marker-position beg) (marker-position end) table pred))
               ;; Important: `completion-in-region-function' is set to corfu at
               ;; this moment, so `completion-in-region' (single -) doesn't work
               ;; below.
               ((bound-and-true-p ido-mode)
                (completion--in-region beg end table pred))
               (t (error "No minibuffer completion UI available for moving to!")))))))

;;;###autoload
  (defun +corfu/smart-sep-toggle-escape ()
    "Insert `corfu-separator' or toggle escape if it's already there."
    (interactive)
    (cond ((and (char-equal (char-before) corfu-separator)
                (char-equal (char-before (1- (point))) ?\\))
           (save-excursion (delete-char -2)))
          ((char-equal (char-before) corfu-separator)
           (save-excursion (backward-char 1)
                           (insert-char ?\\)))
          (t (call-interactively #'corfu-insert-separator))))

  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        global-corfu-modes '((not
                              erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              vterm-mode)
                             t)
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (or (featurep 'vertico)
                                       (featurep 'orderless))
                                   'separator t)
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))
  (add-to-list 'corfu-auto-commands #'lispy-colon)
  (add-to-list 'corfu-continue-commands #'+corfu/move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'+corfu/smart-sep-toggle-escape)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  ;; If you want to update the visual hints after completing minibuffer commands
  ;; with Corfu and exiting, you have to do it manually.
  (defadvice! +corfu--insert-before-exit-minibuffer-a ()
    :before #'exit-minibuffer
    (when (or (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))
              (and (featurep 'corfu-terminal)
                   (popon-live-p corfu-terminal--popon)))
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  ;; HACK: If your dictionaries aren't set up in text-mode buffers, ispell will
  ;;   continuously pester you about errors. This ensures it only happens once
  ;;   per session.
  (defadvice! +corfu--auto-disable-ispell-capf-a (fn &rest args)
    "If ispell isn't properly set up, only complain once per session."
    :around #'ispell-completion-at-point
    (condition-case-unless-debug e
        (apply fn args)
      ('error
       (message "Error: %s" (error-message-string e))
       (message "Auto-disabling `text-mode-ispell-word-completion'")
       (setq text-mode-ispell-word-completion nil)
       (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t))))
  )

(unless (display-graphic-p)
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  (add-hook! prog-mode
    (defun +corfu-add-cape-file-h ()
      (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (add-hook! (org-mode markdown-mode)
    (defun +corfu-add-cape-elisp-block-h ()
      (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t)))
  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (setq cape-dabbrev-check-other-buffers t)
  ;; Set up `cape-dabbrev' options.
  (defun +dabbrev-friend-buffer-p (other-buffer)
    (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))
  (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup
                        eshell-mode)
    (defun +corfu-add-cape-dabbrev-h ()
      (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))
  (after! dabbrev
    (setq dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
          dabbrev-ignored-buffer-regexps
          '("\\` "
            "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
          dabbrev-upcase-means-case-search t)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

  ;; Make these capfs composable.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
  ;; Emacs28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))


(use-package yasnippet-capf
  :defer t
  :init
  (add-hook! 'yas-minor-mode-hook
    (defun +corfu-add-yasnippet-capf-h ()
      (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))))

(provide 'init-completion)
;;; init-completion.el ends here

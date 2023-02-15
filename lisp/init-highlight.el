;;; init-highlight.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
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

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :defines helpful-mode-map
  :hook ((html-mode css-mode php-mode helpful-mode) . rainbow-mode)
  :bind (:map special-mode-map
         ("w" . rainbow-mode))
  :init (with-eval-after-load 'helpful
          (bind-key "w" #'rainbow-mode helpful-mode-map))
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

;; Color picker https://github.com/ncruces/zenity/releases
;; Emacs not support xwidgets use zenity.

(unless (featurep 'xwidget-internal)
  (use-package webkit-color-picker
    :ensure t
    :bind (("C-c C-p" . webkit-color-picker-show)))
  (if (executable-find "zenity")
      (use-package zenity-color-picker
        :bind (("C-c C-p" . zenity-cp-color-at-point-dwim)))))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :config
  (setq beacon-color "#9b4086"))

;; Highlight TODO
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#e45649")))
  (dolist (keyword '("HACK" "TRICK" "WORKAROUND"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#d0bf8f")))
  (dolist (keyword '("DEBUG" "STUB"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#7cb8bb")))
  (map! :localleader
        :map hl-todo-mode-map
        (:prefix ("t" . "TODO")
         "C-o" #'hl-todo-occur
         "C-p" #'hl-todo-previous
         "C-n" #'hl-todo-next
         "C-o" #'hl-todo-occur
         "C-i" #'hl-todo-insert)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if IS-LINUX #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; Pulse current line
(use-package pulse
  :ensure nil
  :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window switch-to-buffer
                   aw-select toggle-window-split
                   windmove-do-window-select
                   pager-page-down pager-page-up
                   treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))

;; Highlight indentions
(use-package highlight-indent-guides
  :diminish
  :hook ((prog-mode yaml-mode) . (lambda ()
                                   "Highlight indentations in small files for better performance."
                                   (unless (too-long-file-p)
                                     (highlight-indent-guides-mode 1))))
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-suppress-auto-error t)
  :config
  (with-no-warnings
    ;; Don't display first level of indentation
    (defun my-indent-guides-for-all-but-first-column (level responsive display)
      (unless (> 0 level)
        (highlight-indent-guides--highlighter-default level responsive display)))
    (setq highlight-indent-guides-highlighter-function
          #'my-indent-guides-for-all-but-first-column)

    ;; Disable in `macrostep' expanding
    (with-eval-after-load 'macrostep
      (advice-add #'macrostep-expand
                  :after (lambda (&rest _)
                           (when highlight-indent-guides-mode
                             (highlight-indent-guides-mode -1))))
      (advice-add #'macrostep-collapse
                  :after (lambda (&rest _)
                           (when (derived-mode-p 'prog-mode 'yaml-mode)
                             (highlight-indent-guides-mode 1)))))

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (when (bound-and-true-p ivy-mode)
      (with-eval-after-load 'ivy
        (defun my-ivy-cleanup-indentation (str)
          "Clean up indentation highlighting in ivy minibuffer."
          (let ((pos 0)
                (next 0)
                (limit (length str))
                (prop 'highlight-indent-guides-prop))
            (while (and pos next)
              (setq next (text-property-not-all pos limit prop nil str))
              (when next
                (setq pos (text-property-any next limit prop nil str))
                (ignore-errors
                  (remove-text-properties next pos '(display nil face nil) str))))))
        (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation)))))

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg ))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))

;; Pulse modified region
(if (>= emacs-major-version 27)
    (use-package goggles
      :diminish
      :hook ((prog-mode text-mode) . goggles-mode))
  (use-package volatile-highlights
    :diminish
    :hook (after-init . volatile-highlights-mode)))

(provide 'init-highlight)
;;; init-highlight.el ends here

;;; init-ui.el ---                                   -*- lexical-binding: t; -*-

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

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Title
(setq frame-title-format '("DotFairy Emacs - %b")
      icon-title-format frame-title-format)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Menu/Tool/Scroll bars
;; Disable tool, menu, and scrollbars. Doom is designed to be keyboard-centric,
;; so these are just clutter (the scrollbar also impacts performance). Whats
;; more, the menu bar exposes functionality that Doom doesn't endorse.
(unless (>= emacs-major-version 27)
  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Settings for UI theme
;; theme:
;;     doom-monokai-classic
;;     doom-snazzy
;;     doom-one-light
;;     doom-dark+
(use-package doom-themes
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :init (dotfairy-load-theme dotfairy-theme t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable customized theme
  ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config)))

;; Mode-line
(use-package doom-modeline
  :custom
  (doom-modeline-icon display-icon)
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-encoding t)
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil))
  )

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Settings for delete multi line spaces
(use-package emacs
  :bind ((("M-/" . comment-line)
          ("M-?" . comment-or-uncomment-region)))
  :hook ((before-save . delete-trailing-whitespace)
         (after-init . delete-selection-mode))
  ;; Settings for the TAB behavior

  :init (setq-default tab-width 4
                      indent-tabs-mode nil
                      display-time-24hr-format t
                      display-time-day-and-date t)
  ;; Display time
  (display-time-mode 1))

;; need install all-the-icons fonts
;; web site https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :init (unless (or IS-WINDOWS (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t))
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

;; Settings for highlight parentheses
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-set-bar 'over
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-height 24
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "•")
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(when dotfairy-dashboard
  (use-package dashboard
    :ensure t
    :init
    (dashboard-setup-startup-hook)
    :config
    (setq dashboard-startup-banner (or dotfairy-logo 'official)
          dashboard-set-heading-icons t
          dashboard-center-content t
          dashboard-set-file-icons t
          dashboard-set-footer t
          dashboard-footer-icon (cond ((icons-displayable-p)
                                       (all-the-icons-faicon "heart"
                                                             :height 1.1
                                                             :v-adjust -0.05
                                                             :face 'error))
                                      ((char-displayable-p ?🧡) "🧡 ")
                                      (t (propertize ">" 'face 'dashboard-footer)))
          dashboard-items '((recents . 5)
                            (projects . 5)))))

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
              ("C-=" . default-text-scale-increase)
              ("C--" . default-text-scale-decrease)
              ("C-0" . default-text-scale-reset)))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

(use-package whitespace :defer t
  :config
  (setq whitespace-line-column nil)  ;When nil, set the value to `fill-column'
  (setq whitespace-style
        '(face
          trailing                    ;White space at end of lines
          tabs                        ;tab-mark ;`tab-mark' shows tabs as '»'
          spaces space-mark           ;`space-mark' shows spaces as '.'
          space-before-tab space-after-tab ;Mix of tabs and spaces
          ;; lines   ;highlight lines that extend beyond `whitespace-line-column'
          lines-tail ;highlight only characters beyond `whitespace-line-column'
          ;; newline newline-mark
          ;; empty ;blank lines at BOB or EOB
          indentation)) ;highlight spaces/tabs at BOL depending on `indent-tabs-mode'
  )

;;;###autoload
(defun dotfairy/toggle-line-numbers ()
  "Toggle line numbers.
Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.
See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar dotfairy--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq dotfairy--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq dotfairy--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))
(provide 'init-ui)
;;; init-ui.el ends here

;;; init-ui.el ---                                   -*- lexical-binding: t; -*-

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

;; Optimization
(setq-default cursor-in-non-selected-windows nil
              tab-width 4
              indent-tabs-mode nil)
(setq idle-update-delay 1.0
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.628)
                            (height . 0.8)
                            (fullscreen)))

;; Title
(setq frame-title-format '("DotFairy Emacs - %b")
      icon-title-format frame-title-format)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen)

  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (if (display-graphic-p)
                  (menu-bar-mode 1)
                (menu-bar-mode -1))))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

(use-package time
  :hook (after-init . display-time-mode)
  :init (setq display-time-default-load-average nil
              display-time-format "%H:%M"))

(use-package solarized-theme)

;; Settings for UI theme
;; theme:
;;     doom-monokai-classic
;;     doom-snazzy
;;     doom-one-light
;;     doom-dark+
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :init
  (dotfairy-load-theme dotfairy-theme t))


;; Mode-line
(use-package doom-modeline
  :autoload (+modeline-update-env-in-all-windows-h +modeline-clear-env-in-all-windows-h)
  :bind (:map doom-modeline-mode-map
         ("<f6>" . doom-modeline-hydra/body))
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon display-icon
        doom-modeline-minor-modes t)
  :config
;;;###autoload
  (defun +modeline-update-env-in-all-windows-h (&rest _)
    "Update version strings in all buffers."
    (dolist (window (window-list))
      (with-selected-window window
        (when (fboundp 'doom-modeline-update-env)
          (doom-modeline-update-env))
        (force-mode-line-update))))

  :pretty-hydra
  ((:title (pretty-hydra-title "Mode Line" 'sucicon "nf-custom-emacs" :face 'nerd-icons-purple)
    :color amaranth :quit-key ("q" "C-g"))
   ("Icon"
    (("i" (setq doom-modeline-icon (not doom-modeline-icon))
      "display icons" :toggle doom-modeline-icon)
     ("u" (setq doom-modeline-unicode-fallback (not doom-modeline-unicode-fallback))
      "unicode fallback" :toggle doom-modeline-unicode-fallback)
     ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
      "major mode" :toggle doom-modeline-major-mode-icon)
     ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
      "colorful major mode" :toggle doom-modeline-major-mode-color-icon)
     ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
      "buffer state" :toggle doom-modeline-buffer-state-icon)
     ("o" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
      "modification" :toggle doom-modeline-buffer-modification-icon)
     ("v" (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
      "modal" :toggle doom-modeline-modal-icon))
    "Segment"
    (("H" (setq doom-modeline-hud (not doom-modeline-hud))
      "hud" :toggle doom-modeline-hud)
     ("M" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
      "minor modes" :toggle doom-modeline-minor-modes)
     ("W" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
      "word count" :toggle doom-modeline-enable-word-count)
     ("E" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
      "encoding" :toggle doom-modeline-buffer-encoding)
     ("I" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
      "indent" :toggle doom-modeline-indent-info)
     ("C" (setq doom-modeline-display-misc-in-all-mode-lines (not doom-modeline-display-misc-in-all-mode-lines))
      "misc info" :toggle doom-modeline-display-misc-in-all-mode-lines)
     ("L" (setq doom-modeline-lsp (not doom-modeline-lsp))
      "lsp" :toggle doom-modeline-lsp)
     ("P" (setq doom-modeline-workspace-name (not doom-modeline-workspace-name))
      "workspace" :toggle doom-modeline-workspace-name)
     ("G" (setq doom-modeline-github (not doom-modeline-github))
      "github" :toggle doom-modeline-github)
     ("N" (setq doom-modeline-gnus (not doom-modeline-gnus))
      "gnus" :toggle doom-modeline-gnus)
     ("U" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
      "mu4e" :toggle doom-modeline-mu4e)
     ("R" (setq doom-modeline-irc (not doom-modeline-irc))
      "irc" :toggle doom-modeline-irc)
     ("F" (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
      "irc buffers" :toggle doom-modeline-irc-buffers)
     ("S" (progn
            (setq doom-modeline-check-simple-format (not doom-modeline-check-simple-format))
            (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
      "simple checker" :toggle doom-modeline-check-simple-format)
     ("T" (setq doom-modeline-time (not doom-modeline-time))
      "time" :toggle doom-modeline-time)
     ("V" (setq doom-modeline-env-version (not doom-modeline-env-version))
      "version" :toggle doom-modeline-env-version))
    "Style"
    (("a" (setq doom-modeline-buffer-file-name-style 'auto)
      "auto"
      :toggle (eq doom-modeline-buffer-file-name-style 'auto))
     ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
      "buffer name"
      :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
     ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
      "file name"
      :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
     ("F" (setq doom-modeline-buffer-file-name-style 'file-name-with-project)
      "file name with project"
      :toggle (eq doom-modeline-buffer-file-name-style 'file-name-with-project))
     ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
      "truncate upto project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
     ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
      "truncate from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
     ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
      "truncate with project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
     ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
      "truncate except project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
     ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
      "truncate upto root"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
     ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
      "truncate all"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
     ("t n" (setq doom-modeline-buffer-file-name-style 'truncate-nil)
      "truncate none"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-nil))
     ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      "relative from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
     ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
      "relative to project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
    "Project Detection"
    (("p f" (setq doom-modeline-project-detection 'ffip)
      "ffip"
      :toggle (eq doom-modeline-project-detection 'ffip))
     ("p t" (setq doom-modeline-project-detection 'projectile)
      "projectile"
      :toggle (eq doom-modeline-project-detection 'projectile))
     ("p p" (setq doom-modeline-project-detection 'project)
      "project"
      :toggle (eq doom-modeline-project-detection 'project))
     ("p n" (setq doom-modeline-project-detection nil)
      "disable"
      :toggle (eq doom-modeline-project-detection nil)))
    "Misc"
    (("n" (progn
            (message "Fetching GitHub notifications...")
            (run-with-timer 300 nil #'doom-modeline--github-fetch-notifications)
            (browse-url "https://github.com/notifications"))
      "github notifications" :exit t)
     ("e" (if (bound-and-true-p flycheck-mode)
              (flycheck-list-errors)
            (flymake-show-diagnostics-buffer))
      "list errors" :exit t)
     ("w" (if (bound-and-true-p grip-mode)
              (grip-browse-preview)
            (message "Not in preview"))
      "browse preview" :exit t)
     ("z h" (set-from-minibuffer 'doom-modeline-height)
      "set height" :exit t)
     ("z w" (set-from-minibuffer 'doom-modeline-bar-width)
      "set bar width" :exit t)
     ("z g" (set-from-minibuffer 'doom-modeline-github-interval)
      "set github interval" :exit t)
     ("z n" (set-from-minibuffer 'doom-modeline-gnus-timer)
      "set gnus interval" :exit t)))))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Settings for delete multi line spaces
(use-package emacs
  :bind ((("M-/" . comment-line)
          ("M-?" . comment-or-uncomment-region)))
  :hook ((after-init . delete-selection-mode))
  :config
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :commands (dotfairy/toggle-line-numbers)
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t)
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
                 (_ (symbol-name next)))))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Settings for highlight parentheses
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(when dotfairy-dashboard
  (use-package dashboard
    :diminish dashboard-mode
    :hook (dashboard-mode . (lambda ()
                              ;; No title
                              (setq-local frame-title-format nil)
                              ;; Enable `page-break-lines-mode'
                              (when (fboundp 'page-break-lines-mode)
                                (page-break-lines-mode 1))))
    :functions (nerd-icons-faicon
                nerd-icons-mdicon
                winner-undo
                widget-forward)
    :custom-face
    (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    (dashboard-items-face ((t (:weight normal))))
    (dashboard-no-items-face ((t (:weight normal))))
    :init
    (dashboard-setup-startup-hook)
    :config
    (setq dashboard-startup-banner (or dotfairy-logo 'official)
          dashboard-set-heading-icons t
          dashboard-center-content t
          dashboard-vertically-center-content t
          dashboard-set-file-icons t
          ;; dashboard-set-footer t
          ;; dashboard-set-navigator t
          dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-newline
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-newline
                                      dashboard-insert-footer)
          dashboard-projects-backend 'projectile
          dashboard-path-style 'truncate-middle
          dashboard-path-max-length 60
          dashboard-display-icons-p #'icons-displayable-p
          dashboard-icon-type 'nerd-icons
          dashboard-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda    . "nf-oct-calendar")
                                    (projects  . "nf-oct-briefcase")
                                    (registers . "nf-oct-database"))
          dashboard-footer-icon
          (if (icons-displayable-p)
              (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
            (propertize ">" 'face 'dashboard-footer))
          dashboard-items '((recents . 10)
                            (projects . 10)))))

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

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (unless (not (eq system-type 'darwin))
    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

;; Smooth scrolling over images
(when emacs/26
  (use-package iscroll
    :diminish
    :hook (image-mode . iscroll-mode)))

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package whitespace :defer t
  :config
  (setq whitespace-line-column nil)  ;When nil, set the value to `fill-column'
  (setq whitespace-style
        '(trailing                    ;White space at end of lines
          tabs                        ;tab-mark ;`tab-mark' shows tabs as '»'
          spaces space-mark           ;`space-mark' shows spaces as '.'
          space-before-tab space-after-tab ;Mix of tabs and spaces
          ;; lines   ;highlight lines that extend beyond `whitespace-line-column'
          lines-tail ;highlight only characters beyond `whitespace-line-column'
          ;; newline newline-mark
          ;; empty ;blank lines at BOB or EOB
          ;; highlight spaces/tabs at BOL depending on `indent-tabs-mode'
          indentation)))


(provide 'init-ui)
;;; init-ui.el ends here

;;; init-windows.el ---                              -*- lexical-binding: t; -*-

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

;; Frame
(defvar dotfairy/frame--geometry nil)
(defun dotfairy/frame--save-geometry ()
  "Save current frame's geometry."
  (setq-local dotfairy/frame--geometry
              `((left . ,(frame-parameter nil 'left))
                (top . ,(frame-parameter nil 'top))
                (width . ,(frame-parameter nil 'width))
                (height . ,(frame-parameter nil 'height))
                (fullscreen))))

(defun dotfairy/frame--fullscreen-p ()
  "Returns Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun dotfairy/frame-maximize ()
  "Maximize the frame."
  (interactive)
  (dotfairy/frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun dotfairy/frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil dotfairy/frame--geometry))

(defun dotfairy/frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (dotfairy/frame--fullscreen-p)
    (dotfairy/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun dotfairy/frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (dotfairy/frame--fullscreen-p)
    (dotfairy/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun dotfairy/frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (dotfairy/frame--fullscreen-p)
    (dotfairy/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun dotfairy/frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (dotfairy/frame--fullscreen-p)
    (dotfairy/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))


;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . (lambda ()
                        (windmove-default-keybindings 'super))))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Quickly switch windows
(use-package ace-window
  ;; :bind (("M-o" . 'ace-window))
  :pretty-hydra
  ((:title (pretty-hydra-title "Window Management" 'faicon "nf-fa-th")
    :foreign-keys warn :quit-key ("q" "C-g"))
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete" :exit t)
     ("m" ace-delete-other-windows "maximize" :exit t)
     ("s" ace-swap-window "swap" :exit t)
     ("a" ace-select-window "select" :exit t)
     ("f" toggle-frame-fullscreen "fullscreen" :exit t))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance"))
    "Split"
    (("b" split-window-right "horizontally")
     ("B" split-window-horizontally "horizontally")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically "vertically")
     ("t" toggle-window-split "toggle"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))
    "Appearance"
    (("F" set-frame-font "font")
     ("P" treemacs "treemacs")
     ("T" counsel-load-theme "theme"))))
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind (([remap other-window] . ace-window))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  ;; Bind hydra to dispatch list
  (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)

  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number (string-to-number (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))

;; Enforce rules for popups
(when emacs/26
  (use-package popper
    :defines popper-echo-dispatch-actions
    :autoload popper-group-by-directory
    :bind (:map popper-mode-map
           ("C-h z" . popper-toggle-latest)
           ("C-<tab>"   . popper-cycle)
           ("C-M-<tab>" . popper-toggle-type))
    :hook (after-init . popper-mode)
    :init
    (setq popper-group-function #'popper-group-by-directory)
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$" "\\*Pp Eval Output\\*$"
            "\\*Compile-Log\\*"
            "\\*Completions\\*"
            "\\*Warnings\\*"
            "\\*Async Shell Command\\*"
            "\\*Apropos\\*"
            "\\*Backtrace\\*"
            "\\*Calendar\\*"
            "\\*Finder\\*"
            "\\*Kill Ring\\*"
            "\\*Go-Translate\\*"

            bookmark-bmenu-mode
            comint-mode
            compilation-mode
            help-mode helpful-mode
            tabulated-list-mode
            Buffer-menu-mode

            flymake-diagnostics-buffer-mode
            flycheck-error-list-mode flycheck-verify-mode

            gnus-article-mode devdocs-mode
            grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
            ivy-occur-mode ivy-occur-grep-mode
            youdao-dictionary-mode osx-dictionary-mode fanyi-mode

            "^\\*Process List\\*" process-menu-mode
            list-environment-mode cargo-process-mode

            "^\\*eshell.*\\*$"         eshell-mode
            "^\\*shell.*\\*$"          shell-mode
            "^\\*term.*\\*$"           term-mode
            "^\\*vterm[inal]*.*\\*.*$" vterm-mode

            "\\*DAP Templates\\*$" dap-server-log-mode
            "\\*ELP Profiling Restuls\\*" profiler-report-mode
            "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
            "\\*[Wo]*Man.*\\*$"
            "\\*ert\\*$" overseer-buffer-mode
            "\\*gud-debug\\*$"
            "\\*lsp-help\\*$" "\\*lsp session\\*$"
            "\\*quickrun\\*$"
            "\\*tldr\\*$"
            "\\*vc-.*\\*$"
            "^\\*macro expansion\\**"

            "\\*Agenda Commands\\*" "\\*Org Agenda.*\\*"
            "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
            "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
            "\\*docker-.+\\*"
            "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
            "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
            rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))
    
    (with-eval-after-load 'doom-modeline
      (setq popper-mode-line
            '(:eval (let ((face (if (doom-modeline--active)
                                    'mode-line-emphasis
                                  'mode-line-inactive)))
                      (if (and (icons-displayable-p)
                               (bound-and-true-p doom-modeline-mode))
                          (format " %s " (nerd-icons-octicon "nf-oct-pin" :face face))
                        (propertize " POP" 'face face))))))
    (setq popper-echo-dispatch-actions t)
    :config
    (popper-echo-mode 1)
    (with-no-warnings
      (defun my-popper-fit-window-height (win)
        "Determine the height of popup window WIN by fitting it to the buffer's content."
        (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 3)))
      (setq popper-window-height #'my-popper-fit-window-height)

      (defun popper-close-window-hack (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-hack))))

  (provide 'init-windows)
;;; init-windows.el ends here

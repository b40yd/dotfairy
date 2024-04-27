;;; init-evil.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2024, b40yd, all rights reserved.

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

(use-package evil
  :commands (evil-set-initial-state evil-ex-define-cmd)
  :init (evil-mode +1)
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        ;; evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        ;; evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system (if (> emacs-major-version 27) 'undo-redo))

  (defadvice! +evil--persist-state-a (fn &rest args)
    "When changing major modes, Evil's state is lost. This advice preserves it."
    :around #'set-auto-mode
    (if evil-state
        (evil-save-state (apply fn args))
      (apply fn args)))

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; PERF: Stop copying the selection to the clipboard each time the cursor
  ;; moves in visual mode. Why? Because on most non-X systems (and in terminals
  ;; with clipboard plugins like xclip.el active), Emacs will spin up a new
  ;; process to communicate with the clipboard for each movement. On Windows,
  ;; older versions of macOS (pre-vfork), and Waylang (without pgtk), this is
  ;; super expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; UX: It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)
  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  )

;; add package evil-collection
(use-package evil-collection
  :defer 2
  ;; :disabled
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (setq evil-collection-outline-bind-tab-p nil
        evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list (delete 'company evil-collection-mode-list))
  (evil-collection-init))

;; gcc comments out a line
;; gc comments out the target of a motion
;; gcap comments out a paragraph
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :init (evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :diminish evil-matchit-mode
  :config (global-evil-matchit-mode t))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :hook (after-init . global-evil-mc-mode)
  :commands (evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-mode
             evil-mc-undo-all-cursors
             global-evil-mc-mode)
  :init (global-evil-mc-mode 1))

(use-package evil-mc-extras
  :after evil-mc
  :commands global-evil-mc-extras-mode
  :diminish evil-mc-extras-mode
  :init (global-evil-mc-extras-mode 1))

(use-package evil-escape
  :commands evil-escape
  :hook (after-init . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook! 'evil-escape-inhibit-functions
    (defun +evil-inhibit-escape-in-minibuffer-fn ()
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p))))))

(use-package evil-traces
  :after evil-ex
  :config
  ;;;###autoload (autoload '+evil:align "editor/evil/autoload/ex" nil t)
  (evil-define-command +evil:align (beg end pattern &optional flags)
    "Ex interface to `align-regexp'.

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
    (interactive "<r></>")
    (align-regexp
     beg end
     (concat "\\(\\s-*\\)" (evil-transform-vim-style-regexp pattern))
     1 1 (memq ?g flags)))

;;;###autoload (autoload '+evil:align-right "editor/evil/autoload/ex" nil t)
  (evil-define-command +evil:align-right (beg end pattern &optional flags)
    "Ex interface to `align-regexp' that right-aligns matches.

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
    (interactive "<r></>")
    (align-regexp
     beg end
     (concat "\\(" (evil-transform-vim-style-regexp pattern) "\\)")
     -1 1 (memq ?g flags)))
  (pushnew! evil-traces-argument-type-alist
            '(+evil:align . evil-traces-global)
            '(+evil:align-right . evil-traces-global)
            '(+multiple-cursors:evil-mc . evil-traces-substitute))
  (evil-traces-mode))

(provide 'init-evil)
;;; init-evil.el ends here

;;; init-iedit.el ---                                -*- lexical-binding: t; -*-

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
(require 'init-funcs)


(global-unset-key (kbd "M-<drag-mouse-1>"))
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-unset-key (kbd "M-<mouse-3>"))

;; On-the-fly spell checker
(use-package flyspell
  :ensure t
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after ivy
    :bind (:map flyspell-mode-map
           ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
    :init (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(when emacs/27
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure t
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :autoload drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (:map text-mode-map
         ("<C-return>" . rect-hydra/body)
         :map prog-mode-map
         ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<C-M-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wgrep
    (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'mdicon "nf-md-border_all")
    :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Visual `align-regexp'
(use-package ialign)

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil)
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Treat undo history as a tree
(if emacs/27
    (use-package vundo
      :bind ("C-x u" . vundo)
      :config (setq vundo-glyph-alist vundo-unicode-symbols))
  (use-package undo-tree
    :diminish
    :hook (after-init . global-undo-tree-mode)
    :init (setq undo-tree-visualizer-timestamps t
                undo-tree-visualizer-diff t
                undo-tree-enable-undo-in-region nil
                undo-tree-auto-save-history nil)))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (("C-c +" . er/expand-region)
         ("C-c -" . er/contract-region)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-M-<mouse-1>" . mc/add-cursor-on-click)
         ("<mouse-1>" . mc/keyboard-quit)
         ("C-M->" . mc/unmark-next-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("<double-mouse-1>" . mouse-set-point)
         ("M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("M-|" . mc/vertical-align-with-space)
         ("C-c m" . multiple-cursors-hydra/body)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space))
  :pretty-hydra
  ((:title (pretty-hydra-title "Multiple-Cursors" 'mdicon "nf-md-cursor_move")
    :color amaranth :quit-key ("q" "C-g"))
   ("Actions"
    (
     ("@" mc/edit-lines               "edit lines")
     ("," mc/skip-to-next-like-this                   "skip to next like this")
     ("." mc/skip-to-previous-like-this               "skip to previous like this")
     ("/" mc/mark-pop "Mark pop")
     (";" mc/mark-all-words-like-this                 "Only mark all matches word")
     ("[" mc/mark-next-word-like-this                 "Only mark next like this world")
     ("]" mc/mark-previous-word-like-this             "Only mark previous like this word")
     ("+" mc/mark-all-words-like-this-in-defun        "Only mark all current function matches word")
     ("f" mc/mark-all-like-this-in-defun              "Mark all current function matches the current region")
     )
    "---"
    (
     ("m" mc/mark-all-like-this                       "Mark all matches the current region")
     ("n" mc/mark-next-like-this                      "Add cursor to next line")
     ("M" mc/mark-all-dwim                            "Smart mark all")
     ("N" mc/unmark-next-like-this                    "Unmark next like this")
     ("p" mc/mark-previous-like-this                  "Add cursor to previous line")
     ("P" mc/unmark-previous-like-this                "Unmark previous like this")
     ("w" mc/mark-next-like-this-word                 "Mark next like this word")
     ("W" mc/mark-previous-like-this-word             "Mark previous like this word")
     ("<mouse-1>" mc/add-cursor-on-click              "Bind to a mouse event to add cursors by clicking")
     )))
  :config
  (defun my-append-to-list (list-var elements)
    "Append ELEMENTS to the end of LIST-VAR.
The return value is the new value of LIST-VAR."
    (unless (consp elements)
      (error "ELEMENTS must be a list"))
    (let ((list (symbol-value list-var)))
      (if list
          (setcdr (last list) elements)
        (set list-var elements)))
    (symbol-value list-var))

  (with-eval-after-load 'multiple-cursors-core
    (my-append-to-list 'mc--default-cmds-to-run-once '(swiper-mc
                                                       counsel-M-x
                                                       delete-horizontal-space
                                                       mouse-drag-region-rectangle
                                                       multiple-cursors-hydra/body
                                                       multiple-cursors-hydra/mc/mark-next-like-this
                                                       multiple-cursors-hydra/mc/mark-next-like-this-word
                                                       multiple-cursors-hydra/mc/mark-next-word-like-this
                                                       multiple-cursors-hydra/mc/mark-previous-like-this
                                                       multiple-cursors-hydra/mc/mark-previous-like-this-word
                                                       multiple-cursors-hydra/mc/mark-previous-word-like-this
                                                       multiple-cursors-hydra/mc/add-cursor-on-click
                                                       multiple-cursors-hydra/mc/mark-pop
                                                       multiple-cursors-hydra/mc/unmark-next-like-this
                                                       multiple-cursors-hydra/mc/unmark-previous-like-this
                                                       multiple-cursors-hydra/mc/skip-to-next-like-this
                                                       multiple-cursors-hydra/mc/skip-to-previous-like-this
                                                       multiple-cursors-hydra/mc/edit-lines
                                                       multiple-cursors-hydra/mc/mark-all-like-this
                                                       multiple-cursors-hydra/mc/mark-all-words-like-this
                                                       multiple-cursors-hydra/mc/mark-all-like-this-in-defun
                                                       multiple-cursors-hydra/mc/mark-all-words-like-this-in-defun
                                                       multiple-cursors-hydra/mc/mark-all-dwim
                                                       multiple-cursors-hydra/nil))

    (my-append-to-list 'mc--default-cmds-to-run-for-all '(mc/vertical-align-with-space
                                                          mwim-beginning-of-code-or-line-or-comment
                                                          mwim-end-of-code-or-line))
    (mc/save-lists))
  (with-eval-after-load 'hungry-delete
    (add-to-list 'mc--default-cmds-to-run-for-all 'hungry-delete-backward)
    (mc/save-lists))
  (setq mc/list-file (concat dotfairy-etc-dir "mc-lists.el")))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))


(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; NOTE: Disable in large files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (when (too-long-file-p)
                          (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode
                  asm-mode web-mode html-mode css-mode
                  go-mode scala-mode
                  shell-mode term-mode vterm-mode
                  prolog-inferior-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish (beginend-mode beginend-global-mode)
  :hook (after-init . beginend-global-mode)
  :config
  (mapc (lambda (pair)
          (add-hook (car pair) (lambda () (diminish (cdr pair)))))
        beginend-modes))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (
         ("C-c e g l" . avy-goto-line)
         ("C-c e g c 1" . avy-goto-char)
         ("C-c e g c 2" . avy-goto-char-2)
         ("C-c e g w 1" . avy-goto-word-1)
         ("C-c e g w 2" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'pre))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package link-hint
  :bind (("M-o" . link-hint-open-link)
         ("C-c l o" . link-hint-open-link)
         ("C-c l c" . link-hint-copy-link)))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)
         :map prog-mode-map
         ("C-a" . mwim-beginning-of-code-or-line-or-comment)
         ("C-e" . mwim-end-of-code-or-line)))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Record and jump to the last point in the buffer
(use-package goto-last-point
  :diminish
  :bind ("C-." . goto-last-point)
  :hook (after-init . goto-last-point-mode))

;; Jump to definition
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
    :color blue :quit-key ("q" "C-g"))
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Flexible text folding
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
    :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖"))
    ""
    (("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind (:map hs-minor-mode-map
         ("M-`" . hideshow-hydra/body))
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

(unless IS-WINDOWS
  (use-package sudo-edit))

(provide 'init-iedit)
;;; init-iedit.el ends here

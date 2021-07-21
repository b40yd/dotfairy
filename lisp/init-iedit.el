;;; init-iedit.el ---                                -*- lexical-binding: t; -*-

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

;; On-the-fly spell checker
(use-package flyspell
  :ensure t
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
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

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure t
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; delete hungry
(use-package hungry-delete)

;; ;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
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
  :init (with-eval-after-load 'org
          (bind-key "<C-M-return>" #'rect-hydra/body org-mode-map))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'material "border_all" :height 1.1 :v-adjust -0.225)
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

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; edit undo tree
(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package rainbow-mode
  :hook ((html-mode css-mode) . rainbow-mode)
  :bind (("C-x c m" . rainbow-mode-hydra/body))
  :pretty-hydra
  ((:title (pretty-hydra-title "Colors Management" 'faicon "windows")
           :foreign-keys warn :quit-key "q")
   ("Actions"
    (("w" kurecolor-decrease-brightness-by-step "kurecolor-decrease-brightness-by-step")
     ("W" kurecolor-increase-brightness-by-step "kurecolor-increase-brightness-by-step")
     ("d" kurecolor-decrease-saturation-by-step "kurecolor-decrease-saturation-by-step")
     ("D" kurecolor-increase-saturation-by-step "kurecolor-increase-saturation-by-step")
     ("e" kurecolor-decrease-hue-by-step "kurecolor-decrease-hue-by-step")
     ("E" kurecolor-increase-hue-by-step "kurecolor-increase-hue-by-step"))))
  :config
  (use-package kurecolor))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (("M-+" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package selected
  :bind (:map selected-keymap
              ("@" . copy-region-as-kill)
              ("=" . er/expand-region)
              ("-" . er/contract-region)
              ("`'" . downcase-region)
              ("~" . upcase-region)
              ("|" . kill-region)
              ("?" . count-words-region)
              ("!" . apply-macro-to-region-lines)
              ("/" . indent-region)
              (";" . comment-or-uncomment-region))
  :init
  (selected-global-mode))

;; Multiple cursors
(use-package multiple-cursors
  :pretty-hydra
  ((:title (pretty-hydra-title "Multiple-Cursors" 'material "border_all" :height 1.1 :v-adjust -0.225)
           :color amaranth :quit-key ("q" "C-g"))
   ("Actions"
    (
     ("@" set-rectangular-region-anchor               "set-mark except you're marking a rectangular region")
     ("," mc/skip-to-next-like-this                   "skip to next like this")
     ("." mc/skip-to-previous-like-this               "skip to previous like this")
     ("/" mc/mark-pop                                 "Mark pop")
     (";" mc/mark-all-words-like-this                 "Only mark all matches word")
     ("[" mc/mark-next-word-like-this                 "Only mark next like this world")
     ("{" mc/mark-next-symbol-like-this               "Only mark next like this symbol")
     ("]" mc/mark-previous-word-like-this             "Only mark previous like this word")
     ("}" mc/mark-previous-symbol-like-this           "Only mar previous like this symbol")
     ("-" mc/mark-all-symbols-like-this               "Only mark all matches symbol")
     ("+" mc/mark-all-words-like-this-in-defun        "Only mark all current function matches word")
     ("_" mc/mark-all-symbols-like-this-in-defun      "Only mark all current function matches symbol")
     )

    "---"
    (
     ("s" mc/sort-regions                             "Sort the marked regions alphabetically")
     ("a" mc/edit-beginnings-of-lines                 "Add cursor at the start of each line in the current region")
     ("A" mc/mark-all-in-region                       "Prompts for a string to match in the region")
     ("e" mc/edit-ends-of-lines                       "Add cursor at the end of each line in the current region")
     ("f" mc/mark-all-like-this-in-defun              "Mark all current function matches the current region")
     ("i" mc/insert-numbers                           "Insert increasing numbers for each cursor")
     ("I" mc/insert-letters                           "Insert increasing letters for each cursor")
     ("l" mc/edit-lines                               "Add cursor to each line in the current region")
     ("m" mc/mark-all-like-this                       "Mark all matches the current region")
     ("n" mc/mark-next-like-this                      "Add cursor to next line")
     ("M" mc/mark-all-dwim                            "Smart mark all")
     ("N" mc/unmark-next-like-this                    "Unmark next like this")
     )
    "----"
    (
     ("p" mc/mark-previous-like-this                  "Add cursor to previous line")
     ("P" mc/unmark-previous-like-this                "Unmark previous like this")
     ("r" mc/reverse-regions                          "Reverse the order of the marked regions")
     ("s" mc/mark-next-like-this-symbol               "Mark next like this symbol")
     ("S" mc/mark-previous-like-this-symbol           "Mark previous like this symbol")
     ("t" mc/mark-sgml-tag-pair                       "Mark the current opening and closing tag")
     ("w" mc/mark-next-like-this-word                 "Mark next like this word")
     ("W" mc/mark-previous-like-this-word             "Mark previous like this word")
     ("<mouse-1>" mc/add-cursor-on-click              "Bind to a mouse event to add cursors by clicking")
     )))

  :bind (("C-M-<mouse-1>" . mc/add-cursor-on-click)
         ("<mouse-1>" . mc/keyboard-quit)
         ("<double-mouse-1>" . mouse-set-point)
         (:map selected-keymap
               ("a" . mc/mark-all-like-this)
               ("p" . mc/mark-previous-like-this)
               ("n" . mc/mark-next-like-this)
               ("P" . mc/unmark-previous-like-this)
               ("N" . mc/unmark-next-like-this)
               ("m" . mc/mark-more-like-this-extended)
               ("h" . mc-hide-unmatched-lines-mode)
               ("\\" . mc/vertical-align-with-space)
               ("#" . mc/insert-numbers) ; use num prefix to set the starting number
               ("^" . mc/edit-beginnings-of-lines)
               ("$" . mc/edit-ends-of-lines)))
  :config (progn
            (defun mc/prompt-for-inclusion-in-whitelist (original-command)
              "Rewrite of `mc/prompt-for-inclusion-in-whitelist' to not ask yes/no for every newly seen command."
              (add-to-list 'mc/cmds-to-run-for-all original-command)
              (mc/save-lists)
              t))
  :init
  (setq mc/list-file (locate-user-emacs-file "mc-lists"))
  (eval-after-load 'multiple-cursors-core
    '(progn
       (nconc mc--default-cmds-to-run-for-all
              '(autopair-backspace
                sp--self-insert-command
                c-electric-backspace
                yas-expand))
       (nconc mc--default-cmds-to-run-once
              '(counsel-M-x
                smex
                ivy-done
                hydra-launcher/body
                multiple-cursors-hydra/body
                multiple-cursors-hydra/mc/edit-lines-and-exit
                multiple-cursors-hydra/mc/mark-all-like-this-and-exit
                multiple-cursors-hydra/mc/mark-next-like-this
                multiple-cursors-hydra/mc/mark-next-like-this-word
                multiple-cursors-hydra/mc/mark-next-like-this-symbol
                multiple-cursors-hydra/mc/mark-next-word-like-this
                multiple-cursors-hydra/mc/mark-next-symbol-like-this
                multiple-cursors-hydra/mc/mark-previous-like-this
                multiple-cursors-hydra/mc/mark-previous-like-this-word
                multiple-cursors-hydra/mc/mark-previous-like-this-symbol
                multiple-cursors-hydra/mc/mark-previous-word-like-this
                multiple-cursors-hydra/mc/mark-previous-symbol-like-this
                multiple-cursors-hydra/mc/mark-more-like-this-extended
                multiple-cursors-hydra/mc/add-cursor-on-click
                multiple-cursors-hydra/mc/mark-pop
                multiple-cursors-hydra/mc/unmark-next-like-this
                multiple-cursors-hydra/mc/unmark-previous-like-this
                multiple-cursors-hydra/mc/skip-to-next-like-this
                multiple-cursors-hydra/mc/skip-to-previous-like-this
                multiple-cursors-hydra/mc/edit-lines
                multiple-cursors-hydra/mc/edit-beginnings-of-lines
                multiple-cursors-hydra/mc/edit-ends-of-lines
                multiple-cursors-hydra/mc/mark-all-like-this
                multiple-cursors-hydra/mc/mark-all-words-like-this
                multiple-cursors-hydra/mc/mark-all-symbols-like-this
                multiple-cursors-hydra/mc/mark-all-in-region
                multiple-cursors-hydra/mc/mark-all-like-this-in-defun
                multiple-cursors-hydra/mc/mark-all-words-like-this-in-defun
                multiple-cursors-hydra/mc/mark-all-symbols-like-this-in-defun
                multiple-cursors-hydra/mc/mark-all-dwim
                multiple-cursors-hydra/set-rectangular-region-anchor
                multiple-cursors-hydra/mc/mark-sgml-tag-pair
                multiple-cursors-hydra/mc/insert-numbers
                multiple-cursors-hydra/mc/insert-letters
                multiple-cursors-hydra/mc/sort-regions
                multiple-cursors-hydra/mc/reverse-regions
                multiple-cursors-hydra/nil))))
  )

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

(use-package pomidor
  :bind ("<f12>" . pomidor)
  :init
  (setq alert-default-style 'mode-line)

  (with-eval-after-load 'all-the-icons
    (setq alert-severity-faces
          '((urgent   . all-the-icons-red)
            (high     . all-the-icons-orange)
            (moderate . all-the-icons-yellow)
            (normal   . all-the-icons-green)
            (low      . all-the-icons-blue)
            (trivial  . all-the-icons-purple))
          alert-severity-colors
          `((urgent   . ,(face-foreground 'all-the-icons-red))
            (high     . ,(face-foreground 'all-the-icons-orange))
            (moderate . ,(face-foreground 'all-the-icons-yellow))
            (normal   . ,(face-foreground 'all-the-icons-green))
            (low      . ,(face-foreground 'all-the-icons-blue))
            (trivial  . ,(face-foreground 'all-the-icons-purple))))))

(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

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

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :config
  (setq beacon-color "#666600"))

;; TODO
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-purple bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))


;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

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
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "anchor")
           :color blue :quit-key "q")
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
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Hideshow
(use-package hideshow
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding)))

;; Flexible text folding
(use-package origami
  :pretty-hydra
  ((:title (pretty-hydra-title "Origami" 'octicon "fold" :height 1.1 :v-adjust -0.05)
           :color amaranth :quit-key "q")
   ("Node"
    ((":" origami-recursively-toggle-node "toggle recursively")
     ("a" origami-toggle-all-nodes "toggle all")
     ("t" origami-toggle-node "toggle current")
     ("o" origami-show-only-node "only show current"))
    "Actions"
    (("u" origami-undo "undo")
     ("d" origami-redo "redo")
     ("r" origami-reset "reset"))))
  :bind (:map origami-mode-map
              ("C-`" . origami-hydra/body))
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face)
  ;; lsp-origami provides support for origami.el using language server protocol’s
  ;; textDocument/foldingRange functionality.
  ;; https://github.com/emacs-lsp/lsp-origami/
  (use-package lsp-origami
    :hook ((lsp-after-open . lsp-origami-try-enable))))

;; Highlight some operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode)
  :config
  (with-no-warnings
    (when (fboundp 'pulse-momentary-highlight-region)
      (defun my-vhl-pulse (beg end &optional _buf face)
        "Pulse the changes."
        (pulse-momentary-highlight-region beg end face))
      (advice-add #'vhl/.make-hl :override #'my-vhl-pulse))))


(provide 'init-iedit)
;;; init-iedit.el ends here

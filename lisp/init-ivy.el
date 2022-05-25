;;; init-ivy.el ---                                  -*- lexical-binding: t; -*-

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
(require 'init-funcs)
(require 'init-keybinds)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind
  (("C-s"   . swiper-isearch)
   ("C-r"   . swiper-isearch-backward)
   :map ivy-minibuffer-map
   ("C-w" . ivy-yank-word)

   :map counsel-find-file-map
   ("C-h" . counsel-up-directory)

   :map swiper-map
   ("M-s" . swiper-isearch-toggle)
   ("M-%" . swiper-query-replace)

   :map isearch-mode-map
   ("M-s" . swiper-isearch-toggle))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 12
        swiper-action-recenter t
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
        ivy-on-del-error-function #'ignore
        ivy-initial-inputs-alist nil)

  ;; Better performance on Windows
  (when IS-WINDOWS
    (setq ivy-dynamic-exhibit-delay-ms 200))

  (setq counsel-find-file-at-point t
        counsel-preselect-current-file t
        counsel-yank-pop-separator "\n────────\n")
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'"))
  (setq counsel-fzf-cmd
        "fd --type f --hidden --follow --exclude .git --color never || git ls-tree -r --name-only HEAD || rg --files --hidden --follow --glob '!.git' --color never || find .")

  (when (and IS-MAC (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  :config
  (with-no-warnings
    ;; persist views
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'ivy-views))

    ;; Highlight the selected item
    (defun my-ivy-format-function (cands)
      "Transform CANDS into a string for minibuffer."
      (if (display-graphic-p)
          (ivy-format-function-line cands)
        (ivy-format-function-arrow cands)))
    (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function)

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-pt))

    (defvar-local my-ivy-fly--travel nil)
    (defun my-ivy-fly-back-to-present ()
      (cond ((and (memq last-command my-ivy-fly-commands)
                  (equal (this-command-keys-vector) (kbd "M-p")))
             ;; repeat one time to get straight to the first history item
             (setq unread-command-events
                   (append unread-command-events
                           (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command '(self-insert-command
                                      ivy-forward-char ivy-delete-char delete-forward-char kill-word kill-sexp
                                      end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
                                      yank ivy-yank-word ivy-yank-char ivy-yank-symbol counsel-yank-pop))
                 (equal (this-command-keys-vector) (kbd "M-n")))
             (unless my-ivy-fly--travel
               (delete-region (point) (point-max))
               (when (memq this-command '(ivy-forward-char
                                          ivy-delete-char delete-forward-char
                                          kill-word kill-sexp
                                          end-of-line mwim-end-of-line
                                          mwim-end-of-code-or-line
                                          mwim-end-of-line-or-code))
                 (insert (ivy-cleanup-string ivy-text))
                 (when (memq this-command '(ivy-delete-char
                                            delete-forward-char
                                            kill-word kill-sexp))
                   (beginning-of-line)))
               (setq my-ivy-fly--travel t)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (insert (propertize
                 (save-excursion
		           (set-buffer (window-buffer (minibuffer-selected-window)))
		           (ivy-thing-at-point))
                 'face 'shadow))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)
        (beginning-of-line)))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

    ;;
    ;; Improve search experience of `swiper' and `counsel'
    ;;
    (defun my-ivy-switch-to-swiper (&rest _)
      "Switch to `swiper' with the current input."
      (swiper ivy-text))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (swiper-isearch ivy-text))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (swiper-all ivy-text))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (ivy-quit-and-run (rg-dwim default-directory)))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (counsel-rg ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (counsel-git-grep ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (counsel-find-file ivy-text))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (counsel-fzf ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (counsel-git ivy-text))

    (defun my-ivy-switch-to-list-bookmarks (&rest _)
      "Switch to `list-bookmarks'."
      (ivy-quit-and-run (call-interactively #'list-bookmarks)))

    (defun my-ivy-switch-to-list-colors (&rest _)
      "Switch to `list-colors-display'."
      (ivy-quit-and-run (list-colors-display)))

    (defun my-ivy-switch-to-list-packages (&rest _)
      "Switch to `list-packages'."
      (ivy-quit-and-run (list-packages)))

    (defun my-ivy-switch-to-list-processes (&rest _)
      "Switch to `list-processes'."
      (ivy-quit-and-run (list-processes)))

    (defun my-ivy-copy-library-path (lib)
      "Copy the full path of LIB."
      (let ((path (find-library-name lib)))
        (kill-new path)
        (message "Copied path: \"%s\"." path)))

    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
       (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
           (my-ivy-switch-to-counsel-rg)
         (my-ivy-switch-to-swiper-isearch))))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
      (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with the current input."
        (interactive)
        (ivy-quit-and-run
         (rg-dwim default-directory)))
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map))

    (defun my-swiper-toggle-swiper-isearch ()
      "Toggle `swiper' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
       (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
           (swiper ivy-text)
         (swiper-isearch ivy-text))))
    (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

    (defun my-counsel-find-file-toggle-fzf ()
      "Toggle `counsel-fzf' with the current `counsel-find-file' input."
      (interactive)
      (ivy-quit-and-run
       (counsel-fzf (or ivy-text "") default-directory)))
    (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)

    (defun my-counsel-toggle ()
      "Toggle `counsel' commands and original commands."
      (interactive)
      (pcase (ivy-state-caller ivy-last)
        ('counsel-bookmark (my-ivy-switch-to-list-bookmarks))
        ('counsel-colors-emacs (my-ivy-switch-to-list-colors))
        ('counsel-colors-web (my-ivy-switch-to-list-colors))
        ('counsel-list-processes (my-ivy-switch-to-list-processes))
        ('counsel-package (my-ivy-switch-to-list-packages))
        (_ (ignore))))
    (bind-key "<C-return>" #'my-counsel-toggle ivy-minibuffer-map)

    ;; More actions
    (ivy-add-actions
     #'swiper-isearch
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'swiper
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'swiper-all
     '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
       ("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")))

    (ivy-add-actions
     #'counsel-rg
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

    (ivy-add-actions
     #'counsel-git-grep
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("r" my-ivy-switch-to-rg-dwim "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'counsel-find-file
     '(("g" my-ivy-switch-to-counsel-git "git")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     #'counsel-git
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-fzf
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("g" my-ivy-switch-to-counsel-git "git")))

    (ivy-add-actions
     'counsel-find-library
     '(("p" my-ivy-copy-library-path "copy path")))

    (ivy-add-actions
     'counsel-load-library
     '(("p" my-ivy-copy-library-path "copy path")))

    (ivy-add-actions
     #'counsel-bookmark
     '(("l" my-ivy-switch-to-list-bookmarks "list")))

    (ivy-add-actions
     #'counsel-colors-emacs
     '(("l" my-ivy-switch-to-list-colors "list")))

    (ivy-add-actions
     #'counsel-colors-web
     '(("l" my-ivy-switch-to-list-colors "list")))

    (ivy-add-actions
     #'counsel-package
     '(("l" my-ivy-switch-to-list-packages "list packages")))

    (ivy-add-actions
     #'counsel-list-processes
     '(("l" my-ivy-switch-to-list-processes "list"))))

  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20
                amx-save-file (concat dotfairy-cache-dir "amx-items")))

  ;; Avy integration
  (use-package ivy-avy
    :bind (:map ivy-minibuffer-map
           ("C-'" . ivy-avy)))

  ;; Better sorting and filtering
  (use-package prescient
    :commands prescient-persist-mode
    :init (prescient-persist-mode 1))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-current-match ((t (:inherit hl-line :distant-foreground nil :background nil))))
    (ivy-minibuffer-match-face-1 ((t (:distant-foreground nil :background nil))))
    (ivy-minibuffer-match-face-2 ((t (:distant-foreground nil :background nil))))
    (ivy-minibuffer-match-face-3 ((t (:distant-foreground nil :background nil))))
    (ivy-minibuffer-match-face-4 ((t (:distant-foreground nil :background nil))))
    :config
    ;; NOTE prescient config duplicated with `company'
    (setq prescient-save-file (concat dotfairy-cache-dir "prescient-save.el"))
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (setq ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist
          '((counsel-ag . ivy-prescient-non-fuzzy)
            (counsel-rg . ivy-prescient-non-fuzzy)
            (counsel-pt . ivy-prescient-non-fuzzy)
            (counsel-grep . ivy-prescient-non-fuzzy)
            (counsel-imenu . ivy-prescient-non-fuzzy)
            (counsel-yank-pop . ivy-prescient-non-fuzzy)
            (swiper . ivy-prescient-non-fuzzy)
            (swiper-isearch . ivy-prescient-non-fuzzy)
            (swiper-all . ivy-prescient-non-fuzzy)
            (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
            (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
            (insert-char . ivy-prescient-non-fuzzy)
            (counsel-unicode-char . ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder))
          ivy-prescient-sort-commands
          '(counsel-M-x execute-extended-command execute-extended-command-for-buffer))

    (ivy-prescient-mode 1)
    (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :init
    (setq ivy-read-action-function 'ivy-hydra-read-action)

    (when (childframe-workable-p)
      (setq hydra-hint-display-type 'posframe)

      (with-no-warnings
        (defun my-hydra-posframe-prettify-string (fn str)
          (funcall fn (concat (propertize "\n" 'face '(:height 0.3))
                              str
                              (propertize "\n\n" 'face '(:height 0.3)))))
        (advice-add #'hydra-posframe-show :around #'my-hydra-posframe-prettify-string)

        (defun ivy-hydra-poshandler-frame-center-below (info)
          (let (ivy-posframe-visible-p
                (pos (posframe-poshandler-frame-center-near-bottom info)))
            (catch 'break
              (dolist (frame (visible-frame-list))
                (when (string= (car (frame-parameter frame 'posframe-buffer))
                               ivy-posframe-buffer)
                  (setq ivy-posframe-visible-p t)
                  (throw 'break t))))
            (cons
             (car pos)
             (- (cdr pos)
                (if ivy-posframe-visible-p
                    (- (plist-get info :posframe-height)
                       (plist-get hydra-posframe-show-params :internal-border-width))
                  0)))))

        (defun my-ivy-posframe-read-action-by-key (actions)
          "Ivy-posframe's `ivy-read-action-by-key'. Use `ivy-hydra-read-action' instead."
          (ivy-hydra-read-action actions))
        (advice-add #'ivy-posframe-read-action-by-key
                    :override #'my-ivy-posframe-read-action-by-key)

        (defun hydra-set-posframe-show-params ()
          "Set hydra-posframe style."
          (setq hydra-posframe-show-params
                `(:left-fringe 10
                  :right-fringe 10
                  :internal-border-width 3
                  :internal-border-color ,(face-background 'posframe-border nil t)
                  :background-color ,(face-background 'tooltip nil t)
                  :lines-truncate t
                  :poshandler ivy-hydra-poshandler-frame-center-below)))
        (hydra-set-posframe-show-params)
        (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init
    (defun +ivy/projectile-find-file ()
      "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME or /, `counsel-file-jump' if invoked
from a non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.
The point of this is to avoid Emacs locking up indexing massive file trees."
      (interactive)
      ;; Spoof the command so that ivy/counsel will display the (well fleshed-out)
      ;; actions list for `counsel-find-file' on C-o. The actions list for the other
      ;; commands aren't as well configured or are empty.
      (let ((this-command 'counsel-find-file))
        (call-interactively
         (cond ((or (file-equal-p default-directory "~")
                    (file-equal-p default-directory "/")
                    (when-let (proot (dotfairy-project-root))
                      (file-equal-p proot "~")))
                #'counsel-find-file)

               ((dotfairy-project-p)
                (let ((files (projectile-current-project-files)))
                  (if (<= (length files) ivy-sort-max-size)
                      #'counsel-projectile-find-file
                    #'projectile-find-file)))

               (#'counsel-file-jump)))))
    (define-key!
      [remap projectile-find-file]        #'+ivy/projectile-find-file
      [remap projectile-find-dir]         #'counsel-projectile-find-dir
      [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
      [remap projectile-grep]             #'counsel-projectile-grep
      [remap projectile-ag]               #'counsel-projectile-ag
      [remap projectile-switch-project]   #'counsel-projectile-switch-project)
    (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
    :config
    ;; no highlighting visited files; slows down the filtering
    (ivy-set-display-transformer #'counsel-projectile-find-file nil)
    (setq counsel-projectile-sort-files t))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet)

  ;; Display world clock using Ivy
  (use-package counsel-world-clock)

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :config
    ;; tramp support 2FA(TOTP)
    (setq
     tramp-password-prompt-regexp
     "^.*\\([pP]assword\\|passphrase\\|Response\\|[vV]erification code\\).*:\0? *"))
  (define-key!
    [remap swiper-backward]          #'counsel-grep-or-swiper-backward
    [remap dired]                    #'counsel-dired
    [remap insert-char]              #'counsel-unicode-char
    [remap recentf-open-files]       #'counsel-recentf
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap compile]                  #'+ivy/compile
    [remap describe-bindings]        #'counsel-descbinds
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap describe-symbol]          #'counsel-describe-symbol
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap imenu]                    #'counsel-imenu
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap load-theme]               #'counsel-load-theme
    [remap locate]                   #'counsel-locate
    [remap org-goto]                 #'counsel-org-goto
    [remap org-set-tags-command]     #'counsel-org-tag
    [remap set-variable]             #'counsel-set-variable
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap unicode-chars-list-chars] #'counsel-unicode-char
    [remap yank-pop]                 #'counsel-yank-pop)

  (map! :leader
    (:prefix ("I" . "ivy")
     :desc "ag"                   "a" #'counsel-ag
     :desc "apropos"              "A" #'counsel-apropos
     :desc "bookmarked-directory" "B" #'counsel-bookmarked-directory
     (:prefix ("d" . "describe")
      :desc "describe function"   "f" #'counsel-describe-function
      :desc "describe variable"   "v" #'counsel-describe-variable
      :desc "describe symbol"     "s" #'counsel-describe-symbol
      :desc "info lookup symbol"  "l" #'counsel-info-lookup-symbol)
     :desc "colors emacs"         "e" #'counsel-colors-emacs
     :desc "find library"         "f" #'counsel-find-library
     :desc "faces"                "F" #'counsel-faces
     :desc "git"                  "G" #'counsel-git
     :desc "command history"      "h" #'counsel-command-history
     :desc "minibuffer history"   "H" #'counsel-minibuffer-history
     :desc "locate"               "l" #'counsel-locate
     :desc "load library"         "L" #'counsel-load-library
     :desc "mark ring"            "m" #'counsel-mark-ring
     :desc "outline"              "o" #'counsel-outline
     :desc "find file extern"     "O" #'counsel-find-file-extern
     :desc "pt"                   "p" #'counsel-pt
     :desc "package"              "P" #'counsel-package
     :desc "list processes"       "R" #'counsel-list-processes
     :desc "load theme"           "t" #'counsel-load-theme
     :desc "unicode char"         "u" #'counsel-unicode-char
     :desc "colors web"           "w" #'counsel-colors-web
     :desc "set variable"         "v" #'counsel-set-variable
     :desc "fzf"                  "z" #'counsel-fzf)))

(use-package ivy-dired-history
  :demand t
  :after (savehist dired)
  :bind (:map dired-mode-map
         ("," . dired))
  :init (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if (icons-displayable-p)
  :config
  ;; Fixd Windows icons
  (setq all-the-icons-ivy-rich-icon-size 0.8)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook (;; Must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . ivy-rich-project-root-cache-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

;; Display completion in child frame
(when (childframe-workable-p)
  (use-package ivy-posframe
    :custom-face
    (ivy-posframe ((t (:inherit tooltip))))
    (ivy-posframe-border ((t (:inherit posframe-border))))
    :hook (ivy-mode . ivy-posframe-mode)
    :init
    (setq ivy-height 15
          ivy-posframe-border-width 3
          ivy-posframe-parameters '((left-fringe . 8)
                                    (right-fringe . 8)))
    :config
    (with-no-warnings
      ;; HACK: hide minibuffer with same colors
      (defun my-ivy-posframe--minibuffer-setup (fn &rest args)
        "Advice function of FN, `ivy--minibuffer-setup' with ARGS."
        (if (not (display-graphic-p))
            (apply fn args)
          (let ((ivy-fixed-height-minibuffer nil))
            (apply fn args))
          (when (and ivy-posframe-hide-minibuffer
                     (posframe-workable-p)
                     (string-match-p "^ivy-posframe" (symbol-name ivy--display-function)))
            (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
              (overlay-put ov 'window (selected-window))
              (overlay-put ov 'ivy-posframe t)
              (overlay-put ov 'face
                           (let* ((face (if (facep 'solaire-default-face)
                                            'solaire-default-face
                                          'default))
                                  (bg-color (face-background face nil t)))
                             `(:background ,bg-color :foreground ,bg-color
                               :box nil :underline nil
                               :overline nil :strike-through nil)))
              (setq-local cursor-type nil)))))
      (advice-add #'ivy-posframe--minibuffer-setup :override #'my-ivy-posframe--minibuffer-setup)

      ;; Prettify the buffer
      (defun my-ivy-posframe--prettify-buffer (&rest _)
        "Add top and bottom margin to the prompt."
        (with-current-buffer ivy-posframe-buffer
          (goto-char (point-min))
          (insert (propertize "\n" 'face '(:height 0.3)))
          (goto-char (point-max))
          (insert (propertize "\n" 'face '(:height 0.3)))))
      (advice-add #'ivy-posframe--display :after #'my-ivy-posframe--prettify-buffer)

      ;; Adjust the postion
      (defun ivy-posframe-display-at-frame-center-near-bottom (str)
        (ivy-posframe--display str #'posframe-poshandler-frame-center-near-bottom))
      (setf (alist-get t ivy-posframe-display-functions-alist)
            #'ivy-posframe-display-at-frame-center-near-bottom))))

(provide 'init-ivy)
;;; init-ivy.el ends here

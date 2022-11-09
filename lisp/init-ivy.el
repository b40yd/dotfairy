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

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face
  (symbol-overlay-default-face ((t (:background ,(doom-color 'region)))))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
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
    (when (derived-mode-p 'prog-mode 'yaml-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)
  (map! :localleader
    (:prefix ("s" . "Overlay")
     "d" #'symbol-overlay-remove-all
     "m" #'symbol-overlay-put
     "n" #'symbol-overlay-jump-next
     "N" #'symbol-overlay-switch-forward
     "p" #'symbol-overlay-jump-prev
     "P" #'symbol-overlay-switch-backward)))

(use-package ivy
  :commands (+ivy/woccur)
  :bind
  ((:map ivy-minibuffer-map
    ("C-w" . ivy-yank-word)
    ("C-c C-e" . +ivy/woccur)))
  :config
  (defvar +ivy-buffer-preview nil
    "If non-nil, preview buffers while switching, à la `counsel-switch-buffer'.
When nil, don't preview anything.
When non-nil, preview non-virtual buffers.
When 'everything, also preview virtual buffers")

  (defvar +ivy-edit-functions nil
    "A plist mapping ivy/counsel commands to commands that generate an editable
results buffer.")

  (defun +ivy/woccur ()
    "Invoke a wgrep buffer on the current ivy results, if supported."
    (interactive)
    (unless (window-minibuffer-p)
      (user-error "No completion session is active"))
    (require 'wgrep)
    (let ((caller (ivy-state-caller ivy-last)))
      (if-let (occur-fn (plist-get +ivy-edit-functions caller))
          (ivy-exit-with-action
           (lambda (_) (funcall occur-fn)))
        (if-let (occur-fn (plist-get ivy--occurs-list caller))
            (let ((buffer (generate-new-buffer
                           (format "*ivy-occur%s \"%s\"*"
                                   (if caller (concat " " (prin1-to-string caller)) "")
                                   ivy-text))))
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (funcall occur-fn))
                (if (ivy-state-text ivy-last)
                    (setf (ivy-state-text ivy-last) ivy-text))
                (setq ivy-occur-last ivy-last)
                (setq-local ivy--directory ivy--directory))
              (ivy-exit-with-action
               `(lambda (_)
                  (pop-to-buffer ,buffer)
                  (ivy-wgrep-change-to-wgrep-mode))))
          (user-error "%S doesn't support wgrep" caller)))))


;;;###autoload
  (defun +ivy/compile ()
    "Execute a compile command from the current buffer's directory."
    (interactive)
    (counsel-compile default-directory))

;;;###autoload
  (defun +ivy/project-compile ()
    "Execute a compile command from the current project's root."
    (interactive)
    (counsel-compile (projectile-project-root)))

  ;;
  ;; Library

  (defun +ivy--switch-buffer-preview ()
    (let (ivy-use-virtual-buffers ivy--virtual-buffers)
      (counsel--switch-buffer-update-fn)))

  (defalias '+ivy--switch-buffer-preview-all #'counsel--switch-buffer-update-fn)
  (defalias '+ivy--switch-buffer-unwind      #'counsel--switch-buffer-unwind)

  (defun +ivy--is-workspace-buffer-p (buffer)
    (let ((buffer (car buffer)))
      (when (stringp buffer)
        (setq buffer (get-buffer buffer)))
      (+workspace-contains-buffer-p buffer)))

;;;###autoload
  (defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
    "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")

  (defun +ivy--is-workspace-other-buffer-p (buffer)
    (let ((buffer (car buffer)))
      (when (stringp buffer)
        (setq buffer (get-buffer buffer)))
      (and (not (eq buffer (current-buffer)))
           (+workspace-contains-buffer-p buffer))))


  (defun +ivy--switch-buffer (workspace other)
    (let ((current (not other))
          prompt action filter update unwind)
      (cond ((and workspace current)
             (setq prompt "Switch to workspace buffer: "
                   action #'ivy--switch-buffer-action
                   filter #'+ivy--is-workspace-other-buffer-p))
            (workspace
             (setq prompt "Switch to workspace buffer in other window: "
                   action #'ivy--switch-buffer-other-window-action
                   filter #'+ivy--is-workspace-buffer-p))
            (current
             (setq prompt "Switch to buffer: "
                   action #'ivy--switch-buffer-action))
            ((setq prompt "Switch to buffer in other window: "
                   action #'ivy--switch-buffer-other-window-action)))
      (when +ivy-buffer-preview
        (cond ((not (and ivy-use-virtual-buffers
                         (eq +ivy-buffer-preview 'everything)))
               (setq update #'+ivy--switch-buffer-preview
                     unwind #'+ivy--switch-buffer-unwind))
              ((setq update #'+ivy--switch-buffer-preview-all
                     unwind #'+ivy--switch-buffer-unwind))))
      (ivy-read prompt 'internal-complete-buffer
                :action action
                :predicate filter
                :update-fn update
                :unwind unwind
                :preselect (buffer-name (other-buffer (current-buffer)))
                :matcher #'ivy--switch-buffer-matcher
                :keymap ivy-switch-buffer-map
                ;; NOTE A clever disguise, needed for virtual buffers.
                :caller #'ivy-switch-buffer)))

;;;###autoload
  (defun +ivy/switch-workspace-buffer (&optional arg)
    "Switch to another buffer within the current workspace.
If ARG (universal argument), open selection in other-window."
    (interactive "P")
    (+ivy--switch-buffer t arg))

;;;###autoload
  (defun +ivy/switch-workspace-buffer-other-window ()
    "Switch another window to a buffer within the current workspace."
    (interactive)
    (+ivy--switch-buffer t t))

;;;###autoload
  (defun +ivy/switch-buffer ()
    "Switch to another buffer."
    (interactive)
    (+ivy--switch-buffer nil nil))

;;;###autoload
  (defun +ivy/switch-buffer-other-window ()
    "Switch to another buffer in another window."
    (interactive)
    (+ivy--switch-buffer nil t))

;;;###autoload
  (cl-defun +ivy-file-search (&key query in all-files (recursive t) prompt args)
    "Conduct a file search using ripgrep.
:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
    (declare (indent defun))
    (unless (executable-find "rg")
      (user-error "Couldn't find ripgrep in your PATH"))
    (require 'counsel)
    (let* ((this-command 'counsel-rg)
           (project-root (or (dotfairy-project-root) default-directory))
           (directory (or in project-root))
           (args (concat (if all-files " -uu")
                         (unless recursive " --maxdepth 1")
                         " --hidden -g !.git -g !.svn -g !.hg -g !.ccls-cache "
                         (mapconcat #'shell-quote-argument args " "))))
      (setq deactivate-mark t)
      (counsel-rg
       (or query
           (when (dotfairy-region-active-p)
             (replace-regexp-in-string
              "[! |]" (lambda (substr)
                        (cond ((string= substr " ")
                               "  ")
                              ((string= substr "|")
                               "\\\\\\\\|")
                              ((concat "\\\\" substr))))
              (rxt-quote-pcre (dotfairy-thing-at-point-or-region)))
             (ivy-thing-at-point)))
       directory args
       (or prompt
           (format "Search project [%s]: "
                   (cond ((equal directory default-directory)
                          "./")
                         ((equal directory project-root)
                          (projectile-project-name))
                         ((file-relative-name directory project-root)))
                   (string-trim args))))))

;;;###autoload
  (defun +ivy/project-search (&optional arg initial-query directory)
    "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
    (interactive "P")
    (+ivy-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
  (defun +ivy/project-search-from-cwd (&optional arg initial-query)
    "Performs a project search recursively from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
    (interactive "P")
    (+ivy/project-search arg initial-query default-directory)))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :custom-face
  (ivy-current-match ((t (:inherit region :distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-1 ((t (:foreground "dimgray" :distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-2 ((t (:distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-3 ((t (:distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-4 ((t (:distant-foreground nil :background nil))))
  :bind
  (("C-s"   . swiper-isearch)
   ("C-r"   . swiper-isearch-backward)
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
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
        ivy-on-del-error-function #'ignore
        ivy-initial-inputs-alist nil)
  ;; Use orderless regex strategy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;; Set minibuffer height for different commands
  (setq ivy-height-alist '((counsel-evil-registers . 5)
                           (counsel-yank-pop . 8)
                           (counsel-git-log . 4)
                           (swiper . 15)
                           (counsel-projectile-ag . 15)
                           (counsel-projectile-rg . 15)))

  ;; Better performance on Windows
  (when IS-WINDOWS
    (setq ivy-dynamic-exhibit-delay-ms 200))

  (setq counsel-find-file-at-point t
        counsel-preselect-current-file t
        counsel-yank-pop-separator "\n────────\n")
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Use the faster search tool: ripgrep (`rg')
  (cond
   ((executable-find "ugrep")
    (setq counsel-grep-base-command "ugrep --color=never -n -e '%s' '%s'"))
   ((executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'")))

  (when (executable-find "fd")
    (setq counsel-fzf-cmd
          "fd --type f --hidden --follow --exclude .git --color never || git ls-tree -r --name-only HEAD || rg --files --hidden --follow --glob '!.git' --color never || find ."))

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
    (defconst my-ivy-fly-commands
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
      (ivy-quit-and-run (swiper ivy-text)))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (ivy-quit-and-run (swiper-isearch ivy-text)))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (ivy-quit-and-run (swiper-all ivy-text)))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (interactive)
      (ivy-exit-with-action #'rg-dwim))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (ivy-quit-and-run (counsel-rg ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (ivy-quit-and-run (counsel-git-grep ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (ivy-quit-and-run (counsel-find-file ivy-text)))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (ivy-quit-and-run (counsel-fzf ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (ivy-quit-and-run (counsel-git ivy-text)))

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
      (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
          (my-ivy-switch-to-counsel-rg)
        (my-ivy-switch-to-swiper-isearch)))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
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

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :init
    (setq ivy-read-action-function 'ivy-hydra-read-action)

    (when (childframe-completion-workable-p)
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

;; Support pinyin in Ivy
;; Input prefix '!' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package pinyinlib
  :autoload pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) "!")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string "!" "" str )
                            "")))
              ""))
            (t nil)))

    (defun my-ivy--regex-pinyin (fn str)
      "The regex builder advice to support pinyin."
      (or (pinyin-to-utf8 str)
          (funcall fn str)))
    (advice-add #'ivy--regex-plus :around #'my-ivy--regex-pinyin)
    (advice-add #'ivy--regex-ignore-order :around #'my-ivy--regex-pinyin)))

(use-package ivy-dired-history
  :demand t
  :after dired
  :defines (savehist-additional-variables desktop-globals-to-save)
  :bind (:map dired-mode-map
         ("," . dired))
  :init
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'ivy-dired-history-variable)))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (when (executable-find "ugrep")
    (setq counsel-projectile-grep-base-command "ugrep --color=never -rnEI %s"))

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
  ;; A more sensible `counsel-projectile-find-file' that reverts to
  ;; `counsel-find-file' if invoked from $HOME, `counsel-file-jump' if invoked
  ;; from a non-project, `projectile-find-file' if in a big project (more than
  ;; `ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.
  (setf (alist-get 'projectile-find-file counsel-projectile-key-bindings)
        #'+ivy/projectile-find-file)
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)
  (setq counsel-projectile-sort-files t))

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
(when (childframe-completion-workable-p)
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


;;;###package swiper
(setq swiper-action-recenter t)

(provide 'init-ivy)
;;; init-ivy.el ends here

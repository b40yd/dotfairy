;;; init-vertico.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2022, 7ym0n, all rights reserved.

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
(require 'init-funcs)
(require 'init-keybinds)

(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.
The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

(defvar +vertico-consult-fd-args nil
  "Shell command and arguments the vertico module uses for fd.")

(defvar dotfairy-projectile-fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "The filename of the `fd' executable. On some distros it's 'fdfind' (ubuntu,
debian, and derivatives). On most it's 'fd'.")
;;;###autoload
(defvar embark-quit-after-action)

;;
;;; Packages

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char))


(use-package orderless
  :config
  (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
    "Highlight company matches correctly, and try default completion styles before
orderless."
    :around #'company-capf--candidates
    (let ((orderless-match-faces [completions-common-part])
          (completion-styles +vertico-company-completion-styles))
      (apply fn args)))

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :defer t
  :bind (("C-s" . consult-line))
  :init
  (define-key!
    [remap apropos]                       #'consult-apropos
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  (setq consult-project-root-function #'dotfairy-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (unless +vertico-consult-fd-args
    (setq +vertico-consult-fd-args
          (if dotfairy-projectile-fd-binary
              (format "%s --color=never -i -H -E .git --regex %s"
                      dotfairy-projectile-fd-binary
                      (if IS-WINDOWS "--path-separator=/" ""))
            consult-find-args)))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "C-x SPC"))
  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-x SPC") :debounce 0.5 'any))
  (after! org
    (defvar +vertico--consult-org-source
      `(:name     "Org"
        :narrow   ?o
        :hidden t
        :category buffer
        :state    ,#'consult--buffer-state
        :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append))
  (map! :map consult-crm-map
        :desc "Select candidate" [tab] #'+vertico/crm-select
        :desc "Select candidate and keep input" [backtab] #'+vertico/crm-select-keep-input
        :desc "Enter candidates" "RET" #'+vertico/crm-exit))


(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package consult-flycheck
  :when (featurep 'syntax)
  :after (consult flycheck))


(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)

  (map! [remap describe-bindings] #'embark-bindings
        "C-c ;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
         "C-;"               #'embark-act
         "C-c C-;"           #'embark-export
         "C-c C-l"           #'embark-collect
         :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:localleader
         :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
  :config

  (defadvice! +vertico--embark-which-key-prompt-a (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    :around #'embark-completing-read-prompter
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico-embark-target-package-fn
        (nthcdr pos embark-target-finders)))

  (map! (:map embark-file-map
         :desc "Open target with sudo"        "s"   #'dotfairy/sudo-find-file
         (:when (featurep 'magit-mode)
          :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status))))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (advice-add #'marginalia--project-root :override #'dotfairy-project-root)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(dotfairy/find-file-in-emacsd . project-file)
            '(dotfairy/find-file-in-other-project . project-file)
            '(flycheck-error-list-set-filter . builtin)
            '(persp-switch-to-buffer . buffer)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))


(use-package embark-consult
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package vertico-posframe
  :when (featurep 'childframe)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (add-hook 'after-load-theme #'posframe-delete-all))

(defun +vertico--workspace-buffer-state ()
  (let ((preview
         ;; Only preview in current window and other window.
         ;; Preview in frames and tabs is not possible since these don't get cleaned up.
         (if (memq consult--buffer-display
                   '(switch-to-buffer switch-to-buffer-other-window))
             (let ((orig-buf (current-buffer))
                   other-win
                   cleanup-buffers)
               (lambda (action cand)
                 (when (eq action 'preview)
                   (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                              (not other-win))
                     (switch-to-buffer-other-window orig-buf)
                     (setq other-win (selected-window)))
                   (let ((win (or other-win (selected-window))))
                     (when (window-live-p win)
                       (with-selected-window win
                         (cond
                          ((and cand (get-buffer cand))
                           (unless (+workspace-contains-buffer-p cand)
                             (cl-pushnew cand cleanup-buffers))
                           (switch-to-buffer cand 'norecord))
                          ((buffer-live-p orig-buf)
                           (switch-to-buffer orig-buf 'norecord)
                           (mapc #'persp-remove-buffer cleanup-buffers)))))))))
           #'ignore)))
    (lambda (action cand)
      (funcall preview action cand))))

(defun +vertico--workspace-generate-sources ()
  "Generate list of consult buffer sources for all workspaces"
  (let* ((active-workspace (+workspace-current-name))
         (workspaces (+workspace-list-names))
         (key-range (append (cl-loop for i from ?1 to ?9 collect i)
                            (cl-loop for i from ?a to ?z collect i)
                            (cl-loop for i from ?A to ?Z collect i)))
         (last-i (length workspaces))
         (i 0))
    (mapcar (lambda (name)
              (cl-incf i)
              `(:name     ,name
                :hidden   ,(not (string= active-workspace name))
                :narrow   ,(nth (1- i) key-range)
                :category buffer
                :state    +vertico--workspace-buffer-state
                :items    ,(lambda ()
                             (consult--buffer-query
                              :sort 'visibility
                              :as #'buffer-name
                              :predicate
                              (lambda (buf)
                                (when-let (workspace (+workspace-get name t))
                                  (+workspace-contains-buffer-p buf workspace)))))))
            (+workspace-list-names))))

(autoload 'consult--multi "consult")
;;;###autoload
(defun +vertico/switch-workspace-buffer (&optional force-same-workspace)
  "Switch to another buffer in the same workspace.
Type the workspace's number (starting from 1) followed by a space to display its
buffer list. Selecting a buffer in another workspace will switch to that
workspace instead. If FORCE-SAME-WORKSPACE (the prefix arg) is non-nil, that
buffer will be opened in the current workspace instead."
  (interactive "P")
  (when-let (buffer (consult--multi (+vertico--workspace-generate-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt (format "Switch to buffer (%s): "
                                                    (+workspace-current-name))
                                    :history 'consult--buffer-history
                                    :sort nil))
    (let ((origin-workspace (plist-get (cdr buffer) :name)))
      ;; Switch to the workspace the buffer belongs to, maybe
      (if (or (equal origin-workspace (+workspace-current-name))
              force-same-workspace)
          (funcall consult--buffer-display (car buffer))
        (+workspace-switch origin-workspace)
        (message "Switched to %S workspace" origin-workspace)
        (if-let (window (get-buffer-window (car buffer)))
            (select-window window)
          (funcall consult--buffer-display (car buffer)))))))

;;;###autoload
(defun +vertico/embark-open-in-new-workspace (x)
  "Open X (a file) in a new workspace."
  (interactive)
  (+workspace/new)
  (find-file x))

;;;###autoload
(defun +vertico/embark-magit-status (file)
  "Run `magit-status` on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.
:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (or (dotfairy-project-root) default-directory))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading --line-number "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'shell-quote-argument args " ")
                  " ."))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query (or query
                    (when (dotfairy-region-active-p)
                      (regexp-quote (dotfairy-thing-at-point-or-region)))))
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-builder directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Peforms a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))

;;;###autoload
(defun +vertico/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun +vertico-embark-target-package-fn ()
  "Targets Dotfairy's package! statements and returns the package name"
  (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'org-mode))
    (save-excursion
      (when (and (search-backward "(" nil t)
                 (looking-at "(\\s-*package!\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*"))
        (let ((pkg (match-string 1)))
          (set-text-properties 0 (length pkg) nil pkg)
          `(package . ,pkg))))))

;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

(defvar +vertico/find-file-in--history nil)
;;;###autoload
(defun +vertico/find-file-in (&optional dir initial)
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
    (find-file
     (consult--read
      (split-string (cdr (apply #'dotfairy-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

;;;###autoload
(defun +vertico/jump-list (jump)
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive
   (let (buffers)
     (require 'consult)
     (unwind-protect
         (list
          (consult--read
           ;; REVIEW Refactor me
           (nreverse
            (delete-dups
             (delq
              nil (mapcar
                   (lambda (mark)
                     (when mark
                       (cl-destructuring-bind (path pt _id) mark
                         (let* ((visiting (find-buffer-visiting path))
                                (buf (or visiting (find-file-noselect path t)))
                                (dir default-directory))
                           (unless visiting
                             (push buf buffers))
                           (with-current-buffer buf
                             (goto-char pt)
                             (font-lock-fontify-region
                              (line-beginning-position) (line-end-position))
                             (format "%s:%d: %s"
                                     (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                         (file-relative-name (buffer-file-name buf) dir))
                                                   #'< :key #'length))
                                     (line-number-at-pos)
                                     (string-trim-right (or (thing-at-point 'line) ""))))))))
                   (cddr (better-jumper-jump-list-struct-ring
                          (better-jumper-get-jumps (better-jumper--get-current-context))))))))
           :prompt "jumplist: "
           :sort nil
           :require-match t
           :category 'jump-list))
       (mapc #'kill-buffer buffers))))
  (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
      (user-error "No match")
    (let ((file (match-string-no-properties 1 jump))
          (line (match-string-no-properties 2 jump)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (string-to-number line)))))

;;;###autoload
(defun +vertico-embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;;;###autoload
(defun +vertico/crm-select ()
  "Toggle selection of current candidate in `consult-completing-read-multiple'.
If the candidate has been selected, move the index up by one, to allow for quick
selection of multiple subsequent candidates."
  (interactive)
  (let* ((selected-p (get-text-property 0 'consult--crm-selected (vertico--candidate)))
         (goto-idx (+ vertico--index (if selected-p 0 1))))
    (run-at-time 0 nil (cmd! (vertico--goto goto-idx) (vertico--exhibit))))
  (vertico-exit))

;;;###autoload
(defun +vertico/crm-select-keep-input ()
  "Like `+vertico/crm-select', but keeps the current minibuffer input."
  (interactive)
  (let* ((input (substring-no-properties (car vertico--input)))
         (selected-p (get-text-property 0 'consult--crm-selected (vertico--candidate)))
         (goto-idx (+ vertico--index (if selected-p 0 1))))
    (run-at-time 0 nil (cmd! (insert input) (vertico--exhibit) (vertico--goto goto-idx) (vertico--exhibit))))
  (vertico-exit))

;;;###autoload
(defun +vertico/crm-exit ()
  "Exit `consult-completing-read-multiple' session in a dwim way.
If there are no selected candidates, select the current candidate and exit.
If there are selected candidates, disregard the current candidate and exit."
  (interactive)
  (if (consult--crm-selected)
      (progn
        (when (minibuffer-contents)
          (delete-minibuffer-contents)
          (vertico--exhibit))
        (vertico--goto -1)
        (vertico-exit))
    (run-at-time 0 nil #'vertico-exit)
    (vertico-exit)))

;;;###autoload
(defun +vertico--consult--fd-builder (input)
  (pcase-let* ((cmd (split-string-and-unquote +vertico-consult-fd-args))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended)))
    (when re
      (list :command (append cmd
                             (list (consult--join-regexps re 'extended))
                             opts)
            :highlight hl))))

(autoload #'consult--directory-prompt "consult")
;;;###autoload
(defun +vertico/consult-fd (&optional dir initial)
  (interactive "P")
  (if dotfairy-projectile-fd-binary
      (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
             (default-directory (cdr prompt-dir)))
        (find-file (consult--find (car prompt-dir) #'+vertico--consult--fd-builder initial)))
    (consult-find dir initial)))

;;;###autoload
(defun +vertico-basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

;;;###autoload
(defun +vertico-basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

(provide 'init-vertico)
;;; init-vertico.el ends here

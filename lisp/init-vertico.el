;;; init-vertico.el --                                 -*- lexical-binding: t; -*-

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

(use-package vertico
  :hook (after-init . vertico-mode)
  :commands (+vertico/embark-preview)
  :bind (("s-SPC" . #'+vertico/embark-preview))
  :config
  (defun +vertico/embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

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
  (map! :map vertico-map "DEl" #'vertico-directory-delete-char)
  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args))))

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
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
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


  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))



  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :defer t
  :commands (+vertico/jump-list +vertico/switch-workspace-buffer +vertico/embark-open-in-new-workspace)
  :bind (("C-s" . consult-line))
  :init
  (if IS-WINDOWS
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))


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
  (defun +vertico/embark-open-in-new-workspace (x)
    "Open X (a file) in a new workspace."
    (interactive)
    (+workspace/new)
    (find-file x))
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
        consult-async-input-debounce 0.1)
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
   :preview-key (kbd "C-SPC"))
  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-SPC") :debounce 0.5 'any))
  (when (featurep 'org)
    (defvar +vertico--consult-org-source
      (list :name     "Org Buffer"
            :category 'buffer
            :narrow   ?o
            :hidden   t
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :new
            (lambda (name)
              (with-current-buffer (get-buffer-create name)
                (insert "#+title: " name "\n\n")
                (org-mode)
                (consult--buffer-action (current-buffer))))
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (if (featurep 'org)
                          (org-buffer-list)
                        (seq-filter
                         (lambda (x)
                           (eq (buffer-local-value 'major-mode x) 'org-mode))
                         (buffer-list)))))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append)))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("s-d" . consult-dir)))

(use-package consult-flycheck
  :after (consult flycheck))


(use-package embark
  :defer t
  :commands (+vertico/embark-export-write)
  :bind (("C-c ;" . embark-act)
         :map minibuffer-local-map
         ("C-c ;" . embark-act)
         ("C-c C-l" . embark-export)
         ("C-c C-e" . +vertico/embark-export-write))
  :init
  :config
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
  (defun +vertico/embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))
  (map! (:map embark-file-map
         :desc "Open target with sudo"        "s"   #'dotfairy/sudo-find-file
         :desc "Open in new workspace"       "TAB" #'+vertico/embark-open-in-new-workspace
         (:when (featurep 'magit)
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
  :ensure t
  :after (embark consult)
  :demand
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))



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

(defun +vertico/project-search (&optional arg initial-query directory)
  "Peforms a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))



(defun +vertico/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))
(provide 'init-vertico)
;;; init-vertico.el ends here

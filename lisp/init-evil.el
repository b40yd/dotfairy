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
  :init
  ;; Set these defaults before `evil'; use `defvar' so they can be changed prior
  ;; to loading.
  (defvar evil-want-C-g-bindings t)
  (defvar evil-want-C-i-jump nil)  ; we do this ourselves
  (defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
  (defvar evil-want-C-u-delete t)
  (defvar evil-want-C-w-delete t)
  (defvar evil-want-Y-yank-to-eol t)
  (defvar evil-want-abbrev-expand-on-insert-exit nil)
  (defvar evil-respect-visual-line-mode nil)
  (setq evil-want-keybinding nil)
  (evil-mode +1)
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(bar +evil-emacs-cursor-fn)
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
  (after! evil
    (setq evil-default-state 'normal))
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

  (add-hook! 'dotfairy-escape-hook
    (defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t)))

  (unless noninteractive
    (setq save-silently t)
    (add-hook! 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (dotfairy-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))

  ;; HACK '=' moves the cursor to the beginning of selection. Disable this,
  ;;      since it's more disruptive than helpful.
  (defadvice! +evil--dont-move-cursor-a (fn &rest args)
    :around #'evil-indent
    (save-excursion (apply fn args)))

  ;; REVIEW In evil, registers 2-9 are buffer-local. In vim, they're global,
  ;;        so... Perhaps this should be PRed upstream?
  (defadvice! +evil--make-numbered-markers-global-a (char)
    :after-until #'evil-global-marker-p
    (and (>= char ?2) (<= char ?9)))

  ;; REVIEW Fix #2493: dir-locals cannot target fundamental-mode when evil-mode
  ;;        is active. See hlissner/doom-emacs#2493. Revert this if
  ;;        emacs-evil/evil#1268 is resolved upstream.
  (defadvice! +evil--fix-local-vars-a (&rest _)
    :before #'turn-on-evil-mode
    (when (eq major-mode 'fundamental-mode)
      (hack-local-variables)))

  ;; HACK Invoking helpful from evil-ex throws a "No recursive edit is in
  ;;      progress" error because, between evil-ex and helpful,
  ;;      `abort-recursive-edit' gets called one time too many.
  (defadvice! +evil--fix-helpful-key-in-evil-ex-a (key-sequence)
    :before #'helpful-key
    (when (evil-ex-p)
      (run-at-time 0.1 nil #'helpful-key key-sequence)
      (abort-recursive-edit)))

  ;;;###autoload
  (defun +evil-escape-a (&rest _)
    "Call `dotfairy/escape' if `evil-force-normal-state' is called interactively."
    (when (called-interactively-p 'any)
      (call-interactively #'dotfairy/escape)))

  ;; Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
  ;; spaces. It doesn't in vim, so it shouldn't in evil.
  (defadvice! +evil--no-squeeze-on-fill-a (fn &rest args)
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply fn args)))

  ;; Make ESC (from normal mode) the universal escaper. See `dotfairy-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  (after! evil-ex
  ;;;###autoload (autoload '+evil:delete-this-file "editor/evil/autoload/files" nil t)
    (evil-define-command +evil:delete-this-file (&optional filename force-p)
      "Delete FILENAME (defaults to the file associated with current buffer) and
kills the buffer. If FORCE-P, force the deletion (don't ask for confirmation)."
      :repeat nil
      (interactive "<f><!>")
      (dotfairy/delete-this-file filename force-p))

    (evil-define-command +evil:move-this-file (new-path &optional force-p)
      "Move current buffer's file to NEW-PATH. Replaces %, # and other vim-esque
filename modifiers (see `+evil*ex-replace-special-filenames'). If FORCE-P,
overwrite the destination file if it exists, without confirmation."
      :repeat nil
      (interactive "<f><!>")
      (when (or (not new-path) (string-empty-p new-path))
        (user-error "No new path was specified"))
      (dotfairy/move-this-file new-path force-p))

    (evil-define-command +evil:copy-this-file (new-path &optional force-p)
      "Copy current buffer's file to NEW-PATH. Replaces %, # and other vim-esque
filename modifiers (see `+evil*ex-replace-special-filenames'). If FORCE-P,
overwrite the destination file if it exists, without confirmation."
      :repeat nil
      (interactive "<f><!>")
      (when (or (not new-path) (string-empty-p new-path))
        (user-error "No new path was specified"))
      (dotfairy/copy-this-file new-path force-p))

    ;;;###autoload (autoload '+evil:pwd "editor/evil/autoload/ex" nil t)
    (evil-define-command +evil:pwd (bang)
      "Display the current working directory. If BANG, copy it to your clipboard."
      (interactive "<!>")
      (if (not bang)
          (pwd)
        (kill-new default-directory)
        (message "Copied to clipboard")))

    (evil-define-command +evil:make (arguments &optional bang)
      "Run make with ARGUMENTS.
If BANG is non-nil, open compilation output in a comint buffer.

If BANG, then run ARGUMENTS as a full command. This command understands vim file
modifiers (like %:p:h). See `+evil-replace-filename-modifiers-a' for details."
      (interactive "<sh><!>")
      (let ((compile-command "make"))
        (+evil:compile (if (stringp arguments)
                           (evil-ex-replace-special-filenames arguments)
                         "")
                       bang)))


    (evil-define-command +evil:compile (arguments &optional bang)
      "Run `compile-command' with ARGUMENTS.
If BANG is non-nil, open compilation output in a comint buffer.

This command understands vim file modifiers (like %:p:h). See
`+evil-replace-filename-modifiers-a' for details."
      (interactive "<sh><!>")
      (compile (evil-ex-replace-special-filenames
                (format "%s %s"
                        (eval compile-command)
                        arguments))
               bang))
    (evil-define-command +evil:cd (&optional path)
      "Change `default-directory' with `cd'."
      (interactive "<f>")
      (let ((path (or path "~")))
        (cd path)
        (message "Changed directory to '%s'" (abbreviate-file-name (expand-file-name path)))))

    (evil-ex-define-cmd "make"        #'+evil:make)
    (evil-ex-define-cmd "cd"          #'+evil:cd)
    (evil-ex-define-cmd "pwd"         #'+evil:pwd)
    (evil-ex-define-cmd "com[pile]"   #'+evil:compile)
    (evil-ex-define-cmd "al[ign]"     #'+evil:align)
    (evil-ex-define-cmd "ral[ign]"    #'+evil:align-right)
    (evil-ex-define-cmd "cp"          #'+evil:copy-this-file)
    (evil-ex-define-cmd "mv"          #'+evil:move-this-file)
    (evil-ex-define-cmd "rm"          #'+evil:delete-this-file))
  (require 'init-evil-ex))

;; add package evil-collection
(use-package evil-collection
  :after evil
  :init
  (defvar evil-collection-key-blacklist)
  (when (and (not noninteractive))
    (defvar evil-collection-setup-minibuffer nil)

    ;; We do this ourselves, and better.
    (defvar evil-collection-want-unimpaired-p nil)
    ;; Dotfairy binds goto-reference on gD and goto-assignments on gA ourselves
    (defvar evil-collection-want-find-usages-bindings-p nil)
    ;; Reduces keybind conflicts between outline-mode and org-mode (which is
    ;; derived from outline-mode).
    (defvar evil-collection-outline-enable-in-minor-mode-p nil)

    ;; We handle loading evil-collection ourselves
    (defvar evil-collection--supported-modes nil)

    (defvar +evil-collection-disabled-list
      '(anaconda-mode
        buff-menu
        calc
        comint
        company
        custom
        eldoc
        elisp-mode
        ert
        free-keys
        helm
        help
        image
        indent
        kotlin-mode
        lispy
        outline
        replace
        shortdoc
        simple
        slime
        tab-bar)
      "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

    (defvar evil-collection-mode-list
      `(2048-game
        ag
        alchemist
        anaconda-mode
        apropos
        arc-mode
        atomic-chrome
        auto-package-update
        beginend
        bluetooth
        bm
        bookmark
        (buff-menu "buff-menu")
        bufler
        calc
        calendar
        cider
        cmake-mode
        color-rg
        comint
        company
        compile
        consult
        corfu
        crdt
        (custom cus-edit)
        cus-theme
        dashboard
        daemons
        deadgrep
        debbugs
        debug
        devdocs
        dictionary
        diff-hl
        diff-mode
        dired
        dired-sidebar
        disk-usage
        distel
        doc-view
        docker
        ebib
        ebuku
        edbi
        edebug
        ediff
        eglot
        elpaca
        ement
        explain-pause-mode
        eldoc
        elfeed
        elisp-mode
        elisp-refs
        elisp-slime-nav
        embark
        emms
        ,@(if (> emacs-major-version 28) '(emoji))
        epa
        ert
        eshell
        eval-sexp-fu
        evil-mc
        eww
        fanyi
        finder
        flycheck
        flymake
        forge
        free-keys
        geiser
        ggtags
        git-timemachine
        gited
        gnus
        go-mode
        grep
        guix
        hackernews
        helm
        help
        helpful
        hg-histedit
        hungry-delete
        ibuffer
        (image image-mode)
        image-dired
        image+
        imenu
        imenu-list
        (indent "indent")
        indium
        info
        ivy
        js2-mode
        leetcode
        lispy
        lms
        log-edit
        log-view
        lsp-ui-imenu
        lua-mode
        kotlin-mode
        macrostep
        man
        (magit magit-repos magit-submodule)
        magit-section
        magit-todos
        markdown-mode
        monky
        mpc
        mpdel
        mu4e
        mu4e-conversation
        neotree
        newsticker
        notmuch
        nov
        omnisharp
        org
        org-present
        org-roam
        osx-dictionary
        outline
        p4
        (package-menu package)
        pass
        (pdf pdf-tools)
        popup
        proced
        prodigy
        profiler
        python
        quickrun
        racer
        racket-describe
        realgud
        reftex
        replace
        restclient
        rg
        ripgrep
        rjsx-mode
        robe
        rtags
        ruby-mode
        scheme
        scroll-lock
        selectrum
        sh-script
        ,@(if (> emacs-major-version 27) '(shortdoc))
        simple
        simple-mpc
        slime
        sly
        snake
        so-long
        speedbar
        tab-bar
        tablist
        tar-mode
        telega
        (term term ansi-term multi-term)
        tetris
        thread
        tide
        timer-list
        transmission
        trashed
        tuareg
        typescript-mode
        vc-annotate
        vc-dir
        vc-git
        vdiff
        vertico
        view
        vlf
        vterm
        vundo
        w3m
        wdired
        wgrep
        which-key
        woman
        xref
        xwidget
        yaml-mode
        youtube-dl
        zmusic
        (ztree ztree-diff)))

    (defun +evil-collection-init (module &optional disabled-list)
      "Initialize evil-collection-MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup)."
      (unless (memq (or (car-safe module) module) disabled-list)
        (dotfairy-log "editor:evil: loading evil-collection-%s %s"
                      (or (car-safe module) module)
                      (if after-init-time "" "(too early!)"))
        (with-demoted-errors "evil-collection error: %s"
          (evil-collection-init (list module)))))
    (defadvice! +evil-collection-disable-blacklist-a (fn)
      :around #'evil-collection-vterm-toggle-send-escape  ; allow binding to ESC
      (let (evil-collection-key-blacklist)
        (funcall-interactively fn)))
    ;; These modes belong to packages that Emacs always loads at startup, causing
    ;; evil-collection and it's co-packages to all load immediately. We avoid this
    ;; by loading them after evil-collection has first loaded...
    (with-eval-after-load 'evil-collection
      ;; Don't let evil-collection interfere with certain keys
      (setq evil-collection-key-blacklist
            (append (list dotfairy-leader-key dotfairy-localleader-key
                          dotfairy-leader-alt-key)
                    evil-collection-key-blacklist
                    '("[" "]" "gz" "<escape>")))

      (evil-define-key* 'normal process-menu-mode-map
        "q" #'kill-current-buffer
        "d" #'process-menu-delete-process)

      (mapc #'+evil-collection-init '(comint custom)))

    ;; ...or on first invokation of their associated major/minor modes.
    (after! evil
      ;; Emacs loads these two packages immediately, at startup, which needlessly
      ;; convolutes load order for evil-collection-help.
      (add-transient-hook! 'help-mode
        (+evil-collection-init 'help))
      (add-transient-hook! 'Buffer-menu-mode
        (+evil-collection-init '(buff-menu "buff-menu")))
      (add-transient-hook! 'calc-mode
        (+evil-collection-init 'calc))
      (add-transient-hook! 'image-mode
        (+evil-collection-init 'image))
      (add-transient-hook! 'emacs-lisp-mode
        (+evil-collection-init 'elisp-mode))
      (add-transient-hook! 'occur-mode
        (+evil-collection-init 'replace))
      (add-transient-hook! 'indent-rigidly
        (+evil-collection-init '(indent "indent")))
      (add-transient-hook! 'minibuffer-setup-hook
        (when evil-collection-setup-minibuffer
          (+evil-collection-init 'minibuffer)
          (evil-collection-minibuffer-insert)))
      (add-transient-hook! 'process-menu-mode
        (+evil-collection-init '(process-menu simple)))
      (add-transient-hook! 'shortdoc-mode
        (+evil-collection-init 'shortdoc))
      (add-transient-hook! 'tabulated-list-mode
        (+evil-collection-init 'tabulated-list))
      (add-transient-hook! 'tab-bar-mode
        (+evil-collection-init 'tab-bar))

      ;; HACK Do this ourselves because evil-collection break's `eval-after-load'
      ;;      load order by loading their target plugin before applying keys. This
      ;;      makes it hard for end-users to overwrite these keybinds with a
      ;;      simple `after!' or `with-eval-after-load'.
      (dolist (mode evil-collection-mode-list)
        (dolist (req (or (cdr-safe mode) (list mode)))
          (with-eval-after-load req
            (+evil-collection-init mode +evil-collection-disabled-list)))))))

;; gcc comments out a line
;; gc comments out the target of a motion
;; gcap comments out a paragraph
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :hook (after-init . evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :hook (after-init . global-evil-surround-mode))

(use-package evil-matchit
  :after evil
  :diminish evil-matchit-mode
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :hook (after-init . global-evil-mc-mode)
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))

  :config
  ;; HACK evil-mc's design is bizarre. Its variables and hooks are lazy loaded
  ;;   rather than declared at top-level, some hooks aren't defined or
  ;;   documented, it's a bit initializer-function drunk, and its minor modes
  ;;   are intended to be perpetually active -- even when no cursors are active
  ;;   (causing #6021). I undo all of that here.
  (evil-mc-define-vars)
  (evil-mc-initialize-vars)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-incompatible-modes)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-initialize-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-teardown-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-resume-incompatible-modes)
  (advice-add #'evil-mc-initialize-hooks :override #'ignore)
  (advice-add #'evil-mc-teardown-hooks :override #'evil-mc-initialize-vars)
  (advice-add #'evil-mc-initialize-active-state :before #'turn-on-evil-mc-mode)
  (advice-add #'evil-mc-teardown-active-state :after #'turn-off-evil-mc-mode)
  (defadvice! +multiple-cursors--dont-reinit-vars-a (fn &rest args)
    :around #'evil-mc-mode
    (letf! ((#'evil-mc-initialize-vars #'ignore))
      (apply fn args)))

  ;; REVIEW This is tremendously slow on macos and windows for some reason.
  (setq evil-mc-enable-bar-cursor (featurep :system 'linux))

  (after! smartparens
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; Whitelist more commands
  (dolist (fn '((backward-kill-word)
                (company-complete-common . evil-mc-execute-default-complete)
                ;; :emacs undo
                (undo-fu-only-undo . evil-mc-execute-default-undo)
                (undo-fu-only-redo . evil-mc-execute-default-redo)
                ;; :editor evil
                (evil-delete-back-to-indentation . evil-mc-execute-default-call)
                (evil-escape . evil-mc-execute-default-evil-normal-state)  ; C-g
                (evil-numbers/inc-at-pt-incremental)
                (evil-numbers/dec-at-pt-incremental)
                (evil-digit-argument-or-evil-beginning-of-visual-line
                 (:default . evil-mc-execute-default-call)
                 (visual . evil-mc-execute-visual-call))
                ;; :lang org
                (evil-org-delete . evil-mc-execute-default-evil-delete)))
    (setf (alist-get (car fn) evil-mc-custom-known-commands)
          (if (and (cdr fn) (listp (cdr fn)))
              (cdr fn)
            (list (cons :default
                    (or (cdr fn)
                        #'evil-mc-execute-default-call-with-count))))))

  ;; HACK Allow these commands to be repeated by prefixing them with a numerical
  ;;      argument. See gabesoft/evil-mc#110
  (defadvice! +multiple-cursors--make-repeatable-a (fn)
    :around '(evil-mc-make-and-goto-first-cursor
              evil-mc-make-and-goto-last-cursor
              evil-mc-make-and-goto-prev-cursor
              evil-mc-make-and-goto-next-cursor
              evil-mc-skip-and-goto-prev-cursor
              evil-mc-skip-and-goto-next-cursor
              evil-mc-make-and-goto-prev-match
              evil-mc-make-and-goto-next-match
              evil-mc-skip-and-goto-prev-match
              evil-mc-skip-and-goto-next-match)
    (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1))
      (funcall fn)))

  ;; If we're entering insert mode, it's a good bet that we want to start using
  ;; our multiple cursors
  (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors)

  (pushnew! evil-mc-incompatible-minor-modes
            ;; evil-escape's escape key leaves behind extraneous characters
            'evil-escape-mode
            ;; Lispy commands don't register on more than 1 cursor. Lispyville
            ;; is fine though.
            'lispy-mode)

  (add-hook! 'dotfairy-escape-hook
    (defun +multiple-cursors-escape-multiple-cursors-h ()
      "Clear evil-mc cursors and restore state."
      (when (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
        (evil-mc-resume-cursors)
        t)))
  (evil-define-key* '(normal emacs) mc/keymap [escape] #'mc/keyboard-quit)
  (defvar-local +mc--compat-evil-prev-state nil)
  (defvar-local +mc--compat-mark-was-active nil)

  (add-hook! 'multiple-cursors-mode-enabled-hook
    (defun +multiple-cursors-compat-switch-to-emacs-state-h ()
      (when (and (bound-and-true-p evil-local-mode)
                 (not (memq evil-state '(insert emacs))))
        (setq +mc--compat-evil-prev-state evil-state)
        (when (region-active-p)
          (setq +mc--compat-mark-was-active t))
        (let ((mark-before (mark))
              (point-before (point)))
          (evil-emacs-state 1)
          (when (or +mc--compat-mark-was-active (region-active-p))
            (goto-char point-before)
            (set-mark mark-before))))))

  (add-hook! 'multiple-cursors-mode-disabled-hook
    (defun +multiple-cursors-compat-back-to-previous-state-h ()
      (when +mc--compat-evil-prev-state
        (unwind-protect
            (cl-case +mc--compat-evil-prev-state
              ;; For `evil-multiedit', marked occurrences aren't saved after
              ;; exiting mc, so we should return to normal state anyway
              ((normal visual multiedit multiedit-insert)
               (evil-force-normal-state))
              (t (message "Don't know how to handle previous state: %S"
                          +mc--compat-evil-prev-state)))
          (setq +mc--compat-evil-prev-state nil)
          (setq +mc--compat-mark-was-active nil)))))
  ;; When running edit-lines, point will return (position + 1) as a result of
  ;; how evil deals with regions
  (defadvice! +multiple--cursors-adjust-mark-for-evil-a (&rest _)
    :before #'mc/edit-lines
    (when (and (bound-and-true-p evil-local-mode)
               (not (memq evil-state '(insert emacs))))
      (if (> (point) (mark))
          (goto-char (1- (point)))
        (push-mark (1- (mark))))))

  (add-hook! 'rectangular-region-mode-hook
    (defun +multiple-cursors-evil-compat-rect-switch-state-h ()
      (if rectangular-region-mode
          (+multiple-cursors-compat-switch-to-emacs-state-h)
        (setq +mc--compat-evil-prev-state nil))))

  (defvar mc--default-cmds-to-run-once nil)
  )


(use-package evil-mc-extras
  :after evil-mc
  :commands global-evil-mc-extras-mode
  :diminish evil-mc-extras-mode
  :hook (after-init . global-evil-mc-extras-mode))

(use-package evil-escape
  :after evil
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
  :hook (after-init . evil-traces-mode))

(use-package evil-anzu
  :config (global-anzu-mode +1))
(provide 'init-evil)
;;; init-evil.el ends here

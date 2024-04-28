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

  (defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
    "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

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

;;;###autoload
  (defun +evil/next-beginning-of-method (count)
    "Jump to the beginning of the COUNT-th method/function after point."
    (interactive "p")
    (beginning-of-defun (- count)))

;;;###autoload
  (defun +evil/previous-beginning-of-method (count)
    "Jump to the beginning of the COUNT-th method/function before point."
    (interactive "p")
    (beginning-of-defun count))

;;;###autoload
  (defalias #'+evil/next-end-of-method #'end-of-defun
    "Jump to the end of the COUNT-th method/function after point.")

;;;###autoload
  (defun +evil/previous-end-of-method (count)
    "Jump to the end of the COUNT-th method/function before point."
    (interactive "p")
    (end-of-defun (- count)))

;;;###autoload
  (defun +evil/next-preproc-directive (count)
    "Jump to the COUNT-th preprocessor directive after point.

By default, this only recognizes C preproc directives. To change this see
`+evil-preprocessor-regexp'."
    (interactive "p")
    ;; TODO More generalized search, to support directives in other languages?
    (if (re-search-forward +evil-preprocessor-regexp nil t count)
        (goto-char (match-beginning 0))
      (user-error "No preprocessor directives %s point"
                  (if (> count 0) "after" "before"))))

;;;###autoload
  (defun +evil/previous-preproc-directive (count)
    "Jump to the COUNT-th preprocessor directive before point.

See `+evil/next-preproc-directive' for details."
    (interactive "p")
    (+evil/next-preproc-directive (- count)))

;;; ] SPC / [ SPC
;;;###autoload
  (defun +evil/insert-newline-below (count)
    "Insert COUNT blank line(s) below current line. Does not change modes."
    (interactive "p")
    (dotimes (_ count)
      (save-excursion (evil-insert-newline-below))))

;;;###autoload
  (defun +evil/insert-newline-above (count)
    "Insert COUNT blank line(s) above current line. Does not change modes."
    (interactive "p")
    (dotimes (_ count)
      (save-excursion (evil-insert-newline-above))))

;;; ]t / [t
;;;###autoload
  (defun +evil/next-frame (count)
    "Focus next frame."
    (interactive "p")
    (dotimes (_ (abs count))
      (let ((frame (if (> count 0) (next-frame) (previous-frame))))
        (if (eq frame (selected-frame))
            (user-error "No other frame")
          (select-frame-set-input-focus frame)))))

;;;###autoload
  (defun +evil/previous-frame (count)
    "Focus previous frame."
    (interactive "p")
    (+evil/next-frame (- count)))

;;; ]f / [f
  ;;;###autoload
  (defun dotfairy-glob (&rest segments)
    "Return file list matching the glob created by joining SEGMENTS.

The returned file paths will be relative to `default-directory', unless SEGMENTS
concatenate into an absolute path.

Returns nil if no matches exist.
Ignores `nil' elements in SEGMENTS.
If the glob ends in a slash, only returns matching directories."
    (declare (side-effect-free t))
    (let* (case-fold-search
           file-name-handler-alist
           (path (apply #'file-name-concat segments)))
      (if (string-suffix-p "/" path)
          (cl-delete-if-not #'file-directory-p (file-expand-wildcards (substring path 0 -1)))
        (file-expand-wildcards path))))

  (defun +evil--next-file (n)
    (unless buffer-file-name
      (user-error "Must be called from a file-visiting buffer"))
    (let* ((directory (file-name-directory buffer-file-name))
           (filename (file-name-nondirectory buffer-file-name))
           (files (cl-remove-if-not #'file-regular-p (dotfairy-glob (file-name-directory buffer-file-name) "[!.]*")))
           (index (cl-position filename files :test #'file-equal-p)))
      (when (null index)
        (user-error "Couldn't find this file in current directory"))
      (let ((index (+ index n)))
        (cond ((>= index (length files))
               (user-error "No files after this one"))
              ((< index 0)
               (user-error "No files before this one"))
              ((expand-file-name (nth index files) directory))))))

;;;###autoload
  (defun +evil/next-file (count)
    "Open file following this one, alphabetically, in the same directory."
    (interactive "p")
    (find-file (+evil--next-file count)))

;;;###autoload
  (defun +evil/previous-file (count)
    "Open file preceding this one, alphabetically, in the same directory."
    (interactive "p")
    (find-file (+evil--next-file (- count))))


  ;;
;;; Encoding/Decoding

  ;; NOTE For ]x / [x see :lang web
  ;; - `+web:encode-html-entities'
  ;; - `+web:decode-html-entities'

  (defun +evil--encode (beg end fn)
    (save-excursion
      (goto-char beg)
      (let* ((end (if (eq evil-this-type 'line) (1- end) end))
             (text (buffer-substring-no-properties beg end)))
        (delete-region beg end)
        (insert (funcall fn text)))))

;;; ]u / [u
  (evil-define-operator +evil:url-encode (_count &optional beg end)
    "TODO"
    (interactive "<c><r>")
    (+evil--encode beg end #'url-encode-url))

  (evil-define-operator +evil:url-decode (_count &optional beg end)
    "TODO"
    (interactive "<c><r>")
    (+evil--encode beg end #'url-unhex-string))

;;; ]y / [y
  (evil-define-operator +evil:c-string-encode (_count &optional beg end)
    "TODO"
    (interactive "<c><r>")
    (+evil--encode
     beg end
     (lambda (text)
       (replace-regexp-in-string "[\"\\]" (lambda (ch) (concat "\\" ch)) text))))

  (evil-define-operator +evil:c-string-decode (_count &optional beg end)
    "TODO"
    (interactive "<c><r>")
    (+evil--encode
     beg end
     (lambda (text)
       (replace-regexp-in-string "\\\\[\"\\]" (lambda (str) (substring str 1)) text))))

;;;###autoload
  (defun +evil/shift-right ()
    "vnoremap < <gv"
    (interactive)
    (call-interactively #'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))

;;;###autoload
  (defun +evil/shift-left ()
    "vnoremap > >gv"
    (interactive)
    (call-interactively #'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

;;;###autoload
  (defun +evil/alt-paste ()
    "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents."
    (interactive)
    (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
      (call-interactively #'evil-paste-after)))
;;; Standalone
;;; gp
;;;###autoload
  (defun +evil/reselect-paste ()
    "Return to visual mode and reselect the last pasted region."
    (interactive)
    (cl-destructuring-bind (_ _ _ beg end &optional _)
        evil-last-paste
      (evil-visual-make-selection
       (save-excursion (goto-char beg) (point-marker))
       end)))
  ;;;###autoload
  (defun +evil--window-swap (direction)
    "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
    (when (window-dedicated-p)
      (user-error "Cannot swap a dedicated window"))
    (let* ((this-window (selected-window))
           (this-buffer (current-buffer))
           (that-window (window-in-direction direction nil this-window))
           (that-buffer (window-buffer that-window)))
      (when (or (minibufferp that-buffer)
                (window-dedicated-p this-window))
        (setq that-buffer nil that-window nil))
      (if (not (or that-window (one-window-p t)))
          (funcall (pcase direction
                     ('left  #'evil-window-move-far-left)
                     ('right #'evil-window-move-far-right)
                     ('up    #'evil-window-move-very-top)
                     ('down  #'evil-window-move-very-bottom)))
        (unless that-window
          (setq that-window
                (split-window this-window nil
                              (pcase direction
                                ('up 'above)
                                ('down 'below)
                                (_ direction))))
          (with-selected-window that-window
            (switch-to-buffer (dotfairy-fallback-buffer)))
          (setq that-buffer (window-buffer that-window)))
        (window-swap-states this-window that-window)
        (select-window that-window))))

;;;###autoload
  (defun +evil/window-move-left ()
    "Swap windows to the left."
    (interactive) (+evil--window-swap 'left))
;;;###autoload
  (defun +evil/window-move-right ()
    "Swap windows to the right"
    (interactive) (+evil--window-swap 'right))
;;;###autoload
  (defun +evil/window-move-up ()
    "Swap windows upward."
    (interactive) (+evil--window-swap 'up))
;;;###autoload
  (defun +evil/window-move-down ()
    "Swap windows downward."
    (interactive) (+evil--window-swap 'down))

;;;###autoload
  (defun +evil/window-split-and-follow ()
    "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
    (interactive)
    (let ((evil-split-window-below (not evil-split-window-below)))
      (call-interactively #'evil-window-split)))

;;;###autoload
  (defun +evil/window-vsplit-and-follow ()
    "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
    (interactive)
    (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
      (call-interactively #'evil-window-vsplit)))
  (map!
   ;; custom vim-unmpaired-esque keys
   :m  "]#"    #'+evil/next-preproc-directive
   :m  "[#"    #'+evil/previous-preproc-directive
   :m  "]e"    #'next-error
   :m  "[e"    #'previous-error
   :n  "]F"    #'+evil/next-frame
   :n  "[F"    #'+evil/previous-frame
   :m  "]h"    #'outline-next-visible-heading
   :m  "[h"    #'outline-previous-visible-heading
   :m  "]m"    #'+evil/next-beginning-of-method
   :m  "[m"    #'+evil/previous-beginning-of-method
   :m  "]M"    #'+evil/next-end-of-method
   :m  "[M"    #'+evil/previous-end-of-method
   :n  "[o"    #'+evil/insert-newline-above
   :n  "]o"    #'+evil/insert-newline-below
   :n  "gp"    #'+evil/reselect-paste
   :v  "gp"    #'+evil/alt-paste
   ;; don't leave visual mode after shifting
   :v  "<"     #'+evil/shift-left  ; vnoremap < <gv
   :v  ">"     #'+evil/shift-right  ; vnoremap > >gv
   ;; window management (prefix "C-w")
   (:map evil-window-map
    ;; Navigation
    "C-h"     #'evil-window-left
    "C-j"     #'evil-window-down
    "C-k"     #'evil-window-up
    "C-l"     #'evil-window-right
    "C-w"     #'other-window
    ;; Extra split commands
    "S"       #'+evil/window-split-and-follow
    "V"       #'+evil/window-vsplit-and-follow
    ;; Swapping windows
    "H"       #'+evil/window-move-left
    "J"       #'+evil/window-move-down
    "K"       #'+evil/window-move-up
    "L"       #'+evil/window-move-right
    "C-S-w"   #'ace-swap-window)
   )
  )

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
  :commands (evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-mode
             evil-mc-undo-all-cursors
             global-evil-mc-mode))

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

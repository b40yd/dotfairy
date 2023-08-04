;;; init-keybinds.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2020-2024 b40yd

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
(require 'init-custom)
(require 'init-funcs)
(require 'cl-lib)
(defvar dotfairy-leader-key "C-c"
  "The leader prefix key for users.")

(defvar dotfairy-leader-alt-key "C-c M-c"
  "An alternative leader prefix key,
 used for Insert and Emacs states, and for users.")

(defvar dotfairy-localleader-key "C-c SPC"
  "The localleader prefix key, for major-mode specific commands.")

(defvar dotfairy-localleader-alt-key "C-c SPC"
  "The localleader prefix key, for major-mode specific commands.")

(defvar dotfairy-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

;; Key Modifiers
(with-no-warnings
  (cond
   (IS-MAC
    ;; mac-* variables are used by the special emacs-mac build of Emacs by
    ;; Yamamoto Mitsuharu, while other builds use ns-*.
    (setq mac-command-modifier      'super
          ns-command-modifier       'super
          mac-option-modifier       'meta
          ns-option-modifier        'meta
          ;; Free up the right option for character composition
          mac-right-option-modifier 'none
          ns-right-option-modifier  'none))
   (IS-WINDOWS
    (setq w32-pass-lwindow-to-system nil
          w32-pass-rwindow-to-system nil
          w32-pass-apps-to-system nil
          w32-lwindow-modifier 'super
          w32-apps-modifier 'hyper)
    (w32-register-hot-key [s-t]))))

(use-package general
  :init
  (require 'general)
  ;; Convenience aliases
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind)
  :config
  (add-hook 'after-init-hook #'general-auto-unbind-keys))

;; HACK `map!' uses this instead of `define-leader-key!' because it consumes
;; 20-30% more startup time, so we reimplement it ourselves.
(defmacro dotfairy--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (dotfairy-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key dotfairy-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t dotfairy-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t dotfairy-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.
Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.
See `dotfairy-leader-key' and `dotfairy-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'dotfairy-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.
Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.
See `dotfairy-localleader-key' and `dotfairy-localleader-alt-key' to change the
localleader prefix."
  (if (featurep 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix dotfairy-localleader-key
        :non-normal-prefix dotfairy-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix dotfairy-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Dotfairy's startup time!
(define-prefix-command 'dotfairy/leader 'dotfairy-leader-map)
(define-key dotfairy-leader-map [override-state] 'all)

;; Bind `dotfairy-leader-key' and `dotfairy-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'after-init-hook
  (defun dotfairy-init-leader-keys-h ()
    "Bind `dotfairy-leader-key' and `dotfairy-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal dotfairy-leader-alt-key "C-c")
                   (set-keymap-parent dotfairy-leader-map mode-specific-map))
                  ((equal dotfairy-leader-alt-key "C-x")
                   (set-keymap-parent dotfairy-leader-map ctl-x-map)))
            (define-key map (kbd dotfairy-leader-alt-key) 'dotfairy/leader))
        (evil-define-key* '(normal visual motion) map (kbd dotfairy-leader-key) 'dotfairy/leader)
        (evil-define-key* '(emacs insert) map (kbd dotfairy-leader-alt-key) 'dotfairy/leader))
      (general-override-mode +1))))
;;
;;; Packages
(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-description-length 30
        which-key-max-display-columns nil
        which-key-lighter nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  (which-key-mode)
  :config
  (defvar dotfairy--initial-which-key-replacement-alist which-key-replacement-alist)
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)

  (which-key-add-key-based-replacements dotfairy-leader-key "<leader>")
  (which-key-add-key-based-replacements dotfairy-localleader-key "<localleader>")

  (when (childframe-completion-workable-p)
    (use-package which-key-posframe
      :diminish
      :functions posframe-poshandler-frame-center-near-bottom
      :custom-face
      (which-key-posframe ((t (:inherit tooltip))))
      (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
      :init
      (setq which-key-posframe-border-width posframe-border-width
            which-key-posframe-poshandler #'posframe-poshandler-frame-center-near-bottom
            which-key-posframe-parameters '((left-fringe . 8)
                                            (right-fringe . 8)))
      (which-key-posframe-mode 1)
      :config
      (with-no-warnings
        (defun my-which-key-posframe--show-buffer (act-popup-dim)
          "Show which-key buffer when popup type is posframe.
Argument ACT-POPUP-DIM includes the dimension, (height . width)
of the buffer text to be displayed in the popup"
          (when (posframe-workable-p)
            (with-current-buffer (get-buffer-create which-key-buffer-name)
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (insert (propertize "\n" 'face '(:height 0.3)))
                (goto-char (point-max))
                (insert (propertize "\n\n\n" 'face '(:height 0.3)))))
            (posframe-show which-key--buffer
		                   :font which-key-posframe-font
		                   :position (point)
		                   :poshandler which-key-posframe-poshandler
		                   :background-color (face-attribute 'which-key-posframe :background nil t)
		                   :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
		                   :height (1+ (car act-popup-dim))
		                   :width (1+ (cdr act-popup-dim))
		                   :internal-border-width which-key-posframe-border-width
		                   :internal-border-color (face-attribute 'which-key-posframe-border :background nil t)
		                   :override-parameters which-key-posframe-parameters)))
        (advice-add #'which-key-posframe--show-buffer :override #'my-which-key-posframe--show-buffer)))))


;;
;;; `map!' macro
(defvar dotfairy-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun dotfairy--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.
For example, :nvi will map to (list 'normal 'visual 'insert). See
`dotfairy-evil-state-alist' to customize this."
  (cl-loop for l across (dotfairy-keyword-name keyword)
           if (assq l dotfairy-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar dotfairy--map-forms nil)
(defvar dotfairy--map-fn nil)
(defvar dotfairy--map-batch-forms nil)
(defvar dotfairy--map-state '(:dummy t))
(defvar dotfairy--map-parent-state nil)
(defvar dotfairy--map-evil-p nil)
(after! evil (setq dotfairy--map-evil-p t))

(defun dotfairy--map-process (rest)
  (let ((dotfairy--map-fn dotfairy--map-fn)
        dotfairy--map-state
        dotfairy--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (dotfairy--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (dotfairy--map-commit)
                  (setq dotfairy--map-fn 'dotfairy--define-leader-key))
                 (:localleader
                  (dotfairy--map-commit)
                  (setq dotfairy--map-fn 'define-localleader-key!))
                 (:after
                  (dotfairy--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (dotfairy--map-set :keymaps `(quote ,(dotfairy-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (dotfairy-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (dotfairy--map-nested (list (intern (dotfairy-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "dotfairy-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            dotfairy--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (dotfairy--map-set (if dotfairy--map-fn :infix :prefix)
                                       prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          dotfairy--map-forms)))
                 (_
                  (condition-case _
                      (dotfairy--map-def (pop rest) (pop rest)
                                         (dotfairy--map-keyword-to-states key)
                                         desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((dotfairy--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (dotfairy--map-commit)
    (macroexp-progn (nreverse (delq nil dotfairy--map-forms)))))

(defun dotfairy--map-append-keys (prop)
  (let ((a (plist-get dotfairy--map-parent-state prop))
        (b (plist-get dotfairy--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun dotfairy--map-nested (wrapper rest)
  (dotfairy--map-commit)
  (let ((dotfairy--map-parent-state (dotfairy--map-state)))
    (push (if wrapper
              (append wrapper (list (dotfairy--map-process rest)))
            (dotfairy--map-process rest))
          dotfairy--map-forms)))

(defun dotfairy--map-set (prop &optional value)
  (unless (equal (plist-get dotfairy--map-state prop) value)
    (dotfairy--map-commit))
  (setq dotfairy--map-state (plist-put dotfairy--map-state prop value)))

(defun dotfairy--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (dotfairy-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state dotfairy--map-batch-forms)))
  t)

(defun dotfairy--map-commit ()
  (when dotfairy--map-batch-forms
    (cl-loop with attrs = (dotfairy--map-state)
             for (state . defs) in dotfairy--map-batch-forms
             if (or dotfairy--map-evil-p (not state))
             collect `(,(or dotfairy--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) dotfairy--map-forms))
    (setq dotfairy--map-batch-forms nil)))

(defun dotfairy--map-state ()
  (let ((plist
         (append (list :prefix (dotfairy--map-append-keys :prefix)
                       :infix  (dotfairy--map-append-keys :infix)
                       :keymaps
                       (append (plist-get dotfairy--map-parent-state :keymaps)
                               (plist-get dotfairy--map-state :keymaps)))
                 dotfairy--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.
If evil isn't loaded, evil-specific bindings are ignored.
Properties
  :leader [...]                   an alias for (:prefix dotfairy-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]
  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.
States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')
  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').
  These must be placed right before the key string.
  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #'dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #'dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #'dosomething)"
  (dotfairy--map-process rest))



(provide 'init-keybinds)
;;; init-keybinds.el ends here

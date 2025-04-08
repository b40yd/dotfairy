;;; init-popup.el --- popup window -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 b40yd
;;
;; Author: b40yd <bb.qnyd@gmail.com>
;; Maintainer: b40yd <bb.qnyd@gmail.com>
;; Created: November 04, 2024
;; Modified: November 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar +popup--internal nil)

(defun +popup--remember (windows)
  "Remember WINDOWS (a list of windows) for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (cons (window-buffer w)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer. If it fails, eg.
the buffer is visible, then set another timer and try again later."
  (let ((inhibit-quit t))
    (cond ((not (buffer-live-p buffer)))
          ((not (get-buffer-window buffer t))
           (with-demoted-errors "Error killing transient buffer: %s"
             (with-current-buffer buffer
               (let ((kill-buffer-hook (remq '+popup-kill-buffer-hook-h kill-buffer-hook))
                     confirm-kill-processes)
                 (when-let (process (get-buffer-process buffer))
                   (when (eq (process-type process) 'real)
                     (kill-process process)))
                 (let (kill-buffer-query-functions)
                   ;; HACK The debugger backtrace buffer, when killed, called
                   ;;      `top-level'. This causes jumpiness when the popup
                   ;;      manager tries to clean it up.
                   (cl-letf (((symbol-function #'top-level) #'ignore))
                     (kill-buffer buffer)))))))
          ((let ((ttl (if (= ttl 0)
                          (or (plist-get +popup-defaults :ttl) 3)
                        ttl)))
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))))))

(defun +popup--delete-window (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it get a chance to
  run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the window has a
  `transient' window parameter (see `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    (and (or (buffer-file-name buffer)
             (if-let* ((base-buffer (buffer-base-buffer buffer)))
                 (buffer-file-name base-buffer)))
         (buffer-modified-p buffer)
         (let ((autosave (+popup-parameter 'autosave window)))
           (cond ((eq autosave 't))
                 ((null autosave)
                  (y-or-n-p "Popup buffer is modified. Save it?"))
                 ((functionp autosave)
                  (funcall autosave buffer))))
         (with-current-buffer buffer (save-buffer)))
    (let ((ignore-window-parameters t))
      (if-let* ((wconf (window-parameter window 'saved-wconf)))
          (set-window-configuration wconf)
        (delete-window window)))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (+popup-buffer-mode -1)
        (unless +popup--inhibit-transient
          (let ((ttl (+popup-parameter 'ttl window)))
            (when (eq ttl 't)
              (setq ttl (plist-get +popup-defaults :ttl)))
            (cond ((null ttl))
                  ((functionp ttl)
                   (funcall ttl buffer))
                  ((not (integerp ttl))
                   (signal 'wrong-type-argument (list 'integerp ttl)))
                  ((= ttl 0)
                   (+popup--kill-buffer buffer 0))
                  ((add-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h nil t)
                   (setq +popup--timer
                         (run-at-time ttl nil #'+popup--kill-buffer
                                      buffer ttl))))))))))

(defun +popup--delete-other-windows (window)
  "Fixes `delete-other-windows' when used from a popup window."
  (when-let (window (ignore-errors (+popup/raise window)))
    (let ((ignore-window-parameters t))
      (delete-other-windows window)))
  nil)

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with ALIST."
  (when alist
    (let ((alist  ; handle defaults
           (cl-remove-duplicates
            (append alist +popup-default-alist)
            :key #'car-safe :from-end t))
          (parameters
           (cl-remove-duplicates
            (append (cdr (assq 'window-parameters alist))
                    +popup-default-parameters)
            :key #'car-safe :from-end t)))
      ;; handle `size'
      (when-let* ((size  (cdr (assq 'size alist)))
                  (side  (or (cdr (assq 'side alist)) 'bottom))
                  (param (if (memq side '(left right))
                             'window-width
                           'window-height)))
        (setq alist (assq-delete-all 'size alist))
        (setf (alist-get param alist) size))
      (setf (alist-get 'window-parameters alist)
            parameters)
      ;; Fixes #1305: addresses an edge case where a popup with a :size, :width
      ;; or :height greater than the current frame's dimensions causes
      ;; hanging/freezing (a bug in Emacs' `display-buffer' API perhaps?)
      (let ((width  (cdr (assq 'window-width  alist)))
            (height (cdr (assq 'window-height alist))))
        (setf (alist-get 'window-width alist)
              (if (numberp width)
                  (min width (frame-width))
                width))
        (setf (alist-get 'window-height alist)
              (if (numberp height)
                  (min height (frame-height))
                height))
        alist))))

(defun +popup--split-window (window size side)
  "Ensure a non-dedicated/popup window is selected when splitting a window."
  (unless +popup--internal
    (cl-loop for win
             in (cons (or window (selected-window))
                      (window-list nil 0 window))
             unless (+popup-window-p win)
             return (setq window win)))
  (let ((ignore-window-parameters t))
    (split-window window size side)))

(defun +popup--maybe-select-window (window origin)
  "Select a window based on `+popup--inhibit-select' and this window's `select' parameter."
  (unless +popup--inhibit-select
    ;; REVIEW: Once our minimum version is bumped up to Emacs 30.x, replace this
    ;;   with `post-command-select-window' window parameter.
    (let ((select (+popup-parameter 'select window)))
      (if (functionp select)
          (funcall select window origin)
        (select-window (if select window origin))))))

;;;###autoload
(defun +popup--init (window &optional alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (setq alist (delq (assq 'actions alist) alist))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'split-window #'+popup--split-window)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup--delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))


;;
;; Public library

;;;###autoload
(defun +popup-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a popup buffer. Defaults to the current buffer."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (and (bufferp buffer)
           (buffer-live-p buffer)
           (buffer-local-value '+popup-buffer-mode buffer)
           buffer))))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW is a popup window. Defaults to the current window."
  (when +popup-mode
    (let ((window (or window (selected-window))))
      (and (windowp window)
           (window-live-p window)
           (window-parameter window 'popup)
           window))))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((origin (selected-window))
         (window-min-height 3)
         (alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions alist))
                      +popup-default-display-buffer-actions)))
    (or (let* ((alist (remove (assq 'window-width alist) alist))
               (alist (remove (assq 'window-height alist) alist))
               (window (display-buffer-reuse-window buffer alist)))
          (when window
            (+popup--maybe-select-window window origin)
            window))
        (when-let (popup (cl-loop for func in actions
                                  if (funcall func buffer alist)
                                  return it))
          (+popup--init popup alist)
          (+popup--maybe-select-window popup origin)
          popup))))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a function, run it
with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

;;;###autoload
(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.

Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

;;;###autoload
(defun +popup-alist-from-window-state (state)
  "Convert window STATE (from `window-state-get') to a `display-buffer' alist."
  (let* ((params (alist-get 'parameters state)))
    `((side          . ,(alist-get 'window-side params))
      (window-width  . ,(alist-get 'total-width state))
      (window-height . ,(alist-get 'total-height state))
      (window-parameters ,@params))))


;;
;; Hooks

;;;###autoload
(defun +popup-adjust-fringes-h ()
  "Hides the fringe in popup windows, restoring them if `+popup-buffer-mode' is
disabled."
  (when (+popup-window-p)
    (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
      (set-window-fringes nil f f fringes-outside-margins))))

;;;###autoload
(defun +popup-adjust-margins-h ()
  "Creates padding for the popup window determined by `+popup-margin-width',
restoring it if `+popup-buffer-mode' is disabled."
  (when (and +popup-margin-width (+popup-window-p))
    (unless (memq (window-parameter nil 'window-side) '(left right))
      (let ((m (if (bound-and-true-p +popup-buffer-mode) +popup-margin-width)))
        (set-window-margins nil m m)))))

(defvar hide-mode-line-format)
;;;###autoload
(defun +popup-set-modeline-on-enable-h ()
  "Don't show modeline in popup windows without a `modeline' window-parameter.
Possible values for this parameter are:

  t            show the mode-line as normal
  nil          hide the modeline entirely (the default)
  a function   `mode-line-format' is set to its return value

Any non-nil value besides the above will be used as the raw value for
`mode-line-format'."
  (when (bound-and-true-p +popup-buffer-mode)
    (let ((modeline (+popup-parameter 'modeline)))
      (cond ((eq modeline 't))
            ((null modeline)
             ;; TODO use `mode-line-format' window parameter instead (emacs 26+)
             (hide-mode-line-mode +1))
            ((let ((hide-mode-line-format
                    (if (functionp modeline)
                        (funcall modeline)
                      modeline)))
               (hide-mode-line-mode +1)))))))
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)

;;;###autoload
(defun +popup-unset-modeline-on-disable-h ()
  "Restore the modeline when `+popup-buffer-mode' is deactivated."
  (when (and (not (bound-and-true-p +popup-buffer-mode))
             (bound-and-true-p hide-mode-line-mode)
             (not (bound-and-true-p global-hide-mode-line-mode)))
    (hide-mode-line-mode -1)))

;;;###autoload
(defun +popup-close-on-escape-h ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup windows (see
`+popup/close-all')."
  (if (+popup-window-p)
      (+popup/close)
    (+popup/close-all)))

;;;###autoload
(defun +popup-cleanup-rules-h ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (setq +popup--display-buffer-alist
        (cl-delete-duplicates +popup--display-buffer-alist
                              :key #'car :test #'equal :from-end t))
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))

;;;###autoload
(defun +popup-kill-buffer-hook-h ()
  "TODO"
  (when-let (window (get-buffer-window))
    (when (+popup-window-p window)
      (let ((+popup--inhibit-transient t))
        (+popup--delete-window window)))))


;;
;; Commands

;;;###autoload
(defalias 'other-popup #'+popup/other)

;;;###autoload
(defun +popup/buffer ()
  "Open this buffer in a popup window."
  (interactive)
  (let ((+popup-default-display-buffer-actions
         '(+popup-display-buffer-stacked-side-window-fn))
        (display-buffer-alist +popup--display-buffer-alist)
        (buffer (current-buffer)))
    (push (+popup-make-rule "." +popup-defaults) display-buffer-alist)
    (bury-buffer)
    (pop-to-buffer buffer)))

;;;###autoload
(defun +popup/other ()
  "Cycle through popup windows, like `other-window'. Ignores regular windows."
  (interactive)
  (if-let* ((popups (cl-remove-if-not
                     (lambda (w) (or (+popup-window-p w)
                                ;; This command should be able to hop between
                                ;; windows with a `no-other-window'
                                ;; parameter, since `other-window' won't.
                                (window-parameter w 'no-other-window)))
                     (window-list))))
      (select-window (if (or (+popup-window-p)
                             (window-parameter nil 'no-other-window))
                         (let ((window (selected-window)))
                           (or (car-safe (cdr (memq window popups)))
                               (car (delq window popups))
                               (car popups)))
                       (car popups)))
    (user-error "No popups are open")))

;;;###autoload
(defun +popup/close (&optional window force-p)
  "Close WINDOW, if it's a popup window.

This will do nothing if the popup's `quit' window parameter is either nil or
'other. This window parameter is ignored if FORCE-P is non-nil."
  (interactive
   (list (selected-window)
         current-prefix-arg))
  (let ((window (or window (selected-window))))
    (when (and (+popup-window-p window)
               (or force-p
                   (memq (+popup-parameter-fn 'quit window window)
                         '(t current))))
      (when +popup--remember-last
        (+popup--remember (list window)))
      (delete-window window)
      t)))

;;;###autoload
(defun +popup/close-all (&optional force-p)
  "Close all open popup windows.

This will ignore popups with an `quit' parameter that is either nil or 'current.
This window parameter is ignored if FORCE-P is non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (memq (+popup-parameter-fn 'quit window window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc #'delete-window targets)
      t)))

;;;###autoload
(defun +popup/toggle ()
  "Toggle any visible popups.
If no popups are available, display the *Messages* buffer in a popup window."
  (interactive)
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows) (+popup/close-all t))
          ((ignore-errors (+popup/restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

;;;###autoload
(defun +popup/restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer . state) in +popup--last
           if (buffer-live-p buffer)
           do (+popup-buffer buffer (+popup-alist-from-window-state state)))
  (setq +popup--last nil)
  t)

;;;###autoload
(defun +popup/raise (window &optional arg)
  "Raise the current popup window into a regular window and
return it. If prefix ARG, raise the current popup into a new
window and return that window."
  (interactive
   (list (selected-window) current-prefix-arg))
  (cl-check-type window window)
  (unless (+popup-window-p window)
    (user-error "Cannot raise a non-popup window"))
  (let ((buffer (current-buffer))
        (+popup--inhibit-transient t)
        +popup--remember-last)
    (+popup/close window 'force)
    (let (display-buffer-alist)
      (if arg
          (pop-to-buffer buffer)
        (switch-to-buffer buffer)))
    (selected-window)))

;;;###autoload
(defun +popup/diagnose ()
  "Reveal what popup rule will be used for the current buffer."
  (interactive)
  (if-let* ((rule (cl-loop with bname = (buffer-name)
                           for (pred . action) in display-buffer-alist
                           if (and (functionp pred) (funcall pred bname action))
                           return (cons pred action)
                           else if (and (stringp pred) (string-match-p pred bname))
                           return (cons pred action))))
      (message "Rule matches: %s" rule)
    (message "No popup rule for this buffer")))


;;
;;; Advice

;;;###autoload
(defun +popup-close-a (&rest _)
  "TODO"
  (+popup/close nil t))

;;;###autoload
(defun +popup-save-a (fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (save-popups! (apply fn args)))

;;;###autoload
(defun +popup-display-buffer-fullframe-fn (buffer alist)
  "Displays the buffer fullscreen."
  (let ((wconf (current-window-configuration)))
    (when-let (window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist)))
      (set-window-parameter window 'saved-wconf wconf)
      (add-to-list 'window-persistent-parameters '(saved-wconf . t))
      (delete-other-windows window)
      window)))

;;;###autoload
(defun +popup-display-buffer-stacked-side-window-fn (buffer alist)
  "A `display-buffer' action that serves as an alternative to
`display-buffer-in-side-window', but allows for stacking popups with the `vslot'
alist entry.

Accepts the same arguments as `display-buffer-in-side-window'. You must set
`window--sides-inhibit-check' to non-nil for this work properly."
  (let* ((side  (or (cdr (assq 'side alist)) 'bottom))
         (slot  (or (cdr (assq 'slot alist))  0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (display-buffer-mark-dedicated (or display-buffer-mark-dedicated 'popup)))

    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))

    (let* ((live (get-window-with-predicate
                  (lambda (window)
                    (and (eq (window-parameter window 'window-side) side)
                         (eq (window-parameter window 'window-vslot) vslot)))
                  nil))
           ;; As opposed to the `window-side' property, our `window-vslot'
           ;; parameter is set only on a single live window and never on internal
           ;; windows. Moreover, as opposed to `window-with-parameter' (as used
           ;; by the original `display-buffer-in-side-window'),
           ;; `get-window-with-predicate' only returns live windows anyway. In
           ;; any case, we will have missed the major side window and got a
           ;; child instead if the major side window happens to be an internal
           ;; window with multiple children. In that case, all childen should
           ;; have the same `window-vslot' parameter, and the major side window
           ;; is the parent of the live window.
           (prev (and live (window-prev-sibling live)))
           (next (and live (window-next-sibling live)))
           (prev-vslot (and prev (window-parameter prev 'window-vslot)))
           (next-vslot (and next (window-parameter next 'window-vslot)))
           (major (and live
                       (if (or (eq prev-vslot vslot) (eq next-vslot vslot))
                           (window-parent live)
                         live)))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next 'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots
            (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                 window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let (window (window--make-major-side-window buffer side slot alist))
                 (set-window-parameter window 'window-vslot vslot)
                 (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot)))
                       ((= this-slot slot) ; A window with a matching slot found
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        ;; Check if this window has a better slot value wrt the
                        ;; slot of the window we want.
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))

             ;; `this-window' is the first window with the same SLOT.
             ;; `prev-window' is the window with the largest slot < SLOT. A new
             ;; window will be created after it.
             ;; `next-window' is the window with the smallest slot > SLOT. A new
             ;; window will be created before it.
             ;; `best-window' is the window with the smallest absolute
             ;; difference of its slot and SLOT.
             (or (and this-window
                      ;; Reuse `this-window'.
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer this-window 'reuse alist))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'.
                               (let ((next-side (if left-or-right 'above 'left))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window nil next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'.
                               (let ((prev-side (if left-or-right 'below 'right))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window nil prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (set-window-parameter window 'window-vslot vslot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer window 'window alist))
                 (and best-window
                      ;; Reuse `best-window'.
                      (progn
                        ;; Give best-window the new slot value.
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer
                         buffer best-window 'reuse alist)))))))))

;;;###autoload
(defvar +popup--display-buffer-alist nil)

;;;###autoload
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default properties for popup rules defined with `set-popup-rule!'.")

;;;###autoload
(defun +popup-make-rule (predicate plist)
  (if (plist-get plist :ignore)
      (list predicate nil)
    (let* ((plist (append plist +popup-defaults))
           (alist
            `((actions       . ,(plist-get plist :actions))
              (side          . ,(plist-get plist :side))
              (size          . ,(plist-get plist :size))
              (window-width  . ,(plist-get plist :width))
              (window-height . ,(plist-get plist :height))
              (slot          . ,(plist-get plist :slot))
              (vslot         . ,(plist-get plist :vslot))))
           (params
            `((ttl      . ,(plist-get plist :ttl))
              (quit     . ,(plist-get plist :quit))
              (select   . ,(plist-get plist :select))
              (modeline . ,(plist-get plist :modeline))
              (autosave . ,(plist-get plist :autosave))
              ,@(plist-get plist :parameters))))
      `(,predicate (+popup-buffer)
                   ,@alist
                   (window-parameters ,@params)))))

;;;###autodef
(defun set-popup-rule! (predicate &rest plist)
  "Define a popup rule.

These rules affect buffers displayed with `pop-to-buffer' and `display-buffer'
(or their siblings). Buffers displayed with `switch-to-buffer' (and its
variants) will not be affected by these rules (as they are unaffected by
`display-buffer-alist', which powers the popup management system).

PREDICATE accepts anything that the CONDITION argument in `buffer-match-p' takes
(if you're on Emacs 29 or newer). On Emacs 28 or older, it can either be a) a
regexp string (matched against the buffer's name) or b) a function that takes
two arguments (a buffer name and the ACTION argument of `display-buffer') and
returns a boolean.

PLIST can be made up of any of the following properties:

:ignore BOOL
  If BOOL is non-nil, popups matching PREDICATE will not be handled by the popup
  system. Use this for buffers that have their own window management system like
  magit or helm.

:actions ACTIONS
  ACTIONS is a list of functions or an alist containing (FUNCTION . ALIST). See
  `display-buffer''s second argument for more information on its format and what
  it accepts. If omitted, `+popup-default-display-buffer-actions' is used.

:side 'bottom|'top|'left|'right
  Which side of the frame to open the popup on. This is only respected if
  `+popup-display-buffer-stacked-side-window-fn' or `display-buffer-in-side-window'
  is in :actions or `+popup-default-display-buffer-actions'.

:size/:width/:height FLOAT|INT|FN
  Determines the size of the popup. If more than one of these size properties are
  given :size always takes precedence, and is mapped with window-width or
  window-height depending on what :side the popup is opened. Setting a height
  for a popup that opens on the left or right is harmless, but comes into play
  if two popups occupy the same :vslot.

  If a FLOAT (0 < x < 1), the number represents how much of the window will be
    consumed by the popup (a percentage).
  If an INT, the number determines the size in lines (height) or units of
    character width (width).
  If a function, it takes one argument: the popup window, and can do whatever it
    wants with it, typically resize it, like `+popup-shrink-to-fit'.

:slot/:vslot INT
  (This only applies to popups with a :side and only if :actions is blank or
  contains the `+popup-display-buffer-stacked-side-window-fn' action) These control
  how multiple popups are laid out. INT can be any integer, positive and
  negative.

  :slot controls lateral positioning (e.g. the horizontal positioning for
    top/bottom popups, or vertical positioning for left/right popups).
  :vslot controls popup stacking (from the edge of the frame toward the center).

  Let's assume popup A and B are opened with :side 'bottom, in that order.
    If they possess the same :slot and :vslot, popup B will replace popup A.
    If popup B has a higher :slot, it will open to the right of popup A.
    If popup B has a lower :slot, it will open to the left of popup A.
    If popup B has a higher :vslot, it will open above popup A.
    If popup B has a lower :vslot, it will open below popup A.

:ttl INT|BOOL|FN
  Stands for time-to-live. It can be t, an integer, nil or a function. This
  controls how (and if) the popup system will clean up after the popup.

  If any non-zero integer, wait that many seconds before killing the buffer (and
    any associated processes).
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed and is left to its own devices.
  If t, resort to the default :ttl in `+popup-defaults'. If none exists, this is
    the same as nil.
  If a function, it takes one argument: the target popup buffer. The popup
    system does nothing else and ignores the function's return value.

:quit FN|BOOL|'other|'current
  Can be t, 'other, 'current, nil, or a function. This determines the behavior
  of the ESC/C-g keys in or outside of popup windows.

  If t, close the popup if ESC/C-g is pressed anywhere.
  If 'other, close this popup if ESC/C-g is pressed outside of any popup. This
    is great for popups you may press ESC/C-g a lot in.
  If 'current, close the current popup if ESC/C-g is pressed from inside of the
    popup. This makes it harder to accidentally close a popup until you really
    want to.
  If nil, pressing ESC/C-g will never close this popup.
  If a function, it takes one argument: the to-be-closed popup window, and is
    run when ESC/C-g is pressed while that popup is open. It must return one of
    the other values to determine the fate of the popup.

:select BOOL|FN
  Can be a boolean or function. The boolean determines whether to focus the
  popup window after it opens (non-nil) or focus the origin window (nil).

  If a function, it takes two arguments: the popup window and originating window
    (where you were before the popup opened). The popup system does nothing else
    and ignores the function's return value.

:modeline BOOL|FN|LIST
  Can be t (show the default modeline), nil (show no modeline), a function that
  returns a modeline format or a valid value for `mode-line-format' to be used
  verbatim. The function takes no arguments and is run in the context of the
  popup buffer.

:autosave BOOL|FN
  This parameter determines what to do with modified buffers when closing popup
  windows. It accepts t, 'ignore, a function or nil.

  If t, no prompts. Just save them automatically (if they're file-visiting
    buffers). Same as 'ignore for non-file-visiting buffers.
  If nil (the default), prompt the user what to do if the buffer is
    file-visiting and modified.
  If 'ignore, no prompts, no saving. Just silently kill it.
  If a function, it is run with one argument: the popup buffer, and must return
    non-nil to save or nil to do nothing (but no prompts).

:parameters ALIST
  An alist of custom window parameters. See `(elisp)Window Parameters'.

If any of these are omitted, defaults derived from `+popup-defaults' will be
used.

\(fn PREDICATE &key IGNORE ACTIONS SIDE SIZE WIDTH HEIGHT SLOT VSLOT TTL QUIT SELECT MODELINE AUTOSAVE PARAMETERS)"
  (declare (indent defun))
  (push (+popup-make-rule predicate plist) +popup--display-buffer-alist)
  ;; TODO: Don't overwrite user entries in `display-buffer-alist'
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

;;;###autodef
(defun set-popup-rules! (&rest rulesets)
  "Defines multiple popup rules.

Every entry in RULESETS should be a list of alists where the CAR is the
predicate and CDR is a plist. See `set-popup-rule!' for details on the predicate
and plist.

Example:

  (set-popup-rules!
    '((\"^ \\*\" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
      (\"^\\*\"  :slot 1 :vslot -1 :select t))
    '((\"^\\*Completions\" :slot -1 :vslot -2 :ttl 0)
      (\"^\\*Compil\\(?:ation\\|e-Log\\)\" :size 0.3 :ttl 0 :quit t)))"
  (declare (indent 0))
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup-make-rule (car rule) (cdr rule))
            +popup--display-buffer-alist)))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.")

(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . ignore) ; remove later
    (no-other-window . t))
  "The default window parameters.")

(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to disable margin
adjustment.")

(defvar +popup--inhibit-transient nil)
(defvar +popup--inhibit-select nil)
(defvar +popup--old-display-buffer-alist nil)
(defvar +popup--remember-last t)
(defvar +popup--last nil)
(defvar-local +popup--timer nil)


;;
;; Global modes

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when (featurep 'evil)
      ;; For maximum escape coverage in emacs state buffers; this only works in
      ;; GUI Emacs, in tty Emacs use C-g instead
      (define-key map [escape] #'dotfairy/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing Dotfairy's popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'dotfairy-escape-hook #'+popup-close-on-escape-h 'append)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist
               window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'dotfairy-escape-hook #'+popup-close-on-escape-h)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil)
         (+popup-cleanup-rules-h)
         (dolist (prop +popup-window-parameters)
           (delq (assq prop window-persistent-parameters)
                 window-persistent-parameters)))))

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and disabled when
that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t)
    (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
              nil 'local)
    (when (timerp +popup--timer)
      (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)


;;
;; Macros

(defmacro with-popup-rules! (rules &rest body)
  "Evaluate BODY with popup RULES. RULES is a list of popup rules. Each rule
should match the arguments of `+popup-define' or the :popup setting."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         display-buffer-alist)
     (set-popup-rules! ,rules)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     ,@body))

(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let* ((in-popup-p (+popup-buffer-p))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
          buffer-list-update-hook
          +popup--last)
     (dolist (p popups)
       (+popup/close p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup/restore)
           (unless in-popup-p
             (select-window origin)))))))

;;
;; Default popup rules & bootstrap

(set-popup-rules!
  '(("^\\*Completions" :ignore t)
    ("^\\*Local variables\\*$"
     :vslot -1 :slot 1 :size +popup-shrink-to-fit)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
     :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
    ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*\\(?:v?term\\|e?shell\\)"  ; editing buffers (interaction required)
     :vslot -5 :size 0.25 :select t :modeline nil :quit nil :ttl nil)
    ("^\\*\\(?:Wo\\)?Man "
     :vslot -6 :size 0.45 :select t :quit t :ttl 0)
    ("^\\*Calc"
     :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
    ("^\\*Customize"
     :slot 2 :side right :size 0.5 :select t :quit nil)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ;; `help-mode', `helpful-mode'
    ("^\\*\\([Hh]elp\\|Apropos\\)"
     :slot 2 :vslot -8 :size 0.42 :select t)
    ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
     :vslot -11 :size 0.35 :select t)
    ("^\\*xwidget"
     :vslot -11 :size 0.35 :select nil)
    ("^\\*info\\*$"  ; `Info-mode'
     :slot 2 :vslot 2 :size 0.45 :select t)
    ("^\\*docker" :vslot -5 :size 0.5 :select t :modeline nil :quit nil :ttl nil)
    ("^\\*Warnings" :vslot 99 :size 0.25)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*Flycheck error messages\\*" :select nil)
    ("^\\*Flycheck errors\\*" :size 0.25)
    ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t)))

(add-hook 'after-init-hook #'+popup-mode 'append)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           #'+popup-set-modeline-on-enable-h
           #'+popup-unset-modeline-on-disable-h)


(defadvice! +popup--make-case-sensitive-a (fn &rest args)
  "Make regexps in `display-buffer-alist' case-sensitive.

To reduce fewer edge cases and improve performance when `display-buffer-alist'
grows larger."
  :around #'display-buffer-assq-regexp
  (let (case-fold-search)
    (apply fn args)))

;; Don't try to resize popup windows
(advice-add #'balance-windows :around #'+popup-save-a)

(defun +popup/quit-window (&optional arg)
  "The regular `quit-window' sometimes kills the popup buffer and switches to a
buffer that shouldn't be in a popup. We prevent that by remapping `quit-window'
to this commmand."
  (interactive "P")
  (let ((orig-buffer (current-buffer)))
    (quit-window arg)
    (when (and (eq orig-buffer (current-buffer))
               (+popup-buffer-p))
      (+popup/close nil 'force))))
(define-key +popup-buffer-mode-map [remap quit-window] #'+popup/quit-window)


;;
;;; External functions

;;;###package buff-menu
(define-key Buffer-menu-mode-map (kbd "RET") #'Buffer-menu-other-window)

;;;###package compile
(defadvice! +popup--compilation-goto-locus-a (fn &rest args)
  "Fix links in popup compilation buffers creating a new window each time they
were followed."
  :around #'compilation-goto-locus
  (letf! (defun pop-to-buffer (buffer &optional action norecord)
           (let ((pop-up-windows (not (+popup-buffer-p (current-buffer)))))
             (funcall pop-to-buffer buffer action norecord)))
    (apply fn args)))


;;;###package eshell
(progn
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defadvice! +popup--eshell-undedicate-popup (&rest _)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    :before #'eshell-exec-visual
    (when (+popup-window-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
                           (set-window-dedicated-p nil t)))))


;;;###package evil
(progn
  (defadvice! +popup--evil-command-window-execute-a ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    :override #'evil-command-window-execute
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (execute-window (get-buffer-window evil-command-window-current-buffer))
          (popup (selected-window)))
      (if execute-window
          (select-window execute-window)
        (user-error "Originating buffer is no longer active"))
      ;; (kill-buffer "*Command Line*")
      (delete-window popup)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))

  ;; Don't mess with popups
  (advice-add #'+evil--window-swap           :around #'+popup-save-a)
  (advice-add #'evil-window-move-very-bottom :around #'+popup-save-a)
  (advice-add #'evil-window-move-very-top    :around #'+popup-save-a)
  (advice-add #'evil-window-move-far-left    :around #'+popup-save-a)
  (advice-add #'evil-window-move-far-right   :around #'+popup-save-a))


(after! help-mode
  (defun +popup--switch-from-popup (location)
    (let (origin enable-local-variables)
      (save-popups!
       (switch-to-buffer (car location) nil t)
       (if (not (cdr location))
           (message "Unable to find location in file")
         (goto-char (cdr location))
         (recenter)
         (setq origin (selected-window))))
      (select-window origin)))

  ;; Help buffers use `pop-to-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button types need to be
  ;; redefined to set aside the popup before following a link.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (when (eq file 'C-source)
        (setq file (help-C-file-name (indirect-function fun) 'fun)))
      (+popup--switch-from-popup (find-function-search-for-symbol fun nil file))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function
    (lambda (var &optional file)
      (when (eq file 'C-source)
        (setq file (help-C-file-name var 'var)))
      (+popup--switch-from-popup (find-variable-noselect var file))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (+popup--switch-from-popup (find-function-search-for-symbol fun 'defface file)))))


;;;###package helpful
(defadvice! +popup--helpful-open-in-origin-window-a (button)
  "Open links in non-popup, originating window rather than helpful's window."
  :override #'helpful--navigate
  (let ((path (substring-no-properties (button-get button 'path)))
        enable-local-variables
        origin)
    (save-popups!
     (find-file path)
     (when-let (pos (get-text-property button 'position
                                       (marker-buffer button)))
       (goto-char pos))
     (setq origin (selected-window))
     (recenter))
    (select-window origin)))

;;;###package Info
(defadvice! +popup--switch-to-info-window-a (&rest _)
  :after #'info-lookup-symbol
  (when-let (win (get-buffer-window "*info*"))
    (when (+popup-window-p win)
      (select-window win))))


;;;###package latex
(defadvice! +popup--use-popup-window-for-reftex-citation-a (fn &rest args)
  :around #'reftex-do-citation
  (letf! ((#'switch-to-buffer-other-window #'pop-to-buffer))
    (apply fn args)))


(after! org
  (defadvice! +popup--suppress-delete-other-windows-a (fn &rest args)
    "Org has a scorched-earth window management policy I'm not fond of. i.e. it
kills all other windows just so it can monopolize the frame. No thanks. We can
do better."
    :around #'org-add-log-note
    :around #'org-capture-place-template
    :around #'org-export--dispatch-ui
    :around #'org-agenda-get-restriction-and-command
    :around #'org-goto-location
    :around #'org-fast-tag-selection
    :around #'org-fast-todo-selection
    (if +popup-mode
        (letf! ((#'delete-other-windows #'ignore)
                (#'delete-window        #'ignore))
          (apply fn args))
      (apply fn args)))

  (defadvice! +popup--org-fix-goto-a (fn &rest args)
    "`org-goto' uses `with-output-to-temp-buffer' to display its help buffer,
for some reason, which is very unconventional, and so requires these gymnastics
to tame (i.e. to get the popup manager to handle it)."
    :around #'org-goto-location
    (if +popup-mode
        (letf! (defun internal-temp-output-buffer-show (buffer)
                 (let ((temp-buffer-show-function
                        (dotfairy-rpartial #'+popup-display-buffer-stacked-side-window-fn nil)))
                   (with-current-buffer buffer
                     (+popup-buffer-mode +1))
                   (funcall internal-temp-output-buffer-show buffer)))
          (apply fn args))
      (apply fn args)))

  (defadvice! +popup--org-fix-popup-window-shrinking-a (fn &rest args)
    "Hides the mode-line in *Org tags* buffer so you can actually see its
content and displays it in a side window without deleting all other windows.
Ugh, such an ugly hack."
    :around #'org-fast-tag-selection
    :around #'org-fast-todo-selection
    (if +popup-mode
        (letf! ((defun read-char-exclusive (&rest args)
                  (message nil)
                  (apply read-char-exclusive args))
                (defun split-window-vertically (&optional _size)
                  (funcall split-window-vertically (- 0 window-min-height 1)))
                (defun org-fit-window-to-buffer (&optional window max-height min-height shrink-only)
                  (when-let (buf (window-buffer window))
                    (with-current-buffer buf
                      (+popup-buffer-mode)))
                  (when (> (window-buffer-height window)
                           (window-height window))
                    (fit-window-to-buffer window (window-buffer-height window)))))
          (apply fn args))
      (apply fn args)))

  (defadvice! +popup--org-edit-src-exit-a (fn &rest args)
    "If you switch workspaces or the src window is recreated..."
    :around #'org-edit-src-exit
    (let* ((window (selected-window))
           (popup-p (+popup-window-p window)))
      (prog1 (apply fn args)
        (when (and popup-p (window-live-p window))
          (delete-window window))))))

;;;###package org-journal
(defadvice! +popup--use-popup-window-a (fn &rest args)
  :around #'org-journal--search-by-string
  (letf! ((#'switch-to-buffer #'pop-to-buffer))
    (apply fn args)))


;;;###package persp-mode
(defadvice! +popup--persp-mode-restore-popups-a (&rest _)
  "Restore popup windows when loading a perspective from file."
  :after #'persp-load-state-from-file
  (dolist (window (window-list))
    (when (+popup-parameter 'popup window)
      (+popup--init window nil))))

(after! pdf-tools
  (setq tablist-context-window-display-action
        '((+popup-display-buffer-stacked-side-window-fn)
          (side . left)
          (slot . 2)
          (window-height . 0.3)
          (inhibit-same-window . t))
        pdf-annot-list-display-buffer-action
        '((+popup-display-buffer-stacked-side-window-fn)
          (side . left)
          (slot . 3)
          (inhibit-same-window . t))))


;;;###package profiler
(defadvice! +popup--profiler-report-find-entry-in-other-window-a (fn function)
  :around #'profiler-report-find-entry
  (letf! ((#'find-function #'find-function-other-window))
    (funcall fn function)))


;;;###package undo-tree
(defadvice! +popup--use-popup-window-for-undo-tree-visualizer-a (fn &rest args)
  "TODO"
  :around #'undo-tree-visualize
  (if undo-tree-visualizer-diff
      (apply fn args)
    (letf! ((#'switch-to-buffer-other-window #'pop-to-buffer))
      (apply fn args))))

;;;###package wdired
(progn
  ;; close the popup after you're done with a wdired buffer
  (advice-add #'wdired-abort-changes :after #'+popup-close-a)
  (advice-add #'wdired-finish-edit :after #'+popup-close-a))

;;;###package wgrep
(progn
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'+popup-close-a)
  (advice-add #'wgrep-finish-edit :after #'+popup-close-a))


(after! which-key
  (when (eq which-key-popup-type 'side-window)
    (setq which-key-popup-type 'custom
          which-key-custom-popup-max-dimensions-function
          (lambda (_) (which-key--side-window-max-dimensions))
          which-key-custom-hide-popup-function #'which-key--hide-buffer-side-window
          which-key-custom-show-popup-function
          (lambda (act-popup-dim)
            (letf! (defun display-buffer-in-side-window (buffer alist)
                     (+popup-display-buffer-stacked-side-window-fn
                      buffer (append '((vslot . -9999) (select . t)) alist)))
              ;; HACK Fix #2219 where the which-key popup would get cut off.
              (setcar act-popup-dim (1+ (car act-popup-dim)))
              (which-key--show-buffer-side-window act-popup-dim))))))


;;;###package windmove
;; Users should be able to hop into popups easily, but Elisp shouldn't.
(defadvice! +popup--ignore-window-parameters-a (fn &rest args)
  "Allow *interactive* window moving commands to traverse popups."
  :around '(windmove-up windmove-down windmove-left windmove-right)
  (letf! (defun windmove-find-other-window (dir &optional arg window)
           (window-in-direction
            (pcase dir (`up 'above) (`down 'below) (_ dir))
            window (bound-and-true-p +popup-mode) arg windmove-wrap-around t))
    (apply fn args)))

(provide 'init-popup)
;;; init-popup.el ends here

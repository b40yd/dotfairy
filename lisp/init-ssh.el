;;; init-ssh.el --- A SSH manager remote servers  tools -*- lexical-binding: t -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
;; Keywords: ssh, tools
;; URL: https://github.com/7ym0n/dotfairy-ssh-manager
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (f "0.20.0"))

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
;; A SSH session manager and files upload or download tools for Emacs.
;; It's like `xshell', `mobaxterm' or other tools same work.

;;; Code:
(require 'cl-generic)
(require 'cl-lib)
(require 'dash)
(require 'term)
(require 'f)
(require 'dired)
(require 'subr-x)

(defcustom ssh-manager-sessions '()
  "Set SSH connect session."
  :group 'ssh-manager
  :type '((:session-name "demo"
           :kind "proxy"
           :proxy-host "localhost"
           :proxy-port "22"
           :proxy-user "root"
           :proxy-password ""
           :remote-host "127.0.0.1"
           :remote-port "22"
           :remote-user "root"
           :remote-password ""
           :totp-kind "FreeOTP" ; default '(FreeOTP custom)
           :totp-key ""
           :totp-message "verification code:")))



(defcustom ssh-manager-totp-hooks '((:name "FreeOTP"
                                     :function (lambda (&rest args)
                                                 (with-temp-buffer
                                                   (or (apply
                                                        #'call-process "oathtool" nil t nil `("--totp" "-b" ,@args))
                                                       "")
                                                   (string-trim (buffer-string)))))
                                    (:name "Custom"
                                     :function (lambda (&rest args)
                                                 (setq args (read-string "Enter TOTP key: ")))))
  "Set totp verification code hook."
  :group 'ssh-manager
  :type 'list)

(defun ssh-manager--all-totp-name ()
  "Lookup all TOTP name."
  (let ((names '()))
    (dolist (hook ssh-manager-totp-hooks)
      (let ((name  (plist-get hook :name)))
        (push name names)))
    names))

(defun ssh-manager-lookup-totp-func (name &rest args)
  "Lookup TOTP hook function.
Argument NAME TOTP kind name.
Optional argument ARGS TOTP handler function args."
  (let ((fun nil))
    (dolist (hook ssh-manager-totp-hooks)
      (if (string= name  (plist-get hook :name))
          (setq fun (plist-get hook :function))))
    (if (not (equal fun nil))
        (apply fun args))))

;; ssh-manager-mode
(cl-defstruct ssh-manager-session-groups
  ;; contains the folders that are part of the current session
  servers
  (metadata (make-hash-table :test 'equal)))

(defvar ssh-manager--session-groups nil
  "Contain the `ssh-manager-session-groups' for the current Emacs instance.")

(defun ssh-manager-session-groups ()
  "Get all session by group."
  (or ssh-manager--session-groups (setq ssh-manager--session-groups (make-ssh-manager-session-groups))))

(defun ssh-manager-show-ssh-session-groups ()
  "Show SSH server groups."
  (interactive)
  (print (ssh-manager-session-groups-servers (ssh-manager-session-groups))))

(defun ssh-manager-add-this-ssh-session-to-groups ()
  "Add this SSH server session to groups."
  (interactive)
  (cl-pushnew (buffer-name) (ssh-manager-session-groups-servers (ssh-manager-session-groups)) :test 'equal))

(defun ssh-manager-remove-this-ssh-session-from-groups ()
  "Remove this SSH server session from groups."
  (interactive)
  (ssh-manager--remove-buffer-name-from-groups (buffer-name)))

(defun ssh-manager--remove-buffer-name-from-groups (buf-name)
  "Remove buffer name from groups.
Argument BUF-NAME select buffer name."

  (setf (ssh-manager-session-groups-servers (ssh-manager-session-groups))
        (-remove-item buf-name (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))

(defun ssh-manager-remove-ssh-session-from-groups (session)
  "Remove SSH server SESSION from groups."
  (interactive  (list (completing-read "Select server to connect: "
                                       (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))
  (ssh-manager--remove-buffer-name-from-groups session))

(defun ssh-manager-send-cmd-to-session-groups (cmd)
  "Send CMD to session."
  (let ((current-buf (current-buffer)))
    (dolist (server (->> (ssh-manager-session-groups)
                         (ssh-manager-session-groups-servers)))
      (ssh-manager--send-cmd-to-buffer server cmd))
    (switch-to-buffer current-buf)))

(defvar ssh-manager-mode-map (let ((keymap (make-sparse-keymap)))
                               (define-key keymap (kbd "C-c C-c") 'ssh-manager-execute-buffer-cmd-to-ssh)
                               (define-key keymap (kbd "C-c C-e") 'ssh-manager-execute-region-cmd-to-ssh)
                               (define-key keymap (kbd "C-c C-.") 'ssh-manager-execute-current-line-cmd-to-ssh)
                               keymap)
  "Keymap for `ssh-manager-mode'.")

(define-derived-mode ssh-manager-mode prog-mode "SSH Manager Mode"
  (with-no-warnings
    (font-lock-fontify-buffer))
  (use-local-map ssh-manager-mode-map))

(defun ssh-manager-execute-current-line-cmd-to-ssh ()
  "Execute command to SSH server groups."
  (interactive)
  (let ((line (ssh-manager-read-current-line-cmd)))
    (ssh-manager-send-cmd-to-session-groups line)
    (reindent-then-newline-and-indent)))

(defun ssh-manager-read-current-line-cmd ()
  "Read current line command."
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun ssh-manager-execute-region-cmd-to-ssh ()
  "Execute region cmd to SSH."
  (interactive)
  (let ((begin (region-beginning))
        (end (region-end)))
    (ssh-manager-send-cmd-to-session-groups (buffer-substring begin end))))

(defun ssh-manager-execute-buffer-cmd-to-ssh ()
  "Execute buffer cmd to SSH."
  (interactive)
  (ssh-manager-send-cmd-to-session-groups (buffer-substring-no-properties (point-min) (point-max))))

(defun ssh-manager ()
  "Enable SSH manager mode."
  (interactive)
  (let ((buffer (generate-new-buffer "*SSH Manager*")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)
    (funcall 'ssh-manager-mode)
    (setq buffer-offer-save t)))

;; SSH connect session
(cl-defstruct ssh-manager-session
  ;; contains the folders that are part of the current session
  servers
  folders
  (metadata (make-hash-table :test 'equal)))

(defcustom ssh-manager-session-file (expand-file-name (locate-user-emacs-file ".ssh-manager-session-v1"))
  "File where session information is stored."
  :group 'ssh-manager
  :type 'file)

(defvar ssh-manager--session nil
  "Contain the `ssh-manager-session' for the current Emacs instance.")
(defvar ssh-manager--show-message t
  "If non-nil, show debug message from `ssh-manager'.")

(defun ssh-manager--message  (format &rest args)
  "Wapper for message.
Argument FORMAT format.
Optional argument ARGS."
  (when ssh-manager--show-message
    (let ((inhibit-message (and (minibufferp)
                                (version< emacs-version "27.0"))))
      (apply #'message format args))))

(defun ssh-manager--info (format &rest args)
  "Display `ssh-manager' info message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'success) (apply #'format format args)))

(defun ssh-manager--warn (format &rest args)
  "Display `ssh-manager' warn message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'warning) (apply #'format format args)))

(defun ssh-manager--error (format &rest args)
  "Display `ssh-manager' error message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'error) (apply #'format format args)))
(defun ssh-manager--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun ssh-manager--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME.
This function creates the parent directories if they don't exist
yet."
  (let ((print-length nil)
        (print-level nil))
    ;; Create all parent directories:
    (apply #'f-mkdir (f-split (f-parent file-name)))
    (f-write-text (prin1-to-string to-persist) 'utf-8 file-name)))

(defun ssh-manager--persist-session (session)
  "Persist SESSION to `ssh-manager-session-file'."
  (ssh-manager--persist ssh-manager-session-file (make-ssh-manager-session
                                                  :servers (ssh-manager-session-servers session)
                                                  :folders (ssh-manager-session-folders session))))
(defun ssh-manager--load-default-session ()
  "Load default session."
  (setq ssh-manager--session (or (condition-case err
                                     (ssh-manager--read-from-file ssh-manager-session-file)
                                   (error (ssh-manager--error "Failed to parse the session %s, starting with clean one? "
                                                              (error-message-string err))
                                          nil))
                                 (make-ssh-manager-session))))

(defun ssh-manager-session ()
  "Get the session associated with the current buffer."
  (or ssh-manager--session (setq ssh-manager--session (ssh-manager--load-default-session))))

(defun ssh-manager--term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

;;; This code is referenced from multi-term.el
(defcustom ssh-manager-term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'ssh-manager)

(defcustom ssh-manager-term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("<escape>" . ssh-manager-term-send-esc)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . ssh-manager-term-send-return)
    ("C-y" . term-paste)
    ("<backspace>" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . ssh-manager-term-send-forward-kill-word)
    ("M-N" . ssh-manager-term-send-backward-kill-word)
    ("<C-backspace>" . ssh-manager-term-send-backward-kill-word)
    ("C-c C-a" . ssh-manager-add-this-ssh-session-to-groups)
    ("C-c C-r" . ssh-manager-remove-this-ssh-session-from-groups)
    ("C-c M-a" . ssh-manager-show-ssh-session-groups)
    ("M-," . term-send-raw)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'ssh-manager)

(defun ssh-manager-term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun ssh-manager-term-send-return ()
  "Use `term-send-raw-string' \\C\\-m instead `term-send-input'.
Because `term-send-input' have bug that will duplicate input when you \\C\\-a and \\C\\-m in terminal."
  (interactive)
  (term-send-raw-string "\C-m"))

(defun ssh-manager-term-send-M-x ()
  "Type \\M\\-x in `term-mode'."
  (interactive)
  (term-send-raw-string "\ex"))

(defun ssh-manager-term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun ssh-manager-term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun ssh-manager-keystroke-setup ()
  "Keystroke setup of `term-char-mode'.
By default, the key bindings of `term-char-mode' conflict with user's keystroke.
  So this function unbinds some keys with `term-raw-map',
  and binds some keystroke with `term-raw-map'."
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (cl-dolist (unbind-key ssh-manager-term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (cl-dolist (element ssh-manager-term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))
;;; end

(defun ssh-manager--init-term-mode (term-name)
  "Init term mode.
Argument TERM-NAME set name."
  (remove-hook 'term-mode-hook 'ssh-manager-keystroke-setup)
  (add-hook 'term-mode-hook 'ssh-manager-keystroke-setup)
  (term-mode)
  (normal-erase-is-backspace-mode)
  (term-char-mode)
  (ssh-manager--term-handle-close)
  (add-hook 'kill-buffer-hook 'ssh-manager-term-kill-buffer-hook)
  (switch-to-buffer (format "*%s*" term-name))
  ;; use backspace delete
  (term-send-raw-string "stty erase '^?'\n"))

;;
;; sshpass for MacOS
;; curl -L https://raw.githubusercontent.com/kadwanev/bigboybrew/master/Library/Formula/sshpass.rb -o sshpass.rb
;; brew install sshpass.rb
;; sshpass with TOTP support https://github.com/dora38/sshpass.git
;; brew install oath-toolkit
;; yum install oathtool gnupg2
;; sshpass for Linux(debian, centos)
;; sudo apt-get install sshpass
;; sudo yum install sshpass
(defun ssh-manager-connect-ssh (server)
  "Connect to remote SERVER."
  (let* ((session-name (plist-get server :session-name))
         (kind (intern (plist-get server :kind)))
         (username (plist-get server :remote-user))
         (password (if (string= kind "proxy")
                       (plist-get server :proxy-password)
                     (plist-get server :remote-password)))
         (port (plist-get server :remote-port))
         (host (plist-get server :remote-host))
         (totp-key (ssh-manager-lookup-totp-func (plist-get server :totp-kind)
                                                 (plist-get server :totp-key)))
         (totp-message (if (string-empty-p (plist-get server :totp-message))
                           ""
                         (format "%s" (plist-get server :totp-message))))
         (proxy-host (plist-get server :proxy-host))
         (proxy-port (plist-get server :proxy-port))
         (proxy-user (plist-get server :proxy-user))
         (index 1))
    (while (buffer-live-p (get-buffer (format "*%s<%s>*" session-name index)))
      (setq index (1+ index)))
    (let* ((argv '())
           (term-name (format "%s<%s>" session-name index)))
      (if (not (string-empty-p password))
          (setq argv (append argv `("-p" ,password))))
      (if (not (string= totp-key nil))
          (setq argv (append argv `("-o" ,totp-key))))
      (if (not (string-empty-p totp-message))
          (setq argv (append argv `("-O" ,totp-message))))
      (setq argv (append argv `("ssh" "-o" "StrictHostKeychecking=no")))
      (if (not (string-empty-p username))
          (setq username (format "%s@" username)))
      (if (not (string-empty-p host))
          (setq argv (append argv `(,(format "%s%s" username host))))
        (ssh-manager--error "SSH hostname must be set. it cannot empty. "))
      (if (not (string-empty-p port))
          (setq argv (append argv `("-p" ,port))))
      (if (and (string= kind "proxy")
               (not (string= proxy-host nil))
               (not (string= proxy-user nil))
               (not (string= proxy-port nil)))
          (setq argv (append argv `("-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port)))))
      ;; (ssh-manager--info (mapconcat 'identity `("sshpass" ,@argv) " "))
      (set-buffer (apply 'make-term term-name
                         "sshpass"
                         nil
                         argv))
      (ssh-manager--init-term-mode term-name))))

(defun ssh-manager-term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    ;; Quit the current subjob
    ;; when have alive process with current term buffer.
    (when (term-check-proc (current-buffer))
      ;; Quit sub-process.
      (term-quit-subjob))
    (ssh-manager--info "removed %s from server groups." (buffer-name (current-buffer)))
    (ssh-manager--remove-buffer-name-from-groups (buffer-name (current-buffer)))))

(defun ssh-manager--send-cmd-to-buffer (&optional buffer string)
  "Send STRING to a shell process associated with BUFFER.
By default, BUFFER is \"*terminal*\" and STRING is empty."
  (let ((process (get-buffer-process (or buffer "*terminal*"))))
    (when (process-live-p process)
      (with-current-buffer (process-buffer process)
        (let ((input (or string "")))
          (cond ((derived-mode-p 'comint-mode)
                 (insert input)
                 (comint-send-input))
                ((derived-mode-p 'term-mode)
                 (term-send-string process input)
                 (term-send-input))))))))

(defun ssh-manager--filter-ssh-session ()
  "Filter SSH session name list."
  (let ((lst '()))
    (dolist (server (->> (ssh-manager-session)
                         (ssh-manager-session-servers)))
      (setq lst (append lst (list (plist-get server :session-name)))))
    (dolist (server ssh-manager-sessions)
      (setq lst (append lst (list (plist-get server :session-name)))))
    lst))

(defun ssh-manager-switch-to-server (session)
  "Select SSH server to connect.
Argument SESSION server session info."
  (interactive (list (completing-read "Select server to connect: "
                                      (ssh-manager--filter-ssh-session))))
  (let ((connect '()))
    (dolist (server (->> (ssh-manager-session)
                         (ssh-manager-session-servers)))
      (if (equal (plist-get server :session-name) session)
          (setq connect server)))

    (if (equal connect nil)
        (dolist (server ssh-manager-sessions)
          (if (equal (plist-get server :session-name) session)
              (setq connect server))))

    (if (not (equal connect nil))
        (ssh-manager-connect-ssh connect))))

(defun ssh-manager--read-session-config-from-minibuffer (&optional ssh-session-config)
  "Read session config from minibuffer.
Optional argument SSH-SESSION-CONFIG set session config."
  (let* ((ssh-session '())
         (session-name (read-string "Session Name: " (if (not (equal ssh-session-config nil))
                                                         (plist-get ssh-session-config :session-name)))))
    (if (string-empty-p session-name)
        (ssh-manager--error "Session name cannot empty. ")
      (let* ((kind (completing-read "Select connect style: " '(proxy direct)))
             (proxy-host (if (string= kind 'proxy)
                             (read-string "Proxy hostname: "
                                          (if (not (equal ssh-session-config nil))
                                              (plist-get ssh-session-config :proxy-host)))
                           ""))
             (proxy-port (if (string= kind 'proxy)
                             (read-string "Proxy port(22): "
                                          (if (not (equal ssh-session-config nil))
                                              (plist-get ssh-session-config :proxy-port)))
                           "22"))
             (proxy-user (if (string= kind 'proxy)
                             (read-string "Proxy username(root): "
                                          (if (not (equal ssh-session-config nil))
                                              (plist-get ssh-session-config :proxy-user)))
                           ""))
             (proxy-password (if (string= kind 'proxy)
                                 (read-passwd "Proxy password: "
                                              (if (not (equal ssh-session-config nil))
                                                  (plist-get ssh-session-config :proxy-password)))
                               ""))
             (remote-host (read-string "Remote hostname(127.0.0.1): " (if (not (equal ssh-session-config nil))
                                                                          (plist-get ssh-session-config :remote-host))))
             (remote-port (read-string "Remote hostport(22): " (if (not (equal ssh-session-config nil))
                                                                   (plist-get ssh-session-config :remote-port))))
             (remote-user (read-string "Remote username(root): " (if (not (equal ssh-session-config nil))
                                                                     (plist-get ssh-session-config :remote-user))))
             (remote-password (read-passwd "Remote password: "))
             (totp-enable (y-or-n-p "Are you use TOTP? "))
             (totp-kind (if totp-enable
                            (completing-read "Select TOTP kind: "
                                             (ssh-manager--all-totp-name))))
             (totp-key (if totp-enable
                           (read-passwd "2FA(TOTP) key: " (if (not (equal ssh-session-config nil))
                                                              (plist-get ssh-session-config :totp-key)))
                         ""))
             (totp-message (if totp-enable
                               (read-string "2FA(TOTP) message: " (if (not (equal ssh-session-config nil))
                                                                      (plist-get ssh-session-config :totp-message)))
                             "")))
        (setq ssh-session
              `(:session-name ,session-name
                :kind ,kind
                :proxy-host ,proxy-host
                :proxy-port ,(if (string-empty-p proxy-port)
                                 "22"
                               proxy-port)
                :proxy-user ,(if (string-empty-p proxy-user)
                                 "root"
                               proxy-user)
                :proxy-password ,(if (and (string-empty-p proxy-password)
                                          (string= kind 'proxy))
                                     (ssh-manager--error "Proxy connect password cannot empty. ")
                                   proxy-password)
                :remote-host ,remote-host
                :remote-port ,(if (string-empty-p remote-port)
                                  "22"
                                remote-port)
                :remote-user ,(if (string-empty-p remote-user)
                                  "root"
                                remote-user)
                :remote-password ,remote-password
                :totp-kind ,totp-kind
                :totp-key ,totp-key
                :totp-message ,totp-message))))
    ssh-session))

(defun ssh-manager-create-ssh-remote ()
  "Create and connect SSH session."
  (interactive)
  (let* ((ssh-session (ssh-manager--read-session-config-from-minibuffer)))
    (cl-pushnew ssh-session
                (ssh-manager-session-servers (ssh-manager-session)) :test 'equal)
    (ssh-manager--persist-session (ssh-manager-session))
    (cond ((string= (plist-get ssh-session :kind) 'proxy)
           (if (or (string-empty-p (plist-get ssh-session :proxy-host))
                   (string-empty-p (plist-get ssh-session :remote-host)))
               (ssh-manager--error "<Proxy host> and <Remote host> must be set. please check it. ")
             (ssh-manager-connect-ssh ssh-session)))
          ((string= (plist-get ssh-session :kind) 'direct)
           (if (string-empty-p (plist-get ssh-session :remote-host))
               (ssh-manager--error "<Remote host> must be set. it cannot empty. ")
             (ssh-manager-connect-ssh ssh-session))))))


(defun ssh-manager-edit-ssh-session-config (session)
  "Edit SSH SESSION config."
  (interactive (list (completing-read "Select server to edit: "
                                      (ssh-manager--filter-ssh-session))))

  (let* ((let-sessions (ssh-manager-session))
         (let-server nil))
    (dolist (server (->> let-sessions
                         (ssh-manager-session-servers)))
      (if (string= session (plist-get server :session-name))
          (setq let-server server)))
    (cl-pushnew (ssh-manager--read-session-config-from-minibuffer let-server)
                (ssh-manager-session-servers let-sessions) :test 'equal)
    (setf (ssh-manager-session-servers let-sessions)
          (-remove-item let-server (ssh-manager-session-servers let-sessions)))
    (ssh-manager--persist-session let-sessions)))

(defun ssh-manager-remove-ssh-server (session)
  "Remove SESSION from the list of servers."
  (interactive (list (completing-read "Select server to connect: "
                                      (ssh-manager--filter-ssh-session))))
  (dolist (server (->> (ssh-manager-session)
                       (ssh-manager-session-servers)))
    (if (string= session (plist-get server :session-name))
        (let* ((let-sessions (ssh-manager-session)))
          (setf (ssh-manager-session-servers let-sessions)
                (-remove-item server (ssh-manager-session-servers let-sessions)))
          (ssh-manager--persist-session (ssh-manager-session))))))

(defun ssh-manager-remove-history-file-from-ssh-server (history)
  "Remove HISTORY file from the list of folders."
  (interactive (list (completing-read "Select remove from folders: "
                                      (ssh-manager-session-folders (ssh-manager-session)))))
  (let* ((let-sessions (ssh-manager-session)))
    (setf (ssh-manager-session-folders let-sessions)
          (-remove-item history (ssh-manager-session-folders let-sessions)))
    (ssh-manager--persist-session (ssh-manager-session))))

(defun ssh-manager-install-tools ()
  "Install SSH manager tools."
  (interactive)
  (if (not (executable-find "sshpass"))
      (ssh-manager-exec-process "sh" "-c" (concat
                                           "rm -rf /tmp/sshpass &&"
                                           " git"
                                           " clone"
                                           " https://github.com/dora38/sshpass"
                                           " /tmp/sshpass"
                                           " &&"
                                           " cd /tmp/sshpass"
                                           " &&"
                                           " ./bootstrap"
                                           " &&"
                                           " ./configure --prefix=/usr/local "
                                           "&&"
                                           " make install; cd -")))
  (if (not (executable-find "oathtool"))
      (ssh-manager--info "your need install oathtool if used 2FA."))
  (ssh-manager--info "installed."))

(defun ssh-manager--replace-in-string (what with in)
  "Replace substring.
Argument WHAT regexp.
Argument WITH string.
Argument IN substring."
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

;;;###autoload
(defun ssh-manager-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.
Unlike `ssh-manager-call-process',
this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell,
so batch scripts could run external programs
synchronously without sacrificing their output.

Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "ssh-manager"
                               :buffer (current-buffer)
                               :command (cons command (remq nil args))
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (_process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(defun ssh-manager--upload-or-download-files (server method cmd)
  "Use rsync command tool upload or download files.
Argument SERVER SSH session.
Argument METHOD select download or upload.
Argument CMD use rsync or scp."
  (let* ((argv '())
         (kind (plist-get server :kind))
         (password (if (string= kind 'proxy)
                       (plist-get server :proxy-password)
                     (plist-get server :remote-password)))
         (totp-kind (plist-get server :totp-kind))
         (totp-key (if (or (null totp-kind)
                           (string= totp-kind ""))
                       ""
                     (ssh-manager-lookup-totp-func totp-kind
                                                   (plist-get server :totp-key))))
         (totp-message (plist-get server :totp-message))
         (proxy-host (plist-get server :proxy-host))
         (proxy-port (plist-get server :proxy-port))
         (proxy-user (plist-get server :proxy-user))
         (host (plist-get server :remote-host))
         (port (plist-get server :remote-port))
         (user (plist-get server :remote-user)))
    (cond ((string= cmd "rsync")
           (if (not (string-empty-p password))
               (setq argv (append argv `("sshpass" "-p" ,password))))
           (if (not (string-empty-p totp-key))
               (setq argv (append argv `("-o" ,totp-key))))
           (if (not (string-empty-p totp-message))
               (setq argv (append argv `("-O" ,(format "'%s'" totp-message)))))
           (if (string= kind 'proxy)
               (setq argv (append argv `("ssh" "-o" "'StrictHostKeychecking=no'" "-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port))))
             (setq argv (append argv `("ssh" "-o" "'StrictHostKeychecking=no'"))))
           (if (not (string-empty-p port))
               (setq argv (append argv `("-p" ,port))))
           (setq argv (list "rsync" "-r" "-P" (concat "-e \"" (mapconcat 'identity argv " ") "\""))))
          ((string= cmd "scp")
           (if (not (string-empty-p password))
               (setq argv (append argv `("-p" ,password))))
           (if (not (string-empty-p totp-key))
               (setq argv (append argv `("-o" ,totp-key))))
           (if (not (string-empty-p totp-message))
               (setq argv (append argv `("-O" ,totp-message))))
           (if (string-empty-p host)
               (ssh-manager--error "SSH hostname must be set. HOST cannot empty. ")
             (setq argv (append argv `("scp" "-r" "-o" "StrictHostKeychecking=no")))
             (if (string= kind 'proxy)
                 (setq argv (append argv `("-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port)))))
             (if (not (string-empty-p port))
                 (setq argv (append argv `("-P" ,port)))))))
    (if (and (not (string-empty-p host))
             (not (string-empty-p user)))
        (let* ((remote-dir-or-file (completing-read (format "Set remote file path (/home/%s): " user)
                                                    (ssh-manager-session-folders (ssh-manager-session))
                                                    nil nil))
               (target nil))
          (if (string-empty-p remote-dir-or-file)
              (setq remote-dir-or-file (format "/home/%s" user)))
          (cl-pushnew remote-dir-or-file (ssh-manager-session-folders (ssh-manager-session)) :test 'equal)
          (ssh-manager--persist-session (ssh-manager-session))
          (cond ((string= method "upload")
                 (if (derived-mode-p 'dired-mode)
                     (setq argv (append argv `(,@(dired-get-marked-files) ,(format "%s@%s:%s" user host remote-dir-or-file))))
                   (if-let ((ask (y-or-n-p "Upload current buffer file? ")))
                       (setq target (buffer-file-name))
                     (setq target (read-file-name "Set upload for files: " )))
                   (setq argv (append argv `(,target ,(format "%s@%s:%s" user host remote-dir-or-file))))))
                ((string= method "download")
                 (if (derived-mode-p 'dired-mode)
                     (setq argv (append argv `(,(format "%s@%s:%s" user host remote-dir-or-file) ,(dired-current-directory))))
                   (setq target (read-file-name "Set download to: "))
                   (setq argv (append argv `(,(format "%s@%s:%s" user host remote-dir-or-file) ,target))))))))))

(defun ssh-manager-upload-or-download-files-to-remote-host (method)
  "SSH upload or download files.
Argument METHOD select download or upload."
  (interactive (list (completing-read "Select upload or download: "
                                      '(upload download))))
  (let ((session-name (completing-read "Select connect to server: "
                                       (ssh-manager--filter-ssh-session))))
    (dolist (session (->> (ssh-manager-session)
                          (ssh-manager-session-servers)))
      (if (string= session-name (plist-get session :session-name))
          (cond ((executable-find "rsync")
                 (if-let ((argv (ssh-manager--upload-or-download-files session method "rsync")))
                     (ssh-manager-exec-process "sh" "-c" (mapconcat 'identity argv " "))))
                ((executable-find "scp")
                 (if-let ((argv (ssh-manager--upload-or-download-files session method "scp")))
                     (apply 'ssh-manager-exec-process "sshpass" argv))))))
    (if (derived-mode-p 'dired-mode)
        (cond ((string= method "download")
               (revert-buffer))
              ((string= method "upload")
               (dired-unmark-all-marks))))))
(provide 'init-ssh)
;;; ssh-manager.el ends here

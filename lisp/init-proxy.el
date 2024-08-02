;;; init-proxy.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-custom)
(defvar socks-noproxy)
(defvar socks-server)

;; Network Proxy
(defun dotfairy/show-http-proxy ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" dotfairy-proxy)
    (message "No HTTP proxy")))

(defun dotfairy/enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,dotfairy-proxy)
          ("https" . ,dotfairy-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (dotfairy/show-http-proxy))

(defun dotfairy/disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (dotfairy/show-http-proxy))

(defun dotfairy/toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (dotfairy/disable-http-proxy)
    (dotfairy/enable-http-proxy)))

(defun dotfairy/show-socks-proxy ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun dotfairy/enable-socks-proxy ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string dotfairy-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" dotfairy-socks-proxy))
  (dotfairy/show-socks-proxy))

(defun dotfairy/disable-socks-proxy ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (dotfairy/show-socks-proxy))

(defun dotfairy/toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-server)
      (dotfairy/disable-socks-proxy)
    (dotfairy/enable-socks-proxy)))

(defun dotfairy/enable-proxy ()
  "Enbale proxy."
  (interactive)
  (dotfairy/enable-http-proxy)
  (dotfairy/enable-socks-proxy))

(defun dotfairy/disable-proxy ()
  "Disable proxy."
  (interactive)
  (dotfairy/disable-http-proxy)
  (dotfairy/disable-socks-proxy))

(defun dotfairy/toggle-proxy ()
  "Toggle proxy."
  (interactive)
  (dotfairy/toggle-http-proxy)
  (dotfairy/toggle-socks-proxy))

(provide 'init-proxy)
;;; init-proxy.el ends here

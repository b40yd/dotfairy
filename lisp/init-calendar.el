;;; init-calendar.el ---                                   -*- lexical-binding: t; -*-

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
(use-package cal-china-x
  :after calendar
  :autoload cal-china-x-setup
  :init (cal-china-x-setup)
  :config
  ;; Holidays
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 7 7 "七夕节")
                                       (holiday-fixed 3 8 "妇女节")
                                       (holiday-fixed 3 12 "植树节")
                                       (holiday-fixed 5 4 "青年节")
                                       (holiday-fixed 6 1 "儿童节")
                                       (holiday-lunar 9 9 "重阳节")
                                       (holiday-fixed 9 10 "教师节"))

        holiday-other-holidays '((holiday-fixed 2 14 "情人节")
                                 (holiday-fixed 4 1 "愚人节")
                                 (holiday-float 5 0 2 "母亲节")
                                 (holiday-float 6 0 3 "父亲节")
                                 (holiday-float 11 4 4 "感恩节")
                                 (holiday-fixed 12 25 "圣诞节"))
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays
                                  holiday-other-holidays)))

(use-package calfw
  :commands cfw:open-calendar-buffer
  :config
  ;; better frame for calendar
  ;; Another unicode chars
  (setq cfw:face-item-separator-color nil
        cfw:render-line-breaker 'cfw:render-line-breaker-none)
  ;; Month
  (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])

  ;; Week days
  (setq calendar-day-name-array
        ["星期天" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])

  (define-key cfw:calendar-mode-map "q" #'+calendar/quit)

  (add-hook 'cfw:calendar-mode-hook #'dotfairy-mark-buffer-as-real-h)
  ;; (add-hook 'cfw:calendar-mode-hook #'hide-mode-line-mode)

  :init
  (require 'calfw)
  (defvar cfw:git-current-file nil "current buffer's file name")
  (defvar cfw:git-command "git" "git command")

  (defun cfw:git-schedule-period-to-calendar (begin end)
    (let ((begin-date (cfw:strtime begin))
          (end-date (cfw:strtime end)))
      (with-temp-buffer
        (cd cfw:git-current-file)       ;;; NEW
        (call-process cfw:git-command nil (current-buffer) nil
                      "--no-pager" "log"
                      "--after" begin-date "--before" end-date
                      "--date" "iso" "--stat"
                      "--pretty=format:%x00%ad%n%H%n%an%n%s%n%b%n")
        ;; 0 date / hash / author name / subject / body
        (goto-char (point-min))
        (let (entries (cont t) pps ps)
          (while cont
            (setq ps (re-search-forward "\x0" nil t))
            (unless pps (setq pps ps))
            (cond
             ((and ps (< pps ps))
              (let* ((lines (split-string (buffer-substring pps (1- ps)) "\n"))
                     (datestr (nth 0 lines)) (date (date-to-time datestr))
                     (hash (nth 1 lines))
                     (author (nth 2 lines))
                     (subject (nth 3 lines))
                     (body (mapconcat 'identity (nthcdr 4 lines) "\n"))
                     (e (make-cfw:event :title subject :start-date (cfw:emacs-to-calendar date)
                                        :description (format "Author: %s\nHash:%s\n%s" author hash body))))
                (push e entries))
              (setq pps ps))
             (ps nil) ; try next search
             (t (setq cont nil))))
          entries))))

  (defun cfw:git-create-source (&optional color)
    "Create a git log source."
    (make-cfw:source
     :name "git log"
     :color (or color "SteelBlue")
     :update 'cfw:git-schedule-cache-clear
     :data 'cfw:git-schedule-period-to-calendar))

  (defun cfw:git-open-calendar ()
    "Open git log calendar."
    (interactive)
    (setq cfw:git-current-file (file-name-directory buffer-file-name))  ;;; NEW
    (let ((cp (cfw:create-calendar-component-buffer
               :view 'month
               :contents-sources (list (cfw:git-create-source)))))
      (switch-to-buffer (cfw:cp-get-buffer cp)))))

;;;###autoload
(defun dotfairy-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

(defvar +calendar-open-function #'+calendar/open-calendar
  "TODO")

(defvar +calendar--wconf nil)
(defvar +calendar-workspace-name "*calendar*"
  "Name of the workspace created by `=calendar', dedicated to calfw.")
(defun +calendar--init ()
  (if-let (win (cl-find-if (lambda (b) (string-match-p "^\\*cfw:" (buffer-name b)))
                           (dotfairy-visible-windows)
                           :key #'window-buffer))
      (select-window win)
    (call-interactively +calendar-open-function)))

;;;###autoload
(defun =calendar ()
  "Activate (or switch to) `calendar' in its workspace."
  (interactive)

  (setq +calendar--wconf (current-window-configuration))
  (delete-other-windows)
  (switch-to-buffer (dotfairy-fallback-buffer))
  (+calendar--init))

;;;###autoload
(defun +calendar/quit ()
  "TODO"
  (interactive)
  (when (window-configuration-p +calendar--wconf)
    (set-window-configuration +calendar--wconf))
  (setq +calendar--wconf nil)
  (dotfairy-kill-matching-buffers "^\\*cfw[:-]"))
  ;;;###autoload
(defun +calendar/open-calendar ()
  "TODO"
  (interactive)
  (cfw:open-calendar-buffer
   ;; :custom-map cfw:my-cal-map
   :contents-sources
   (list
    (cfw:org-create-source (face-foreground 'default))  ; orgmode source
    )))

(use-package calfw-org
  :commands (cfw:open-org-calendar
             cfw:org-create-source
             cfw:org-create-file-source
             cfw:open-org-calendar-withkevin))


(use-package calfw-cal
  :commands (cfw:cal-create-source))


(use-package calfw-ical
  :commands (cfw:ical-create-source))


(use-package org-gcal
  :defer t
  :init
  (defvar org-gcal-dir (concat dotfairy-cache-dir "org-gcal/"))
  (defvar org-gcal-token-file (concat org-gcal-dir "token.gpg")))


(provide 'init-calendar)
;;; init-calendar.el ends here

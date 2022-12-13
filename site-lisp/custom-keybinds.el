;;; custom-keybinds.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

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
(require 'init-funcs)

;; Sensible deafult key bindings for non-evil users
(setq dotfairy-leader-alt-key "C-c"
      dotfairy-localleader-alt-key "C-c SPC")

;; persp-mode and projectile in different prefixes
(setq persp-keymap-prefix (kbd "C-c w"))
(after! projectile
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(autoload 'org-capture-goto-target "org-capture" nil t)
(map! :leader
  (:prefix-map ("a" . "appliction")
   (:prefix ("c" . "Command log")
    :desc "Enable command log mode"    "e" #'command-log-mode
    :desc "Toggle command log buffer"  "g" #'clm/toggle-command-log-buffer)
   (:prefix ("m" . "Music Player")
    :desc "Music Player"                        "b" #'bongo
    :desc "Music Player for mpd"                "m" #'mpc
    :desc "Music Player for mingus"             "M" #'mingus
    :desc "Simple Player for mpc"               "s" #'simple-mpc)
   :desc "Reading"                             "r" #'olivetti-mode
   :desc "Quick Run"                           "R" #'quickrun-hydra/body)

  (:prefix-map ("b" . "buffers")
   :desc "Kill all buffers"                    "a" #'dotfairy/kill-all-buffers
   :desc "Kill this buffer in all windows"     "A" #'dotfairy/kill-this-buffer-in-all-windows
   :desc "Switch to buffer"                    "b" #'switch-to-buffer
   :desc "Kill buffer"                         "k" #'kill-buffer
   :desc "Kill buried buffers"                 "K" #'dotfairy/kill-buried-buffers
   :desc "New buffer"                          "n" #'+default/new-buffer
   :desc "Save and kill buffer"                "s" #'dotfairy/save-and-kill-buffer
   :desc "Kill other buffers"                  "o" #'dotfairy/kill-other-buffers
   :desc "kill matching buffers"               "m" #'dotfairy/kill-matching-buffers
   )
      ;;; <leader> c --- code
  (:prefix-map ("c" . "coding")
   :desc "Compile or Recompile"                "c" #'+default/compile)
  (:prefix-map ("e" . "editor")
   :desc "Hungry delete backward"              "b" #'hungry-delete-backward
   :desc "Dired change to wdired-mode"         "e" #'wdired-change-to-wdired-mode
   :desc "Hungry delete forward"               "f" #'hungry-delete-forward
   :desc "Delete trailing whitespace"          "w" #'delete-trailing-whitespace
   :desc "List errors"                         "x" #'flycheck-list-errors)
      ;;; <leader> f --- file
  (:prefix-map ("f" . "file")
   :desc "Open project editorconfig"   "." #'editorconfig-find-current-editorconfig
   :desc "Copy this file"              "c" #'dotfairy/copy-this-file
   :desc "Rename this file name"       "C" #'dotfairy/copy-file-name
   :desc "Find directory"              "d" #'dired
   :desc "Delete this file"            "D" #'dotfairy/delete-this-file
   :desc "Find file in emacs.d"        "e" #'dotfairy/find-file-in-emacsd
   :desc "Browse in emacs.d"           "E" #'dotfairy/browse-in-emacsd
   :desc "Find file"                   "f" #'find-file
   :desc "Find file from here"         "F" #'+default/find-file-under-here
   :desc "Locate file"                 "l" #'locate
   :desc "Reload init file"            "L" #'dotfairy/reload-init-file
   :desc "Rename/move this file"       "m" #'dotfairy/move-this-file
   :desc "Rename this buffer file"     "M" #'dotfairy/rename-this-file
   :desc "Recent files"                "r" #'recentf-open-files
   :desc "Recent project files"        "R" #'projectile-recentf
   :desc "Sudo this file"              "u" #'dotfairy/sudo-this-file
   :desc "Sudo find file"              "U" #'dotfairy/sudo-find-file
   :desc "Open init file"              "i" #'dotfairy/open-init-file
   :desc "Open custom file"            "I" #'dotfairy/open-custom-file)

      ;;; <leader> s --- search
  (:prefix-map ("s" . "search")
   :desc "Internet Search Engine"       "/" #'webjump
   :desc "Search All Opened buffer"     "B" #'swiper-all
   :desc "Search current directory"     "d" #'+default/search-cwd
   :desc "Search other directory"       "D" #'+default/search-other-cwd
   :desc "Counsel grep or swiper"       "g" #'counsel-grep-or-swiper
   :desc "Counsel grep"                 "G" #'counsel-grep
   :desc "Counsel git grep"             "j" #'counsel-git-grep
   :desc "Jump to bookmark"             "m" #'bookmark-jump
   :desc "Search project"               "p" #'+default/search-project
   :desc "Search other project"         "P" #'+default/search-other-project
   :desc "Counsel rg search"            "r" #'counsel-rg
   :desc "rg menu"                      "R" #'rg-menu
   :desc "Search buffer"                "s" #'+default/search-buffer
   :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point)
      ;;; <leader> i --- insert
  (:prefix-map ("i" . "insert")
   :desc "Snippet"                       "s" #'yas-insert-snippet
   :desc "Unicode"                       "u" #'unicode-property-table-internal)

  (:prefix-map ("k" . "kill")
   :desc "Kill all buffers"                    "a" #'dotfairy/kill-all-buffers
   :desc "Kill this buffer in all windows"     "A" #'dotfairy/kill-this-buffer-in-all-windows
   :desc "Kill buried buffers"                 "k" #'kill-buffer
   :desc "Kill buried buffers"                 "K" #'dotfairy/kill-buried-buffers
   :desc "Kill other buffers"                  "o" #'dotfairy/kill-other-buffers
   :desc "kill matching buffers"               "m" #'dotfairy/kill-matching-buffers)
      ;;; <leader> n --- notes
  (:prefix-map ("n" . "notes")
   :desc "Org agenda"                     "a" #'org-agenda
   :desc "Cancel current org-clock"       "C" #'org-clock-cancel
   :desc "Find file in notes"             "f" #'+default/find-in-notes
   :desc "Browse notes"                   "F" #'+default/browse-notes
   :desc "Org capture"                    "n" #'org-capture
   :desc "Goto capture"                   "N" #'org-capture-goto-target)
      ;;; <leader> o --- open
  "o" nil ; we need to unbind it first as Org claims this prefix
  (:prefix-map ("o" . "open")
   :desc "Browser"            "b"  #'browse-url-of-file
   :desc "Open Dired"         "d"  #'+default/dired
   :desc "New frame"          "f"  #'make-frame
   :desc "Dired"              "-"  #'dired-jump
   :desc "Find file in project sidebar" "P" #'treemacs-find-file)

      ;;; <leader> p --- project
  (:prefix ("p" . "project")
   :desc "Add directory to project"        "a" #'dotfairy/add-directory-as-project
   :desc "Remove blacklist from workspace" "r" #'lsp-workspace-blacklist-remove
   :desc "Restart current workspace"       "R" #'lsp-workspace-restart
   :desc "Find file in current project"    "f" #'projectile-find-file
   :desc "Find file in other project"      "F" #'dotfairy/find-file-in-other-project
   :desc "Add to workspace"                "i" #'lsp-workspace-folders-add
   :desc "Kill project buffers"            "k" #'dotfairy/kill-project-buffers
   :desc "Browse project"                  "p" #'+default/browse-project
   :desc "Browse other project"            "P" #'dotfairy/browse-in-other-project
   :desc "Search project for symbol at point"  "y" #'+default/search-project-for-symbol-at-point
   :desc "Search project"                  "s" #'+default/search-project
   :desc "Search Other Project"            "S" #'+default/search-other-project
   :desc "List project todos"              "t" #'magit-todos-list
   (:prefix ("4" . "in other window"))
   (:prefix ("5" . "in other frame")))

      ;;; <leader> q --- quit/restart
  (:prefix-map ("q" . "quit/restart")
   :desc "Delete frame"                 "f" #'delete-frame
   :desc "Clear current frame"          "F" #'dotfairy/kill-all-buffers
   :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
   :desc "Quit Emacs"                   "q" #'kill-emacs
   :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
   :desc "Restart & restore Emacs"      "r" #'dotfairy/restart-and-restore
   :desc "Restart Emacs"                "R" #'dotfairy/restart)

      ;;; <leader> & --- snippets
  (:prefix-map ("&" . "snippets")
   :desc "New snippet"           "n" #'yas-new-snippet
   :desc "Insert snippet"        "i" #'yas-insert-snippet
   :desc "Find global snippet"   "/" #'yas-visit-snippet-file
   :desc "Reload snippets"       "r" #'yas-reload-all
   :desc "Read snippets name from minibuffer" "y" #'ivy-yasnippet)

      ;;; <leader> t --- toggle
  (:prefix-map ("t" . "toggle")
   :desc "Flymake"                      "f" #'flymake-mode
   :desc "Indent style"                 "I" #'dotfairy/toggle-indent-style
   :desc "Line numbers"                 "l" #'dotfairy/toggle-line-numbers
   :desc "Read-only mode"               "r" #'read-only-mode)

      ;;; <leader> v --- versioning
  (:prefix-map ("v" . "versioning")
   :desc "Git revert file"            "R"   #'vc-revert
   :desc "Git time machine"           "t"   #'git-timemachine-toggle
   :desc "Git messager"               "m"   #'git-messenger:popup-message
   :desc "Magit dispatch"             "/"   #'magit-dispatch
   :desc "Magit file dispatch"        "."   #'magit-file-dispatch
   :desc "Forge dispatch"             "'"   #'forge-dispatch
   :desc "Magit status"               "g"   #'magit-status
   :desc "Magit status here"          "G"   #'magit-status-here
   :desc "Magit file restore"         "x"   #'magit-file-checkout
   :desc "Magit blame"                "B"   #'magit-blame-addition
   :desc "Magit clone"                "C"   #'magit-clone
   :desc "Magit fetch"                "F"   #'magit-fetch
   :desc "Magit buffer log"           "L"   #'magit-log
   :desc "Git stage file"             "S"   #'magit-stage-file
   :desc "Git unstage file"           "U"   #'magit-unstage-file
   (:prefix ("f" . "find")
    :desc "Find file"                 "f"   #'magit-find-file
    :desc "Find commit"               "c"   #'magit-show-commit)
   (:prefix ("l" . "list")
    :desc "List repositories"         "r"   #'magit-list-repositories
    :desc "List submodules"           "s"   #'magit-list-submodules)
   (:prefix ("c" . "create")
    :desc "Initialize repo"           "r"   #'magit-init
    :desc "Clone repo"                "R"   #'magit-clone
    :desc "Commit"                    "c"   #'magit-commit-create
    :desc "Fixup"                     "f"   #'magit-commit-fixup))

      ;;; <leader> w --- workspaces/windows
  (:prefix-map ("w" . "workspaces/windows")
   :desc "Display workspaces"           "d" #'+workspace/display
   (:prefix-map ("f" . "frame")
    :desc "Frame maximized"              "f" #'dotfairy/frame-maximize
    :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
    :desc "Frame restore"                "r" #'dotfairy/frame-restore
    :desc "Frame bottom half"            "j" #'dotfairy/frame-bottom-half
    :desc "Frame top half"               "k" #'dotfairy/frame-top-half
    :desc "Frame left half"              "h" #'dotfairy/frame-left-half
    :desc "Frame right half"             "l" #'dotfairy/frame-right-half)
   :desc "Rename workspace"             "r" #'+workspace/rename
   :desc "Create workspace"             "c" #'+workspace/new-named
   :desc "Delete workspace"             "k" #'+workspace/delete
   :desc "Save workspace"               "S" #'+workspace/save
   :desc "Switch to other workspace"    "o" #'+workspace/other
   :desc "Switch to left workspace"     "p" #'+workspace/switch-left
   :desc "Switch to right workspace"    "n" #'+workspace/switch-right
   :desc "Switch to"                    "w" #'+workspace/switch-to
   :desc "Switch to workspace 1"        "1" #'+workspace/switch-to-0
   :desc "Switch to workspace 2"        "2" #'+workspace/switch-to-1
   :desc "Switch to workspace 3"        "3" #'+workspace/switch-to-2
   :desc "Switch to workspace 4"        "4" #'+workspace/switch-to-3
   :desc "Switch to workspace 5"        "5" #'+workspace/switch-to-4
   :desc "Switch to workspace 6"        "6" #'+workspace/switch-to-5
   :desc "Switch to workspace 7"        "7" #'+workspace/switch-to-6
   :desc "Switch to workspace 8"        "8" #'+workspace/switch-to-7
   :desc "Switch to workspace 9"        "9" #'+workspace/switch-to-8
   :desc "Switch to last workspace"     "0" #'+workspace/switch-to-final
   :desc "Autosave session"             "a" #'dotfairy/quicksave-session
   :desc "Save session"                 "s" #'dotfairy/save-session
   :desc "Load session"                 "l" #'dotfairy/load-session
   :desc "Load last autosaved session"  "L" #'dotfairy/quickload-session
   :desc "Management windows"           "m" #'ace-window-hydra/body
   :desc "Close window or workspace"    "q" #'+workspace/close-window-or-workspace
   :desc "Undo window config"           "u" #'winner-undo
   :desc "Redo window config"           "U" #'winner-redo)

      ;;; <leader> m --- multiple cursors
  (:prefix-map ("m" . "multiple-cursors")
   :desc "Toggle Multiple Cursors"                           "m"       #'multiple-cursors-hydra/body
   :desc "Use arrow keys to quickly mark/skip next/previous" "<SPC>"   #'mc/mark-more-like-this-extended
   :desc "Docker Management"                                 "d"       #'docker
   :desc "Kubernetes Overview"                               "b"       #'kubernetes-overview))
(provide 'custom-keybinds)
;;; custom-keybinds.el ends here

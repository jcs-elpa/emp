;;; emp.el --- Emacs Music Playlist  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-03-02 20:54:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Emacs Music Playlist.
;; Keyword: music player playlist table meida
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (f "0.20.0"))
;; URL: https://github.com/jcs090218/emp

;; This file is NOT part of GNU Emacs.

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
;; Emacs Music Playlist.
;;

;;; Code:

(require 'f)
(require 'tabulated-list)


(defgroup emp nil
  "Emacs Music Playlist."
  :prefix "emp-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/emp"))


(defconst emp--data-file "~/.emacs.d/emp.dat"
  "Data file path to store music history.")

(defconst emp--format
  (vector (list "PN" 3 t)  ; Playing
          (list "Title" 30 t)
          (list "Path" 120 t))
  "Format to assign to `tabulated-list-format' variable.")

(defvar emp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emp-play-sound)
    (define-key map (kbd "<mouse-1>") 'emp-play-sound)
    map)
  "Keymap for `emp-mode'.")

(defvar emp--history-path '()
  "List of history music path.")

(defvar emp--sound-process nil
  "Process that plays the sound.")

(defvar emp--volume 75
  "Current play sound volume.")

(defvar emp--loop nil
  "Current play sound loop.")

(defvar emp--current-id -1
  "Current play sound id.")


(defun emp--list-to-string (lst)
  "Convert LST to string."
  (let ((str ""))
    (dolist (item lst)
      (setq str (concat str item "\n")))
    str))

(defun emp--read-file (path)
  "Read a file from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun emp--read-history-data ()
  "Read history `emp--data-file' to data file `emp--history-path'."
  (setq emp--history-path (split-string (emp--read-file emp--data-file) "\n" t)))

(defun emp--write-history-data ()
  "Write history `emp--history-path' to data file `emp--data-file'."
  (write-region (emp--list-to-string emp--history-path)
                nil
                (expand-file-name emp--data-file)))

(defun emp--async-play-sound (path volume)
  "Async play sound file PATH."
  (when (processp emp--sound-process) (kill-process emp--sound-process))
  (let ((command (car command-line-args)))
    (setq emp--sound-process
          (start-process "play-sound-file"
                         nil command "-Q" "--batch" "--eval"
                         (format "(play-sound-file %s %s)"
                                 (shell-quote-argument path)
                                 volume)))))

(defun emp--revert-buffer ()
  "Revert `emp-mode' buffer."
  (tabulated-list-revert)
  (tabulated-list-print-fake-header))

(defun emp-play-sound ()
  "Play sound for current item."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry))
        (mark nil) (fname nil) (path nil)
        (old-pt (point)))
    (when (vectorp entry)
      (setq mark (aref entry 0))
      (setq fname (aref entry 1))
      (setq path (aref entry 2))
      ;;(emp--async-play-sound path emp--volume)
      (emp--revert-buffer)
      (goto-char old-pt)
      (tabulated-list-delete-entry)
      (tabulated-list-print-entry id (vector "*" fname path))
      (goto-char old-pt))))


(defun emp--new-music-entry (path)
  "Add a music by PATH."
  (let ((new-entry '()) (new-entry-value '())
        (id (length tabulated-list-entries)))
    (push path new-entry-value)  ; Path
    (push (f-filename path) new-entry-value)  ; Title
    (push "" new-entry-value)  ; PD
    (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
    (push (number-to-string id) new-entry)  ; ID
    new-entry))

(defun emp--get-entries ()
  "Get all the music entries."
  (emp--read-history-data)
  (let ((entries '()))
    (dolist (path emp--history-path)
      (push (emp--new-music-entry path) entries))
    entries))

(define-derived-mode emp-mode tabulated-list-mode
  "emp-mode"
  "Major mode for Emacs Music Playlist."
  :group 'emp
  (setq tabulated-list-format emp--format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list--header-string
        (format "Volume: %s, Loop: %s" emp--volume emp--loop))
  (setq tabulated-list-sort-key (cons "Title" t))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (emp--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

;;;###autoload
(defun emp ()
  "Start `emp-mode'."
  (interactive)
  (pop-to-buffer "*emp-mode*" nil)
  (emp-mode))


(provide 'emp)
;;; emp.el ends here

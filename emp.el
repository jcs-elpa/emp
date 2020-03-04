;;; emp.el --- Emacs Music Playlist  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-03-02 20:54:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Emacs Music Playlist.
;; Keyword: music player playlist table meida
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (f "0.20.0") (async "1.9.3")
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

(require 'async)
(require 'f)
(require 'tabulated-list)


(defgroup emp nil
  "Emacs Music Playlist."
  :prefix "emp-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/emp"))


(defconst emp--data-file "~/.emacs.d/emp.dat"
  "Data file path to store music history.")

(defconst emp--buffer-name "*emp*"
  "Name of the EMP buffer.")

(defconst emp--format
  (vector (list "PN" 3 t)  ; Playing
          (list "Title" 50 t)
          (list "Path" 120 t))
  "Format to assign to `tabulated-list-format' variable.")

(defvar emp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emp-select-music)
    (define-key map (kbd "<mouse-1>") 'emp-select-music)
    (define-key map (kbd "SPC") 'emp-stop-sound)
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

(defvar emp--current-path ""
  "Current play music path.")


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
  (unless (file-exists-p emp--data-file) (emp--write-history-data))
  (setq emp--history-path (split-string (emp--read-file emp--data-file) "\n" t)))

(defun emp--write-history-data ()
  "Write history `emp--history-path' to data file `emp--data-file'."
  (write-region (emp--list-to-string emp--history-path)
                nil
                (expand-file-name emp--data-file)))

(defun emp--revert-buffer ()
  "Revert `emp-mode' buffer."
  (interactive)
  (if (get-buffer emp--buffer-name)
      (with-current-buffer emp--buffer-name
        (let ((old-pt (point)))
          (setq tabulated-list-entries (emp--get-entries))
          (tabulated-list-revert)
          (tabulated-list-print-fake-header)
          (goto-char old-pt)))
    (error "[ERROR] Can't revert emp buffer if is not inside *emp* buffer list")))

(defun emp--async-play-sound (path volume)
  "Async play sound file PATH and with VOLUME."
  (emp-stop-sound)
  (setq emp--sound-process
        (async-start
         (lambda ()
           (play-sound-file path volume))
         (lambda (res)
           (when emp--loop
             (emp--async-play-sound path volume))))))

(defun emp-stop-sound ()
  "Stop the sound from current process."
  (interactive)
  (when (processp emp--sound-process)
    (ignore-errors (kill-process emp--sound-process))
    (setq emp--sound-process nil)
    (setq emp--current-path "")
    (emp--revert-buffer)))

(defun emp-pause-sound ()
  "Pause the sound process."
  (interactive)
  ;; TODO: ..
  (when (processp emp--sound-process)
    (stop-process emp--sound-process)))

(defun emp-resume-sound ()
  "Continue the sound process."
  (interactive)
  ;; TODO: ..
  (when (processp emp--sound-process)
    (continue-process emp--sound-process)))

(defun emp-select-music ()
  "Play sound for current item."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (when (vectorp entry)
      (let ((mark (aref entry 0)) (fname (aref entry 1)) (path (aref entry 2)))
        (emp--async-play-sound path emp--volume)
        (setq emp--current-path path)
        (emp--revert-buffer)))))

(defun emp--new-music-entry (path)
  "Add a music by PATH."
  (let ((new-entry '()) (new-entry-value '())
        (id (length tabulated-list-entries)))
    (push path new-entry-value)  ; Path
    (push (f-filename path) new-entry-value)  ; Title
    (if (string= path emp--current-path)  ; PD
        (push "*" new-entry-value)
      (push "" new-entry-value))
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
  (setq-local tabulated-list--header-string
              (format "> Volume: %s, Loop: %s"
                      emp--volume
                      (if emp--loop "On" "Off")))
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (emp--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

;;;###autoload
(defun emp ()
  "Start `emp-mode'."
  (interactive)
  (pop-to-buffer emp--buffer-name nil)
  (emp-mode))


(provide 'emp)
;;; emp.el ends here

;;; notif.el --- Quick & customizable note creation tool for GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Kyle Belleau

;; Author: Kyle Belleau <kylejbelleau@gmail.com>
;; URL: https://github.com/beelleau/notif
;; Package-Requires (emacs "26.1") (yasnippet "0.10.0")
;; Keywords: notes

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

;; `notif' helps streamline quick note creation and organization in
;; a customizable method, acting as a wrapper around existing Emacs utilities.
;; It relies on the YASnippet package, which can be found in the
;; the ELPA repository. You can create your own YASnippet template for
;; your notes, and have a quick method to create new notes with your
;; own organizational procedure.

;;; Code:
(require 'yasnippet)

(defgroup notif nil
  "notif --- A quick, customizable note creation tool for Emacs."
  :group 'convenience
  :link '(url-link "https://github.com/beelleau/notif"))

;; customizable variables
(defcustom notif-directory
  (file-name-as-directory (concat (getenv "HOME") "/" "notes"))
  "Path to the user's notif-managed notes directory."
  :type 'directory
  :group 'notif)

(defcustom notif-note-snippet
  "notif-note"
  "The name of your `YASnippet' snippet used by `notif' for notes."
  :type 'string
  :group 'notif)

(defcustom notif-ticket-snippet
  "notif-ticket"
  "The name of your `YASnippet' snippet used by `notif' for tickets."
  :type 'string
  :group 'notif)

(defcustom notif-todo-snippet
  "notif-todo"
  "The name of your `YASnippet' todo template used by `notif' for the TODO."
  :type 'string
  :group 'notif)

;; internal function
(defun notif--new-buffer (snippet)
  "Actions to run in a new notif note buffer."
  (org-mode)
  (yas-expand-snippet (yas-lookup-snippet snippet)))

;; interactive functions
;;;###autoload
(defun notif-find-note (notename &optional wildcards snippet)
  "Edit the NOTENAME.
Switches to a buffer named NOTENAME, or creates it if it doesn't exist.
Interactively, the default location is the current directory.
`notif-find-note' is essentially a modified `find-file'. View
`find-file' for more information."
  (interactive
   (let ((default-directory
          (if (string-prefix-p (expand-file-name notif-directory)
                               (expand-file-name default-directory))
              default-directory
            notif-directory)))

     (let ((args (find-file-read-args "Find note: "
                                      (confirm-nonexistent-file-or-buffer))))
       (list (car args) nil notif-note-snippet))))

  (let ((value (find-file-noselect notename nil nil wildcards)))
    (if (listp value)
        (mapc 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))
    ;; run find-file hooks
    (if (not (file-exists-p notename))
        (progn
          (run-hooks 'find-file-not-found-functions)
          (notif--new-buffer (or snippet notif-note-snippet)))
      (run-hooks 'find-file-hook))))

;;;###autoload
(defun notif-find-note-read-only (notename &optional wildcards)
  "Edit file NOTENAME but don't allow changes.
`notif-find-note' is essentially a modified `find-file-read-only'. View
`find-file-read-only' for more information."
  (interactive
   (find-file-read-args "Find note read-only: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file--read-only #'find-file notename wildcards))

;;;###autoload
(defun notif-find-ticket (ticketname &optional wildcards)
  "Edit a Ticket with the name TICKETNAME.
Switches to a buffer named TICKETNAME in the Tickets directory,
or creates it if it doesn't exist."
  (interactive
   (let ((default-directory
          (expand-file-name "Tickets/" notif-directory)))
     (find-file-read-args "Find ticket: "
                          (confirm-nonexistent-file-or-buffer))))

  (notif-find-note
   (concat
    (file-name-as-directory (expand-file-name "Tickets/" notif-directory))
    ticketname)
   wildcards notif-ticket-snippet))

;;;###autoload
(defun notif-find-todo ()
  "Edit the TODO note.
Switches to a buffer named TODO in the `notif-directory',
or creates it if it doesn't exist."
  (interactive)
  (let ((todo-path (concat
                    (file-name-as-directory notif-directory)
                    "TODO")))
    ;; create the TODO note if it doesn't exist
    (notif-find-note todo-path nil notif-todo-snippet)))

;;;###autoload
(defun notif-find-notepad ()
  "Edit the Notepad note.
Switches to a buffer named Notepad in the `notif-directory',
or creates it if it doesn't exist."
  (interactive)
  (let ((notepad-path (concat
                       (file-name-as-directory notif-directory)
                       "Notepad")))
    ;; create the Notepad note if it doesn't exist
    (notif-find-note notepad-path)))

(provide 'notif)
;;; notif.el ends here

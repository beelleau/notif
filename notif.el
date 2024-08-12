;;; notif.el --- A quick, customizable note creation method -*- lexical-binding: t -*-

;; Copyright (C) 2024 Kyle Belleau

;; Author: Kyle Belleau <kylejbelleau@gmail.com>
;; URL: https://github.com/kbelleau/notif
;; Package-Requires (emacs "26.1") (yasnippet "0.10.0")
;; Keywords: notes

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

;; notif.el helps streamline quick note creation and organization in
;; a customizable method, acting as a wrapper around existing Emacs utilities.
;; It relies on the YASnippet package, which can be found in the
;; the ELPA repository. You can create your own YASnippet template for
;; your notes, and have a quick method to create new notes with your
;; own organizational procedure.

;;; Code:

(require 'yasnippet)

(defgroup notif nil
  "notif --- A quick, customizable note creation method."
  :group 'convenience
  :link '(url-link "https://github.com/kbelleau/notif"))

;; customizable variables
(defcustom notif-directory
  (concat (getenv "HOME") "/" "notes" "/")
  "Path to the user's notif-managed notes directory."
  :type 'directory
  :group 'notif)

(defcustom notif-snippet
  "notif"
  "Name of your `YASnippet' template used by `notif'."
  :type 'string
  :group 'notif)

(defcustom notif-ticket-snippet
  "notif-ticket"
  "Name of your `YASnippet' ticket template used by `notif'."
  :type 'string
  :group 'notif)

(defcustom notif-todo-snippet
  "notif-todo"
  "Name of your `YASnippet' todo template used by `notif'."
  :type 'string
  :group 'notif)

;; static variables
(defvar notif-todo-note-name
  "TODO"
  "Name of your `notif' TODO note.")

(defvar notif-notepad-note-name
  "Notepad"
  "Name of your `notif' Notepad note.")

(defvar notif-ticket-directory
  (concat notif-directory "Tickets/")
  "Location of Tickets in `notif-directory'.")

;; autoloads
(autoload 'notif-find-note "notif"
  "Opens `find-file' inside of your `notif-directory'."
  t)

(autoload 'notif-read-note "notif"
  "Opens `find-file-read-only' inside of your `notif-directory'."
  t)

(autoload 'notif-find-ticket "notif"
  "Opens `find-file' inside of your `notif-ticket-directory'."
  t)

(autoload 'notif-find-todo "notif"
  "Opens your `notif' todo note."
  t)

(autoload 'notif-find-notepad "notif"
  "Opens your `notif' notepad note."
  t)

;; functions
(defun notif-find-note ()
  "Opens `find-file' inside of your `notif-directory'."
  (interactive)
  (let ((default-directory notif-directory))
    (call-interactively 'find-file)
    (unless (file-exists-p buffer-file-name)
      (org-mode)
      (yas-expand-snippet (yas-lookup-snippet notif-snippet)))))

(defun notif-read-note ()
  "Opens `find-file-read-only' inside of your `notif-directory'."
  (interactive)
  (let ((default-directory notif-directory))
    (call-interactively 'find-file-read-only)))

(defun notif-find-ticket ()
  "Opens `find-file' inside of your `notif-ticket-directory'."
  (interactive)
  (let ((default-directory notif-ticket-directory))
    (call-interactively 'find-file)
    (unless (file-exists-p buffer-file-name)
      (org-mode)
      (yas-expand-snippet (yas-lookup-snippet notif-ticket-snippet)))))

(defun notif-find-todo ()
  "Opens your `notif' todo note."
  (interactive)
  (let ((default-directory notif-directory))
    (find-file notif-todo-note-name)
    (unless (file-exists-p buffer-file-name)
      (org-mode)
      (yas-expand-snippet (yas-lookup-snippet notif-todo-snippet)))))

(defun notif-find-notepad ()
  "Opens your `notif' notepad note."
  (interactive)
  (let ((default-directory notif-directory))
    (find-file notif-notepad-note-name)
    (unless (file-exists-p buffer-file-name)
      (org-mode)
      (yas-expand-snippet (yas-lookup-snippet notif-snippet)))))

(provide 'notif)
;;; notif.el ends here

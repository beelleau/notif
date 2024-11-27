# notif.el
notif is a quick, configurable note creation system to help you create notes in GNU Emacs.  

`notif.el` depends on the Emacs extension `yasnippet`, found in the ELPA repository. You can read more about YASnippet [here](https://joaotavora.github.io/yasnippet).  

It uses Emacs' org-mode for note files.

## Installation
To install notif, please follow these instructions.  

Clone this repository:  
``` sh
git clone https://github.com/beelleau/notif.git
```

Either symlink or copy `notif.el` into a path that gets loaded in your `init.el`. I typically use a symlink:  
```sh
ln -s notif/notif.el .emacs.d/lisp/notif.el
```

Ensure your path is enabled in `load-path` and load `notif`:  
```elisp
(add-to-list 'load-path
             (concat user-emacs-directory "lisp/"))
			 
(require 'notif)
```

## Usage
`notif-find-note`: this function is a modified version of Emacs' `find-file`. It will open the minibuffer prompt inside of your `NOTIF-DIRECTORY`. With this function, you can open existing notes, or create new ones. When a new note is created, it will automatically expand your `notif-note-snippet` in the buffer.  

`notif-find-note-read-only`: this function is a modified version of Emacs' `find-file-read-only`. It will open the minibuffer prompt inside of your `notif-directory`. With this function, you can open existing notes in read-only mode.  

`notif-find-ticket`: this function utilizes `notif-find-note`. It will open the minibuffer prompt inside the path `NOTIF-DIRECTORY/Tickets/`. With this function, you can open existing tickets, or create new ones. When a new ticket is created, it will automatically expand your `notif-ticket-snippet` in the buffer.  

`notif-find-todo`: this function utilizes `notif-find-file` to open your notif TODO file. It is located at: `NOTIF-DIRECTORY/TODO`. If this file does not already exist, it will be created, and your `notif-todo-snippet` will be  automatically expanded in the buffer.  

`notif-find-notepad`: this function utilizes Emacs' `find-file` to open your notif Notepad file. It is located at: `NOTIF-DIRECTORY/Notepad`. If this file does not already exist, it will be created, and your `notif-note-snippet` will be automatically expanded in the buffer.  

Call the notif functions as appropriate. Use keybindings, or call the functions with `M-x <function>`

## Configuration
Before using `notif.el`, you'll want to make some configurations. You'll typically want to configure three things: snippets, defaults, and keybindings.

### Setting Up Snippets
`notif.el` relies on code snippets created with [YASnippet](https://joaotavora.github.io/yasnippet/snippet-development.html). You'll want to have a basic understanding of how YASnippet works before continuing with notif.  

Create a snippet for a default note template. Here, you can create whatever you want. The snippet will need to be an `org-mode` snippet.  

View my snippet template for some ideas:  
```yasnippet
# -*- mode: snippet -*-
# name: notif-note
# key: -notif-note
# --

# -*- mode: org -*-
#+title:        ${1:title}
#+tags:         `(file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))` ${2:}
#+date:         `(format-time-string "%B %d %Y T%H:%M")`
#+notif-id:     `(concat "N-" (secure-hash 'sha1 (buffer-name)))`
#+startup:      overview

$0
```

### Setting Defaults
There are several settings to configure to use notif.  

The first is the `notif-directory`. By default, it's configured to: `~/notes/`. However, you can change this value by adding the following to your `init.el`:  
```elisp
(setq notif-directory "/path/to/your/notif-directory/")
```
_keep the trailing slash_  

Additionally, you'll want to setup a `notif-note-snippet`. This value is the name of your snippet that you want to automatically expand each time you create a new note. By default, it is set to `notif-note`, but you can change this value by adding the following to your `init.el`:  
```elisp
(setq notif-note-snippet "snippet-name")
```

You can state the YASnippet name to be used with `notif-find-ticket`, named `notif-ticket-snippet`. This value is the name of the snippet that you want to automatically expand each time you create a new ticket. By default, it is set to `notif-ticket`, but you can change this value by adding the following to your `init.el`:  
```elisp
(setq notif-ticket-snippet "snippet-name")
```

You can state the YASnippet name to be used with `notif-find-todo`, named `notif-todo-snippet`. This value is the name of the snippet that you want to automatically expand each time you create a new TODO file. By default, it is set to `notif-ticket`, but you can change this value by adding the following to your `init.el`:  
```elisp
(setq notif-todo-snippet "snippet-name")
```

### Keybindings
There are a few functions you'll likely want to configure keybindings for. It is not required, of course.  

Functions:  
`notif-find-note`  
`notif-find-note-read-only`  
`notif-find-ticket`  
`notif-find-todo`  
`notif-find-notepad`  

There are multiple ways to configure keybindings in Emacs. This is how I do it:  
```elisp
;;; -*- lexical-binding: t -*-

(define-prefix-command 'notif-prefix-map)

(let ((map global-map))

  ;; notif functions
  (define-key map (kbd "C-c n") notif-prefix-map)
  (define-key map (kbd "C-c n f") #'notif-find-note)
  (define-key map (kbd "C-c n r") #'notif-read-note)
  (define-key map (kbd "C-c n i") #'notif-find-ticket)
  (define-key map (kbd "C-c n t") #'notif-find-todo)
  (define-key map (kbd "C-c n n") #'notif-find-notepad))
```

# notif.el
notif is a quick, customizable note creation system to help you organize your notes in GNU Emacs.  

`notif.el` depends on the Emacs extension `yasnippet`, found in the ELPA repository. You can read more about YASnippet [here](https://joaotavora.github.io/yasnippet).  

It uses Emacs' org-mode (other options may be available in the future).

## Installation
To install notif, please follow these instructions.

Clone this repository:  
``` sh
git clone https://github.com/kbelleau/notif
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
`notif-find-note`: this function is a wrapper around Emacs' `find-file`. It will open the `find-file` prompt inside of your `notif-directory`. With this function, you can open existing notes, or create new ones. When a new note is created, it will automatically expand your `notif-snippet` in the buffer.  

`notif-read-note`: this function is a wrapper around Emacs' `find-file-read-only`. It will open the `find-file-read-only` prompt inside of your `notif-directory`. With this function, you can open existing notes in read-only.  

(Optional) `notif-find-todo`: this function utilizes Emacs' `find-file` to open your notif TODO file. It is located at: `notif-directory/TODO`. If this file does not already exist, it will be created, and your `notif-todo-snippet` will be  automatically expanded in the buffer.  

(Optional) `notif-find-notepad`: this function utilizes Emacs' `find-file` to open your notif Notepad file. It is located at: `notif-directory/Notepad`. If this file does not already exist, it will be created, and your `notif-notepad-snippet` will be automatically expanded in the buffer.  

Call the notif functions as appropriate. Use keybindings, or call the functions with `M-x <function>`

## Configuration
Before using `notif.el`, you'll want to make some configurations. You'll typically want to configure three things: snippets, defaults, and keybindings.

### Setting Up Snippets
`notif.el` relies on code snippets created with [YASnippet](https://joaotavora.github.io/yasnippet/snippet-development.html). You'll want to have a basic understanding of how YASnippet works before continuing with notif.  

Create a snippet for a default note template. Here, you can create whatever you want. The snippet will need to be an `org-mode` snippet.  

View my snippet template for some ideas:  
```yasnippet
# -*- mode: snippet -*-
# name: notif
# key: -notif
# --

# -*- mode: org -*-
#+title:        ${1:title}
#+tags:         `(file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))` ${2:}
#+date:         `(format-time-string "%B %d %Y T%H:%M")`
#+notif-id:     `(concat "N-" (secure-hash 'sha1 (buffer-name)))`
#+startup:      overview

$0
```

### Setting Defaults and Optional Features
There are several settings to configure to use notif.  

The first is the `notif-directory`. By default, it's configured to: `~/notes/`. However, you can change this value by adding the following to your `init.el`:  
```elisp
(setq notif-directory "/path/to/your/notif-directory/")
```

Additionally, you'll want to setup a `notif-snippet`. This value is the name of your snippet that you want to automatically expand each time you create a new note. By default, it is set to `notif`, but you can change this value by adding the following to your `init.el`:  
```elisp
(setq notif-snippet "snippet-name")
```

The next setting is `notif-todo-enable`. This value enables a function named `notif-find-todo`. Read more about the functions provided by notif in the next section of this document. If you'd like notif to manage a TODO file (located inside of your `notif-directory`), you can enable it with:  
```elisp
(setq notif-todo-enable t)
```

You can state the snippet name to be used with `notif-find-todo` (if the file does not already exist). By default, it is configured to `notif-todo`. You can change like so:  
```elisp
(setq notif-todo-snippet "snippet-name")
```

Similar to `notif-todo-enable` is `notif-notepad-enable`. This value enables a function named `notif-find-notepad`. Read more about the functions provided by notif in the next section of this document. If you'd like notif to manage a Notepad file (located inside of your `notif-directory`), you can enable it with:  
```elisp
(setq notif-notepad-enable t)
```

You can state the snippet name to be used with `notif-find-notepad` (if the file does not already exist). By default, it is configured to `notif-notepad`. You can change it like so:  
```elisp
(setq notif-notepad-snippet "snippet-name")
```

### Keybindings
There are a few functions you'll likely want to configure keybindings for. It is not required, of course.

Functions:  
`notif-find-note`  
`notif-read-note`  
and, if enabled:  
`notif-find-todo`  
`notif-find-notepad`  

There are multiple ways to configure keybindings in Emacs. This is how I do it:  
```elisp
;;; -*- lexical-binding: t -*-

(define-prefix-command 'notif-prefix-map)

(let ((map global-map))

  ;; notif functions
  (define-key map (kbd "C-c n") notif-prefix-map)
  (define-key map (kbd "C-c n n") #'notif-find-note)
  (define-key map (kbd "C-c n r") #'notif-read-note)
  (define-key map (kbd "C-c n t") #'notif-find-todo)
  (define-key map (kbd "C-c n s") #'notif-find-notepad))
```

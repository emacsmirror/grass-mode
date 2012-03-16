;; This file contains the minimum code necessary to get grass-mode
;; running on your system. I use it for testing without running the rest
;; of the code in my .emacs (i.e., emacs -Q -l grass.emacs). The
;; comments should also serve as an installation guide.

;;;;;;;;;;;;;;;;;
;; yas/snippet ;;
;;;;;;;;;;;;;;;;;

;; You need to have yasnippet installed. As part of that process, you
;; should already have 'required' that package, so the following lines
;; (or something similar) should appear somewhere in your .emacs by now:

(add-to-list 'load-path "~/.emacs.d/yasnippet/")
(require 'yasnippet) 
(yas/load-directory "~/.emacs.d/yasnippet/snippets/")
(yas/initialize)

;; Additional yas/snippet codes is required to load snippets for other
;; modes, but I don't include it in this file.

;; If yas/snippet works for you, you need to let Emacs know where you've
;; put the grass-mode snippets directory. By default, it will be in
;; ~/.emacs.d/grass-mode/snippets. If that's where you've put it,
;; no further codes is necessary. If not, modify and uncomment the
;; following line:

;; (setq grass-snippets "~/path/to/grass-mode/snippets")

;;;;;;;;;;;;;;;;
;; Help-files ;;
;;;;;;;;;;;;;;;;

;; grass-mode needs to know where the built-in help files are. The
;; default assumes they are in the location used by the Debian package,
;; which is /usr/share/doc/grass-doc/html. If that's not where they are
;; on your system, modify and uncomment the following:

;; (setq grass-doc-dir "/path/to/grass/help/docs")

;; If you have installed w3m, you should have something like the
;; following in your .emacs:

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/w3m/")

;; Tell grass-moode you want to use w3m as your help browser with the
;; following: 

;; (setq grass-help-w3m t)

;; I use that for testing, but if you don't have w3m installed, leave it
;; commented out!

;; If this is done before you load grass-mode.el, w3m will be loaded for
;; you. If you change this variable after you have started grass, you'll
;; need to load w3m manually.

;; If you don't use w3m, your help files will be opened by whatever
;; browse-url uses on your system. For me it defaults to firefox.


;;;;;;;;;;;;;;;
;; grassdata ;;
;;;;;;;;;;;;;;;

;; If your grass database is anywhere other than ~/grassdata, uncomment
;; and modify the following:

;; (setq grassdata "~/path/to/grassdata")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grass programs and scripts ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; grass-mode assumes that all the individual programs and scripts that Grass uses are
;; found in /usr/lib/grass64 (actually, in the bin and scripts directories within that
;; directory). Again, this is the Debian default, so if you're not running Debian you may
;; need to uncomment and modify the following line:

;; (setq gisbase "/usr/lib/grass64")

;;;;;;;;;;;;;;;;;;;
;; Other options ;;
;;;;;;;;;;;;;;;;;;;

;; You don't need to change any of the following variables, but you
;; might want to. See the help for each variable if the name isn't
;; enough to explain what they do:

;; grass-default-location
;; grass-default-mapset
;; grass-prompt

;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading grass-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; All that's left is to load the mode:

(add-to-list 'load-path "~/.emacs.d/grass-mode")
(require 'grass-mode)

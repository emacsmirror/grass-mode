;; Quick start for the impatient:
;; If the following default locations don't make sense on your system, uncomment the
;; appropriate line and edit as necessary:

;; (setq grass-doc-dir "/path/to/grass/help/docs")
;; (setq grassdata "~/path/to/grassdata")
;; (setq gisbase "/usr/lib/grass64")  ;; the grass executables 

;; Add the following to your .emacs, making sure that the load-path is pointed to the
;; right directory on your system (wherever you unpacked grass-mode)

(add-to-list 'load-path "~/.emacs.d/grass-mode.el")
(require 'grass-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Details for the less impatient: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file contains the minimum code necessary to get grass-mode
;; running on your system. I use it for testing without running the rest
;; of the code in my .emacs (i.e., emacs -Q -l grass.emacs). The
;; comments should also serve as an installation guide.

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


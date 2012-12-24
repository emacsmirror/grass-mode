;; grass-mode.el --- Provides Emacs modes for interacting with the GRASS GIS program
;; Copyright (C) Tyler Smith 2012

;; Author: Tyler Smith <tyler.smith@mail.mcgill.ca>

;; Keywords: GRASS, GIS

;; Package-Requires: shell-mode

;; This file is not part of GNU Emacs

;; grass-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; grass-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with grass-mode (see the file COPYING).  If not, see
;; <http://www.gnu.org/licenses/>. 

;;; Commentary:

;; When installed via Emacs' packaging facility, grass-mode should
;; require only minimal configuration. Add (require 'grass-mode) to your
;; .emacs file, and check the grass-mode customization group to ensure
;; that the paths are set properly.

;; If you have downloaded grass-mode via the bitbucket repository,
;; you'll need to make sure the files are all in one directory, and that
;; directory is in your load path. Then proceed as described in the
;; previous paragraph.

;;; TODO:

;; Fix grass-member so that it doesn't require cl.el
;; Make sgrass into a minor-mode
;; Make w3m customizations into a minor-mode
;; History browser?
;; per-location logging?
;; per-location scripting support (add to exec-path)?

;;;;;;;;;;;;;;;;;;
;; Dependencies ;;
;;;;;;;;;;;;;;;;;;

(require 'shell)
(require 'cl) ;; fix grass-member so this isn't necessary!!

(defun grass-mapcar* (f &rest xs)
  "MAPCAR for multiple sequences.
Included to obviate the need for cl.el."
  (if (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
      (apply 'grass-mapcar* f (mapcar 'cdr xs)))))

;; Oops - this was supposed to replace member* in cl, but it is not
;; self-contained yet!
(defun grass-member (cl-item cl-list &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if cl-keys
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-list (not (cl--check-test cl-item (car cl-list))))
	  (setq cl-list (cdr cl-list)))
	cl-list)
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(member cl-item cl-list)
      (memq cl-item cl-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup grass-mode nil 
  "Running GRASS GIS from within an Emacs buffer."
  :version "24.2.50.1")

(defcustom grassdata "~/grassdata"
  "The directory where grass locations are stored."
  :group 'grass-mode)

(defcustom gisbase "/usr/lib/grass64"
  "The top-level directory that includes the bin and scripts directories for grass."
  :group 'grass-mode)

(defcustom grass-doc-dir "/usr/share/doc/grass-doc/html/"
  "The location of the Grass html documentation."
  :group 'grass-mode)

(defcustom grass-default-location nil
  "The default starting location."
  :group 'grass-mode)

(defcustom grass-default-mapset "PERMANENT"
  "The default starting mapset."
  :group 'grass-mode)

(defcustom grass-prompt "$LOCATION_NAME:$MAPSET> "
  "String to format the Grass prompt.
$LOCATION_NAME expands to the name of the grass location.
$MAPSET expands to the name of the grass location.
Normal bash prompt expansions are available, such as:
\\w - the current working directory
\\W - the  basename  of the current working directory"
  :link '(url-link :tag "Bash Prompt Escapes"
  "http://tldp.org/HOWTO/Bash-Prompt-HOWTO/bash-prompt-escape-sequences.html") 
  :group 'grass-mode)

(defcustom grass-prompt-2 "> "
  "String to format the Grass continuation-line prompt, PS2.
The same formatting options from grass-prompt are available."
  :group 'grass-mode)

(defcustom grass-log-dir (concat grassdata "/logs")
  "The default directory to store interactive grass session logs.
Set this to nil to turn off logging."
  :group 'grass-mode
  :set-after '(grassdata))

(defcustom grass-help-w3m nil 
  "If non-nil, use w3m to browse help docs within Emacs. Otherwise, use
browse-url. w3m must be installed separately in your Emacs to use this!"
  :type 'boolean
  :require 'w3m
  :group 'grass-mode)

(defvar igrass-mode-hook nil)
(defvar sgrass-mode-hook nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Global Variables       ;;
;; (shouldn't be set by users) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq grass-location nil      ; The currently active grass location
      grass-process nil       ; The active Grass process
      grass-mapset nil        ; The currently active grass mapset
      grass-doc-files         ; The list of grass help files
         (delete nil (mapcar #'(lambda (x) 
                                 (if (string-match-p "html$" x)
                                     x))
                             (directory-files grass-doc-dir)))
      grass-help nil)          ; The buffer where the grass help is found

;;;;;;;;;;;;;;;;;;;;;
;; Initializations ;;
;;;;;;;;;;;;;;;;;;;;;

(defun grass-init-command-list ()
  "Parses the help files, extracting a list of commands and their parameters"
  (setq grass-commands nil)

  ;; Parse the help files:
  (mapc #'(lambda (x)
            (with-temp-buffer 
              (insert-file-contents (cdr x))
              (beginning-of-buffer)
              (if (search-forward "<h3>Parameters:</h3>\n<DL>" nil t)
                  (push (cons 
                         (car x)
                         (let ((start (point))
                               (end (search-forward "/DL"))
                               result-list)
                           (goto-char start)
                           (while (search-forward-regexp "<b>\\(.*\\)</b>" end t)
                             (let ((parameter (concat (match-string-no-properties 1) "="))
                                   (doc-string (progn 
                                                 (search-forward-regexp "<DD>\\(.*\\)</DD>" end t)
                                                 (match-string-no-properties 1))))
                               (push (list parameter doc-string)
                                     result-list)))
                           result-list))
                        grass-commands)
                (push (list (car x)) grass-commands))))
        grass-doc-table)

  ;; load the parameter values
    (load "grass-commands.el"))

(defun grass-p-comp (pairs completion)
  "set the completion string/function for the parameter of command"
  (dolist (p pairs)
    (setcdr
     (cdr (assoc (concat (second p) "=") (assoc (first p) grass-commands)))
     (cons completion nil))))

(defun grass-get-location ()
  "Prompt the user for the location."
  (assoc (completing-read
          (format "Grass location (%s): " grass-default-location)
          (grass-location-list) nil t nil nil grass-default-location)
         (grass-location-list)))

(defun grass-get-mapset ()
  "Prompt the user for the mapset for the current location."
  (completing-read (format "Grass mapset (%s): " grass-default-mapset)
                   (grass-mapset-list) nil t nil nil grass-default-mapset))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion Utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grass-vector-maps (&optional location mapset)
  "Returns a list of all the vector maps in location and mapset.
Defaults to the current location and mapset."
  (let ((loc (if location location grass-location))
        (mapst (if mapset mapset grass-mapset)))
    (let ((map-dir (concat (cdr loc) "/" mapst)))
      (if (member "vector" (directory-files map-dir))
          (directory-files (concat map-dir "/" "vector") nil "^[^.]")))))

(defun grass-raster-maps (&optional location mapset)
  "Returns a list of all the raster maps in location and mapset.
Defaults to the current location and mapset." 
  (let ((loc (if location location grass-location))
        (mapst (if mapset mapset grass-mapset)))
    (let ((map-dir (concat (cdr loc) "/" mapst)))
      (if (member "cell" (directory-files map-dir))
          (directory-files (concat map-dir "/" "cell") nil "^[^.]")))))

(defun grass-all-maps (&optional location mapset)
  "Returns a list of all maps, raster and vector.
Defaults to the current location & mapset"
  (let ((loc (if location location grass-location))
        (mapst (if mapset mapset grass-mapset)))
    (let ((map-dir (concat (cdr loc) "/" mapst)))
      (append (if (member "vector" (directory-files map-dir))
                  (directory-files (concat map-dir "/" "vector") nil "^[^.]"))
              (if (member "cell" (directory-files map-dir))
                  (directory-files (concat map-dir "/" "cell") nil "^[^.]"))))))

(defun grass-complete-foreign-mapsets()
  "Returns a list of all the vector maps in a different location and mapset"
  (let ((f-loc 
         (assoc (save-excursion
                  (comint-bol)
                  (if (search-forward "location=" nil t) 
                      (buffer-substring-no-properties (point)
                                                      (progn (skip-syntax-forward "^ ")
                                                             (point)))))
                (grass-location-list))))
    (if f-loc
        (grass-mapset-list f-loc))))

(defun grass-complete-foreign-vectors()
  "Returns a list of all the vector mapsets in a different location"
  (let ((f-loc 
         (assoc (save-excursion
                  (comint-bol)
                  (if (search-forward "location=" nil t) 
                      (buffer-substring-no-properties (point)
                                                      (progn (skip-syntax-forward "^ ")
                                                             (point)))))
                (grass-location-list)))
        (f-map (save-excursion
                 (comint-bol)
                 (if (search-forward "mapset=" nil t)
                     (buffer-substring-no-properties (point)
                                                     (progn (skip-syntax-forward "^ ")
                                                            (point)))))))
    (if (and f-loc f-map)
        (grass-vector-maps f-loc f-map))))

(defun grass-complete-foreign-rasters()
  "Returns a list of all the raster mapsets in a different location"
  (let ((f-loc 
         (assoc (save-excursion
                  (comint-bol)
                  (if (search-forward "location=" nil t) 
                      (buffer-substring-no-properties (point)
                                                      (progn (skip-syntax-forward "^ ")
                                                             (point)))))
                (grass-location-list)))
        (f-map (save-excursion
                 (comint-bol)
                 (if (search-forward "mapset=" nil t)
                     (buffer-substring-no-properties (point)
                                                     (progn (skip-syntax-forward "^ ")
                                                            (point)))))))
    (if (and f-loc f-map)
        (grass-raster-maps f-loc f-map))))

(defun grass-foreign-vectors()
  "Returns a list of all the vector maps in a different location and mapset"
  (interactive)
  (let ((f-loc (grass-get-location))
        (f-map (grass-get-mapset)))
    (grass-vector-maps f-loc f-map)))

(defun grass-regions (&optional location mapset)
  "List the saved regions for a location and mapset
Defaults to the currently active location and mapset."
  (let ((loc (if location location grass-location))
        (mapst (if mapset mapset grass-mapset)))
    (let ((map-dir (concat (cdr loc) "/" mapst)))
      (if (member "windows" (directory-files map-dir))
          (directory-files (concat map-dir "/" "windows") nil "^[^.]")))))

(defun grass-location-list ()
  "Return an alist of grass locations"
  (when grassdata
    (let* ((location-dirs 
            (remove-if-not 'file-directory-p
                           (directory-files grassdata t "^[^.]")))
           (location-names
            (mapcar 'file-name-nondirectory location-dirs)))
      (grass-mapcar* #'(lambda (x y) (cons x y))
               location-names location-dirs))))

(defun grass-mapset-list (&optional location)
  "List the mapsets for a location, defaulting to the current location."
  (let ((loc (if location
                 location
               grass-location)))
    (mapcar 'file-name-nondirectory
            (remove-if-not 'file-directory-p
                           (directory-files
                            (cdr loc) t "^[^.]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main completion functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grass-completion-at-point ()
  (interactive)
  (let ((pt (point))
        start end)
    (save-excursion                     ;; backup to beginning of multi-line command
      (while (progn (beginning-of-line)
                    (looking-at grass-prompt-2))
        (previous-line))
      (comint-bol)
      ;; skip over the first token:
      (re-search-forward "\\(\\S +\\)\\s ?" nil t) 

      ;; the match-string is the current command, so if pt is within
      ;; this command, we haven't finished entering it:
      (if (and (>= pt (match-beginning 1))
               (<= pt (match-end 1)))
        ;; still entering the initial command, so try completing Grass commands
          (progn
            (goto-char pt)
            (let* ((bol (save-excursion (comint-bol) (point)))
                   (eol (save-excursion (end-of-line) (point)))
                   (start (progn (skip-syntax-backward "^ " bol)
                                 (point)))
                   (end (progn (skip-syntax-forward "^ " eol)
                               (point))))
              (list start end grass-commands :exclusive 'no))) 
        ;; if this fails, control passes to comint-completion-at-point

        ;; we have a complete command, so lookup parameters in the
        ;; grass-commands table:
        (let ((command (match-string-no-properties 1)))
          (when (grass-member command grass-commands :test 'string= :key 'car)
            (goto-char pt)
            (skip-syntax-backward "^ ")
            (setq start (point))
            (skip-syntax-forward "^ ")
            (setq end (point))
            (if (not (string-match "=" (buffer-substring start end)))
                (list start end (cdr (assoc command grass-commands)) :exclusive 'no)
              (grass-complete-parameters
               command 
               (buffer-substring start (search-backward "="))
               (progn
                 (goto-char pt)
                 (re-search-backward "=\\|,")
                 (forward-char)
                 (point))
               (progn (skip-syntax-forward "^ ") (point))))))))))

(defun igrass-complete-commands ()
  "Returns the list of grass programs. I don't know why, but comint-complete finds some
  but not all of them?"
  (save-excursion
    (let* ((bol (save-excursion (comint-bol) (point)))
           (eol (save-excursion (end-of-line) (point)))
           (start (progn (skip-syntax-backward "^ " bol)
                         (point)))
           (end (progn (skip-syntax-forward "^ " eol)
                       (point))))
      (list start end grass-commands :exclusive 'no))))

(defun grass-complete-parameters (command parameter start end)
  (let ((collection (third (assoc (concat parameter "=") (assoc command grass-commands)))))
    (list start end 
          (if (functionp collection)
              (funcall collection)
            collection) 
          :exclusive 'no)))

(defun sgrass-complete-commands ()
  (save-excursion
    (let* ((bol (save-excursion (beginning-of-line) (point)))
           (eol (save-excursion (end-of-line) (point)))
           (start (progn (skip-syntax-backward "^ " bol)
                         (point)))
           (end (progn (skip-syntax-forward "^ " eol)
                       (point))))
      (list start end grass-commands :exclusive 'no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting Grass and the modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun grass ()
  "Start the Grass process, or switch to the process buffer if it's
already active." 
  (interactive)

  ;; initializations
  (setenv "GRASS_PAGER" "cat")
  (setenv "GRASS_VERBOSE" "0")

  ;; Don't modify the path more than once!
  (unless (member (concat gisbase "/bin") exec-path)
    (add-to-list 'exec-path (concat gisbase "/bin") t))
  (unless (member (concat gisbase "/scripts") exec-path)
    (add-to-list 'exec-path (concat gisbase "/scripts") t))

  (setq grass-doc-table ())
  (mapc #'(lambda (x) 
            (push (cons (substring x 0 -5)
                        (concat grass-doc-dir x)) grass-doc-table))
        grass-doc-files)

  (grass-init-command-list)

  ;; Start a new process, or switch to the existing one
  (unless (and (processp grass-process)
               (buffer-name (process-buffer grass-process)))
    (setq grass-location (grass-get-location))
    (setq grass-mapset (grass-get-mapset))
    (setq grass-process (start-process "grass" "*grass*" "grass" "-text"
                                       (concat  (file-name-as-directory
                                                 (cdr grass-location)) 
                                                grass-mapset ))))
  (switch-to-buffer (process-buffer grass-process))
  (comint-send-string grass-process
                      (format "eval `g.gisenv`\nexport PS2=\"%s\"\n"
                              grass-prompt-2))
  (grass-update-prompt)
  (igrass-mode)
  (add-hook 'completion-at-point-functions 'igrass-complete-commands nil t)
  (add-hook 'completion-at-point-functions 'grass-completion-at-point nil t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode igrass-mode shell-mode "igrass"
  "Major mode for interacting with a Grass in an inferior
process.\\<igrass-mode-map> \\[comint-send-input] after the end of the
process' output sends the text from the end of process to the end of
the current line. 

\\{igrass-mode-map}"
  ;; comint-use-prompt-regexp needs to be local; otherwise, it will mess up the
  ;; beginning-of-line type functions in shell mode!
  (make-local-variable 'comint-use-prompt-regexp)
  (setq comint-use-prompt-regexp t)

  ;; Removing '=' from comint-file-name-chars enables file-name
  ;; completion for parameters, e.g., v.in.ascii input=... This may
  ;; cause problems in cases where '=' is part of the file name.

  (unless (memq system-type '(ms-dos windows-nt cygwin))
    (setq comint-file-name-chars
          "[]~/A-Za-z0-9+@:_.$#%,{}-"))

  (setq comint-prompt-regexp "^[^#$%>\n]*[#$%>] +")
  (define-key igrass-mode-map (kbd "C-c C-v") 'grass-view-help)
  (define-key igrass-mode-map (kbd "C-a") 'comint-bol)
  (define-key igrass-mode-map (kbd "C-c C-l") 'grass-change-location)
  (define-key igrass-mode-map (kbd "C-x k") 'grass-quit)
  (run-hooks 'igrass-mode-hook))


(defun grass-view-help (PREFIX)
  "Prompts the user for a help page to view.
If w3m is the help browser, when called with a prefix it will open a new tab."
  (interactive "P")
  (let* ((key (completing-read "Grass help: " grass-doc-table nil t))
         (file (cdr (assoc key grass-doc-table))))
    (if (not grass-help-w3m)
        (browse-url (concat "file://" file))
      (if (buffer-name grass-help) 
          (if (get-buffer-window grass-help)
              (select-window (get-buffer-window grass-help))
            (switch-to-buffer-other-window grass-help))
        (switch-to-buffer-other-window "*scratch*"))
      (if PREFIX
          (w3m-goto-url-new-session (concat "file://" file))
        (w3m-goto-url (concat "file://" file)))
      (setq grass-help (current-buffer)))))

(defun grass-change-location ()
  "Prompt the user for a new location and mapset."
  (interactive)
  ;; Should maybe use local variables first here, to insure we don't
  ;; change the globals until the change has been successful?
  (setq grass-location (grass-get-location)
        grass-mapset (grass-get-mapset))
  (comint-send-string grass-process
                      (format "g.mapset location=%s mapset=%s\n"
                              (car grass-location) grass-mapset))
  (grass-update-prompt))

;; my-today is a utility function defined in my .emacs. Most people
;; won't have that already, so add it for everyone else here:
(unless (fboundp 'my-today)
  (defun my-today ()
    "Returns todays date in the format yyyy-mm-dd"
    (car (split-string (shell-command-to-string "date +%Y-%m-%d") "\n"))))

(defun grass-quit ()
  "Send the grass process the quit command, so it will clean up before exiting.
The transcript of the current session is automatically saved (or appended) to a file in
$grassdata/log"
  (interactive)
  (with-current-buffer (process-buffer grass-process)
    (comint-send-string grass-process "exit\n")
    (if grass-log-dir
        (let ((log-file (concat grass-log-dir "/" (my-today) ".grass")))
          (unless (file-exists-p grass-log-dir)
            (mkdir grass-log-dir))
          (append-to-file (point-min) (point-max) log-file))) 
    (kill-buffer)))

(defun grass-update-prompt ()
  "Updates the grass prompt."
  (comint-send-string grass-process
                      (format "eval `g.gisenv`\nexport PS1=\"%s\"\n"
                              grass-prompt))
  (grass-prep-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              sGrass                       ;;
;; Major mode for editing grass script files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grass-send-region (start end)
  "Send the region to the active Grass process"
  (interactive "r")
  (grass-prep-process)
  (comint-send-region grass-process start end)
  (comint-send-string grass-process "\n"))

(defun grass-prep-process ()
  "Send a newline to the Grass process window.
An ugly hack, without which commands sent directly by Emacs to Grass,
not entered at the command line, produce output starting at the
current prompt, rather than on the next line."  
  (save-window-excursion
    (switch-to-buffer (process-buffer grass-process))
    (goto-char (process-mark grass-process))
    (insert "\n")
    (set-marker (process-mark grass-process) (point))))

(defun grass-send-line ()
  "Send the current line to the active Grass process."
  (interactive)
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (grass-send-region start end))))

(defun grass-send-line-and-step ()
  "Send the current line to the active Grass process."
  (interactive)
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (grass-send-region start end)))
  (forward-line 1))

(define-derived-mode sgrass-mode sh-mode "sgrass"
  "Major mode for editing Grass scripts, and sending commands to a Grass
process. Based on Shell-script mode.

\\{sgrass-mode-map}"
  (define-key sgrass-mode-map (kbd "C-c C-v") 'grass-view-help)
  (define-key sgrass-mode-map (kbd "C-c C-n") 'grass-send-line-and-step)
  (define-key sgrass-mode-map (kbd "C-c C-l") 'grass-change-location)
  (define-key sgrass-mode-map (kbd "C-c C-r") 'grass-send-region)
  (define-key sgrass-mode-map "\t" 'completion-at-point)
  (add-hook 'completion-at-point-functions 'sgrass-complete-commands nil t)
  (add-hook 'completion-at-point-functions 'grass-completion-at-point nil t)
  (run-hooks 'sgrass-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; This should be a minor mode for w3m buffers that are visiting
;; grass help files!

;; If this were a minor mode, we wouldn't need to require w3m unless we
;; were actually using it!  
(require 'w3m)                          


(defun grass-close-w3m-window ()
  "If grass is running, switch to that window. If not, close w3m windows."
  (interactive)
  (if (and (processp grass-process)
               (buffer-name (process-buffer grass-process)))
      (switch-to-buffer (process-buffer grass-process))
    (w3m-close-window)))

(defun grass-jump-to-help-index (ind &optional PREF)
  "Goto a specific grass help index"
  (interactive "c\nP")
  (let ((dest 
         (concat "file://" grass-doc-dir 
                 (case ind
                   (?h "index.html")
                   (?v "vector.html")
                   (?r "raster.html")
                   (?d "display.html")
                   (?b "database.html")
                   (?g "general.html")
                   (t nil)))))
    (unless (string= dest (concat "file://" grass-doc-dir))
      (if PREF 
          (w3m-goto-url-new-session dest)
        (w3m-goto-url dest)))))
                        
(define-key w3m-mode-map "j" 'grass-jump-to-help-index) 
(define-key w3m-mode-map "q" 'grass-close-w3m-window)
(define-key w3m-mode-map "\C-l" 'recenter-top-bottom)
(define-key w3m-ctl-c-map "\C-v" 'grass-view-help)

(provide 'grass-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lies and misdirection beyond this point. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *grass-source* ()
;;   "The buffer where the grass source script is found")

;; (defun grass-set-process-buffer ()
;;   (interactive)
;;   (setq *grass-buffer* (buffer-name)))

;; (defun grass-set-source-buffer ()
;;   (interactive)
;;   (setq *grass-source* (buffer-name)))

;; (defvar grass-scroll-on-output t
;;   "If non-nil, scroll to the end of the Grass process buffer after input.
;; Will always happen when entering data interactively. This variable only
;; changes what happens when sending code from a script buffer to the process
;; buffer.")

;; (defun grass-scroll-to-bottom ()
;;   "Advance point to the end of the grass process buffer."  
;;   (let ((old-buf (current-buffer)))
;;     (switch-to-buffer (process-buffer grass-process))
;;     (goto-char (point-max))
;;     (switch-to-buffer old-buf)))

;;; grass-mode.el ends here

;; grass-mode.el
;; Copyright Tyler Smith 2012
;; For information, contact tyler.smith@mail.mcgill.ca

;; This file is part of grass-mode

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

;;;;;;;;;;;;;;;;;;
;; Dependencies ;;
;;;;;;;;;;;;;;;;;;

;; Need cl.el for mapcar* and remove-if-not. Should add local versions
;; of these so we don't need cl!
(require 'cl)  
(require 'shell)

;; (defun grass-mapcar* (f &rest xs)
;;   "MAPCAR for multiple sequences.
;; Available in cl.el, but requiring cl.el is bad form."
;;   (if (not (memq nil xs))
;;     (cons (apply f (mapcar 'car xs))
;;       (apply 'mapcar* f (mapcar 'cdr xs)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar igrass-mode-hook nil)
(defvar sgrass-mode-hook nil)

(defvar grassdata "~/grassdata"
  "The directory where grass locations are stored.")

(defvar grass-help-w3m nil 
  "If non-nil, use w3m to browse help docs within Emacs. Otherwise, use
browse-url. w3m must be installed separately in your Emacs to use this!")

(defvar grass-default-location nil
  "The default starting location.")

(defvar grass-default-mapset "PERMANENT"
  "The default starting mapset.")

(defvar grass-doc-dir "/usr/share/doc/grass-doc/html/"
  "The location of the Grass html documentation.")

;; Should maybe use a snippet bundle instead?
;;(defvar grass-snippets "~/.emacs.d/grass-mode.el/snippets"
;;  "Directory for all Grass-specific yas templates.")

(defvar grass-prompt "GRASS ($LOCATION_NAME) \\w > "
  "String to format the Grass prompt.")

(defvar grass-prompt-2 "> "
  "String to format the Grass continuation-line prompt, PS2.")

(defvar gisbase "/usr/lib/grass64"
  "The top-level directory that includes the bin and scripts directories for grass.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Global Variables       ;;
;; (shouldn't be set by users) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This adds v and r to the list of possible answers for y-or-n-p
;; (setq grass-query-replace-map (make-sparse-keymap))
;; (set-keymap-parent grass-query-replace-map query-replace-map)
;; (define-key grass-query-replace-map "v" 'act)
;; (define-key grass-query-replace-map "r" 'skip)

;; (setq grass-location-list nil ; The list of grass locations known to grass-mode.el
(setq grass-location nil      ; The currently active grass location
      grass-process nil       ; The active Grass process
      grass-mapset nil        ; The currently active grass mapset
      grass-doc-files 
      (remove-if-not #'(lambda (x) (string-match-p "html$" x))
                     (directory-files grass-doc-dir))
                                        ; The list of Grass html help files
      grass-help nil)          ; The buffer where the grass help is found

;;;;;;;;;;;;;;;;;;;;;
;; Initializations ;;
;;;;;;;;;;;;;;;;;;;;;

(defun grass-init-command-list ()
  "Parses the help files, extracting a list of commands and their parameters"
  (setq grass-commands nil)

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
                             (let ((parameter (match-string-no-properties 1))
                                   (doc-string (progn 
                                                 (search-forward-regexp "<DD>\\(.*\\)</DD>" end t)
                                                 (match-string-no-properties 1))))
                               (push (list parameter doc-string)
                                     result-list)))
                           result-list))
                        grass-commands)
                (push (list (car x)) grass-commands))))
        grass-doc-table))


;; (defun grass-location-list-init ()
;;   "Initialize the alist of grass locations"
;;   (when grassdata
;;     (let* ((location-dirs 
;;             (remove-if-not 'file-directory-p
;;                            (directory-files grassdata t "^[^.]")))
;;            (location-names
;;             (mapcar 'file-name-nondirectory location-dirs)))
;;       (setq grass-location-list
;;             (mapcar* #'(lambda (x y) (cons x y))
;;                      location-names location-dirs)))))

(defun grass-location-list ()
  "Return an alist of grass locations"
  (when grassdata
    (let* ((location-dirs 
            (remove-if-not 'file-directory-p
                           (directory-files grassdata t "^[^.]")))
           (location-names
            (mapcar 'file-name-nondirectory location-dirs)))
      (mapcar* #'(lambda (x y) (cons x y))
               location-names location-dirs))))

;; (defun grass-mapset-list-init ()
;;   "Initialize the alist of mapsets for the current grass location."
;;   (when grass-location
;;     (setq grass-mapset-list 
;;           (mapcar 'file-name-nondirectory
;;                   (remove-if-not 'file-directory-p
;;                                  (directory-files
;;                                   (cdr grass-location) t "^[^.]"))))))

(defun grass-get-location ()
  "Prompt the user for the location."
  (assoc (completing-read
          (format "Grass location (%s): " grass-default-location)
          (grass-location-list) nil t nil nil grass-default-location)
         (grass-location-list)))

(defun grass-mapset-list (&optional location)
  "List the mapsets for a location, defaulting to the current location."
  (let ((loc (if location
                 location
               grass-location)))
    (mapcar 'file-name-nondirectory
            (remove-if-not 'file-directory-p
                           (directory-files
                            (cdr loc) t "^[^.]")))))

(defun grass-get-mapset ()
  "Prompt the user for the mapset for the current location."
  (completing-read (format "Grass mapset (%s): " grass-default-mapset)
                   (grass-mapset-list) nil t nil nil grass-default-mapset))

(defun grass-get-vector ()
  "Prompt the user for a vector map."
  (completing-read "Vector: " (grass-vector-maps)))

(defun grass-get-raster ()
  "Prompt the user for a raster map."
  (completing-read "Raster: " (grass-raster-maps)))

(defun grass-get-map ()
  (funcall (grass-raster-or-vector (grass-current-command))))

(defun grass-current-command ()
  "Returns the current grass command, or nil."
  (save-excursion
    (let ((case-fold-search nil))
      (if (search-backward-regexp
           "\\(\\W\\|^\\)\\(\\([gdvimr]\\|db\\)\\.[^ \t\n]+\\)" nil t)
          (match-string-no-properties 2)))))

(defun grass-raster-or-vector (command)
  "If we can't tell if we need a vector or a raster, we prompt the user.
y or v will return the vector function, n or r the raster function."
  (cond ((string-match "rast" command) 'grass-get-raster)
        ((string-match "vect" command) 'grass-get-vector)
        (t (if (let ((query-replace-map grass-query-replace-map))
                 (y-or-n-p "Vector map?"))
               'grass-get-vector
             'grass-get-raster))))

(defun grass-prompt-raster-or-vector ()
  "Prompts for r or v to indicate raster or vector maps.")

(defun grass-which-map ()
  (grass-raster-or-vector (grass-current-command)))

(defun grass-get-parameter ()
  (interactive)
  (completing-read "Parameter: "
                   (cdr (assoc (grass-current-command) grass-commands))))

(defun grass-completion-at-point ()
  (interactive)
  (let ((pt (point))
        start end)
    (save-excursion                     ;; backup to beginning of multi-line command
      (while (progn (beginning-of-line)
                    (looking-at grass-prompt-2))
        (previous-line))
      (comint-bol)
      (re-search-forward "\\(\\S +\\)\\s ?" nil t)
      (if (and (>= pt (match-beginning 1))
               (<= pt (match-end 1)))
          (progn
            (goto-char pt)
            (let* ((bol (save-excursion (comint-bol) (point)))
                   (eol (save-excursion (end-of-line) (point)))
                   (start (progn (skip-syntax-backward "^ " bol)
                                 (point)))
                   (end (progn (skip-syntax-forward "^ " eol)
                               (point))))
              (list start end grass-commands :exclusive 'no))) 
        ;; still entering the initial command, so try completing Grass commands
        ;; if this fails, control passes to comint-completion-at-point
        (let ((command (match-string-no-properties 1)))
          (when (member* command grass-commands :test 'string= :key 'car)
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
  (let ((collection (third (assoc parameter (assoc command grass-commands)))))
    (list start end 
          (if (functionp collection)
              (funcall collection)
            collection) 
          :exclusive 'no)))

;; (defun grass-complete-parameters (command parameter start end)
;;   (message (concat command " " parameter " " (number-to-string start) " "
;;                    (number-to-string end)))
;;   (message (substring command 0 2))
;;   (cond ((or (string= parameter "vect")  ;; asking for a vector explicitly
;;             (and (or (string-match "vect" command) ;; asking for a map in a vector command
;;                      (string= (substring command 0 2) "v."))
;;                  (or (string= parameter "map")
;;                      (string= parameter "input"))))
;;          (list start end (grass-vector-maps) :exclusive 'no))
;;         ((or (string= parameter "rast")  ;; asking for a raster explicitly
;;              (and (or (string-match "rast" command) ;; asking for a map in a raster command
;;                       (string= (substring command 0 2) "r."))
;;                  (or (string= parameter "map")
;;                      (string= parameter "input"))))
;;          (list start end (grass-raster-maps) :exclusive 'no))
;;         ((string= parameter "location")
;;          (list start end grass-location-list :exclusive 'no))
;;         ((string= parameter "type")
;;          (list start end '("point" "line" "boundary" "centroid" "area" "face") 
;;                :exclusive 'no))
;;         (t nil)))

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

(defun grass ()
  "Start the Grass process, or switch to the process buffer if it's
already active." 
  (interactive)

  ;; initializations
  ;; (grass-location-list-init)
  (setenv "GRASS_PAGER" "cat")
  (setenv "GRASS_VERBOSE" "0")
  (add-to-list 'exec-path (concat gisbase "/bin") t)
  (add-to-list 'exec-path (concat gisbase "/scripts") t)

  ;; (setq ansi-color-for-comint-mode nil) 
  ;; This shouldn't be necessary, but it eliminates the "Marker does not
  ;; point anywhere" issue in the inferior process window. 
  (setq grass-doc-table ())
  (mapc #'(lambda (x) 
            (push (cons (substring x 0 -5)
                        (concat grass-doc-dir x)) grass-doc-table))
        grass-doc-files)

  (grass-init-command-list)
  (require 'grass-commands)

  ;;  (yas/load-directory grass-snippets)

  ;; Start a new process, or switch to the existing one
  (unless (and (processp grass-process)
               (buffer-name (process-buffer grass-process)))
    (setq grass-location (grass-get-location))
    (setq grass-mapset (grass-get-mapset))
    (setq grass-process (start-process "grass" "*grass*" "grass" "-text"
                                       (concat  (file-name-as-directory
                                                 (cdr grass-location)) 
                                                grass-mapset ))))
  ;; (setq grass-process (start-process "grass" "*grass*" "grass" "-text"
  ;;                                    (concat (cdr grass-location) "/"
  ;;                                            grass-mapset))))
  (switch-to-buffer (process-buffer grass-process))
  (comint-send-string grass-process
                      (format "eval `g.gisenv`\nexport PS2=\"%s\"\n"
                              grass-prompt-2))
  (grass-update-prompt)
  (igrass-mode)
  (add-hook 'completion-at-point-functions 'igrass-complete-commands nil t)
  (add-hook 'completion-at-point-functions 'grass-completion-at-point nil t)
  (setq comint-use-prompt-regexp t))

;; (defvar grass-font-lock-keywords
;;   '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-comment-face)
;;     ("^[^ \t\n]+:.*" . font-lock-string-face)
;;     ("^\\[[1-9][0-9]*\\]" . font-lock-string-face)
;;     ("[vdrgi]\\.\\S *") . font-lock-keyword-face)
;;   "Additional expressions to highlight in Shell mode.")

;; (defconst grass-font-lock-keywords-1
;;   (list 
;;    `( ,(concat "\\<" 
;;                (regexp-opt 
;;                 (mapcar #'car grass-commands))
;;                "\\>") . font-lock-builtin-face)))

;; (defvar grass-font-lock-keywords grass-font-lock-keywords-1
;;   "Default highlighting expressions for WPDL mode")


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
  (setq comint-prompt-regexp "^[^#$%>\n]*[#$%>] +")
  (define-key igrass-mode-map (kbd "C-c C-v") 'grass-view-help)
  (define-key igrass-mode-map (kbd "C-a") 'comint-bol)
  (define-key igrass-mode-map (kbd "C-c C-l") 'grass-change-location)
  (run-hooks 'igrass-mode-hook))


(defun grass-view-help ()
  (interactive)
  (let* ((key (completing-read "Grass help: " grass-doc-table nil t))
         (file (cdr (assoc key grass-doc-table))))
    (if (not grass-help-w3m)
        (browse-url (concat "file://" file))
      (if (buffer-name grass-help) 
          (if (get-buffer-window grass-help)
              (select-window (get-buffer-window grass-help))
            (switch-to-buffer-other-window grass-help))
        (switch-to-buffer-other-window "*scratch*"))
      (w3m-goto-url-new-session (concat "file://" file))
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


(defun grass-update-prompt ()
  "Updates the grass prompt."
  (comint-send-string grass-process
                      (format "eval `g.gisenv`\nexport PS1=\"%s\"\n"
                              grass-prompt))
  (grass-prep-process))

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
An ugly hack, without which commands sent directly by Emacs to Grass
(not entered at the command line) produce output starting at the
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
process.\\<igrass-mode-map> \\[comint-send-input] Based on Shell-script mode.

\\{sgrass-mode-map}"
  (define-key sgrass-mode-map (kbd "C-c C-v") 'grass-view-help)
  (define-key sgrass-mode-map (kbd "C-c C-n") 'grass-send-line-and-step)
  (define-key sgrass-mode-map (kbd "C-c C-l") 'grass-change-location)
  (define-key sgrass-mode-map (kbd "C-c C-r") 'grass-send-region)
  (define-key sgrass-mode-map "\t" 'completion-at-point)
  (add-hook 'completion-at-point-functions 'sgrass-complete-commands nil t)
  (add-hook 'completion-at-point-functions 'grass-completion-at-point nil t)
  (run-hooks 'sgrass-mode-hook))


(provide 'grass-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun grass-close-w3m-window ()
  "If grass is running, switch to that window. If not, close w3m windows."
  (interactive)
  (if (and (processp grass-process)
               (buffer-name (process-buffer grass-process)))
      (switch-to-buffer (process-buffer grass-process))
    (w3m-close-window)))

(if grass-help-w3m 
    (progn (require 'w3m)
           (define-key w3m-mode-map "q" 'grass-close-w3m-window)))

(defun goto-grass-help-index ()
  "Goto the grass help index in w3m"
  (interactive)
  (w3m-goto-url (concat "file://" grass-doc-dir "index.html")))

(defun goto-grass-vector-index ()
  "Goto the grass help index in w3m"
  (interactive)
  (w3m-goto-url (concat "file://" grass-doc-dir "vector.html")))

(defun goto-grass-display-index ()
  "Goto the grass help index in w3m"
  (interactive)
  (w3m-goto-url (concat "file://" grass-doc-dir "display.html")))

;; Non-standard bindings here, need to document!

(define-key w3m-mode-map "\C-ch" 'goto-grass-help-index)
(define-key w3m-mode-map "\C-cv" 'goto-grass-vector-index)
(define-key w3m-mode-map "\C-cd" 'goto-grass-display-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lies and misdirection beyond this point. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *grass-source* ()
  "The buffer where the grass source script is found")

(defun grass-set-process-buffer ()
  (interactive)
  (setq *grass-buffer* (buffer-name)))

(defun grass-set-source-buffer ()
  (interactive)
  (setq *grass-source* (buffer-name)))

(defvar grass-scroll-on-output t
  "If non-nil, scroll to the end of the Grass process buffer after input.
Will always happen when entering data interactively. This variable only
changes what happens when sending code from a script buffer to the process
buffer.")

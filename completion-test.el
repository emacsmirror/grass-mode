    (setq my-commands 
          '(("ls"
             ("my-completion-1")
             ("my-completion-2"))
            ("mv"
             ("my-completion-3")
             ("my-completion-4"))))
     
     
    (defun my-completion-at-point ()
      (interactive)
      (let ((pt (point)) ;; collect point
            start end)
        
        (save-excursion ;; collect the command
          (comint-bol)
          (re-search-forward "\\(\\S +\\)\\s ?"))
        (if (and (>= pt (match-beginning 1))
                 (<= pt (match-end 1)))
            () ;; if we're still entering the command, pass completion on to
          ;; comint-completion-at-point 
     
          (let ((command (match-string-no-properties 1)))
            (when (member* command my-commands :test 'string= :key 'car)
              ;; If the command is one of my-commands, use the associated completions 
              (goto-char pt)
              (re-search-backward "\\S *")
              (setq start (point))
              (re-search-forward "\\S *")
              (setq end (point))
              (list start end (cdr (assoc command my-commands)) :exclusive 'no))))))
     
    (push 'my-completion-at-point completion-at-point-functions)


    (defun grass-completion-at-point ()
      (interactive)
      (let ((pt (point))
            start end)
        (save-excursion
          (while (progn (beginning-of-line)
                        (looking-at grass-prompt-2))
            (previous-line))
          (comint-bol)
          (re-search-forward "\\(\\S +\\)\\s ?"))
          (if (and (>= pt (match-beginning 1))
                   (<= pt (match-end 1)))
              () ;; pass completion on to comint-completion-at-point
            (let ((command (match-string-no-properties 1)))
              (when (member* command grass-commands :test 'string= :key 'car)
                (goto-char pt)
                (re-search-backward "\\S *")
                (setq start (point))
                (re-search-forward "\\S *")
                (setq end (point))
                (list start end (cdr (assoc command grass-commands)) :exclusive 'no))))))

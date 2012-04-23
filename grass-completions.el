;; Default value of completion-at-point-functions is: (comint-completion-at-point t)

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

(push 'grass-completion-at-point completion-at-point-functions)

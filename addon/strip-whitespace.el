;;; strip-whitespace.el --- Remove all trailing whitespace from the current buffer.
;;;

(defun strip-trailing-whitespace()
  "Removes trailing whitespace from the current buffer."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[\t ]+$" nil t)
      (replace-match "" nil nil))
    ))



(provide 'strip-whitespace)
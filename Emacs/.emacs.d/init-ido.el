;;----------------------------------------------------------------------------
;; ido setteing
;;----------------------------------------------------------------------------

(defun ido-find-file-in-tag-files ()
      (interactive)
      (save-excursion
        (let ((enable-recursive-minibuffers t))
          (visit-tags-table-buffer))
        (find-file
         (expand-file-name
          (ido-completing-read
           "Project file: " (tags-table-files) nil t)))))
           
(provide 'init-ido)

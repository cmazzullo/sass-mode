(defun indent-file (filein fileout)
  (when (file-readable-p filein)
    (with-temp-buffer
      (insert-file-contents filein)
      (indent-region (point-min) (point-max))
      (when (file-writable-p fileout)
	(write-region (point-min) (point-max) fileout)))))

(indent-file "example.sas" "out.sas")

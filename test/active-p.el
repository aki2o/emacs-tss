(require 'tss)
(require 'typescript)
(require 'el-expectations)
(require 'tenv)

(expectations
  (desc "active-p open new file")
  (expect t
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (let* ((tfile (tenv-get-tmp-file "tss" "active.ts" nil t)))
      (with-current-buffer (find-file-noselect tfile)
        (let ((ret (tss--active-p)))
          (insert "var s1;\n")
          (save-buffer)
          (kill-buffer)))))
  (desc "active-p open exist file")
  (expect t
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (let* ((tfile (tenv-get-tmp-file "tss" "active.ts" nil t)))
      (with-current-buffer (find-file-noselect tfile)
        (tss--active-p))))
  (desc "active-p open not typescript file")
  (expect nil
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (let* ((tfile (tenv-get-tmp-file "tss" "active.txt" nil t)))
      (with-current-buffer (find-file-noselect tfile)
        (tss--active-p))))
  )


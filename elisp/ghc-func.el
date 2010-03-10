;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-func.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

;;; Code:

(defun ghc-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (dotimes (cnt (length string) string)
    (if (char-equal (aref string cnt) from)
        (aset string cnt to))))

(defun ghc-which (cmd)
  (catch 'loop
    (dolist (dir exec-path)
      (let ((path (expand-file-name cmd dir)))
      (if (file-exists-p path)
          (throw 'loop path))))))

(defun ghc-uniq-lol (lol)
  (let ((hash (make-hash-table :test 'equal))
	ret)
    (dolist (lst lol)
      (dolist (key lst)
	(puthash key key hash)))
    (maphash (lambda (key val) (setq ret (cons key ret))) hash)
    ret))

(defun ghc-read-lisp (func)
  (with-temp-buffer
    (funcall func)
    (goto-char (point-min))
    (condition-case nil
	(read (current-buffer))
      (error ()))))

(defun ghc-extract-module ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^import +\\(qualified +\\)?\\([^ (\n]+\\)")
	(match-string-no-properties 2))))

(defun ghc-read-module-name (def)
  (read-from-minibuffer "Module name: " def ghc-input-map))

(provide 'ghc-func)
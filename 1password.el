;;; 1password.el --- A Password manager for Emacs.

;;; AuthorL Javier "PuercoPop" Olaechea <pirata@gmail.com>
;;; URL: http://github.com/PuercoPop/1password.el
;;; Version: 20130721
;;; Keywords: password, productivity

;;; Comentary:

;; It builds upon the pattern described in this post:
;; http://emacs-fu.blogspot.com/2011/02/keeping-your-secrets-secret.html

;; Usage: (1password-register-secrets-file 'secrets.el)
;; M-x 1password

;;; License:
;; Copying is an act of love, please copy. ♡

;;; Todo:
;; make the keymap for the major mode use n and p to go next password
;; Use a minibuffer instead of a buffer
;; Test that the register-secrets-file not only parse one setq form


;;; Code:

(eval-when-compile (require 'cl-lib))

;; So that locate-library works properly.
(setq load-file-rep-suffixes (append load-file-rep-suffixes '(".gpg")))

(define-derived-mode 1password fundamental-mode "1password"
  "Major mode from copying the passwords you store in Emacs to the clipboard")

(defgroup 1password nil
  "A password manager for Emacs"
  :group '1password)

(defcustom 1password-passwords nil
  "A alist containing the passwords."
  :group '1password)


(defun 1password-button-action-callback (button)
  ""
  (let ((variable-name (button-label button)))
    (x-select-text (symbol-value (intern variable-name)))))

(defun 1password-make-button (service-name password)
  (let* ((button-start (point))
         (button-end (+ button-start (length service-name)))) 
    (insert service-name)
    (make-text-button button-start button-end
                      'follow-link t ;; Action also binds the left click :D
                      'action '1password-button-action-callback
                      'help-echo "Copy to Clipboard")))

;;;###autoload
(defun 1password-register-secrets-file (module)
  "Load the setq forms to the module to the 1passwords."
  (with-current-buffer (find-file-noselect
                      (locate-library module) t)
    (setq buffer-read-only t)
    (let ((sexp 
           (read (buffer-substring-no-properties (point-min) (point-max)))))
      (when (equal (car sexp) 'setq)
        (let ((pairs (cdr sexp)))
          (while pairs 
            (add-to-list '1password-passwords `(,(car pairs) . ,(cadr pairs)))
            (setq pairs (cddr pairs))))))))

;;;###autoload
(defun 1password ()
  (interactive)
  (let ((1password-buffer (get-buffer-create "*1password*")))
    (switch-to-buffer 1password-buffer)
    (erase-buffer)
    (loop for (key . value) in  1password-passwords
          do
          (1password-make-button (symbol-name key) value)
          (insert "\n"))
    (goto-char (point-min))))

(provide '1password)
;;; 1password.el ends here

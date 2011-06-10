;;; drupal-mode.el --- Major mode for developing Drupal modules

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'w3m)

(defvar drupal-api-url "http://api.drupal.org/"
  "Drupal API URL.")

(defconst drupal-hook-docstring
  "/**
 * Implements %s()
 */
"
  "Documentation string for hook."
  )

(defun drupal-hook-implement (hook-name)
  "Inserts API code for HOOK-NAME at point."
  (interactive (list (read-string "Hook name: ")))
  (let ((module-name (drupal-module-name))
        (docstring (format drupal-hook-docstring hook-name))
        (url (concat drupal-api-url hook-name)))
    (insert
     (concat docstring (with-current-buffer (get-buffer-create "*drupal-api*")
                         (if (w3m-process-with-wait-handler
                               (w3m-retrieve-and-render url nil nil nil nil handler))
                             (let* ((beg (search-forward "<?php" nil t))
                                    (end (- (search-forward "?>" nil t) 2))
                                    (content (buffer-substring-no-properties beg end)))
                               (replace-regexp-in-string "^function \\(hook\\)_" module-name content nil nil 1))
                           (error "Failed to fetch page.")))))))

;; source: http://drupal.org/node/59868
(defconst drupal-php-style
  '((c-offsets-alist . ((case-label . +)
                        (arglist-intro . +) ; for FAPI arrays and DBTNG
                        (arglist-cont-nonempty . c-lineup-math) ; for DBTNG fields and values
                        (arglist-close . c-lineup-close-paren) ; correct arglist closing parenthesis
                        )))
  "Drupal coding standard.")

;; based on sacha chua's idea
(defun drupal-module-name ()
  "Return the Drupal module name for .module and .install files."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(define-derived-mode drupal-mode
  php-mode "Drupal"
  "Major mode for working with Drupal.
\\{drupal-mode-map}"
  (c-set-style "drupal-php-style")
  (set 'tab-width 2)
  (set 'c-basic-offset 2)
  (local-set-key (kbd "C-c h") 'drupal-hook-implement)
  (set 'indent-tabs-mode nil))

(c-add-style "drupal-php-style" drupal-php-style)

(provide 'drupal-mode)

;;; drupal-mode.el ends here

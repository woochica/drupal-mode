;;; drupal-mode.el --- Major mode for developing Drupal modules

;;; Commentary:
;;


;;; History:
;;

;;; Code:

;; source: http://drupal.org/node/59868
(defconst drupal-php-style
  '((c-offsets-alist . ((case-label . +)
                        (arglist-intro . +) ; for FAPI arrays and DBTNG
                        (arglist-cont-nonempty . c-lineup-math) ; for DBTNG fields and values
                        (arglist-close . c-lineup-close-paren) ; correct arglist closing parenthesis
                        )))
  "Drupal coding standard.")

(define-derived-mode drupal-mode
  php-mode "Drupal"
  "Major mode for working with Drupal.
\\{drupal-mode-map}"
  (c-set-style "drupal-php-style")
  (set 'tab-width 2)
  (set 'c-basic-offset 2)
  (set 'indent-tabs-mode nil))

(c-add-style "drupal-php-style" drupal-php-style)

(provide 'drupal-mode)

;;; drupal-mode.el ends here

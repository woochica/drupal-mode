# Setup

Add the following to your `.emacs`.

    (add-to-list 'load-path "~/.emacs.d/plugins/drupal-mode-el/")
    (require 'drupal-mode)
    (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
    (add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|inc\\)$" . drupal-mode))
    (add-to-list 'auto-mode-alist '("\\.info" . conf-mode))

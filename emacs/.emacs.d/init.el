;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/custom.el")

(setq make-backup-files nil
      auto-save-default nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package fido-mode
  :hook (after-init . fido-mode))

(use-package almost-mono-themes
  :ensure t
  :config
   (load-theme 'almost-mono-black t)
   (load-theme 'almost-mono-white t))

(use-package auto-dark
  :ensure t
  :after almost-mono-themes
  :init  (setq auto-dark-allow-osascript t
	       auto-dark-themes '((almost-mono-black) (almost-mono-white)))
  :hook (after-init . auto-dark-mode))

(use-package which-key
  :ensure t
  :custom (which-key-idle-delay 0.5)
  :hook (after-init . which-key-mode))

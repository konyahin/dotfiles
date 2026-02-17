;; -*- lexical-binding: t; -*-

(use-package emacs
  :init (setq use-short-answers t
	      custom-file "~/.emacs.d/custom.el"
	      make-backup-files nil
	      auto-save-default nil)
  :config
  (set-face-attribute 'default nil :height 150)
  (menu-bar-mode -1)
  (when (display-graphic-p)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(use-package project
  :init (setq compilation-always-kill t))

(use-package fido-mode
  :hook (after-init . fido-mode))

(use-package package
  :config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package almost-mono-themes
  :ensure t
  :config
   (load-theme 'almost-mono-black t t)
   (load-theme 'almost-mono-white t t))

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

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode))

(use-package eglot)

(use-package go-mode
  :ensure t
  :hook ((go-mode . eglot-ensure)
	 (go-mode . (lambda () (setq-local tab-width 4
					   compile-command "go run ./...")))
	 (before-save . gofmt-before-save)))

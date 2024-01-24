;; base settings
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))
(menu-bar-mode 0)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq sentence-end-double-space nil)

(setq inhibit-startup-screen t)

(setq vc-follow-symlinks t)

;; can use s-<arrow> for window switching
(windmove-default-keybindings)

;; abbrev mode
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; isearch settings
(setq search-whitespace-regexp ".*")

;; dired settings
(setq dired-listing-switches "-lap")

(defun knh-dired-find-file-other-frame ()
  "Open file in dired and move it in another emacs frame"
  (interactive)
  (dired-find-file)
  (display-buffer-use-least-recent-window
   (current-buffer) nil)
  (previous-buffer))

(add-hook 'dired-mode-hook
	  (lambda ()
	    "Dired settings"
	    (dired-hide-details-mode)
	    (define-key dired-mode-map (kbd "M-o") 'knh-dired-find-file-other-frame)))

;; settings for packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-install 'markdown-mode)
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

(defun knh-set-buffer-name-md ()
  "Rename md buffer to their header"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (if (search-forward "# " nil t)
	(let ((beg (point)))
	  (move-end-of-line nil)
	  (rename-buffer
	   (string-trim (buffer-substring beg (point))))))))
(add-hook 'markdown-mode-hook 'knh-set-buffer-name-md)

(define-key markdown-mode-map (kbd "M-<up>") 'markdown-previous-visible-heading)
(define-key markdown-mode-map (kbd "M-<down>") 'markdown-next-visible-heading)

(package-install 'howm)
(require 'howm)
(setq howm-file-name-format "%Y-%m-%d-%H%M%S.md")
(setq howm-template "# %title%cursor\n\n%file\n\n")
(setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))
(setq howm-history-file (expand-file-name ".howm-history" howm-directory))
(define-key howm-menu-mode-map "\C-h" nil)
(define-key riffle-summary-mode-map "\C-h" nil)
(define-key howm-view-contents-mode-map "\C-h" nil)

(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x M-f") 'recentf-open-files)

(fido-mode t)

(package-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(package-install 'restclient)
(require 'restclient)
(require 'misc)
(require 'hideshow)

(require 'eshell)
(defalias 'e 'find-file)
(setq eshell-destroy-buffer-when-process-dies nil)
(setq eshell-visual-subcommands '())
(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))

(add-to-list 'load-path "~/.emacs.d/elpa/eshell-port-helper")
(require 'eshell-port-helper)
(defalias 'pdir 'eshell/port-jump-to-port-dir)
(defalias 'psrc 'eshell/port-jump-to-src-dir)
(defalias 'pjump 'eshell/port-jump)
(defalias 'ppatch 'eshell/port-patch)
(defalias 'punpatch 'eshell/port-unpatch)
(defalias 'pmake 'eshell/port-make)
(defalias 'pupdate 'eshell/port-update-patches)

(package-install 'which-key)
(require 'which-key)
(setq which-key-idle-delay 0.5)
(which-key-mode t)

;; custom functions and key bindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

(global-set-key (kbd "M-<f5>") 'compile)
(global-set-key (kbd "<f5>") 'recompile)

(define-key help-mode-map (kbd "n") 'next-line)
(define-key help-mode-map (kbd "p") 'previous-line)

(defun knh/new-line-below ()
  "Insert new line below, without breaking current line"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-c RET") 'knh/new-line-below)

;; borrowed from https://www.omarpolo.com/post/emacs-side-window.html
(defun op/buffer-to-side-window ()
  "Place the current buffer in the side window at the bottom."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer-in-side-window
     buf '((window-height . 0.25)
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-delete-other-windows t)))))
    (delete-window)))

(defun knh/next-input-mark ()
  "Jump to next '<++>' and delete it."
  (interactive)
  (search-forward "<++>")
  (delete-char -4))
(global-set-key (kbd "C-c C-SPC") 'knh/next-input-mark)

;; emacs as shell-editor
(eshell)
(defun knh/jump-to-shell ()
  "Jump to exist shell buffer, or open new"
  (interactive)
  (let ((dir (eshell/pwd)))
	(eshell)
    (message dir)
    (eshell/cd dir)
	;; update shell prompt
	(eshell-interrupt-process)))
(global-set-key (kbd "C-x j") 'knh/jump-to-shell)

(server-start)

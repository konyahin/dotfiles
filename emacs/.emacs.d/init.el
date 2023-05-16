;; base settings
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0))
  (menu-bar-mode 0))

(setq make-backup-files nil)
(setq auto-save-default nil)

;; can use s-<arrow> for window switching
(windmove-default-keybindings)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default c-basic-offset 4)

;; abbrev mode
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; dired settings
(setq dired-listing-switches "-lap")

;; isearch settings
(setq search-whitespace-regexp ".*")

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

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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

;; custom functions and key bindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi))
 '(package-selected-packages '(expand-region restclient howm markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

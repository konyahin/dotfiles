;; -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :height 150)

(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

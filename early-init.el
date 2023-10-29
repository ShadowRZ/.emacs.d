;;; early-init.el -*- lexical-binding: t -*-

;; Disable the menu bar
(menu-bar-mode -1)
;; Disable the tool bar
(tool-bar-mode -1)
;; Disable the scroll bars
(scroll-bar-mode -1)
;; Fullscreen on startup
(push '(fullscreen . fullboth) default-frame-alist)

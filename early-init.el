;;; early-init.el -*- lexical-binding: t -*-

;; Enable CJK N-Gram indexing for Xapian (used by mu)
(setenv "XAPIAN_CJK_NGRAM" "1")

;; Disable the menu bar
(menu-bar-mode -1)
;; Disable the tool bar
(tool-bar-mode -1)
;; Disable the scroll bars
(scroll-bar-mode -1)
;; Fullscreen on startup
(push '(fullscreen . fullboth) default-frame-alist)

;; A marco for installing `PACKAGE' and do `BODY'.
(defmacro ShadowRZ/with-eval-after-install (package &rest body)
  "Execute BODY after PACKAGE is loaded.
This will not automatically require PACKAGE so you can write autoloads
without worring about requires."
  (declare (indent defun))
  `(progn
     (unless (package-installed-p ,package)
       (unless package-archive-contents
         (package-refresh-contents))
       (package-install ,package))
     ,@body))

;; A macro for requiring `FILE' and do `BODY'.
(defmacro ShadowRZ/with-eval-after-require (file &rest body)
  "Load FILE and execute BODY.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See `eval-after-load'
for more details about the different forms of `FILE' and their semantics."
  (declare (indent defun))
  `(progn
     (unless (require ,file nil 'noerror)
       (warn "Loading %s failed" ,file))
     ,@body))

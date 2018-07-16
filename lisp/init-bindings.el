;;; init-bindings.el --- Init bindings.
;;; Commentary:

;;; Code:
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'sgml-mode-hook 'linum-mode)
(dolist (hook (quote
	       (turn-on-auto-fill text-mode-hook-identify linum-mode)))
  (add-hook 'text-mode-hook hook))
(add-hook 'prog-mode-hook 'linum-mode)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-x w") 'elfeed)
(global-set-key (kbd "C-x C-g") 'grep)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f12> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f12> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)

(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-bindings)
;;; init-bindings.el ends here

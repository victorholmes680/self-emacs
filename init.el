;; init.el

;; proxy
;;(setq package-archives
;;      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; ---------------------------
;; 基础 package 初始化
;; ---------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------
;; 设置 lisp 目录
;; ---------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ---------------------------
;; 加载子配置
;; ---------------------------
(load "init-org.el")
(load "init-projectile.el")
;;(load "init-ivy.el")
(load "init-treemacs.el")
(load "init-helm.el")
(load "init-lsp.el")
(load "init-java.el")
;; ---------------------------
;; 其他全局配置
;; ---------------------------
;; (load "init-ui.el")
;; (load "init-keybindings.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

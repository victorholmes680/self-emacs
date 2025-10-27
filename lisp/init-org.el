;; ---------------------------
;; Org Mode 基础配置
;; ---------------------------
(require 'org)

;; 设置 Org 文件存放目录
(setq org-directory "~/org")
;; 自动把 org-directory 下所有 .org 文件加入 agenda
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

;; TODO 状态配置
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; 任务完成自动记录时间
(setq org-log-done 'time)
(setq org-log-into-drawer t) ;; 日志放进 LOGBOOK

;; Agenda 默认显示设置
(setq org-agenda-start-on-weekday nil) ;; 从今天开始
(setq org-agenda-span 7)               ;; 显示一周

;; ---------------------------
;; 快捷键配置
;; ---------------------------
(global-set-key (kbd "C-c a") 'org-agenda)

;; ---------------------------
;; 额外增强（可选）
;; ---------------------------
;; 使用 org-habit 追踪习惯
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

;; org-super-agenda 高级分组（如果安装了 use-package 可以用）
;; (use-package org-super-agenda
;;   :after org
;;   :config
;;   (org-super-agenda-mode))

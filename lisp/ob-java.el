;;; ob-java.el --- Org-babel functions for java evaluation -*- lexical-binding: t -*-
;;; Copyright (C) 2020 Sam Precious

;; Author: Sam Precious <samwdp@gmail.com>
;; URL: http://github.com/samwdp/ob-csharp
;; Keywords: docs org babel csharp
;; Version: 0.0.1
;; Created: 28th Sept 2020
;; Package-Requires: ((org "8") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Org-babel functions for csharp evaluation

;;; Code:
(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("java" . "java"))

(defvar org-babel-default-header-args:java '())

(defun ob-java--build-script-run-command (cmdline path)
  "Create run command with CMDLINE according to the PATH."
  (format "java %s %s" (or cmdline "") path))

(defun org-babel-execute:java (body params)
  "Execute a Java code block with BODY and PARAMS.
Allows interruption with C-g, captures partial output,
     and shows result in a popup window."
  (let* ((processed-params (org-babel-process-params params))
         (cmdline (or (cdr (assoc :cmdline processed-params)) ""))
         (src-file (org-babel-temp-file "org-babel-java-" ".java"))
         (result-buffer "*org-babel-java-result*")
         (result "")
         window)
    ;; 写入 Java 代码到临时文件
    (with-temp-file src-file
      (insert body))
    ;; 清空或创建结果缓冲区
    (with-current-buffer (get-buffer-create result-buffer)
      (erase-buffer))
    ;; 弹出结果缓冲区窗口
    (setq window (display-buffer result-buffer '((display-buffer-pop-up-window))))
    ;; 确保执行完成后关闭弹窗窗口
    (unwind-protect
        (setq result (org-babel-eval (ob-java--build-script-run-command cmdline src-file) ""))
      ;; 执行完成或中断后，关闭弹窗窗口
      (progn
        (delete-file src-file)
        (when (and window (window-live-p window))
          (delete-window window))))
    ;; 返回输出
    (string-trim result)))





;;(defun ob-java--build-script-run-command (cmdline path)
;;  "Create run command according to the PATH."
;;  (format "jshell %s %s" (if (string-empty-p cmdline) "-q" cmdline ) path ))
;;
;;(defun org-babel-execute:java (body params)
;;  (let* ((processed-params (org-babel-process-params params))
;;         (cmpflag (or (cdr (assoc :cmpflag params)) ""))
;;         (cmdline (or (cdr (assoc :cmdline params)) ""))
;;         (src-temp (org-babel-temp-file "java-src-" ".java")))
;;    (with-temp-file src-temp (insert body))
;;    (let ((results (org-babel-eval (ob-java--build-script-run-command cmdline src-temp) "")))
;;      (org-babel-reassemble-table
;;       (org-babel-result-cond (cdr (assoc :result-params params))
;;         (org-babel-read results)
;;         (let ((tmp-file (org-babel-temp-file "c-")))
;;           (with-temp-file tmp-file (insert results))
;;           (org-babel-import-elisp-from-file tmp-file)))
;;       (org-babel-pick-name
;;        (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
;;       (org-babel-pick-name
;;        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

(provide 'ob-java)
;;; ob-java.el ends here

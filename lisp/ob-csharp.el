;;; ob-csharp.el --- Org-babel functions for csharp evaluation -*- lexical-binding: t -*-
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
;; (require 'ob)
;; (require 'csharp-mode)
;;
;; (add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))
;;
;; (defvar org-babel-default-header-args:csharp '())
;;
;; (defun ob-csharp--build-script-run-command (cmdline path)
;;   "Create run command according to the PATH."
;;   (format "dotnet script %s %s" path (or cmdline " ")))
;;
;; (defun org-babel-execute:csharp (body params)
;;   (let* ((processed-params (org-babel-process-params params))
;;          (cmpflag (or (cdr (assoc :cmpflag params)) ""))
;;          (cmdline (or (cdr (assoc :cmdline params)) ""))
;;          (src-temp (org-babel-temp-file "csharp-src-" ".csx")))
;;     (with-temp-file src-temp (insert body))
;;     (let ((results (org-babel-eval (ob-csharp--build-script-run-command cmdline src-temp) "")))
;;       (org-babel-reassemble-table
;;        (org-babel-result-cond (cdr (assoc :result-params params))
;;          (org-babel-read results)
;;          (let ((tmp-file (org-babel-temp-file "c-")))
;;            (with-temp-file tmp-file (insert results))
;;            (org-babel-import-elisp-from-file tmp-file)))
;;        (org-babel-pick-name
;;         (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
;;        (org-babel-pick-name
;;         (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))
;;
;; (provide 'ob-csharp)
;;; ob-csharp.el ends here


(require 'ob)
(require 'csharp-mode)

(add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))

(defvar org-babel-default-header-args:csharp
  '((:release . "no")
    (:nuget . nil))
  "Default arguments for C# Babel blocks.")

(defun ob-csharp--build-script-run-command (cmdline path)
  "Create run command according to the PATH."
  (format "dotnet script %s %s" (or cmdline "") path))

(defun ob-csharp--create-temp-project (nuget-deps code)
  "Create a temporary C# project with NUGET-DEPS and CODE.
Returns the path to the project directory."
  (let ((temp-dir (make-temp-file "ob-csharp-project-" t)))
    (let ((default-directory temp-dir))
      ;; Initialize new console project
      (unless (zerop (call-process "dotnet" nil nil nil "new" "console"))
        (error "Failed to create new dotnet console project"))
      ;; Add NuGet packages if any
      (when (and nuget-deps (listp nuget-deps))
        (dolist (dep nuget-deps)
          (let ((args (split-string dep)))
            (unless (zerop (apply 'call-process "dotnet" nil nil nil "add" "package" args))
              (error "Failed to add NuGet package: %s" dep)))))
      ;; Overwrite Program.cs with user code
      (let ((program-cs (expand-file-name "Program.cs" temp-dir)))
        (with-temp-file program-cs
          (insert code)))
      ;; Verify that the .csproj file exists
      (let ((csproj-files (directory-files temp-dir nil "\\.csproj$")))
        (unless csproj-files
          (error "Project file (.csproj) not found in %s" temp-dir)))
      temp-dir)))

(defun org-babel-execute:csharp (body params)
  "Execute a C# code block with BODY and PARAMS."
  (let* ((processed-params (org-babel-process-params params))
         (release (string= (or (cdr (assoc :release processed-params)) "no") "yes"))
         (nuget (cdr (assoc :nuget processed-params)))
         (cmdline (or (cdr (assoc :cmdline processed-params)) ""))
         (result "")
         (output-buffer "*ob-csharp-output*"))
    (if release
        ;; Release mode: create project, build, and run
        (let ((project-dir (ob-csharp--create-temp-project nuget body)))
          (message "project-dir: %s" project-dir)
          (unwind-protect
              (let ((default-directory project-dir))
                ;; Build the project in Release mode
                (unless (zerop (call-process "dotnet" nil output-buffer t "build" "-c" "Release"))
                  (error "Failed to build the C# project. See %s for details." output-buffer))
                ;; Run the project
                ;; 清空输出缓冲区
                (with-current-buffer (get-buffer-create output-buffer)
                  (erase-buffer))

                (unless (zerop (call-process "dotnet" nil output-buffer t "run" "-c" "Release"))
                  (error "Failed to run the C# project. See %s for details." output-buffer))
                ;; Capture the output
                (with-current-buffer output-buffer
                  (setq result (buffer-string))))
            ;; Clean up: optionally delete the temp project directory
            (delete-directory project-dir t)))
      ;; Script mode: existing behavior
      (let ((src-temp (org-babel-temp-file "csharp-src-" ".csx")))
        (with-temp-file src-temp (insert body))
        (setq result (org-babel-eval (ob-csharp--build-script-run-command cmdline src-temp) ""))))
    ;; Return the result, trimming any trailing whitespace
    (string-trim result)))

(provide 'ob-csharp)
;;; ob-csharp.el ends here

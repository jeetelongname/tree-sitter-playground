;;; tree-sitter-playground.el --- Better tree visualisation for tree sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeetaditya Chatterjee

;; Author: Jeetaditya Chatterjee <jeetelongname@gmail.com>
;; Keywords: languages, tools, tree-sitter
;;; Commentary:
;;; Code:

(require 'tree-sitter)
(require 'seq)

(defgroup tree-sitter-playground nil
  "A better way to visulise the ast."
  :group 'tree-sitter)

(defvar-local tree-sitter-playground--tree-buffer nil
  "Buffer used to display syntax tree.")

(defvar-local tree-sitter-playground--source-code-buffer nil
  "Source buffer that the syntax tree represents.")

(defun tree-sitter-playground--display-node (node depth)
  "Display a NODE, DEPTH."
  (insert (make-string (* 2 depth) ?\ ))
  (let* ((node-range (seq-take (append (tsc-node-range node) nil) 2))
         (node-text (format "%s [%s - %s]:\n" (tsc-node-type node) (car node-range) (cadr node-range))))
    (insert node-text))
  (tsc-mapc-children (lambda (c)
                       (when (tsc-node-named-p c)
                         (tree-sitter-playground--display-node c (1+ depth))))
                     node))

(defun tree-sitter-playground--display-tree (_old-tree)
  "Display Current tree."
  (when-let ((tree tree-sitter-tree))
    (with-current-buffer tree-sitter-playground--tree-buffer
      (let (buffer-read-only)
        (erase-buffer)
        (tree-sitter-playground--display-node (tsc-root-node tree) 0)))))

(defun tree-sitter-playground--setup ()
  "Set up the playground in the current buffer."
  (unless (buffer-live-p tree-sitter-playground--tree-buffer)
    (setq tree-sitter-playground--tree-buffer
          (get-buffer-create (format "tree-sitter-tree: %s" (buffer-name)))))
  (let ((source-buffer (current-buffer)))
    (with-current-buffer tree-sitter-playground--tree-buffer
      (buffer-disable-undo)
      (setq tree-sitter-playground--source-code-buffer source-buffer
            buffer-read-only t)))
  (add-hook 'tree-sitter-after-change-functions #'tree-sitter-playground--display-tree nil :local)
  (add-hook 'kill-buffer-hook #'tree-sitter-playground--teardown nil :local)
  (display-buffer tree-sitter-playground--tree-buffer)
  (tree-sitter-playground--display-tree nil))

(defun tree-sitter-playground--teardown ()
  "Tear down the playground in the current buffer."
  (remove-hook 'tree-sitter-after-change-functions #'tree-sitter-playground--display-tree :local)
  (when (buffer-live-p tree-sitter-playground--tree-buffer)
    (kill-buffer tree-sitter-playground--tree-buffer)
    (setq tree-sitter-playground--tree-buffer nil)))

(define-minor-mode tree-sitter-playground-mode
  "A playground to visulise the tree sitter tree."
  :init-value nil
  :group 'tree-sitter-playground
  (tree-sitter--handle-dependent tree-sitter-playground-mode
    #'tree-sitter-playground--setup
    #'tree-sitter-playground--teardown))

(provide 'tree-sitter-playground)
;;; tree-sitter-playground.el ends here

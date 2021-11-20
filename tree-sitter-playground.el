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

(defcustom tree-sitter-playground-range-face 'tree-sitter-hl-face:number
  "The face used by the range."
  :type 'symbol
  :group 'tree-sitter-playground)

(defcustom tree-sitter-playground-jump-buttons nil
  "Jump to node."
  :type 'boolean
  :group 'tree-sitter-playground)

(defcustom tree-sitter-playground-highlight-jump-region nil
  "Whether to highlight the node jumped to.
This only takes effect if `tree-sitter-debug-jump-buttons' is non-nil."
  :type 'boolean
  :group 'tree-sitter-debug)

(defface tree-sitter-playground-button-face
  '((t :underline t))
  "Face for buttons."
  :group 'tree-sitter-playground)

(defvar-local tree-sitter-playground--tree-buffer nil
  "Buffer used to display syntax tree.")

(defvar-local tree-sitter-playground--source-code-buffer nil
  "Source buffer that the syntax tree represents.")

(defun tree-sitter-playground--button-node-lookup (button)
  "The function to call when a `tree-sitter-playground' BUTTON is clicked."
  (unless tree-sitter-playground--source-code-buffer
    (error "No source code buffer set"))
  (unless (buffer-live-p tree-sitter-playground--source-code-buffer)
    (user-error "Source code buffer has been killed"))
  (unless button
    (user-error "This function must be called on a button"))
  (tree-sitter-playground--goto-node tree-sitter-playground--source-code-buffer
                                     (button-get button 'points-to)))

(defun tree-sitter-playground--goto-node (buffer node)
  "Switch to BUFFER, centering on the region defined by NODE."
  (switch-to-buffer-other-window buffer)
  (let ((range (tsc-node-position-range node)))
    (goto-char (car range))
    (push-mark (cdr range) t tree-sitter-playground-highlight-jump-region)))

(defun tree-sitter-playground--display-node (node depth)
  "Display a NODE at a certain depth DEPTH."
  (insert (make-string (* 2 depth) ?\ ))
  (let* ((node-range (tsc-node-byte-range node))
         (node-text (format "%s [%s - %s]:\n" (propertize (symbol-name (tsc-node-type node)) 'face 'tree-sitter-hl-face:function)
                            (propertize (number-to-string (car node-range)) 'face tree-sitter-playground-range-face)
                            (propertize (number-to-string (cdr node-range)) 'face tree-sitter-playground-range-face))))
    (if tree-sitter-playground-jump-buttons
        (insert-button
         node-text
         'face 'tree-sitter-playground-button-face
         'action #'tree-sitter-playground--button-node-lookup
         'follow-link t
         'help-echo "mouse-2, RET: click me!"
         'points-to node)
      (insert node-text)))
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

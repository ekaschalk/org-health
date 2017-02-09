;;; Unused
(defun health--prompt-table-name ()
  "Collects and prompts a table name"
  (health--read
   "Table"
   (org-element-map (org-element-parse-buffer 'element) 'table
     (-partial #'org-element-property :name))))

;;; Tables
;;;; Access
(defun health--table-headers (TABLE)
  (nth 0 TABLE))
(defun health--table-options (TABLE)
  (nth 1 TABLE))
(defun health--table-values (TABLE)
  (-slice TABLE (+ 1 (-elem-index 'hline TABLE))))
(defun health--table-options-format ()
  (-partial #'s-split "/"))

;;;; Prompts
(cl-defun health--read (HEADER OPTIONS &optional (FORMAT-FUNC #'-list))
  "HEADER prompts resulting list of strings from FORMAT-FUNC applied to OPTIONS"
  (if (or (not OPTIONS)
          (eq "" OPTIONS))
      (read-string
       (concat HEADER ": "))
    (helm :sources (helm-build-sync-source HEADER
                     :candidates (lambda () (funcall FORMAT-FUNC OPTIONS))
                     :fuzzy-match t))))

(defun health--prompt-add-to (TABLE)
  "Prompts a row for TABLE according to its headers and options."
  ;; (--zip-with (health--read it (s-split "/" other))
  (--zip-with (health--read it other (health--table-options-format))
              (health--table-headers TABLE)
              (health--table-options TABLE)))

(defun health--prompt-table ()
  "Prompts tables and returns its lisp representation"
  (save-excursion
    (let* ((tables
            (org-element-map (org-element-parse-buffer 'element) 'table
              (lambda (table)
                (let ((name (org-element-property :name table)))
                  (when name
                    (-list ':name
                           name
                           ':contents-begin
                           (org-element-property :contents-begin table)))))))
           (selection
            (health--read "Table" (--map (plist-get it ':name) tables)))
           (table
            (--first (string= selection (plist-get it ':name)) tables))
           (pos
            (plist-get table :contents-begin)))
      (goto-char pos)
      (org-table-to-lisp))))

;;;; Filters
(defun health--narrow-table (TABLE)
  "Filters TABLE on prompted column"
  (let* ((headers (health--table-headers TABLE))
         (column (-elem-index
                  (health--read "Filter" headers) headers))
         (options (-distinct
                   (-select-column column (health--table-values TABLE)))))
    (health--read "By value" options)))

;;;; Utilities
(defun health--add-to-table (TABLE)
  "Prompts and appends row to TABLE"
  (append TABLE (list (health--prompt-add TABLE))))

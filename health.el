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

;;;; Prompts
(defun health--read (HEADER OPTIONS)
  "HEADER prompts OPTIONS a list of strings or nil for non-helm reading"
  (if (not OPTIONS)
      (read-string (concat HEADER ": "))
    (helm :sources (helm-build-sync-source HEADER
                     :candidates OPTIONS
                     :fuzzy-match t))))

(defun health--prompt-row (TABLE)
  "Prompts a row for TABLE according to its headers and options."
  (-zip-with 'health--read
             (health--table-headers TABLE)
             (--map (s-split "/" it t)
                   (health--table-options TABLE))))

(defun health--prompt-table ()
  "Prompts tables and returns its lisp representation"
  (save-excursion
    (let* ((tables
            (org-element-map (org-element-parse-buffer 'element) 'table
              (lambda (table)
                (when-let ((name (org-element-property :name table)))
                  (-list ':name
                         name
                         ':contents-begin
                         (org-element-property :contents-begin table))))))
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
(defun health--add-to-table ()
  "Prompts and appends row to TABLE"
  (let ((table (health--prompt-table)))
    (-snoc table (health--prompt-row table))))

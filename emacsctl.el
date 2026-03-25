;; emacsctl.el --- Emacs control interface -*- lexical-binding: t -*-

(defcustom emacsctl-query-surrounding-lines 2
  "Number of lines to include in the surrounding when query/grep."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-long-line-limit 1024
  "Number of characters that can be considered \"too long\" and truncated."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-too-many-lines-limit 512
  "Number of lines that can be considered as \"too many\" and truncated."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-too-many-buffers-limit 100
  "Number of buffers that can be considered as \"too many\"."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-many-symbols-limit 15
  "Number of items in a symbols query that can be considered as \"many\"."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-too-many-symbols-limit 50
  "Number of items in a symbols query that can be considered as \"too many\"."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-too-many-grep-result-limit 50
  "Number of items in a grep that can be considered as \"too many\"."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-small-buffer-limit 8192
  "Size of a buffer that can be considered as \"small\".

Buffer smaller than this limit will be fully printed while reading."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-print-level 3
  "Maximum depth for printing nested structures in query results."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-print-length 50
  "Maximum length for printing lists in query results."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-print-string-length 1024
  "Maximum length for printing propertized string values in query results.

Longer strings will be truncated."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-print-propertized-string-length 1024
  "Maximum length for printing propertized string values in query results.

Longer strings will be truncated and have their properties removed."
  :type 'integer
  :group 'emacsctl)

(defcustom emacsctl-max-suggest-candidate-number 10
  "Maximum number of candidates to suggest when invalid query is made."
  :type 'integer
  :group 'emacsctl)

(define-minor-mode emacsctl-supervise-mode
  "Supervise (follow) every changes made by emacsctl."
  :global t
  :lighter nil)

(defvar emacsctl--last-queried-windows nil)

(cl-defstruct emacsctl-pprint-item
  "Items that shall be printed with custom level & length."
  level length string-length propertized-string-length
  print-function
  content)

(defvar emacsctl--pprint-print-func #'prin1
  "The actual function used to print the content of `emacsctl-pprint-item'.")

(defun emacsctl-pprint (item)
  "Prin1 ITEM with probably custom level & length."
  (cl-typecase item
    (emacsctl-pprint-item
     (let ((print-level (emacsctl-pprint-item-level item))
           (print-length (emacsctl-pprint-item-length item))
           (emacsctl-print-string-length (emacsctl-pprint-item-string-length item))
           (emacsctl-print-propertized-string-length (or (emacsctl-pprint-item-propertized-string-length item)
                                                         (emacsctl-pprint-item-string-length item)))
           (emacsctl--pprint-print-func (or (emacsctl-pprint-item-print-function item)
                                            emacsctl--pprint-print-func)))
       (emacsctl-pprint (emacsctl-pprint-item-content item))))
    (null (funcall emacsctl--pprint-print-func nil))
    (list (write-char ?\( )
          (cl-loop for (child . rest) on item
                   do (emacsctl-pprint child)
                   when rest do (write-char ? ))
          (write-char ?\) ))
    (vector (write-char ?\[ )
            (cl-loop for i from 0 below (length item)
                     do (emacsctl-pprint (aref item i))
                     when (< i (1- (length item))) do (write-char ? ))
            (write-char ?\] ))
    (string (if (eq emacsctl--pprint-print-func #'prin1)
                (let ((rep (with-output-to-string
                             (prin1 item))))
                  (if (eql (aref rep 0) ?#)
                      (if (or (null emacsctl-print-propertized-string-length)
                              (< (length rep) emacsctl-print-propertized-string-length))
                          (princ rep)
                        (if (< (length item) emacsctl-print-propertized-string-length)
                            (progn
                              (princ "#(")
                              (prin1 (substring-no-properties item))
                              (princ " ...[PROPERTIES TRUNCATED])"))
                          (prin1 (concat (substring-no-properties
                                          item 0 (- emacsctl-print-propertized-string-length
                                                    (length "...[CONTENT AND PROPERTIES TRUNCATED]")))
                                         "...[CONTENT AND PROPERTIES TRUNCATED]"))))
                    (if (or (null emacsctl-print-string-length)
                            (< (length item) emacsctl-print-string-length))
                        (princ rep)
                      (prin1 (concat (substring item 0 (- emacsctl-print-string-length
                                                          (length "...[TRUNCATED]")))
                                     "...[TRUNCATED]")))))
              (funcall emacsctl--pprint-print-func item)))
    (t (funcall emacsctl--pprint-print-func item))))

(defun emacsctl--make-std-pprint-item (content &optional print-func)
  (make-emacsctl-pprint-item :level emacsctl-print-level
                             :length emacsctl-print-length
                             :string-length emacsctl-print-string-length
                             :propertized-string-length emacsctl-print-propertized-string-length
                             :print-function (or print-func emacsctl--pprint-print-func)
                             :content content))

(defun emacsctl--get-line (orig-point orig-mark &optional no-truncate-p)
  (let* ((bol (pos-bol))
         (eol (pos-eol))
         (is-bof (= bol (point-min)))
         (is-eof (= eol (point-max)))
         (line-len (- eol bol)))
    (cl-flet ((line-with-1-pt (pt marker)
                (if no-truncate-p
                    (concat (if is-bof "[BOF]" "")
                            (buffer-substring-no-properties bol pt)
                            marker
                            (buffer-substring-no-properties pt eol)
                            (if is-eof "[EOF]" ""))
                  (let* ((remain emacsctl-long-line-limit)
                         (end (min eol (+ pt (floor remain 2))))
                         (remain (- remain (- end pt)))
                         (start (max bol (- pt remain)))
                         (remain (- remain (- pt start)))
                         (end (min eol (if (> remain 0) (+ end remain) end))))
                    (concat (if is-bof "[BOF]" "")
                            (if (> start bol) "[TRUNCATED]..." "")
                            (buffer-substring-no-properties start pt)
                            marker
                            (buffer-substring-no-properties pt end)
                            (if (< end eol) "...[TRUNCATED]" "")
                            (if is-eof "[EOF]" "")))))
              (line-with-2-pt (pt1 marker1 pt2 marker2)
                (if no-truncate-p
                    (concat (if is-bof "[BOF]" "")
                            (buffer-substring-no-properties bol pt1)
                            marker1
                            (buffer-substring-no-properties pt1 pt2)
                            marker2
                            (buffer-substring-no-properties pt2 eol)
                            (if is-eof "[EOF]" ""))
                  (let* ((remain emacsctl-long-line-limit)
                         (end1 (min eol (+ pt1 (floor remain 2))))
                         (remain (- remain (- end1 pt1)))
                         (start1 (max bol (- pt1 remain)))
                         (remain (- remain (- pt1 start1)))
                         (end1 (min eol (if (> remain 0) (+ end1 remain) end1)))

                         (remain emacsctl-long-line-limit)
                         (end2 (min eol (+ pt2 (floor remain 2))))
                         (remain (- remain (- end2 pt2)))
                         (start2 (max bol (- pt2 remain)))
                         (remain (- remain (- pt2 start2)))
                         (end2 (min eol (if (> remain 0) (+ end2 remain) end2))))
                    (concat (if is-bof "[BOF]" "")
                            (if (> start1 bol) "[TRUNCATED]..." "")
                            (buffer-substring-no-properties start1 pt1)
                            marker1
                            (if (< start2 end1)
                                (buffer-substring-no-properties pt1 pt2)
                              (concat (buffer-substring-no-properties pt1 end1)
                                      "...[TRUNCATED]..."
                                      (buffer-substring-no-properties start2 pt2)))
                            marker2
                            (buffer-substring-no-properties pt2 end2)
                            (if (< end2 eol) "...[TRUNCATED]" "")
                            (if is-eof "[EOF]" ""))))))
      (cond ((and (<= bol orig-point eol)
                  (<= bol orig-mark eol))
             (if (< orig-point orig-mark)
                 (line-with-2-pt orig-point "█" orig-mark "▄")
               (line-with-2-pt orig-mark "▄" orig-point "█")))
            ((<= bol orig-point eol)
             (line-with-1-pt orig-point "█"))
            ((<= bol orig-mark eol)
             (line-with-1-pt orig-mark "▄"))
            (t (concat (if is-bof "[BOF]" "")
                       (if (> line-len emacsctl-long-line-limit)
                           (concat (buffer-substring-no-properties bol (+ bol emacsctl-long-line-limit))
                                   "...[TRUNCATED]")
                         (buffer-substring-no-properties bol eol))
                       (if is-eof "[EOF]" "")))))))

(cl-defun emacsctl--query-lines (&key start-pos end-pos
                                      start-line end-line
                                      markp no-truncate-p (hl-p t)
                                      (show-point-p t)
                                      (show-mark-p t))
  (cond ((and start-line end-line) t)
        ((and start-pos end-pos)
         (setq start-line (line-number-at-pos start-pos)
               end-line (line-number-at-pos end-pos)))
        (markp (setq start-line (line-number-at-pos (mark))
                     end-line (line-number-at-pos (mark))))
        (t (setq start-line (line-number-at-pos (point))
                 end-line (line-number-at-pos (point)))))
  (let* ((ext-start-line (max (line-number-at-pos (point-min))
                          (- start-line emacsctl-query-surrounding-lines)))
         (ext-end-line (min (line-number-at-pos (point-max))
                        (+ end-line emacsctl-query-surrounding-lines)))
         (point (if show-point-p (point) -1))
         (mark (if show-mark-p (mark) -1)))
    (make-emacsctl-pprint-item
     :string-length nil
     :content (if (and (null no-truncate-p)
                       (> (- ext-end-line ext-start-line) (1+ emacsctl-too-many-lines-limit)))
                  (let ((half (floor emacsctl-too-many-lines-limit 2)))
                    (save-excursion
                      (let ((first-half-end (+ ext-start-line half))
                            (second-half-start (1+ (- ext-end-line half))))
                        (string-join
                         (append (cl-loop initially (progn (goto-char (point-min))
                                                           (forward-line (1- ext-start-line)))
                                          for i from ext-start-line below first-half-end
                                          collect (format (if (and hl-p (<= start-line i end-line))
                                                              ">%d┃%s"
                                                            " %d┃%s")
                                                          i (emacsctl--get-line point mark))
                                          do (forward-line 1))
                                 (list (format (if (and hl-p (<= start-line first-half-end (1- second-half-start) end-line))
                                                   ">%d-%d┃[TRUNCATED]"
                                                 " %d-%d┃[TRUNCATED]")
                                               first-half-end (1- second-half-start)))
                                 (cl-loop initially (progn (goto-char (point-min))
                                                           (forward-line (1- second-half-start)))
                                          for i from second-half-start to ext-end-line
                                          collect (format (if (and hl-p (<= start-line i end-line))
                                                              ">%d┃%s"
                                                            " %d┃%s")
                                                          i (emacsctl--get-line point mark))
                                          do (forward-line 1)))
                         "\n"))))
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- ext-start-line))
                  (string-join
                   (cl-loop for i from ext-start-line to ext-end-line
                            collect (format (if (and hl-p (<= start-line i end-line))
                                                ">%d┃%s"
                                              " %d┃%s")
                                            i (emacsctl--get-line point mark))
                            do (forward-line 1))
                   "\n"))))))

(defun emacsctl--char-before ()
  (if-let* ((c (char-before)))
      (pcase c
        (?\t "\\t")
        (?\n "\\n")
        (?\r "\\r")
        (?\f "\\f")
        (?\v "\\v")
        (_ (string c)))
    'BOF))

(defun emacsctl--char-after ()
  (if-let* ((c (char-after)))
      (pcase c
        (?\t "\\t")
        (?\n "\\n")
        (?\r "\\r")
        (?\f "\\f")
        (?\v "\\v")
        (_ (string c)))
    'EOF))

(defun emacsctl--fuzzy-match (target source)
  "Fuzzy match TARGET in SOURCE."
  (cl-loop with start = -1
           for c across target
           if (> (1+ start) (length source)) do (cl-return)
           else do (setq start (cl-position c source :start (1+ start) :test #'eql))
           if (null start) do (cl-return)
           else collect start))

(defun emacsctl--fuzzy-match-buffer-names (target)
  (let ((result))
    (mapcar (lambda (buf)
              (when-let* ((buf-name (buffer-name buf))
                          (score (emacsctl--fuzzy-match target buf-name)))
                (push (list buf-name score) result)))
            (buffer-list))
    (mapcar #'car
            (sort result :lessp (lambda (list1 list2)
                                  (cl-destructuring-bind (sym1 starts1) list1
                                    (cl-destructuring-bind (sym2 starts2) list2
                                      (let ((n1 (cl-loop for j = 0 then i
                                                         for i in starts1
                                                         sum (- i j)))
                                            (n2 (cl-loop for j = 0 then i
                                                         for i in starts2
                                                         sum (- i j))))
                                        (if (= n1 n2)
                                            (string< sym1 sym2)
                                          (< n1 n2))))))))))

(defun emacsctl--roughly-match (target source)
  "Check if TARGET match SOURCE."
  (cl-loop with start = -1
           for seq in (split-string target "[ \f\t\n\r\v-]+" t)
           if (> (1+ start) (length source)) do (cl-return)
           else do (setq start (cl-search seq source :start2 (1+ start) :test #'eql))
           if (null start) do (cl-return)
           else collect start))

(defun emacsctl--roughly-match-symbols (target)
  (let ((result))
    (mapatoms (lambda (sym)
                (when-let* ((score (emacsctl--roughly-match target (symbol-name sym))))
                  (push (list sym score) result))))
    (mapcar #'car
            (sort result :lessp (lambda (list1 list2)
                                  (cl-destructuring-bind (sym1 starts1) list1
                                    (cl-destructuring-bind (sym2 starts2) list2
                                      (let ((n1 (cl-loop for j = 0 then i
                                                         for i in starts1
                                                         sum (- i j)))
                                            (n2 (cl-loop for j = 0 then i
                                                         for i in starts2
                                                         sum (- i j))))
                                        (if (= n1 n2)
                                            (string< sym1 sym2)
                                          (< n1 n2))))))))))

(defun emacsctl--function-args (sym)
  "Return function arguments for SYM.

Copied from `marginalia--function-args', really thanks."
  (let (tmp)
    (elisp-function-argstring
     (cond
      ((listp (setq tmp (gethash (indirect-function sym)
                                 advertised-signature-table t)))
       tmp)
      ((setq tmp (help-split-fundoc
                  (ignore-errors (documentation sym t))
                  sym))
       (car tmp))
      ((setq tmp (help-function-arglist sym))
       (if (and (stringp tmp) (string-search "not available" tmp))
           ;; A shorter text fits better into the limited Marginalia space.
           "[autoload]"
         tmp))))))

(defun emacsctl--scan-toplevels (buffer)
  (with-current-buffer buffer
    (save-excursion
      (let ((lst (cl-loop initially (setq point (point-min))
                          for point = (ignore-errors (scan-lists point 1 0))
                          while point
                          do (goto-char (scan-lists point -1 0))
                          collect (format "%d ┃%s"
                                          (line-number-at-pos)
                                          (emacsctl--get-line -1 -1)))))
        (string-join lst "\n")))))

(defun emacsctl--scan-imenu (buffer)
  (with-current-buffer buffer
    (make-emacsctl-pprint-item
     :string-length nil
     :content (if (derived-mode-p 'lisp-data-mode)
                  (emacsctl--scan-toplevels buffer)
                (save-excursion
                  (let* ((markers (cl-remove-if-not #'markerp
                                                    (flatten-tree (let ((imenu-auto-rescan t))
                                                                    (imenu--make-index-alist)))))
                         (lst (cl-loop for marker in markers
                                       do (goto-char (marker-position marker))
                                       collect (format "%d ┃%s"
                                                       (line-number-at-pos)
                                                       (emacsctl--get-line -1 -1)))))
                    (string-join lst "\n")))))))

(defun emacsctl--buffer-not-found-error (buffer-name)
  (cl-list* :error (format "Buffer %s not found." buffer-name)
            (when-let* ((suggest (emacsctl--fuzzy-match-buffer-names buffer-name)))
              (list :suggest-candidates (take emacsctl-max-suggest-candidate-number suggest)))))

(defun emacsctl-point-info (&optional surrounding)
  (cl-list* :position (point)
            :line (line-number-at-pos)
            :column (current-column)
            :mark-active mark-active
            :char-before (emacsctl--char-before)
            :char-after (emacsctl--char-after)
            (if mark-active
                (list :active-region (emacsctl--query-lines
                                      :start-pos (min (point) (mark))
                                      :end-pos (max (point) (mark))))
              (list :surrounding (let ((emacsctl-query-surrounding-lines
                                        (or surrounding emacsctl-query-surrounding-lines)))
                                   (emacsctl--query-lines))))))

(defun emacsctl-mark-info (&optional surrounding)
  (let ((mark (mark)))
    (if mark
        (cl-list* :position mark
                  :active mark-active
                  :line (line-number-at-pos mark)
                  :column (save-excursion
                            (goto-char mark)
                            (current-column))
                  :char-before (emacsctl--char-before)
                  :char-after (emacsctl--char-after)
                  (if mark-active
                      (list :active-region (emacsctl--query-lines
                                            :start-pos (min (point) mark)
                                            :end-pos (max (point) mark)))
                    (list :surrounding (let ((emacsctl-query-surrounding-lines
                                              (or surrounding emacsctl-query-surrounding-lines)))
                                         (emacsctl--query-lines :markp t)))))
      (list :error "No mark set yet."))))

(defun emacsctl-buffer-info (&optional buffer)
  (let ((buffer (if buffer (get-buffer buffer) (current-buffer))))
    (if buffer
        (with-current-buffer buffer
          (let* ((file-name (buffer-file-name buffer))
                 (res (list :name (buffer-name buffer)
                            :file file-name
                            :lines (count-lines (point-min) (point-max))
                            :size (format "%d (%s)"
                                          (buffer-size buffer)
                                          (file-size-human-readable (buffer-size buffer)))
                            :modified-p (buffer-modified-p)
                            :read-only-p buffer-read-only
                            :narrowed-p (buffer-narrowed-p))))
            (if (and file-name (local-variable-p 'buffer-file-coding-system))
                (setq res (nconc res (list :coding-system buffer-file-coding-system))))
            (setq res (nconc res (cl-list* :major-mode major-mode
                                           :local-minor-modes (emacsctl--make-std-pprint-item local-minor-modes) ; They can exceed 50...
                                           :global-minor-modes (emacsctl--make-std-pprint-item global-minor-modes)
                                           :point (cl-list* :position (point)
                                                            :line (line-number-at-pos)
                                                            :column (current-column)
                                                            :char-before (emacsctl--char-before)
                                                            :char-after (emacsctl--char-after)
                                                            (unless mark-active
                                                              (list :surrounding (emacsctl--query-lines))))
                                           :mark-active mark-active
                                           (when mark-active
                                             (list :active-region (emacsctl--query-lines
                                                                   :start-pos (min (point) (mark))
                                                                   :end-pos (max (point) (mark))))))))
            res))
      (emacsctl--buffer-not-found-error buffer))))

(cl-defun emacsctl-buffer-imenu (&optional buffer)
  (let ((buffer (if buffer
                    (ignore-errors (get-buffer buffer))
                  (current-buffer))))
    (if buffer
        (list :lines (count-lines (point-min) (point-max))
              :size (format "%d (%s)"
                            (buffer-size buffer)
                            (file-size-human-readable (buffer-size buffer)))
              :imenu (emacsctl--scan-imenu buffer))
      (emacsctl--buffer-not-found-error buffer))))

(cl-defun emacsctl-read-buffer (&key buffer start-line end-line line start-position end-position position surrounding full-p)
  (let ((buffer (if buffer
                    (ignore-errors (get-buffer buffer))
                  (current-buffer))))
    (if buffer
        (let ((res (list :lines (count-lines (point-min) (point-max))
                         :size (format "%d (%s)"
                                       (buffer-size buffer)
                                       (file-size-human-readable (buffer-size buffer)))
                         :point (list :position (point)
                                      :line (line-number-at-pos)
                                      :column (current-column))
                         :mark-active mark-active)))
          (when mark-active
            (setq res (nconc (list :mark (list :position (mark)
                                               :line (line-number-at-pos (mark))
                                               :column (save-excursion
                                                         (goto-char mark)
                                                         (current-column)))))))
          (cond ((or start-line end-line line)
                 (when line
                   (setq start-line line
                         end-line line))
                 (unless start-line
                   (setq start-line (line-number-at-pos (point-min))))
                 (unless end-line
                   (setq end-line (line-number-at-pos (point-max))))
                 (setq res (nconc res (list :content (let ((emacsctl-query-surrounding-lines
                                                            (or surrounding emacsctl-query-surrounding-lines)))
                                                       (emacsctl--query-lines
                                                        :start-line start-line
                                                        :end-line end-line
                                                        :no-truncate-p t
                                                        :hl-p nil))))))
                ((or start-position end-position position)
                 (when position
                   (setq start-position position
                         end-position position))
                 (unless start-position
                   (setq start-position (point-min)))
                 (unless end-position
                   (setq end-position (point-max)))
                 (setq res (nconc res (list :content (let ((emacsctl-query-surrounding-lines
                                                            (or surrounding emacsctl-query-surrounding-lines)))
                                                       (emacsctl--query-lines
                                                        :start-position start-position
                                                        :end-position end-position
                                                        :no-truncate-p t
                                                        :hl-p nil))))))
                ((or full-p
                     (<= (buffer-size buffer) emacsctl-small-buffer-limit))
                 (setq res (nconc res (list :content (emacsctl--query-lines :start-pos (point-min)
                                                                            :end-pos (point-max)
                                                                            :no-truncate-p t
                                                                            :hl-p nil)))))
                (t (setq res (nconc res (list :hint "Buffer is too large, returning imenu. Use `:full-p t` for full content if it's really needed."
                                              :imenu (emacsctl--scan-imenu buffer)))))))
      (emacsctl--buffer-not-found-error buffer))))

(defun emacsctl-buffer-list (&optional match)
  (let ((buffers (cl-remove-if
                  (lambda (buf) (string-prefix-p " " (buffer-name buf)))
                  (if match
                      (mapcar #'get-buffer (emacsctl--fuzzy-match-buffer-names match))
                    (buffer-list)))))
    `(,(if match :matched-number :buffer-number) ,(length buffers)
      ,@(when (> (length buffers) emacsctl-too-many-buffers-limit)
          (list :returned-number emacsctl-too-many-buffers-limit
                :hint (format "Too many matches, only showing first %d. Please refine your query."
                              emacsctl-too-many-buffers-limit)))
      :buffers ,(mapcar (lambda (buf)
                          (with-current-buffer buf
                            (let* ((file-name (buffer-file-name buf))
                                   (res (list :name (buffer-name buf)
                                              :size (format "%d (%s)"
                                                            (buffer-size buf)
                                                            (file-size-human-readable (buffer-size buf)))
                                              :file (buffer-file-name buf)
                                              :major-mode (buffer-local-value 'major-mode buf)
                                              :modified (buffer-modified-p buf)
                                              :read-only (buffer-local-value 'buffer-read-only buf))))
                              (if file-name
                                  (setq res (nconc res (list :file file-name))))
                              res)))
                        (take emacsctl-too-many-buffers-limit buffers)))))

(defun emacsctl-hidden-buffer-list (&optional match)
  (let ((buffers (cl-remove-if-not
                  (lambda (buf) (string-prefix-p " " (buffer-name buf)))
                  (if match
                      (mapcar #'get-buffer (emacsctl--fuzzy-match-buffer-names match))
                    (buffer-list)))))
    `(,(if match :matched-number :buffer-number) ,(length buffers)
      ,@(when (> (length buffers) emacsctl-too-many-buffers-limit)
          (list :returned-number emacsctl-too-many-buffers-limit
                :hint (format "Too many matches, only showing first %d. Please refine your query."
                              emacsctl-too-many-buffers-limit)))
      :buffers ,(mapcar (lambda (buf)
                          (with-current-buffer buf
                            (let* ((file-name (buffer-file-name buf))
                                   (res (list :name (buffer-name buf)
                                              :size (format "%d (%s)"
                                                            (buffer-size buf)
                                                            (file-size-human-readable (buffer-size buf)))
                                              :file (buffer-file-name buf)
                                              :major-mode (buffer-local-value 'major-mode buf)
                                              :modified (buffer-modified-p buf)
                                              :read-only (buffer-local-value 'buffer-read-only buf))))
                              (if file-name
                                  (setq res (nconc res (list :file file-name))))
                              res)))
                        (take emacsctl-too-many-buffers-limit buffers)))))

(defun emacsctl-query-window ()
  (setq emacsctl--last-queried-windows
        (cl-loop for i from 1
                 for win in (window-list-1 nil t 'visible)
                 collect (cons i win)))
  (cl-labels ((process-live-win (win)
                (let* ((buf (window-buffer win))
                       (file-name (when buf (buffer-file-name buf))))
                  (let ((res (list :type :live
                                   :index (car (rassq win emacsctl--last-queried-windows))
                                   :top (window-top-line win)
                                   :left (window-left-column win)
                                   :char-width (window-width win)
                                   :char-height (window-height win)
                                   :selected (eq win (selected-window)))))
                    (when buf
                      (setq res (nconc res (list :buffer (buffer-name buf)
                                                 :buffer-major-mode (buffer-local-value 'major-mode buf)
                                                 :buffer-size (format "%d (%s)"
                                                                      (buffer-size buf)
                                                                      (file-size-human-readable (buffer-size buf)))))))
                    (when file-name
                      (setq res (nconc res (list :buffer-file file-name))))
                    res)))
              (process-win (node)
                (if (windowp node)
                    (process-live-win node)
                  (cl-destructuring-bind (dir edges &rest wins) node
                    (list :type :internal
                          :split (if dir :vertical :horizontal)
                          :children (mapcar #'process-win wins))))))
    (cl-destructuring-bind (root mini) (window-tree)
      (list :root (process-win root)
            :minibuffer (list :type :minibuffer
                              :index (car (rassq mini emacsctl--last-queried-windows))
                              :selected (eq mini (selected-window))
                              :buffer (buffer-name (window-buffer mini)))))))

(defun emacsctl-select-window (index)
  (select-window (cdr (assq index emacsctl--last-queried-windows)))
  (list :result :success
        :current-buffer (buffer-name (current-buffer))))

(defun emacsctl-query-kill-ring ()
  (list :kill-ring-size (length kill-ring)
        :kills
        (cl-loop for i from 0
                 for item in kill-ring
                 collect (let ((str (string-replace "\n" "⏎" (substring-no-properties item))))
                           (when (> (length str) emacsctl-print-string-length)
                             (setq str (concat (substring str 0 emacsctl-print-string-length) "...[TRUNCATED]")))
                           (list :index i
                                 :length (length str)
                                 :content str)))))

(cl-defun emacsctl-search-symbol (match)
  (cl-flet ((full-sym-info (sym &optional full-doc)
              `(
                :name ,sym
                :classp ,(cond ((find-class sym nil) :eieio-class)
                               ((cl-find-class sym) :cl-class))
                :fboundp ,(fboundp sym)
                ,@(when (fboundp sym)
                    (cl-list* :special-form-p (special-form-p sym)
                              :macrop (macrop sym)
                              :lambda-list (emacsctl--make-std-pprint-item (emacsctl--function-args sym) #'princ)
                              :commandp (commandp sym)
                              (append
                               (when (commandp sym)
                                 (when-let* ((key (where-is-internal sym nil 'first-only)))
                                   (list :key-binding (key-description key))))
                               (when-let* ((doc (documentation sym)))
                                 (if full-doc
                                     (list :docstring doc)
                                   (list :brief-docstring (car (split-string doc "\n" t))))))))
                :boundp ,(boundp sym)
                ,@(when (boundp sym)
                    (cl-list* :symbol-value (symbol-value sym)
                              (when-let* ((doc (documentation-property sym 'variable-documentation)))
                                (if full-doc
                                    (list :variable-docstring doc)
                                  (list :brief-variable-docstring (car (split-string doc "\n" t)))))))
                ,@(when (facep sym)
                    (cl-list* :face-attributes (face-all-attributes sym)
                              (when-let* ((doc (face-documentation sym)))
                                (if full-doc
                                    (list :face-docstring doc)
                                  (list :brief-face-docstring (car (split-string doc "\n" t))))))))))
    (let* ((matches (emacsctl--roughly-match-symbols match))
           (exact (cl-find match matches :test #'string=))
           (matches (delq exact matches))
           (res `(,@(when exact
                      (list :exactly-matched-symbol (full-sym-info exact t)))
                  :roughly-matched-number ,(length matches)
                  :roughly-matched-symbols
                  ,(cond ((> (length matches) emacsctl-too-many-symbols-limit)
                          matches)
                         ((> (length matches) emacsctl-many-symbols-limit)
                          (mapcar (lambda (sym)
                                    (cl-list* :name sym
                                              :classp (or ,(find-class sym nil) ,(cl-find-class sym))
                                              :boundp (boundp sym)
                                              :fboundp (fboundp sym)
                                              (when (fboundp sym)
                                                (list :macrop (macrop sym)
                                                      :commandp (commandp sym)))))
                                  matches))
                         (t (mapcar #'full-sym-info matches))))))
      (when (null exact)
        (cond ((> (length matches) emacsctl-too-many-symbols-limit)
               (setq res (nconc res (list :hint (format "Too many matches, refine query to <=%d results for brief information, or <=%d results for detailed information if you need."
                                                        emacsctl-too-many-symbols-limit
                                                        emacsctl-many-symbols-limit)))))
              ((> (length matches) emacsctl-many-symbols-limit)
               (setq res (nconc res (list :hint (format "There are many matches, refine query to <=%d results to get detailed information if you need."
                                                        emacsctl-too-many-symbols-limit
                                                        emacsctl-many-symbols-limit)))))))
      res)))

(cl-defun emacsctl-search-command (match)
  (cl-flet ((command-info (sym &optional full-doc)
              `(
                :name ,sym
                ,@(when-let* ((key (where-is-internal sym nil 'first-only)))
                    (list :key-binding (key-description key)))
                ,@(when-let* ((doc (documentation sym)))
                    (if full-doc
                        (list :docstring doc)
                      (list :brief-docstring (car (split-string doc "\n" t))))))))
    
    (let* ((matches (cl-remove-if-not #'commandp (emacsctl--roughly-match-symbols match)))
           (exact (cl-find match matches :test #'string=))
           (matches (delq exact matches))
           (res `(,@(when exact
                      (list :exactly-matched-symbol (command-info exact t)))
                  :roughly-matched-number ,(length matches)
                  :roughly-matched-symbols
                  ,(cond ((> (length matches) emacsctl-too-many-symbols-limit)
                          matches)
                         (t (mapcar #'command-info matches))))))
      (when (and (null exact)
                 (> (length matches) emacsctl-too-many-symbols-limit))
        (setq res (nconc res (list :hint (format "Too many matches, refine query to <=%d results to get detailed information if you need."
                                                 emacsctl-too-many-symbols-limit)))))
      res)))

(cl-defun emacsctl-search-function (match)
  (cl-flet ((func-info (sym &optional full-doc)
              `(
                :name ,sym
                :special-form-p ,(special-form-p sym)
                :macrop ,(macrop sym)
                :lambda-list ,(emacsctl--make-std-pprint-item (emacsctl--function-args sym) #'princ)
                :commandp ,(commandp sym)
                ,@(when (commandp sym)
                    (when-let* ((key (where-is-internal sym nil 'first-only)))
                      (list :key-binding (key-description key))))
                ,@(when-let* ((doc (documentation sym)))
                    (if full-doc
                        (list :docstring doc)
                      (list :brief-docstring (car (split-string doc "\n" t))))))))
    (let* ((matches (cl-remove-if-not #'fboundp (emacsctl--roughly-match-symbols match)))
           (exact (cl-find match matches :test #'string=))
           (matches (delq exact matches))
           (res `(,@(when exact
                      (list :exactly-matched-symbol (func-info exact t)))
                  :roughly-matched-number ,(length matches)
                  :roughly-matched-symbols
                  ,(if (> (length matches) emacsctl-too-many-symbols-limit)
                       matches
                     (mapcar #'func-info matches)))))
      (when (and (null exact)
                 (> (length matches) emacsctl-too-many-symbols-limit))
        (setq res (nconc res (list :hint (format "Too many matches, refine query to <=%d results to get detailed information if you need."
                                                 emacsctl-too-many-symbols-limit)))))
      res)))

(cl-defun emacsctl-search-variable (match)
  (cl-flet ((var-info (sym &optional full-doc)
              (let ((res (list :name sym))
                    (buf (current-buffer)))
                (when (boundp sym)
                  (setq res (nconc res (list :symbol-value (emacsctl--make-std-pprint-item
                                                            (symbol-value sym))))))
                (when (buffer-local-boundp sym buf)
                  (setq res (nconc res (list :buffer-local-value (emacsctl--make-std-pprint-item
                                                                  (buffer-local-value sym buf))))))
                (when-let* ((doc (documentation-property sym 'variable-documentation)))
                  (if full-doc
                      (setq res (nconc res (list :docstring doc)))
                    (setq res (nconc res (list :brief-docstring (car (split-string doc "\n" t)))))))
                res)))
    (let* ((matches (cl-remove-if-not (lambda (sym) (or (boundp sym)
                                                        (buffer-local-boundp sym (current-buffer))))
                                      (emacsctl--roughly-match-symbols match)))
           (exact (cl-find match matches :test #'string=))
           (matches (delq exact matches))
           (res `(,@(when exact
                      (list :exactly-matched-symbol (var-info exact t)))
                  :roughly-matched-number ,(length matches)
                  :roughly-matched-symbols
                  ,(if (> (length matches) emacsctl-too-many-symbols-limit)
                       matches
                     (mapcar #'var-info matches)))))
      (when (and (null exact)
                 (> (length matches) emacsctl-too-many-symbols-limit))
        (setq res (nconc res (list :hint (format "Too many matches, refine query to <=%d results to get detailed information if you need."
                                                 emacsctl-too-many-symbols-limit)))))
      res)))

(cl-defun emacsctl-insert (&key buffer position line column save-p text)
  (let* ((buffer (or buffer (buffer-name (current-buffer))))
         (buffer-exist-p (get-buffer buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (save-excursion
        (when position
          (if (<= (point-min) position (point-max))
              (goto-char position)
            (signal 'args-out-of-range (list pos (point-min) (point-max)))))
        (when line
          (let ((min-line (line-number-at-pos (point-min)))
                (max-line (line-number-at-pos (point-max))))
            (if (<= min-line line max-line)
                (progn (goto-char (point-min))
                       (forward-line (1- line)))
              (signal 'args-out-of-range (list line min-line max-line)))))
        (when column
          (if (<= 0 column (- (pos-eol) (pos-bol)))
              (move-to-column column)
            (signal 'args-out-of-range (list column 0 (- (pos-eol) (pos-bol))))))
        (setq position (point))
        (insert text)
        (unless emacsctl-supervise-mode
          (push-mark nil t))))
    (prog1 (cl-list* :result :success
                     :edit-point position
                     :char-written (length text)
                     (when (not buffer-exist-p)
                       (list :hint (format "Buffer %s is newly created by this call." buffer-name))))
      (when emacsctl-supervise-mode
        (push-mark nil t)
        (switch-to-buffer (get-buffer buffer))
        (goto-char position))
      (when save-p (save-buffer)))))

(cl-defun emacsctl-replace (&key buffer start-line end-line start-position end-position save-p old-text new-text)
  (let* ((buffer (or buffer (buffer-name (current-buffer))))
         (buffer-exist-p (get-buffer buffer))
         killed-length)
    (with-current-buffer (get-buffer-create buffer)
      (save-excursion
        (let ((case-fold-search nil))
          (if old-text
              (progn
                (setq killed-length (length old-text))
                (if (search-forward old-text nil t)
                  (progn (setq start-position (- (point) (length old-text)))
                         (kill-region start-position (point))
                         (insert new-text))
                (search-backward old-text)
                (setq start-position (point))
                (kill-region start-position (+ start-position (length old-text)))
                (insert new-text)))
            (when start-line
              (goto-char (point-min))
              (forward-line (1- start-line))
              (setq start-position (point)))
            (when end-line
              (goto-char (point-min))
              (forward-line (1- end-line))
              (setq end-position (pos-eol)))
            (when (null start-position)
              (setq start-position (point-min)))
            (when (null end-position)
              (setq end-position (1- (point-max))))
            (cl-incf end-position)
            (setq killed-length (- end-position start-position))
            (kill-region start-position end-position)
            (goto-char start-position)
            (insert new-text))
          (unless emacsctl-supervise-mode
            (push-mark nil t)))))
    (prog1 (cl-list* :result :success
                     :edit-point start-position
                     :killed-length killed-length
                     :char-written (length new-text)
                     (when (not buffer-exist-p)
                       (list :hint (format "Buffer %s is newly created by this call." buffer-name))))
      (when emacsctl-supervise-mode
        (push-mark nil t)
        (switch-to-buffer (get-buffer buffer))
        (goto-char start-position))
      (when save-p (save-buffer)))))

(cl-defun emacsctl-grep (pattern &optional buffer)
  "\(fn PATTERN &optional BUFFER)"
  (when (not (stringp pattern))
    (setq pattern (format "%s" pattern)))
  (cond ((and buffer (null (get-buffer buffer)))
         (cl-list* :error (format "Buffer %s not found." buffer)
                   (when-let* ((suggest (emacsctl--fuzzy-match-buffer-names buffer)))
                     (list :suggest-candidates (take emacsctl-max-suggest-candidate-number suggest)))))
        (t (with-current-buffer (if buffer (get-buffer buffer) (current-buffer))
             (save-excursion
               (let* ((case-fold-search nil))
                 (goto-char (point-min))
                 (let ((lst (save-match-data
                              (cl-loop while (re-search-forward pattern nil t)
                                       collect `(
                                                 :start-position ,(match-beginning 0)
                                                 :end-position ,(match-end 0)
                                                 ,@(let ((start-line (line-number-at-pos (match-beginning 0)))
                                                         (end-line (line-number-at-pos (match-end 0))))
                                                     (if (= start-line end-line)
                                                         (list :line start-line)
                                                       (list :start-line start-line
                                                             :end-line end-line)))
                                                 :matched ,(substring-no-properties (match-string 0))
                                                 :surrounding ,(emacsctl--query-lines :start-pos (match-beginning 0)
                                                                                      :end-pos (match-end 0)
                                                                                      :show-point-p nil
                                                                                      :show-mark-p nil
                                                                                      :no-truncate-p t))))))
                   `(
                     :matched-number ,(length lst)
                     ,@(when (> (length lst) emacsctl-too-many-grep-result-limit)
                         (list :returned-number emacsctl-too-many-grep-result-limit
                               :hint (format "Too many matches, only showing first %d. Please refine your query."
                                             emacsctl-too-many-grep-result-limit)))
                     :matches ,(take emacsctl-too-many-grep-result-limit lst)))))))))



(defcustom emacsctl-eval-prohibit-symbols '()
  "List of symbols that are prohibited in eval queries for security."
  :type 'integer
  :group 'emacsctl)

(define-error 'emacsctl-eval-prohibited "Prohibited symbol in evaluation")

(defcustom emacsctl-default-port 33865
  "Default port for emacsctl server to listen on."
  :type 'integer
  :group 'emacsctl)

(defvar emacsctl-server nil
  "The network process of emacsctl server.")

(defvar emacsctl-port nil
  "Port that emacsctl server current listening.")

(defmacro emacsctl--with-condition-case (&rest body)
  `(condition-case e
       (progn ,@body)
     (error (list :error (car e)
                  :error-data (cdr e)))))

(defun emacsctl--read-all-from-string (string)
  "Read all sexps from STRING and return them as a list."
  (let (result)
    (ignore-errors
      (cl-loop for (sexp . end) = (read-from-string string end)
               do (push sexp result)))
    (nreverse result)))

(defun emacsctl--network-process-filter (proc string)
  (let (eval-string
        replace-args replace-old-text replace-new-text
        insert-args insert-text)
    (with-current-buffer (get-buffer-create (concat " " (process-name proc)))
      (goto-char (point-max))
      (insert string)
      (goto-char (point-min))
      (cond ((search-forward "\4" nil t) ; eval
             (delete-char -1)
             (setq eval-string (buffer-substring-no-properties (point-min) (point)))
             (delete-region (point-min) (point)))
            ((search-forward "\32" nil t) ; replace
             (delete-char -1)
             (cl-destructuring-bind (arg txt &optional new-txt)
                 (split-string (buffer-substring-no-properties (point-min) (point)) "\23")
               (setq replace-args (ignore-errors (car (read-from-string arg))))
               (if new-txt
                   (setq replace-old-text txt
                         replace-new-text new-txt)
                 (setq replace-new-text txt)))
             (delete-region (point-min) (point)))
            ((search-forward "\2" nil t) ; insert
             (delete-char -1)
             (cl-destructuring-bind (arg txt)
                 (split-string (buffer-substring-no-properties (point-min) (point)) "\23")
               (setq insert-args (ignore-errors (car (read-from-string arg)))
                     insert-text txt))
             (delete-region (point-min) (point)))))
    (cond (eval-string
           (let* ((save-forms (mapcar (lambda (sym)
                                        (cons `(,sym 'emacsctl-eval-prohibited)
                                              `(,sym (&rest args)
                                                     (signal 'emacsctl-eval-prohibited ',sym))))
                                      emacsctl-eval-prohibit-symbols))
                  (let-form (mapcar #'car save-forms))
                  (flet-form (mapcar #'cdr save-forms)))
             (process-send-string
              proc
              (with-output-to-string
                (emacsctl-pprint
                 (emacsctl--with-condition-case
                  (eval
                   `(let ,let-form
                      (cl-flet ,flet-form
                        ,@(emacsctl--read-all-from-string eval-string)))
                   t)))))
             (message (truncate-string-to-width
                       (string-replace "\n" "⏎" (format "Emacsctl evaluated: %s" eval-string))
                       10 nil nil "..."))))
          (replace-new-text
           (setq replace-args (nconc replace-args
                                     (list :old-text replace-old-text
                                           :new-text replace-new-text)))
           (process-send-string
            proc
            (with-output-to-string
              (emacsctl-pprint
               (emacsctl--with-condition-case
                (apply #'emacsctl-replace replace-args))))))
          (insert-text
           (setq insert-args (nconc insert-args (list :text insert-text)))
           (process-send-string
            proc
            (with-output-to-string
              (emacsctl-pprint
               (emacsctl--with-condition-case
                (apply #'emacsctl-insert insert-args)))))))))

(defun emacsctl--network-process-sentinel (proc event)
  (let ((buf (get-buffer (concat " " (process-name proc)))))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf))))

(cl-defun emacsctl--start (&key (host 'local) (service emacsctl-default-port) (coding 'utf-8))
  (setq emacsctl-server
        (make-network-process :name "emacsctl"
                              :buffer " *emacsctl-server*"
                              :host host
                              :service service
                              :coding coding
                              :server t
                              :filter #'emacsctl--network-process-filter
                              :sentinel #'emacsctl--network-process-sentinel)
        emacsctl-port (process-contact emacsctl-server :service)))

(define-minor-mode emacsctl-mode
  "Minor mode for emacsctl."
  :global t
  :lighter " Ectl"
  (if emacsctl-mode
      (emacsctl--start)
    (when emacsctl-server
      (when (process-live-p emacsctl-server)
        (delete-process emacsctl-server))
      (setq emacsctl-server nil
            emacsctl-port nil))))

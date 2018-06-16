;;; slurm-mode.el --- interaction with the SLURM job scheduling system

;; Copyright (C) 2012 François Févotte

;; This file is NOT part of Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; If you make improvements to this code or have suggestions, please do not hesitate to fork the
;; repository or submit bug reports on github.  The repository is at:
;;
;;     https://github.com/ffevotte/slurm.el

;;; Code:


(require 'dash)
(require 's)


;; * Customizable variables

;;;###autoload
(defgroup slurm nil
  "Interacting with the SLURM jobs scheduling system."
  :group 'external)

;;;###autoload
(defcustom slurm-display-help t
  "If non-nil, slurm-mode should display an help message at the top of the screen."
  :group 'slurm
  :type 'boolean)

;;;###autoload
(defcustom slurm-filter-user-at-start t
  "If non-nil, the jobs list is filtered by user at start."
  :group 'slurm
  :type 'boolean)

;;;###autoload
(defcustom slurm-scancel-confirm t
  "If non-nil, ask for confirmation before cancelling a job."
  :group 'slurm
  :type  'boolean)

(defun slurm--set-squeue-format (var val)
  (set-default var val)
  (when (fboundp 'slurm-update-squeue-format)
    (slurm-update-squeue-format)))

;;;###autoload
(defcustom slurm-squeue-format
  '((jobid      9 right)
    (partition  9 left)
    (name      37 left)
    (user       8 left)
    (st         2 left)
    (time      10 right)
    (nodes      4 right)
    (priority   4 right)
    (nodelist  40 left))
  "List of fields to display in the jobs list.

Each entry in the list should be of the form:
  (FIELD WIDTH ALIGNMENT)
where:
FIELD is a symbol whose name corresponds to the column title in
      the squeue output.
WIDTH is an integer setting the column width.
ALIGN is either `left' or `right'.

`slurm-update-squeue-format' must be called after this variable
is changed to ensure the new value is used wherever necessary."
  :group 'slurm
  :set   'slurm--set-squeue-format
  :type  '(alist
           :key-type   (symbol :tag "Field")
           :value-type (group (integer :tag "Width")
                              (choice  :tag "Alignment"
                                       (const left)
                                       (const right)))))

;; * Utilities

;; ** Process management

(defvar slurm--buffer)
(defmacro slurm--run-command (&rest args)
  "Synchronously run a command.

ARGS is a plist containing the following entries:

:command (required) - the command to run, as a list.

:message (optional) - a message to be displayed.

:post (optional) - form to be executed in the process buffer
  after completion.  The `slurm--buffer' variable is let-bound
  around this block, pointing to the slurm buffer (i.e. the
  buffer from which `slurm--run-command' was called).

:current-buffer (optional) - if non-nil, insert the output of the
  command at the end of the current buffer."
  (let* ((command        (plist-get args :command))
         (post           (plist-get args :post))
         (current-buffer (plist-get args :current-buffer))
         (message        (plist-get args :message))

         (buffer-sym     (cl-gensym "buffer"))
         (command-sym    (cl-gensym "command"))
         (message-sym    (cl-gensym "message")))
    `(progn
       (let* ((slurm--buffer (current-buffer))
              (,command-sym  ,command)
              (,message-sym  ,message)
              (,buffer-sym   (get-buffer-create " *slurm process*")))
         ,@(when message
             `((message "%s..." ,message-sym)))
         (with-current-buffer ,buffer-sym
           (erase-buffer)
           (apply 'shell-command
                  (combine-and-quote-strings ,command-sym)
                  t
                  nil))
         ,@(when message
             `((message "%s...done." ,message-sym)))
         ,@(when post
             `((with-current-buffer ,buffer-sym
                 ,post)))
         ,@(when current-buffer
             `((save-excursion
                 (goto-char (point-max))
                 (insert-buffer-substring ,buffer-sym))))))))


;; ** Internal state management

(defvar slurm--state nil
  "Internal state of slurm.")

;; -- global
;;
;; :initialized      - if non-nil filters and sort will refresh the view
;; :command          - list of commands actually displayed on the view
;; :running-commands - list of commands currently being executed
;; :view             - name of the current view
;; :partitions       - list of available partitions
;; :old-position     - current position just before refreshing a view. Will try
;;                     to go back there after refreshing.

;; -- specific to the jobs list
;;
;; :filter-user      - user to filter in the jobs list ("" for no filter)
;; :filter-partition - partition to filter in the jobs list ("*ALL*" for no filter)
;; :sort             - sorting order ("" for the default sorting order)

;; -- specific to the detailed job view
;;
;; :jobid - current job id

(defun slurm--get (key)
  "Get surm internal variable value associated to KEY."
  (plist-get slurm--state key))

(defun slurm--set (key value)
  "Set the SLURM internal variable associated to KEY.
Assign it the new value VALUE."
  (setq slurm--state
        (plist-put slurm--state key value)))


;; * Slurm mode

;; ** Mode definition

;;;###autoload
(defun slurm ()
  "Open a slurm-mode buffer to manage jobs."
  (interactive)
  (switch-to-buffer (get-buffer-create "*slurm*"))
  (if (eq major-mode 'slurm-mode)
      (slurm-refresh)
    (slurm-mode)))


(defvar slurm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h")   'describe-mode)
    (define-key map (kbd "?")   'describe-mode)
    (define-key map (kbd "j")   'slurm-job-list)
    (define-key map (kbd "p")   'slurm-partition-list)
    (define-key map (kbd "i")   'slurm-cluster-info)
    (define-key map (kbd "g")   'slurm-refresh)
    (define-key map (kbd "RET") 'slurm-details)
    (define-key map (kbd "d")   'slurm-job-cancel)
    (define-key map (kbd "k")   'slurm-job-cancel)
    (define-key map (kbd "u")   'slurm-job-update)
    (define-key map (kbd "e")   'slurm-job-update)
    (define-key map (kbd "U")   'slurm-job-user-details)
    (define-key map (kbd "/ u") 'slurm-filter-user)
    (define-key map (kbd "/ p") 'slurm-filter-partition)
    (define-key map (kbd "s u") 'slurm-sort-user)
    (define-key map (kbd "s p") 'slurm-sort-partition)
    (define-key map (kbd "s P") 'slurm-sort-priority)
    (define-key map (kbd "s j") 'slurm-sort-jobname)
    (define-key map (kbd "s d") 'slurm-sort-default)
    (define-key map (kbd "s c") 'slurm-sort)
    map)
  "Keymap for slurm-mode.")


(eval-when-compile
  (defvar auto-revert-interval))

(defun slurm-mode ()
  "Major-mode for interacting with slurm.

  \\[describe-mode] - Display this help.

Views:
  \\[slurm-job-list] - View jobs list.
  \\[slurm-partition-list] - View partitions list.
  \\[slurm-cluster-info] - View cluster information.
  \\[slurm-refresh] - Refresh current view.

Operations on partitions:
  \\[slurm-details] - Show partition details.

Operations on jobs:
  \\[slurm-details] - Show job details.
  \\[slurm-job-user-details] - Show information about job submitter, as returned by `finger'.
  \\[slurm-job-cancel] - Kill (cancel) job.
  \\[slurm-job-update] - Edit (update) job.

Manipulations of the jobs list:
  \\[slurm-filter-user] - Filter jobs by user name.
  \\[slurm-filter-partition] - Filter jobs by partition.
  \\[slurm-sort-user] - Sort jobs by user name.
  \\[slurm-sort-partition] - Sort jobs by partition.
  \\[slurm-sort-jobname] - Sort jobs by job/step name.
  \\[slurm-sort-default] - Default jobs sorting order.
  \\[slurm-sort] - Customize jobs sorting order."
  (interactive)
  (kill-all-local-variables)
  (use-local-map slurm-mode-map)
  (setq mode-name "Slurm")
  (setq major-mode 'slurm-mode)
  (hl-line-mode 1)
  (toggle-truncate-lines 1)
  (setq buffer-read-only t)

  (set (make-local-variable 'slurm--state) nil)

  ;; Initialize user filter
  (if slurm-filter-user-at-start
      (slurm-filter-user (shell-command-to-string "echo -n $USER"))
    (slurm-filter-user ""))

  ;; Initialize partition filter
  (slurm--update-partitions)
  (slurm-filter-partition "*ALL*")

  ;; Initialize sorting order
  (slurm-sort "")

  ;; Draw display
  (slurm--set :initialized t) ; From now on, filters and sort will refresh the view
  (slurm-job-list)

  ;; Arrange for `revert-buffer' to call `slurm-refresh'
  (set (make-local-variable 'revert-buffer-function)
       (lambda (&optional ignore-auto noconfirm) (slurm-refresh)))
  (set (make-local-variable 'buffer-stale-function)
       #'(lambda (&optional noconfirm) 'fast))
  (set (make-local-variable 'auto-revert-interval) 30)
  (when (fboundp 'auto-revert-set-timer)
    (auto-revert-set-timer)))


;; ** Views

(defun slurm--in-view (view)
  "Non-nil if the current SLURM view is VIEW."
  (eq (slurm--get :view) view))

(defun slurm--run-one-command ()
  "Run the next SLURM command.
Schedule the following command to be executed after termination of the current one."
  (let ((commands (slurm--get :running-commands)))
    (when commands
      (let ((command (car commands)))
        (slurm--set :running-commands (cdr commands))
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (newline 3)
        (let ((pos1 (point)))
          (insert "> " (combine-and-quote-strings command))
          (add-text-properties pos1 (point) '(face ((:weight bold)))))
        (newline 2)
        (sit-for 0)

        (slurm--run-command
         :message (format "Running %s" (car command))
         :current-buffer t
         :command command)
        (progn
           (delete-trailing-whitespace)
           (goto-char (point-min))
           (forward-line (1- (slurm--get :old-position)))
           (setq buffer-read-only t)
           (set-buffer-modified-p nil)
           (slurm--run-one-command))))))

(defun slurm-refresh ()
  "Refresh current slurm view."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (slurm--set :old-position (max (line-number-at-pos) 8))
    (slurm--set :running-commands (slurm--get :command))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format-time-string "%Y-%m-%d %H:%M:%S\n"))
    (when slurm-display-help
      (forward-line)(newline)
      (insert "Type `h' to display an help message"))
    (slurm--run-one-command)))

(defun slurm-details ()
  "Show details on the current slurm entity (job or partition depending on the context)."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (if (slurm--in-view 'slurm-job-list)       (slurm-job-details))
    (if (slurm--in-view 'slurm-partition-list) (slurm-partition-details))))


;; *** Jobs list

(defvar slurm--squeue-format-switch nil
  "Switch passed to the squeue command to set columns format.
Must be updated using `slurm-update-squeue-format' whenever
`slurm-squeue-format' is modified.")

(defun slurm-job-list ()
  "Switch to slurm jobs list view."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (slurm--set :command `(("squeue"
                            "-o" ,slurm--squeue-format-switch
                            ,@(slurm--squeue-filter-user)
                            ,@(slurm--squeue-filter-partition)
                            ,@(slurm--squeue-sort))))
    (setq mode-name "Slurm (jobs list)")
    (slurm--set :view 'slurm-job-list)
    (slurm-refresh)))


;; **** Squeue output parsing

(defconst slurm--squeue-format-fields
  '((jobid     . "i")
    (partition . "P")
    (name      . "j")
    (user      . "u")
    (st        . "t")
    (time      . "M")
    (nodes     . "D")
    (priority  . "Q")
    (nodelist  . "R"))
  "Mapping between squeue fields and the corresponding '%' type specifications.")

(defvar slurm--squeue-format-columns nil
  "Definition of columns in the squeue output.

Must be updated using `slurm-update-squeue-format' whenever
`slurm-squeue-format' is modified.")


(defun slurm--map-squeue-format (fun)
  "Helper function to walk the squeue format.

FUN is called for each field specification in
`slurm-squeue-format'.  It should have the following prototype:

FUN (name width &optional align)"
  (-map (lambda (field)
          (apply fun field))
        slurm-squeue-format))

(defun slurm-update-squeue-format ()
  "Update internal variables when `slurm-squeue-format' is changed.

Updated variables are `slurm--squeue-format-columns' and
`slurm--squeue-format-switch'."
  (setq slurm--squeue-format-switch
        (-reduce
         (lambda (a b) (concat a " " b))
         (slurm--map-squeue-format
          (lambda (name width &optional align)
            (let ((field (cdr (assq name slurm--squeue-format-fields)))
                  (mod   (if (eq align 'right) "." "")))
              (format "%%%s%d%s" mod width field))))))

  (setq slurm--squeue-format-columns
        (let ((pos 0))
          (slurm--map-squeue-format
           (lambda (name width &optional align)
             (prog1
                 (list name pos (+ pos width))
               (setq pos (+ pos width 1))))))))
(slurm-update-squeue-format)

(defun slurm--squeue-get-column (name)
  "Get the value of the NAME column in the current line.

Returned values are trimmed.  NAME must correspond to a field
listed in `slurm-squeue-format'."
  (let* ((column   (assq name slurm--squeue-format-columns))
         (col-beg  (nth 1 column))
         (col-end  (nth 2 column))
         (line-beg (line-beginning-position)))
    (s-trim (buffer-substring-no-properties
             (+ line-beg col-beg)
             (+ line-beg col-end)))))


;; **** Filtering

(defun slurm-filter-user (user)
  "Filter slurm jobs belonging to USER."
  (interactive (list (read-from-minibuffer "User name (blank for all)? " (slurm--get :filter-user))))
  (when (eq major-mode 'slurm-mode)
    (slurm--set :filter-user user)
    (when (slurm--get :initialized) (slurm-job-list))))

(defun slurm--squeue-filter-user ()
  "Return the squeue switch to filter by user."
  (unless (string= (slurm--get :filter-user) "")
    (list "-u" (slurm--get :filter-user))))


(defun slurm-filter-partition (partition)
  "Filter slurm jobs assigned to PARTITION."
  (interactive (list (completing-read "Partition name: " (append (list "*ALL*") (slurm--get :partitions))
                                      nil nil nil nil (slurm--get :filter-partition))))
  (when (eq major-mode 'slurm-mode)
    (slurm--set :filter-partition partition)
    (when (slurm--get :initialized) (slurm-job-list))))

(defun slurm--squeue-filter-partition ()
  "Return the squeue switch to filter by partition."
  (unless (string= (slurm--get :filter-partition) "*ALL*")
    (list "-p" (slurm--get :filter-partition))))


;; **** Sorting

(defun slurm-sort (arg)
  "Set a custom sorting order for slurm jobs.

ARG must be in a form suitable to be passed as a '-S' switch to the squeue command (see `man squeue')."
  (interactive (list (read-from-minibuffer "Sort by (blank for default)? " (slurm--get :sort))))
  (when (eq major-mode 'slurm-mode)
    (slurm--set :sort arg)
    (when (slurm--get :initialized) (slurm-job-list))))

(defun slurm--squeue-sort ()
  "Return the squeue switch to sort."
  (unless (string= (slurm--get :sort) "")
    (list "-S" (slurm--get :sort))))

(defmacro slurm-define-sort (name char)
  "Define a command to change the slurm jobs sorting order.

The command will be named after NAME, and corresponds to giving
the CHAR argument to squeue's '-S' switch."
  `(defun ,(intern (concat "slurm-sort-" name)) (&optional argp)
     ,(concat "Sort slurm jobs by " name ".\n\n"
              "Give a prefix argument to reverse the sorting order.")
     (interactive "P")
     (if argp
         (slurm-sort ,(concat "-" char))
       (slurm-sort ,char))))
(slurm-define-sort "user"      "u")
(slurm-define-sort "partition" "P")
(slurm-define-sort "priority"  "p")
(slurm-define-sort "jobname"   "j")

(defun slurm-sort-default ()
  "Revert to default slurm jobs sorting order."
  (interactive)
  (slurm-sort ""))



;; **** Jobs manipulation

(defun slurm-job-id ()
  "Return the slurm job id in the current context.

In the `slurm-job-list' view, this is the job displayed on the
current line.  In the `slurm-job-details' view, this is the job
currently being displayed."
  (beginning-of-line)
  (cond ((slurm--in-view 'slurm-job-list)
         (let ((jobid (slurm--squeue-get-column 'jobid)))
           (unless (string-match "^[[:digit:]]" jobid)
             (error "Could not find valid job id on this line"))
           jobid))
        ((slurm--in-view 'slurm-job-details)
         (slurm--get :jobid))
        (t
         (error "Bad context for slurm-job-id"))))

(defun slurm-job-user-details ()
  "Display details on the jub submitter, as returned by the shell `finger' utility."
  (interactive)
  (when (slurm--in-view 'slurm-job-list)
    (let ((user (slurm--squeue-get-column 'user)))
      (slurm--run-command
       :message "Retrieving user details"
       :command `("finger" ,user)
       :post    (message "%s" (buffer-string))))))

(defun slurm-job-details ()
  "Show details about the current SLURM job."
  (when (eq major-mode 'slurm-mode)
    (when (slurm--in-view 'slurm-job-list)
      (let ((jobid  (slurm-job-id)))
        (slurm--set :command `(("scontrol" "show" "job" ,jobid)))
        (slurm--set :jobid   jobid))
      (setq mode-name "Slurm (job details)")
      (slurm--set :view 'slurm-job-details)
      (slurm-refresh))))

(defun slurm-job-cancel (argp)
  "Kill (cancel) current slurm job.

When used with a prefix argument (ARGP non-nil) and the current
job belongs to a job array, cancel the whole array."
  (interactive "P")
  (when (eq major-mode 'slurm-mode)
    (let ((jobid (slurm-job-id))
          (type  "job"))

      (when (and argp
                 (string-match "^\\([[:digit:]]+\\)_\\([[:digit:]]+\\)$"
                               jobid))
        (setq type  "array"
              jobid (match-string 1 jobid)))

      (when (y-or-n-p (format "Really cancel %s %s? " type jobid))
        (slurm--run-command
         :message "Cancelling job"
         :command `("scancel" ,jobid))
        (slurm-refresh)))))

(defun slurm-job-update ()
  "Edit (update) current slurm job."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (let ((jobid (slurm-job-id)))
      (switch-to-buffer (get-buffer-create (format "*slurm update job %s*" jobid)))
      (slurm-update-mode)
      (slurm--set :command `(("scontrol" "show" "job" ,jobid)))
      (slurm-update-refresh))))


;; *** Partitions list

(defun slurm-partition-list ()
  "Switch to slurm partitions list view."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (slurm--set :command '(("scontrol" "show" "partition")))
    (setq mode-name "Slurm (partitions list)")
    (slurm--set :view 'slurm-partition-list)
    (slurm-refresh)))



;; **** Partitions manipulation

(defun slurm--update-partitions ()
  "Update the list of SLURM partitions.
This list will be used to provide completion when filtering jobs
by partition."
  (slurm--run-command
   :command '("scontrol" "show" "partitions")
   :post
   (let ((partitions nil))
     (goto-char (point-min))
     (while (search-forward "PartitionName=" nil t)
       (let ((beg (point)))
         (forward-word)
         (add-to-list 'partitions (buffer-substring beg (point)))))
     (with-current-buffer slurm--buffer
       (slurm--set :partitions partitions)))))

(defun slurm-partition-id ()
  "Return the id of the slurm partition at point."
  (backward-paragraph)(forward-line 1)
  (if (search-forward-regexp "^[[:space:]]*PartitionName=\\(.*\\)[[:space:]]*$" (line-end-position))
      (match-string 1)
    (error "Could not extract partition name on this paragraph")))

(defun slurm-partition-details ()
  "Display details about the partition at point."
  (when (slurm--in-view 'slurm-partition-list)
    (slurm-cluster-info (slurm-partition-id))))


;; *** Cluster information

(defun slurm-cluster-info (partition)
  "Show global information on the current state of the cluster.

If PARTITION is set, only show that partition's state.
If PARTITION is nil, show stats for the entire cluster."
  (interactive (list nil))
  (when (eq major-mode 'slurm-mode)
    (let ((switch (if partition `("-p" ,partition))))
      (slurm--set :command `(("sinfo" ,@switch)
                             ("sinfo" "-o" "%C" ,@switch))))
    (setq mode-name "Slurm (cluster info)")
    (slurm--set :view 'slurm-cluster-info)
    (slurm-refresh)))


;; * Slurm-update-mode

(defvar slurm-update-mode-map nil
  "Keymap for slurm-update-mode.")
(if slurm-update-mode-map ()
  (progn
    (setq slurm-update-mode-map text-mode-map)
    (define-key slurm-update-mode-map (kbd "C-c C-c") 'slurm-update-send)
    (define-key slurm-update-mode-map (kbd "C-c C-q") 'slurm-update-quit)))

(defun slurm-update-mode ()
  "Major-mode for updating slurm entities.

Edit the line you want to update and hit \\[slurm-update-send] to validate your changes.

Key bindings:
  \\[slurm-update-send] - Validate your changes on a line.
  \\[slurm-update-refresh] - Refresh view.
  \\[slurm-update-quit] - Quit this mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map slurm-update-mode-map)
  (setq mode-name "Slurm update")
  (setq major-mode 'slurm-update-mode)
  (make-local-variable 'slurm--state)
  (hl-line-mode 1))

(defun slurm-update-refresh ()
  "Refresh slurm-update buffer."
  (interactive)
  (when (eq major-mode 'slurm-update-mode)
    (slurm--set :old-position (point))
    (erase-buffer)
    (slurm--run-command
     :current-buffer t
     :command (car (slurm--get :command)))

    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]+" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward " [[:alnum:]]+=" nil t)
      (goto-char (match-beginning 0))
      (delete-char 1)
      (newline))
    (goto-char (slurm--get :old-position))))

(defun slurm-update-send ()
  "Validate a parameter change in the slurm-update-buffer."
  (interactive)
  (when (eq major-mode 'slurm-update-mode)
    (let* ((id       (save-excursion
                       (goto-char (point-min))
                       (buffer-substring (line-beginning-position) (line-end-position))))
           (prop     (buffer-substring (line-beginning-position) (line-end-position))))
      (slurm--run-command
       :message "Updating job"
       :command `("scontrol" "update" ,id ,prop))
      (slurm-update-refresh))))

(defun slurm-update-quit ()
  "Quit slurm-update mode."
  (interactive)
  (when (eq major-mode 'slurm-update-mode)
    (kill-buffer)
    (switch-to-buffer "*slurm*")
    (slurm-refresh)))

(provide 'slurm-mode)

;;; slurm-mode.el ends here

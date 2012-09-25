;;; slurm-script-mode.el --- Edit SLURM job submission scripts

;; Copyright (C) 2012, Damien François  <damien.francois@uclouvain.be>
;;                     François Févotte <fevotte@gmail.com>

;; Derived from fic-mode.el by Trey Jackson

;; This file is NOT part of Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; If you make improvements to this code or have suggestions, please do not hesitate to fork the
;; repository or submit bug reports on github. The repository is at:
;;
;;     https://github.com/ffevotte/slurm.el

;;; Code:

;;;###autoload
(defcustom slurm-script-directives-face 'slurm-script-directives
  "Face name to use for SBATCH directives in SLURM job submission scripts."
  :group 'slurm
  :type 'face)

;;;###autoload
(defface slurm-script-directives nil
  "Face to use for SBATCH directives in SLURM job submission scripts."
  :group 'slurm)
(copy-face 'font-lock-type-face 'slurm-script-directives)

(defvar slurm-script-keywords
  '("account"         "acctg-freq"        "begin"           "checkpoint"   "checkpoint-dir"
    "comment"         "constraint"        "constraint"      "contiguous"   "cores-per-socket"
    "cpu-bind"        "cpus-per-task"     "dependency"      "distribution" "error"
    "exclude"         "exclusive"         "extra-node-info" "get-user-env" "get-user-env"
    "gid"             "hint"              "immediate"       "input"        "job-id"
    "job-name"        "licences"          "mail-type"       "mail-user"    "mem"
    "mem-bind"        "mem-per-cpu"       "mincores"        "mincpus"      "minsockets"
    "minthreads"      "network"           "nice"            "nice"         "no-kill"
    "no-requeue"      "nodefile"          "nodelist"        "nodes"        "ntasks"
    "ntasks-per-core" "ntasks-per-socket" "ntasks-per-node" "open-mode"    "output"
    "overcommit"      "partition"         "propagate"       "propagate"    "quiet"
    "requeue"         "reservation"       "share"           "signal"       "socket-per-node"
    "tasks-per-node"  "threads-per-core"  "time"            "tmp"          "uid"
    "wckey"           "workdir"           "wrap")
  "List of allowed SBATCH keywords in SLURM submission scripts.")

(defvar slurm-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'slurm-script-insert-directive)
    map)
  "Keymap for `slurm-script-mode'.")

(defvar slurm-script-directives-re
  (concat "^\\s *\\(#SBATCH\\s +--"
          (regexp-opt slurm-script-keywords)
          "\\b.*\\)$")
  "Regular expression matching SBATCH directives in a SLURM job
  submission script.")

(defun slurm-script-insert-directive (keyword)
  "Interactively insert a SLURM directive of the form:

#SBATCH --keyword
"
  (interactive
   (list (completing-read "Keyword: "
                          slurm-script-keywords nil t)))
  (insert (concat "#SBATCH --" keyword " ")))

;;;###autoload
(define-minor-mode slurm-script-mode
  "Edit SLURM job submission scripts.

When slurm-script-mode is on, SBATCH directives are highlighted.
This mode also provides a command to insert new SBATCH directives :
  \\<slurm-script-mode-map>
  \\[slurm-script-insert-directive] - `slurm-script-insert-directive'
"
  :lighter " slurm"
  :group 'slurm
  :keymap slurm-script-mode-map
  (let ((kwlist `((,slurm-script-directives-re 1 slurm-script-directives-face t))))
    (if slurm-script-mode
        (font-lock-add-keywords nil kwlist)
      (font-lock-remove-keywords nil kwlist))))

;;;###autoload
(defun turn-on-slurm-script-mode ()
  "Turn slurm-mode on if SBATCH directives are found in the script."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward slurm-script-directives-re nil t)
      (slurm-script-mode 1))))

(provide 'slurm-script-mode)

;; slurm-script-mode.el ends here
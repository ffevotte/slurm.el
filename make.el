(let ((generated-autoload-file (concat (file-name-directory (buffer-file-name)) "slurm.el")))
    (dolist (x '("./slurm-mode.el" "./slurm-script-mode.el"))
      (update-file-autoloads x 'save-after)))

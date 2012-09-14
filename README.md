# slurm.el

`slurm.el` is an Emacs extension allowing interaction with the
[SLURM](https://computing.llnl.gov/linux/slurm/) job scheduling system.


## Basic usage

Just run `M-x slurm` to see a list of all SLURM jobs on the cluster.

`slurm.el` defines several views to present SLURM information. By default, the following keys can be
used to switch between them :

- `j`: jobs list (this is the default view)
- `p`: partitions list
- `i`: cluster information

In any view, the `g` key refreshes the display.

### Jobs list

This view displays the list of all jobs managed by SLURM on the cluster, as obtained with the
`squeue` command.

A few operations can be done from this view :

- `d` or `RET`: show job details.

  This presents information on the current job, as obtained with the `scontrol show job JOBID`
  command.

- `U`: show user details on the job submitter.

- `k`: kill (cancel) job.

   This command cancels the current job by running the `scancel JOBID` command.

- `e` or `u`: edit (update) job.

   In this mode, you can update job submission parameters (as could be done using the `scontrol
   update job JOBID` command). Each parameter is presented on its own line. After editing a line,
   press
   - `C-c C-c` to validate you change (on the current line only), or
   - `C-c C-q` to cancel your changes and return to the previous view.

- `/`: filter the jobs list by
  - `u`: user;
  - `p`: partition.

- `s`: sort the jobs list by
  - `u`: user;
  - `p`: partition;
  - `d`: use the default sorting order;
  - `c`: customize sorting order by specifying a suitable argument for the `-S` switch of `squeue`.

### Partitions list

This view displays a list of all partitions on the cluster, as obtained with the `scontrol show
partition` command.

While in this view, pressing `d` or `RET` allows you to access various details about the current
partition state, as obtained with the `sinfo` command.

### Cluster information

This view displays details on the current cluster state, as obtained with the `sinfo` command.


## Customization

Some variables can be set to customize `slurm.el`'s behaviour:

- variable `slurm-display-help`: if non-nil, `slurm-mode` displays an help message at the top of the
  screen;

- variable `slurm-filter-user-at-start`: if non-nil, the initial jobs list is filtered to show only jobs
  owned by the current user.


## Contributing

If you make improvements to this code or have suggestions, please do not hesitate to fork the
repository or submit bug reports on [github](https://github.com/ffevotte/slurm.el). The repository's
URL is:

    https://github.com/ffevotte/slurm.el.git

## License

Copyright (C) 2012 François Févotte

This program is free software: you can redistribute it and/or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not,
see <http://www.gnu.org/licenses/>.

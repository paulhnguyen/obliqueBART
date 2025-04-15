This directory contains the data and code used to perform the analysis of the benchmark datasets from Section 4.2 of the paper. The benchmark_datasets directory contains the raw data files (source) and scripts (scripts) used to preprocess the data, respectively. The sub-directory data contains .RData files for each benchmark datasets.

Each folder in bakeoff_studies contains settings files that tabulate every combination of dataset and simulation number. The study____.R scripts take as input a block of rows of settings and the corresponding method/data/split combination.

To run the experiment for rows start_ix to end_ix locally, you should comment out the lines

args <- commandArgs(TRUE)
job_id <- as.numeric(args[1])

and change the range of the for loop from job_id in block_starts[block_id]:block_ends[block_id]) to job_id in start_ix:end_ix

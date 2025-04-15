This directory contains the data and code used to perform the analysis of the benchmark datasets from Section 4.2 of the paper. The benchmark_datasets directory contains the raw data files (source) and scripts (scripts) used to preprocess the data, respectively. The sub-directory data contains .RData files for each benchmark datasets.

Each folder in bakeoff_studies contains settings files that tabulate every combination of dataset and simulation number. The study_classification.R and study_regression.R scripts take as input a block of rows of settings and the corresponding method/data/split combination.

To run the experiments locally, you should comment out the lines

```
args <- commandArgs(TRUE)
job_id <- as.numeric(args[1])
```

You can manually set args from 1:(nrow(settings)-1) to run any method/data/split combination manually.

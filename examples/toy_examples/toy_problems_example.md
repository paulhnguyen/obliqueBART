Toy Problems Example
================
2024-11-04

## Introduction

This document details the procedure to run the oblique BART and
axis-aligned BART rotated axes and sin wave experiments.

## Settings

Below, we load the settings for the toy experiments. The specific files
to do so are located in the study folder for ‘toy_examples’.

``` r
source("study/settings.R")
head(settings)
```

    ##    model      exp rep n_train n_test   m p sigma delta theta
    ## 1 obart7 rot_axes   1    1000   1000 200 0     1     4     0
    ## 2 obart7 rot_axes   2    1000   1000 200 0     1     4     0
    ## 3 obart7 rot_axes   3    1000   1000 200 0     1     4     0
    ## 4 obart7 rot_axes   4    1000   1000 200 0     1     4     0
    ## 5 obart7 rot_axes   5    1000   1000 200 0     1     4     0
    ## 6 obart7 rot_axes   6    1000   1000 200 0     1     4     0

Each row in the settings dataframe represents the parameters of a single
run of the experiments. There are 3 different models: “obart7”, which is
the default implementation of obliqueBART, “awbart”, the standard
axis-aligned BART, and “rwbart”, which is axis-aligned BART with the
rotated features. The dataframe also contains the experiment type, which
is represented by the variable “exp”. The two experiments are described
in section XXX of XXX. “p” denotes the number of random rotations used
in “rwbart”, and it takes values from $\{1,4,16,50,100,200\}$.

The script study.R takes as input a row index (job_id) and runs the
corresponding method on a training-testing split for the corresponding
method.

To run the experiment for a single row job_id locally, you should
comment out the lines

``` r
args <- commandArgs(TRUE)
job_id <- as.numeric(args[1])
```

and instead manually set job_id (around line 8 of the script)

After running the experiment for all rows, you can tabulate the results
by running the script analyze_results.R script.

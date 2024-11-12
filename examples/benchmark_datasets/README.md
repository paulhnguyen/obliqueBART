Repositiory for benchmarking datasets used for comparing BART implementations.
Each `.RData` file contains:
* `Y`: vector of outcomes
* `X_cont`: matrix of continuous covariates
* `X_cat`: matrix of categorical covariates
* `cat_levels_list`: list containing the number of levels in each categorical covariate
* `test_split_list`: list of random test splits containig 25% of the dataset (rounded up)

List of data sources:
* [UC Irvine Machine Learning Repository](https://archive.ics.uci.edu)
* [Journal of Applied Econometrics Data Archive](http://qed.econ.queensu.ca/jae/datasets/)
* [StatLib Dataset Archive](http://lib.stat.cmu.edu/datasets/)

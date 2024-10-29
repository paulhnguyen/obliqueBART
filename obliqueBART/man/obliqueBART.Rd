\name{obliqueBART}
\alias{obliqueBART}

\title{
BART with oblique decision rules
}
\description{
Implements Chipman et al. (2010)'s Bayesian additive regression trees (BART) method for nonparametric regression with continuous outcomes. The regression function is represented as a sum of binary regression trees. obliqueBART supports the use of so-called "oblique" decision rules that are based on linear combination of continuous predictors.
}
\usage{
obliqueBART(Y_train, 
            X_cont_train = matrix(0, nrow = 1, ncol = 1),
            X_cat_train = matrix(0L, nrow = 1, ncol = 1),
            X_cont_test = matrix(0, nrow = 1, ncol = 1),
            X_cat_test = matrix(0L, nrow = 1, ncol = 1),
            unif_cuts = rep(TRUE, times = ncol(X_cont_train)),
            cutpoints_list = NULL, cat_levels_list = NULL, 
            a_cat = 0, b_cat = 0, 
            prob_single_index_split = 1.0,
            dense_phi = TRUE, use_x0 = FALSE,
            cutpoint_option = 1, x0_option = 0,
            sparse = FALSE,
            M = 200,nd = 1000, burn = 1000, thin = 1, 
            save_samples = TRUE,
            verbose = TRUE, print_every = floor( (nd*thin + burn))/10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y_train}{Vector of continuous responses for training data}
  \item{X_cont_train}{Matrix of continuous predictors for training data. Note, predictors must be re-scaled to lie in the interval [-1,1]. Default is a 1x1 matrix, which signals that there are no continuous predictors in the training data.}
  \item{X_cat_train}{Integer matrix of categorical predictors for training data. Note categorical levels should be 0-indexed. That is, if a categorical predictor has 10 levels, the values should run from 0 to 9. Default is a 1x1 matrix, which signals that there are no categorical predictors in the training data.}
  \item{X_cont_test}{Matrix of continuous predictors for testing data. Default is a 1x1 matrix, which signals that testing data is not provided.}
  \item{X_cat_test}{Integer matrix of categorical predictors for testing data. Default is a 1x1 matrix, which signals that testing data is not provided.}
  \item{unif_cuts}{Vector of logical values indicating whether cutpoints for each continuous predictor should be drawn from a continuous uniform distribution (\code{TRUE}) or a discrete set (\code{FALSE}) specified in \code{cutpoints_list}. Default is \code{TRUE} for each variable in \code{X_cont_train}}
  \item{cutpoints_list}{List of length \code{ncol(X_cont_train)} containing a vector of cutpoints for each continuous predictor. By default, this is set to \code{NULL} so that cutpoints are drawn uniformly from a continuous distribution.}
  \item{cat_levels_list}{List of length \code{ncol(X_cat_train)} containing a vector of levels for each categorical predictor. If the j-th categorical predictor contains L levels, \code{cat_levels_list[[j]]} should be the vector \code{0:(L-1)}. Default is \code{NULL}, which corresponds to the case that no categorical predictors are available.}
  \item{a_cat}{When splitting on an unstructured categorical predictor, prior sends individual levels to the left branch with probability drawn from a Beta(a_cat, b_cat) distribution}
  \item{b_cat}{When splitting on an unstructured categorical predictor, prior sends individual levels to the left branch with probability drawn from a Beta(a_cat, b_cat) distribution}
  \item{prob_single_index_split}{Probability of proposing an axis-aligned decision rule. Default is 1.0 (so that no oblique rules are attempted)}
  \item{dense_phi}{Boolean determining whether oblique rules are based on a dense (\code{TRUE}) or sparse (\code{FALSE}) phi vector.}
  \item{use_x0}{Boolean determining whether oblique rules have the form phi'(x-x0) < 0 (\code{TRUE}) or phi'x < c (\code{FALSE}).}
  \item{cutpoint_option}{Integer determining how cutpoint for oblique rules are proposed. See Details.}
  \item{x0_option}{Integer determining how point on decision boundary is chosen (only applicable when \code{use_x0 == TRUE}).}
  \item{sparse}{Logical, indicating whether or not to perform variable selection based on a sparse Dirichlet prior rather than uniform prior; see Linero 2018. Default is \code{FALSE}}
  \item{M}{Number of trees in the ensemble. Default is 200.}
  \item{nd}{Number of posterior draws to return. Default is 1000.}
  \item{burn}{Number of MCMC iterations to be treated as "warmup" or "burn-in". Default is 1000.}
  \item{thin}{Number of post-warmup MCMC iteration by which to thin. Default is 1.}
  \item{save_samples}{Logical, indicating whether to return all posterior samples. Default is \code{TRUE}. If \code{FALSE}, only posterior mean is returned.}
  \item{verbose}{Logical, inciating whether to print progress to R console. Default is \code{TRUE}.}
  \item{print_every}{As the MCMC runs, a message is printed every \code{print_every} iterations. Default is \code{floor( (nd*thin + burn)/10)} so that only 10 messages are printed.}
}
\details{
Oblique rules, yay!
TO DO: describe the different oblique options
}
\value{
A list containing
\item{y_mean}{Mean of the training observations (needed by \code{predict_flexBART})}
\item{y_sd}{Standard deviation of the training observations (needed by \code{predict_flexBART})}
\item{yhat.train.mean}{Vector containing posterior mean of evaluations of regression function on training data.}
\item{yhat.train}{Matrix with \code{nd} rows and \code{length(Y_train)} columns. Each row corresponds to a posterior sample of the regression function and each column corresponds to a training observation. Only returned if \code{save_samples == TRUE}.}
\item{yhat.test.mean}{Vector containing posterior mean of evaluations of regression function on testing data, if testing data is provided.}
\item{yhat.test}{If testing data was supplied, matrix containing posterior samples of the regression function evaluated on the testing data. Structure is similar to that of \code{yhat_train}. Only returned if testing data is passed and \code{save_samples == TRUE}.}
\item{sigma}{Vector containing ALL samples of the residual standard deviation, including burn-in.}
\item{varcounts}{Matrix that counts the number of times a variable was used in a decision rule in each MCMC iteration. Structure is similar to that of \code{yhat_train}, with rows corresponding to MCMC iteration and columns corresponding to predictors, with continuous predictors listed first followed by categorical predictors}
\item{total_accept}{Vector recording the number of trees that are updated in each MCMC iteration}
\item{aa_proposed}{Vector recording the numer of GROW moves with an axis-aligned decision rule proposed in each MCMC iteration}
\item{aa_rejected}{Vector recording the number of GROW moves with an axis-aligned decision rule rejected in each MCMC iteration}
\item{cat_proposed}{Vector recording the numer of GROW moves with a categorical decision rule proposed in each MCMC iteration}
\item{cat_rejected}{Vector recording the number of GROW moves with a categorical decision rule rejected in each MCMC iteration}
\item{obl_proposed}{Vector recording the numer of GROW moves with an oblique decision rule proposed in each MCMC iteration}
\item{obl_rejected}{Vector recording the number of GROW moves with an oblique decision rule rejected in each MCMC iteration}

}

\examples{
## Friedman's function (from the MARS paper)
# Slight modification: flexBART requires all X's to be in [-1,1]
# Friendman's function
friedman_function <- function(X_cont){
  tmp_X <- (X_cont + 1)/2 # rescale from (-1,1) to (0,1)
  return(10 * sin(pi * tmp_X[,1] * tmp_X[,2]) + 
           20 * (tmp_X[,3] - 0.5)^2 + 
           10 * tmp_X[,4] + 5 * tmp_X[,5])
}

sigma <- 0.1
n <- 2000
n_test <- 100
p_cont <- 10
p_cat <- 10
set.seed(99)
X_cont <- matrix(runif(n*p_cont, min = -1, max = 1), nrow = n, ncol = p_cont)

mu <- friedman_function(X_cont)

y <- mu + sigma * rnorm(n, mean = 0, sd = 1)

# Token run that ensures installation was successful
set.seed(99)
flexBART_fit <- obliqueBART(Y_train = y, X_cont_train = X_cont,nd = 5, burn = 5)
\dontrun{
X_cont_test <- matrix(runif(n_test*p_cont, min = -1, max = 1), nrow = n_test, ncol = p_cont)
mu_test <- friedman_function(X_cont_test)

set.seed(99)
fit <- obliqueBART(Y_train = y,
                   X_cont_train = X_cont, 
                   X_cont_test = X_cont_test,
                   prob_single_index_split = 0.5,
                   dense_phi = TRUE, 
                   use_x0 = FALSE,
                   cutpoint_option = 1)

par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), mfrow = c(1,2))
plot(mu, fit$yhat.train.mean, pch = 16, cex = 0.5, 
     xlab = "Truth", ylab = "Estimate", main = "Training")
abline(a = 0, b = 1, col = 'blue')
plot(mu_test, fit$yhat.test.mean, pch = 16, cex = 0.5, 
     xlab = "Truth", ylab = "Estimate", main = "Testing")
abline(a = 0, b = 1, col = 'blue')
}
}
\name{assess_performance}
\alias{assess_performance}

\title{
Assess posterior predictive fit
}
\description{
Computes predictive error (RMSE and standardized MSE) and coveage and interval scores for posterior predictive intervals.
}
\usage{
assess_performance(y, ystar_summary, 
                   baseline_mean,
                   alpha = c(0.5, 0.8, 0.9, 0.95))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Actual observations}
  \item{ystar_summary}{Data frame with posterior predictive summaries returned by \code{post_pred_summary}.}
  \item{baseline_mean}{Predictions against which to standarize mean square error. Typically mean of training observations.}
  \item{alpha}{Vector of probabilities for desired credible intervals. Default is \code{c(0.5, 0.8, 0.9, 0.95).}.}
}
\details{
Computes lots of predictive losses.
}
\value{
List summarizing the RMSE, standardize MSE, interval coverages, and interval scores.
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
set.seed(99)
X_cont <- matrix(runif(n*p_cont, min = -1, max = 1), nrow = n, ncol = p_cont)

mu <- friedman_function(X_cont)

y <- mu + sigma * rnorm(n, mean = 0, sd = 1)

set.seed(99)
nd <- 100
burn <- 100
fit <- obliqueBART(Y_train = y, X_cont_train = X_cont,nd = nd, burn = burn)
tmp <- post_pred_summary(fit$yhat.train, fit$sigma[-(1:burn)])
assess_performance(y, tmp, mean(y))

}
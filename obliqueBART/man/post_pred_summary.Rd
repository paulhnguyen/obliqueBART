\name{post_pred_summary}
\alias{post_pred_summary}

\title{
Summarize posterior predictive distribution
}
\description{
Computes posterior predictive mean and quantiles.
}
\usage{
post_pred_summary(mu_samples, sigma_samples, alpha = c(0.5, 0.8, 0.9, 0.95))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu_samples}{Matrix of posterior draws of regression function evaluations. Rows index the samples and columns index the evaluation points.}
  \item{sigma_samples}{Vector of posterior draws of residual standard deviation. Note \code{nrow(mu_samples)} must be equal to \code{length(sigma_samples)}.}
  \item{alpha}{Vector of probabilities for desired credible intervals. Default is \code{c(0.5, 0.8, 0.9, 0.95).}.}
}
\details{
The 100 * alpha \% credible interval is formed by computing the (100 * (1-alpha)/2) and (100 * (1+alpha)/2) posterior quantiles.
}
\value{
A data frame with columns for the posterior predictive mean and the lower and upper endpoints of the credible intervals.
}

\examples{
\dontrun{
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

}
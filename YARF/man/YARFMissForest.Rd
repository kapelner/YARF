% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YARF_missForest_imputation.R
\name{YARFMissForest}
\alias{YARFMissForest}
\title{MissForest Imputation}
\usage{
YARFMissForest(
  Xtrain,
  ytrain,
  Xtest = NULL,
  maxiter = 10,
  converge_after_increasing = TRUE,
  verbose_missForest = TRUE,
  ...
)
}
\arguments{
\item{Xtrain}{The training data as a data frame.}

\item{ytrain}{The training responses as numeric / integer / factor vector.}

\item{Xtest}{The test data as a data frame (optional, default \code{NULL} for not specified).}

\item{maxiter}{The maximum number of iterations before stopping (default is 10, as in package \code{missForest}).}

\item{converge_after_increasing}{The algorithm quits after imputation errors increase once. Default is \code{TRUE} but if not,
the algorithm continues all \code{maxiter} runs and then returns the final imputations.}

\item{verbose_missForest}{Prints out messages by iteration. Default is \code{TRUE}.}

\item{...}{Settings to pass to YARF during the iterations.}
}
\value{
A list with objects: Xtrain_imp (and Xtest_imp if Xtest was specified) as well as nrmses which
									is a record of the sum of the normalized rmse's over all numeric features by iteration and npfcs
									which is a record of the sum of the misclassification rates over all categorical features by iteration.
}
\description{
This function imputes missing data using the missForest algorithm which essentially iteratively runs the RF
algorithm on each column with missingness until the predictions do not change significantly. We provide a means
to run this imputation on both training data (with responses) and test data (without responses). The user can
also enter algorithm hyperparameters like convergence settings.
}
\details{
After each iteration the difference between the previous and the new imputed data matrix is assessed for the 
continuous and categorical parts. The stopping criterion is defined such that the imputation process is stopped 
as soon as both differences have become larger once. In case of only one type of variable the computation stops 
as soon as the corresponding difference goes up for the first time. However, the imputation last performed where 
both differences went up is generally less accurate than the previous one. Therefore, whenever the computation 
stops due to the stopping criterion (and not due to 'maxiter') the before last imputation matrix is returned.
}
\author{
Adam Kapelner
}

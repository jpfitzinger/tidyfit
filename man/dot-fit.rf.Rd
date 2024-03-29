% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.rf.R
\name{.fit.rf}
\alias{.fit.rf}
\title{Random Forest regression or classification for \code{tidyfit}}
\usage{
\method{.fit}{rf}(self, data = NULL)
}
\arguments{
\item{self}{a 'tidyFit' R6 class.}

\item{data}{a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).}
}
\value{
A fitted 'tidyFit' class model.
}
\description{
Fits a random forest on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
}
\details{
\strong{Hyperparameters:}
\itemize{
\item ntree (\emph{number of trees})
\item mtry (\emph{number of variables randomly sampled at each split})
}

\strong{Important method arguments (passed to \code{\link{m}})}

The function provides a wrapper for \code{randomForest::randomForest}. See \code{?randomForest} for more details.

\strong{Implementation}

The random forest is always fit with \code{importance = TRUE}. The feature importance values are extracted using \code{coef()}.
}
\examples{
# Load data
data <- tidyfit::Factor_Industry_Returns
data <- dplyr::filter(data, Industry == "HiTec")
data <- dplyr::select(data, -Date, -Industry)

# Stand-alone function
fit <- m("rf", Return ~ ., data)
fit

# Within 'regress' function
fit <- regress(data, Return ~ ., m("rf"))
explain(fit)

}
\references{
Liaw, A. and Wiener, M. (2002).
\emph{Classification and Regression by randomForest.}
R News 2(3), 18--22.
}
\seealso{
\code{\link{.fit.svm}}, \code{\link{.fit.boost}} and \code{\link{m}} methods
}
\author{
Johann Pfitzinger
}

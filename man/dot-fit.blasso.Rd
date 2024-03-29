% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.blasso.R
\name{.fit.blasso}
\alias{.fit.blasso}
\title{Bayesian Lasso regression for \code{tidyfit}}
\usage{
\method{.fit}{blasso}(self, data = NULL)
}
\arguments{
\item{self}{a tidyFit R6 class.}

\item{data}{a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).}
}
\value{
A fitted tidyFit class model.
}
\description{
Fits a Bayesian Lasso regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
}
\details{
\strong{Hyperparameters:}

\emph{None. Cross validation not applicable.}

\strong{Important method arguments (passed to \code{\link{m}})}

The function provides a wrapper for \code{monomvn::blasso}. See \code{?blasso} for more details.

\strong{Implementation}

Features are standardized by default with coefficients transformed to the original scale.
}
\examples{
# Load data
data <- tidyfit::Factor_Industry_Returns

# Stand-alone function
fit <- m("blasso", Return ~ ., data, T = 100)
fit

# Within 'regress' function
fit <- regress(data, Return ~ ., m("blasso", T = 100),
               .mask = c("Date", "Industry"))
coef(fit)

}
\references{
Gramacy RB, (qpgen2/quadprog) wFcfCMaubBAT (2023). \emph{monomvn: Estimation for MVN and Student-t Data with Monotone Missingness}. R package version 1.9-17, \url{https://CRAN.R-project.org/package=monomvn}.
}
\seealso{
\code{\link{.fit.lasso}}, \code{\link{.fit.bridge}} and \code{\link{m}} methods
}
\author{
Johann Pfitzinger
}

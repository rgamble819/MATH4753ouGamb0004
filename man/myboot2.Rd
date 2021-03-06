% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Creates a bootstrapped monte-carlo simulation from a sample when correct assumption applied}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{# of experiments to run}

\item{fun}{# the function to analyze from distribution}

\item{alpha}{# adjust the confidence level}

\item{n}{# size of samples}
}
\description{
Creates a bootstrapped monte-carlo simulation from a sample when correct assumption applied
}
\examples{
myboot(iter=10000, x=sam,alpha=0.05,fun="mean",xlab="mean")

}

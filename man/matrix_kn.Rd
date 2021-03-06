% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_kn.R
\name{matrix_kn}
\alias{matrix_kn}
\title{A slightly modified version of the knockoff function}
\usage{
matrix_kn(resp, expl, Xko, offset = 0, fdr = 0.2, cores = 2, ...)
}
\arguments{
\item{resp}{The response variable, a matrix the same length as expl The first col must be the identifier}

\item{expl}{The data frame to be the explanatory variables, the first col must be the identifier}

\item{Xko}{The knockoff variables created seperately (see the 'knockoff' package for more details). Also must be the same length as expl, resp.}

\item{offset}{The offset is used to control conservative-ness. 0 for liberal and best for smaller data sets, 1 for conservative and larger data sets. 1 can be a very poor choice if you do not have many, many explanatory variables. Passed to the my_kn function.}

\item{fdr}{The "False Discovery Rate", allowed to be between 0,1 (non-inclusive). Defaults to .2, implying that 1/5 of variables returned will be a type 1 error. This will be passed to the my_kn function.}

\item{cores}{The cores to be used in the doMC section of the code, passed to the my_kn function.}

\item{...}{Various other arguments to be passed to the my_kn function.}
}
\value{
This will return of data frame 5 columns.
\itemize{
\item{Resp: } What ever response variable was being tested
\item{Expl: } Gives the name of the signficant explanatory variable chosen
\item{Est: } Estimate of the glm model, defaults to normal
\item{se: } Gives the standard error of the parameter estimate from the glm
\item{p-value: } Gives the p-value associated with that variable estimate from the glm for the null hypothesis the effect magnitude is 0.
}
}
\description{
2 major differences being that the knockoff variables
are to be passed directly to the function, not made inside
of it; and secondly you can pass the amount of cores wanted
directly to the statistic function chosen. Be careful as this
may need to be 'NULL'-ed if the statistic function you choose
does not allow for multi-cores.
}
\note{
Cores will be rounded to be a natural number
}

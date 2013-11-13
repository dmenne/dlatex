\name{xtable.decimal}
\alias{xtable.decimal}
\title{Prints a LaTeX table with numeric columns aligned on their decimal points.}
\usage{
  xtable.decimal(x,
    cols = which(lapply(x, class) == "numeric"),
    colAlignment, tocharFun = prettyNum, ...)
}
\arguments{
  \item{x}{a data frame to create a LaTeX table from.}

  \item{cols}{a numeric vector indicating which columns
  should be aligned on decimal points. It defaults to all
  columns of type numeric.}

  \item{colAlignment}{named character vector where each
  element name corresponds to a}

  \item{tocharFun}{the function used to convert the numeric
  vector to a character vector. This defaults to
  \code{\link{prettyNum}}, but other possible options are
  \code{\link{as.character}}, \code{\link{format}},
  \code{\link{formatC}}, or some other custom function.}

  \item{...}{other parameters passed to \code{tocharFun},
  \code{\link[xtable]{xtable}}, and
  \code{\link[xtable]{print.xtable}}.}
}
\description{
  This function wraps the \code{\link[xtable]{xtable}} and
  \code{\link[xtable]{print.xtable}} functions in the
  \code{xtable} package so that numeric columns are aligned
  on their decimal place.
}
\details{
  See
  \url{http://jason.bryer.org/posts/2013-01-04/xtable_with_aligned_decimals.html}
  for more information.
}
\author{
  Jason Bryer <jason@bryer.org>
}
\seealso{
  \code{\link[xtable]{xtable}}
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex.ftable.r
\name{latex.ftable}
\alias{latex.ftable}
\title{Print LaTeX contrast table for ftable}
\usage{
\method{latex}{ftable}(object, title = "", caption, label, pos = "!htbp",
  mincapwidth, ...)
}
\arguments{
\item{object}{an ftable object}

\item{title}{label of table}

\item{caption}{caption used for table}

\item{label}{label used for table}

\item{pos}{positioning parameter for LaTeX}

\item{mincapwidth}{for ctable, the minimum caption width in mm.}

\item{\dots}{additional parameters; none used currentyle, kept in analogy to
Hmisc}
}
\value{
No result
}
\description{
Prints an ftable result in latex; does not use Hmisc latex, but rather a
home-brew hack. Hmisc should be included for method dispatching.
}
\examples{
library(Hmisc)
latex(ftable(Survived ~ ., data = Titanic), mincapwidth=100, label="tab:tit",
  caption="Titanic survivors")

}
\author{
Dieter Menne, \email{dieter.menne@menne-biomed.de}
}
\keyword{print}

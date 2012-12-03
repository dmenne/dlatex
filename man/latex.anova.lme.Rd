\name{latex.anova.lme}
\alias{latex.anova.lme}
\title{LaTeX ANOVA table for lme}
\usage{
  \method{latex}{anova.lme} (object, title = "", parameter,
    file = "", shadep = 0.05, caption = NULL,
    ctable = FALSE, where = "!htbp", ...)
}
\arguments{
  \item{object}{a fitted model object inheriting from class
  \code{anova.lme}, representing ANOVA of a mixed-effects
  model.}

  \item{title}{to be printed in upper left corner}

  \item{parameter}{label of the estimated parameter
  (required), used for caption and label of the table. Can
  have spaces. See also \code{\link{lmeLabel}}}

  \item{file}{output file name; default prints output to
  the standard output, which is the choice for Sweave.}

  \item{shadep}{table cells for p-values lower than this
  will be shaded; p-values for \code{(Intercept)} are never
  shaded.}

  \item{caption}{caption used for table; default caption
  is: \emph{Contrast table for <parameter> (model
  <formula>).  Value in row (Intercept) gives base value
  for <reference values of variables>}.}

  \item{ctable}{Uses \code{ctable}-formatting of LaTeX.}

  \item{where}{positioning parameter for LaTeX}

  \item{\dots}{Additional parameters passed to \code{latex}
  in \code{Hmisc}.}
}
\value{
  Returns the result of the call to \code{latex} in
  \code{Hmisc}.
}
\description{
  Generates LaTeX code for an ANOVA table from \code{lme}.
  Number of significant digits depends on standard
  deviation of the results. Significant p-values are
  shaded.  The \code{(Intercept)} term is not printed.
}
\note{
  The function is modelled after \code{print.anova.lme}
  from package \code{nlme}. Requires \code{ctable} and
  \code{colortable} in your Snw/tex file.
}
\examples{
# Pinheiro/Bates page 47
library(nlme)
library(Hmisc)
fm1Oats <- lme(yield~ordered(nitro)*Variety, data=Oats,
  random = ~1|Block/Variety)
latex(anova(fm1Oats),parameter="Yield",file ="")
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}
\seealso{
  \code{\link[Hmisc]{latex}}, \code{\link{latex.lme}},
  \code{\link[nlme]{lme}}
}
\keyword{models}
\keyword{print}


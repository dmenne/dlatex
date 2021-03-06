% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex.lme.R
\name{latex.lme}
\alias{latex.lme}
\alias{latex.summary.lme}
\alias{latex.lm}
\alias{latex.glm}
\title{Print LaTeX contrast table for lme, lm and glm models}
\usage{
\method{latex}{lme}(object, title = "", parameter = NULL, file = "",
  shadep = 0.05, caption = NULL, label = NULL, ctable = FALSE,
  form = NULL, interceptp = FALSE, moredec = 0, where = "!htbp", ...)
}
\arguments{
\item{object}{result of a fit by \code{lme}, \code{glm}, \code{lm}, or of
\code{summary.lme}. Note that there is are no variants for \code{summary.lm}
and \code{summary.glm}.}

\item{title}{printed in top left corner}

\item{parameter}{label of the estimated parameter, used for caption and
label of the table. Can have spaces. Defaults to target variable in formula.
See also \code{\link{lmeLabel}}}

\item{file}{output file name; default prints output to the standard output,
which is the method of choice for Sweave.}

\item{shadep}{table cells for p-values lower than this will be shaded;
p-values for \code{(Intercept)} are never shaded.}

\item{caption}{caption used for table; default caption is \emph{ANOVA for
<parameter>}}

\item{label}{label used for table; default label is composed from model
formula by lmeLabel}

\item{ctable}{uses \code{ctable}-formatting of LaTeX by default.}

\item{form}{Optional formula for caption display. By default, object model
formula is used}

\item{interceptp}{If TRUE, show p-value and t-value of \code{intercept}.
Default is FALSE, because in most cases this value should not be
interpreted.}

\item{moredec}{Show more decimals than by default. Fractional values, e.g.
0.5 can be used here such that boundary case are rounded up instead of down}

\item{where}{positioning parameter for LaTeX}

\item{\dots}{Additional parameters passed to \code{latex} in Hmisc.}
}
\value{
Returns the result of the call to \code{latex} in \code{Hmisc}.
}
\description{
Generates a LaTex contrast table for the \code{lme}, \code{glm} and
\code{lm} models, with number of significant digits shown depending on
standard deviation of the contrasts.  Signficant p-values can be shaded.
}
\note{
Requires \code{ctable}, \code{colortable} in your Snw/tex file.
}
\examples{
# Pinheiro/Bates page 47
library(nlme)
library(Hmisc)
fm1Oats <- lme(yield~ordered(nitro)*Variety, data=Oats,
  random = ~1|Block/Variety)
# Both versions give same result, output to console
latex(fm1Oats,"Yield")
latex(summary(fm1Oats),"Yield")
# The following model is nonsense, but it shows how default latex labels
# and captions are constructed in complex cases
fm2Oats <- lme(yield~I(sqrt(nitro))*Variety + I(nitro^2), data=Oats,
  random = ~1|Block/Variety)
latex(fm2Oats,"Yield with dumb model")

# For lm
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
sex <- as.factor(rep(c("f","m"),10))
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
x <- lm(weight ~ group*sex)
latex(x,parameter="Weight",moredec=0.5)

# For glm
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
glm.D <- glm(counts ~ outcome + treatment, family=poisson())
latex(glm.D,parameter="severe")


}
\seealso{
\code{\link[Hmisc]{latex}}, \code{\link{latex.lme}},
\code{\link[nlme]{lme}}
}
\author{
Dieter Menne, \email{dieter.menne@menne-biomed.de}
}
\keyword{models}
\keyword{print}

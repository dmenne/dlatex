#' @title Retrieve items from an mixed-model, a glm, or from a lm summary table.
#' 
#' @description Use these functions in Sweave to retrieve individual factor contrast from a
#' \code{summary.lme}, \code{summary.glm} or \code{summary.lm} object.
#' @name lme.value
#' @aliases lme.value lme.stderr lme.DF lme.p lme.ps 
#' @aliases lm.value lm.stderr lm.t lm.p lm.ps 
#' @aliases glm.value glm.stderr glm.z glm.p glm.ps 
#' @aliases latexSNdouble
#' @param x an object of class \code{summary.lme}.
#' @param factor name of factor or interaction in contrast table. Shorthand for \code{(Intercept)}
#' @param signif number of significant digits, default = 2.
#' @param val a floating point value
#' @return Returns a scalar rounded for use with Sweave and \code{\\Sexpr}. 
#' Functions \code{lme.ps, lm.ps} and \code{glm.ps}  return a string in latex math mode of
#' the form $p=0.34$.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link{latex.lme}}, \code{\link{summary.lme}}
#' @examples
#' 
#' # Pinheiro/Bates page 47
#' library(nlme)
#' fm1Oats.sum <- summary(lme(yield~ordered(nitro)*Variety, data=Oats,
#'   random = ~1|Block/Variety))
#' lme.value(fm1Oats.sum,"VarietyMarvellous", signif = 3)
#' lme.stderr(fm1Oats.sum,"VarietyMarvellous", signif = 4)
#' lme.p(fm1Oats.sum,"VarietyMarvellous")
#' 
#' # For lm
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' sex <- as.factor(rep(c("f","m"),10))
#' group <- gl(2,10,20, labels=c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' x <- summary(lm(weight ~ group*sex))
#' lm.value(x,"sexm", signif = 1)
#' lm.stderr(x,"sexm")
#' lm.p(x,"sexm", signif = 4)
#' lm.ps(x,"sexm", signif = 4)
#' 
#' latexSNdouble(1.342E-12)
#' @export 
#' @rdname lme.value
"lme.value" <- function(x,factor,signif=2)
  .lme.getValue(x,factor,1,signif)
#' @export 
#' @rdname lme.value
"lme.stderr" <- function(x,factor,signif=2)
 .lme.getValue(x,factor,2,signif)
#' @export 
#' @rdname lme.value
"lme.DF" <- function(x,factor)
  .lme.getValue(x,factor,3,0)
#' @export 
#' @rdname lme.value
"lme.p" <- function(x,factor,signif=2) {
  if (inherits(x,"summary.lme"))
    latexSNdouble(.lme.getValue(x,factor,5,signif)) else
  if (inherits(x,"anova.lme") || inherits(x,"summary.gls"))
    latexSNdouble(.lme.getValue(x,factor,4,signif)) else
  stop("Needs summary.(lme,gls) for lme.value",.call=NULL)
}
#' @export 
#' @rdname lme.value
"lme.ps" <- function(x,factor,signif=2) {
  paste("$p=",lme.p(x,factor,signif),"$",sep="")
}

#' @export
#' @rdname lme.value
"lm.value" <- function(x,factor,signif=2)
 .lm.getValue(x,factor,1,signif)
#' @export
#' @rdname lme.value
"lm.stderr" <- function(x,factor,signif=2)
 .lm.getValue(x,factor,2,signif)
#' @export
#' @rdname lme.value
"lm.t" <- function(x,factor,signif=2)
 .lm.getValue(x,factor,3,signif)
#' @export
#' @rdname lme.value
"lm.p" <- function(x,factor,signif=2)
 latexSNdouble(.lm.getValue(x,factor,4,signif))
#' @export
#' @rdname lme.value
"lm.ps" <- function(x,factor,signif=2) {
  paste("$p=",lm.p(x,factor,signif),"$",sep="")
}


#' @export
#' @rdname lme.value
"glm.value" <- function(x,factor,signif=2)
 .glm.getValue(x,factor,1,signif)
#' @export
#' @rdname lme.value
"glm.stderr" <- function(x,factor,signif=2)
 .glm.getValue(x,factor,2,signif)
#' @export
#' @rdname lme.value
"glm.z" <- function(x,factor,signif=2)
 .glm.getValue(x,factor,3,signif)
#' @export
#' @rdname lme.value
"glm.p" <- function(x,factor,signif=2)
 latexSNdouble(.glm.getValue(x,factor,4,signif))
#' @export
#' @rdname lme.value
"glm.ps" <- function(x,factor,signif=2) {
  paste("$p=",glm.p(x,factor,signif),"$",sep="")
}

#' @export
#' @rdname lme.value
"latexSNdouble" <- function (val)
{
  # special copy of Hmisc latexSN with \\\\
  x <- format(val)
  # Skip double if knitr running
  if (exists("knit")) return (latexSN(x))
  x <- sedit(val, c("e+00", "e-0*", "e-*", "e+0*", "e+*"), 
             c("",
             "\\\\!\\\\times\\\\!10^{-*}", 
             "\\\\!\\\\times\\\\!10^{-*}", 
             "\\\\!\\\\times\\\\!10^{*}", 
             "\\\\!\\\\times\\\\!10^{*}"))
  x
}

# local functions
".lme.getValue" <-
  function(x,factor,what,signif) {
    if (inherits(x,"summary.lme") ||inherits(x,"anova.lme") ||
          inherits(x,"summary.gls")) {
      if (factor == "I")  # Shorthand for (Intercept)
        factor="(Intercept)" 
      ind = match(factor, names(x$tTable[,what]))
      if (!is.na(ind)) {
        signif(x$tTable[ind,what],signif)
      } else {
        cat("lme.getValues: factor ",factor," not in \n",
            names(x$tTable[,what]),"\n")
        "???"
      }
    } else
      stop("Needs summary(lme,gls) for lme.value",.call=NULL)
  }

".lm.getValue" <-
  function(x,factor,what,signif=2) {
    if (inherits(x,"summary.lm") ) {
      if (factor == "I")  # Shorthand for (Intercept)
        factor="(Intercept)" 
      xt =coefficients(x)
      ind = match(factor, names(xt[,what]))
      if (!is.na(ind)) {
        signif(xt[ind,what],signif)
      } else {
        cat("lm.getValues: factor ",factor," not in \n",
            names(xt[,what]),"\n")
        "???"
      }
    } else
      stop("Needs summary.lm for lm.value",.call=NULL)
  }

".glm.getValue" <-
  function(x,factor,what,signif=2) {
    if (inherits(x,"summary.glm") ) {
      xt =coefficients(x)
      if (factor == "I")  # Shorthand for (Intercept)
        factor="(Intercept)" 
      ind = match(factor, names(xt[,what]))
      if (!is.na(ind)) {
        signif(xt[ind,what],signif)
      } else {
        cat("glm.getValues: factor ",factor," not in \n",
            names(xt[,what]),"\n")
        "???"
      }
    } else
      stop("Needs summary.glm for glm.value",.call=NULL)
  }

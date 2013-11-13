#' @title LaTeX ANOVA table for lme
#' 
#' @description Generates LaTeX code for an ANOVA table from \code{lme}.  Number of
#' significant digits depends on standard deviation of the results. Significant
#' p-values are shaded.  The \code{(Intercept)} term is not printed.
#' 
#' @method latex anova.lme
#' @param object a fitted model object inheriting from class \code{anova.lme},
#' representing ANOVA of a mixed-effects model.
#' @param title to be printed in upper left corner
#' @param parameter label of the estimated parameter (required), used for
#' caption and label of the table. Can have spaces. See also
#' \code{\link{lmeLabel}}
#' @param file output file name; default prints output to the standard output,
#' which is the choice for Sweave.
#' @param shadep table cells for p-values lower than this will be shaded;
#' p-values for \code{(Intercept)} are never shaded.
#' @param caption caption used for table; default caption is: \emph{Contrast
#' table for <parameter> (model <formula>).  Value in row (Intercept) gives
#' base value for <reference values of variables>}.
#' @param ctable Uses \code{ctable}-formatting of LaTeX.
#' @param where positioning parameter for LaTeX
#' @param \dots Additional parameters passed to \code{latex} in \code{Hmisc}.
#' @return Returns the result of the call to \code{latex} in \code{Hmisc}.
#' @note The function is modelled after \code{print.anova.lme} from package
#' \code{nlme}. Requires \code{ctable} and
#' \code{colortable} in your Snw/tex file.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link[Hmisc]{latex}}, \code{\link{latex.lme}},
#' \code{\link[nlme]{lme}}
#' @keywords print models
#' @import Hmisc
#' @examples
#' 
#' # Pinheiro/Bates page 47
#' library(nlme)
#' library(Hmisc)
#' fm1Oats <- lme(yield~ordered(nitro)*Variety, data=Oats,
#'   random = ~1|Block/Variety)
#' latex(anova(fm1Oats),parameter="Yield",file ="")
#' 
#' @export
"latex.anova.lme" <-
function (object, title="", parameter,file="",
   shadep=0.05, caption=NULL, ctable=FALSE, where="!htbp",...)
{
  options(Hverbose=FALSE)
  if ((rt <- attr(object, "rt")) == 1) {
    # don't show intercept
    object <- object[-1,]
    # background shading; use shadep = 0 for no shading
    sigp <- object[,"p-value"]< shadep
    pval <- format(signif(object[, "p-value"], 2))
    pval[as.double(pval) == 0] <- "$<$.0001"
    object[, "p-value"] <- pval
    object[,"F-value"] <- round(object[,"F-value"],1)
    cellTex <- matrix(rep("", NROW(object) * NCOL(object)), nrow=NROW(object))
    cellTex[sigp,4] <- "cellcolor[gray]{0.9}"
    if (is.null(caption))
      caption = paste("ANOVA for ",parameter,'.',sep="")
    label <- lmeLabel("anova",parameter)
    rowlabel <- ifelse(nchar(parameter) >9,"",parameter)
    names(object) <- c("numDF","denDF","F","p")
    latex(as.data.frame(object),title=title,rowlabel=rowlabel,
      cellTexCmds = cellTex,file=file,where=where,
      label=label, caption=caption,caption.loc="bottom",
      booktabs=!ctable,
      numeric.dollar=FALSE,col.just=rep("r",5),ctable=ctable,...)
  }
  else {
    stop("latex output of anova.lme with multiple arguments not handled")
  }
}


#' Print LaTeX contrast table for lme, lm and glm models
#' 
#' Generates a LaTex contrast table for the \code{lme}, \code{glm} and
#' \code{lm} models, with number of significant digits shown depending on
#' standard deviation of the contrasts.  Signficant p-values can be shaded.
#' 
#' @method latex lme
#' @method latex summary.lme
#' @method latex lm
#' @method latex glm
#' @aliases latex.lme 
#' @aliases latex.summary.lme 
#' @aliases latex.lm 
#' @aliases latex.glm
#' @param object result of a fit by \code{lme}, \code{glm}, \code{lm}, or of
#' \code{summary.lme}. Note that there is are no variants for \code{summary.lm}
#' and \code{summary.glm}.
#' @param title printed in top left corner
#' @param parameter label of the estimated parameter, used for caption and
#' label of the table. Can have spaces. Defaults to target variable in formula.
#' See also \code{\link{lmeLabel}}
#' @param file output file name; default prints output to the standard output,
#' which is the method of choice for Sweave.
#' @param shadep table cells for p-values lower than this will be shaded;
#' p-values for \code{(Intercept)} are never shaded.
#' @param caption caption used for table; default caption is \emph{ANOVA for
#' <parameter>}
#' @param label label used for table; default label is composed from model
#' formula by lmeLabel
#' @param ctable uses \code{ctable}-formatting of LaTeX by default.
#' @param form Optional formula for caption display. By default, object model
#' formula is used
#' @param interceptp If TRUE, show p-value and t-value of \code{intercept}.
#' Default is FALSE, because in most cases this value should not be
#' interpreted.
#' @param moredec Show more decimals than by default. Fractional values, e.g.
#' 0.5 can be used here such that boundary case are rounded up instead of down
#' @param where positioning parameter for LaTeX
#' @param \dots Additional parameters passed to \code{latex} in Hmisc.
#' @return Returns the result of the call to \code{latex} in \code{Hmisc}.
#' @export latex.lme 
#' @export latex.summary.lme 
#' @export latex.lm 
#' @export latex.glm
#' @note Requires \code{ctable}, \code{colortable} in your Snw/tex file.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link[Hmisc]{latex}}, \code{\link{latex.lme}},
#' \code{\link[nlme]{lme}}
#' @keywords print models
#' @examples
#' # Pinheiro/Bates page 47
#' library(nlme)
#' library(Hmisc)
#' fm1Oats <- lme(yield~ordered(nitro)*Variety, data=Oats,
#'   random = ~1|Block/Variety)
#' # Both versions give same result, output to console
#' latex(fm1Oats,"Yield")
#' latex(summary(fm1Oats),"Yield")
#' # The following model is nonsense, but it shows how default latex labels 
#' # and captions are constructed in complex cases
#' fm2Oats <- lme(yield~I(sqrt(nitro))*Variety + I(nitro^2), data=Oats,
#'   random = ~1|Block/Variety)
#' latex(fm2Oats,"Yield with dumb model")
#' 
#' # The following output produces a tex file
#' \dontrun{
#'   w <- latex(fm1Oats,parameter="Yield",file="a.tex")
#'   # nice (undocumented) way to add uspackage(colortbl, ctable)
#'   w$style <- c("colortbl","ctable")
#'   print(dvi(w))
#' }
#' 
#' # For lm
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' sex <- as.factor(rep(c("f","m"),10))
#' group <- gl(2,10,20, labels=c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' x <- lm(weight ~ group*sex)
#' latex(x,parameter="Weight",moredec=0.5)
#' 
#' # For glm
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#' glm.D <- glm(counts ~ outcome + treatment, family=poisson())
#' latex(glm.D,parameter="severe")
#' 
#' 
"latex.lme" <-
function(object, title="",parameter=NULL,file="",shadep=0.05,
  caption=NULL,label=NULL,ctable=FALSE,form=NULL,
  interceptp=FALSE,  moredec= 0, where="!htbp",...) {
  options(Hverbose=FALSE)
  latex.summary.lme(summary(object),title=title,parameter=parameter, 
    file=file, shadep=shadep, caption=caption,
    label=label, ctable=ctable, form=form, moredec=moredec, where=where,...)
}

"latex.lm" <-
  function(object,title="",parameter,file="",
           shadep=0.05,caption=NULL,label=NULL,ctable=FALSE,form=NULL,
           interceptp = FALSE, moredec =0, where="!htbp", ...) {
    
    options(Hverbose=FALSE)
    fixF <- object$call
    xt = summary(object)
    xtTab <- as.data.frame(coefficients(xt))
    sigp <- xtTab[,4]< shadep # cells that will be shaded
    if (!interceptp){
      sigp[1] <- FALSE # intercept will only be shaded on explicit request
      # Replace small significances, discarding p-value for (Intercept)
      xtTab[1,4] = 1 # we do not show it anyway, easier formatting
    }
    pval <- format(zapsmall(xtTab[, 4],4))
    pval[as.double(pval) < 0.0001] <- "\\textless .0001"
    xtTab[, 4] <- pval
    xtTab[,"t value"] <- round(xtTab[,"t value"],1)
    if (any(wchLv <- (as.double(levels(xtTab[, 4])) == 0))) {
      levels(xtTab[, "4"])[wchLv] <- "\\textless .0001"
    }
    # extract formula
    if (is.null(form)) {
      form <- deparse(formula(object$terms))
    }
    if (is.null(label))
      label <- lmeLabel("contr",form)
    # remove I(..). Because finding matched parens is tricky, we leave the ()
    form <- gsub("I\\(","(",form)
    form <- paste(sub('~','$\\\\sim$ ',form),sep="")
    # Determine base level. TODO: check for numeric covariables
    lev <- list()
    for (i in  seq(along=object$xlevels)) {
      levs <- object$xlevels[i]
      lev[i] <- paste(names(levs),levs[[1]][1],sep=" = ")
    }
    levnames <- paste(lev,collapse=", ")
    
    if (is.null(caption)) # TODO: Allow %s substition
      caption <- paste("Linear model (lm) contrast table for \\emph{",
                       parameter, "} (model ",form,
                       "). The value in row (Intercept) gives the reference value for ",
                       levnames,".", " The standard deviation of the residuals is ",
                       signif(xt$sigma,3), " at ", xt$df[2]," degrees of freedom.",
                       sep='')
    caption.lot <- paste("Contrast table for ",parameter, " by ",
                         levnames)
    ndec <- pmax(round(1-log10(xtTab[,2]+0.000001)+moredec),0)
    xtTab[,1] <- formatC(round(xtTab[,1],ndec))
    xtTab[,2] <- formatC(round(xtTab[,2],ndec))
    names(xtTab) <- c("Value","StdErr","t","p")
    # Do not show Intercept p-values and t-value if not explicitely requested
    if (!interceptp) {
      xtTab[1,3] <- NA
      xtTab[1,4] <- ''
    }
    cellTex <- matrix(rep("", NROW(xtTab) * NCOL(xtTab)), nrow=NROW(xtTab))
    cellTex[sigp,4] <- "cellcolor[gray]{0.9}"
    rowlabel <- ifelse(nchar(parameter) >9,"",parameter)
    # All I( in factors are replaced with (
    row.names(xtTab) <-  gsub("I\\(","(",row.names(xtTab))
    row.names(xtTab) <-  gsub("\\^2","\\texttwosuperior",row.names(xtTab))
    latex(xtTab, title=title,file=file, caption=caption,caption.lot=caption.lot,
          caption.loc="bottom", label=label, cellTexCmds = cellTex,
          rowlabel=rowlabel, ctable=ctable, where=where,
          booktabs = !ctable, numeric.dollar=FALSE,col.just=rep("r",5),...)
    # returns summary(object)
    invisible(xt)
  }


"latex.summary.lme" <-
  function(object, title="",parameter=NULL, file="",
           shadep=0.05,caption=NULL,label=NULL,ctable=FALSE,form=NULL,
           interceptp = FALSE, moredec=0, where="!htbp", ...) {
    # This function can be mis-used for gls models when an explicit
    # form is given
    options(Hverbose=FALSE)
    dd <- object$dims
    method <- object$method
    fixF <- object$call$fixed
    xtTab <- as.data.frame(object$tTable)
    sigp <- xtTab[,"p-value"]< shadep # cells that will be shaded
    if (!interceptp){
      sigp[1] <- FALSE # intercept will never be shaded
      # Replace small significances, discarding p-value for (Intercept)
      xtTab[1,"p-value"] = 1 # we do not show it anyway, easier formatting
    }
    pval <- format(zapsmall(xtTab[, "p-value"],4))
    pval[as.double(pval) < 0.0001] <- "\\textless .0001"
    xtTab[, "p-value"] <- pval
    xtTab[,"t-value"] <- round(xtTab[,"t-value"],1)  
    if (ncol(xtTab) == 5) # not for gls
      xtTab[,"DF"] <- as.integer(xtTab[,"DF"])
    # extract formula
    if (is.null(form)) {
      if (!is.null(object$terms)) {
        form=object$terms
      } else {
        form = formula(object)
      }
    }
    if (is.null(parameter)) {
      parameter=as.character(form[[2]])
    }
    if (any(wchLv <- (as.double(levels(xtTab[, "p-value"])) == 0))) {
      levels(xtTab[, "p-value"])[wchLv] <- "\\textless .0001"
    }
    if (is.null(label))
      label <- lmeLabel("contr",form)
    form <- deparse(removeFormFunc(as.formula(form)),width.cutoff=500)
    
    form <- paste(sub('~','$\\\\sim$ ',form),sep="")
    # All I( in factors are replaced with "(" **This could be improved
    row.names(xtTab) <- 
      gsub("I\\(","(",dimnames(object$tTable)[[1]])
    row.names(xtTab) <-  gsub("\\^2","\\texttwosuperior",row.names(xtTab))
    row.names(xtTab) <- TextUnderscore(row.names(xtTab))
    
    # Determine base level  
    levs <- lapply(object$contrasts,function(object) {dimnames(object)[[1]][1]})
    levnames <- paste(names(levs),levs,sep=" = ",collapse=", ")
    # Try to locate numeric covariables
    #  v1 <- all.vars(formula(object))[-1]
    ## Changed 8.10.2008, not regression-tested
    v1 <- all.vars(form)[-1]
    numnames <- v1[is.na(match(v1,names(levs)))]
    if (length(numnames > 0)) {
      numnames <- paste(numnames," = 0",collapse=", ")
      levnames <- paste(levnames,numnames,sep=", ")
    }
    if (is.null(caption)){ # TODO: Allow %s substitution
      if (inherits(object,"lme"))
        md = "Mixed model (lme)" else
          if (inherits(object,"gls"))
            md = "Extended linear model (gls)" else
              md = "Linear model"
      caption <- paste(md," contrast table for \\emph{",
                       parameter, "} (model ",form,
                       "). The value in row (Intercept) gives the reference value for ",
                       levnames,".",sep='')
    }
    caption <- TextUnderscore(caption)
    caption.lot <- TextUnderscore(paste("Contrast table for ",parameter, " by ",
                         levnames))
    ndec <- pmax(round(1-log10(xtTab[,2]+0.000001)+moredec),0)
    xtTab[,1] <- formatC(round(xtTab[,1],ndec))
    xtTab[,2] <- formatC(round(xtTab[,2],ndec))
    if (ncol(xtTab) == 5) {
      names(xtTab) <- c("Value","StdErr","DF","t","p")
      pcol = 5
    } else {# gls misuse
      names(xtTab) <- c("Value","StdErr","t","p")
      pcol = 4
    }
    # Only show intercept p/t when explicitely required
    if (!interceptp){
      xtTab[1,pcol-1] <- NA
      xtTab[1,pcol] <- ''
    }
    cellTex <- matrix(rep("", NROW(xtTab) * NCOL(xtTab)), nrow=NROW(xtTab))
    cellTex[sigp,pcol] <- "cellcolor[gray]{0.9}"
    rowlabel <- ifelse(nchar(parameter) >9,"",parameter)
    latex(xtTab, title=title, file=file, caption=caption,caption.lot=caption.lot,
          caption.loc="bottom", label=label, cellTexCmds = cellTex,
          rowlabel=rowlabel, ctable=ctable, where=where,
          booktabs = !ctable, numeric.dollar=FALSE,col.just=rep("r",5),...)
  }

TextUnderscore = function(x){
  gsub("\\_", "\\\\textunderscore ", x)
}

if (FALSE){
  library(nlme)
  library(Hmisc)
  data(Oats)
  # Underscores do not work yet
  Oats$Variety = as.factor(gsub("ll","_ll",Oats$Variety))
  fm1Oats <- lme(yield~ordered(nitro)*Variety, data=Oats,
                 random = ~1|Block/Variety)
  latex(summary(fm1Oats),"Yield",file="")
}


"latex.glm" <-function(object, title="", parameter, file="", shadep=0.05,
                       caption=NULL, label=NULL, ctable=FALSE, form=NULL,
                       interceptp = FALSE, moredec =0, where="!htbp", 
                       ...) {
  options(Hverbose=FALSE)
  fixF <- object$call
  xt = summary(object)
  xtTab <- as.data.frame(coefficients(xt))
  sigp <- xtTab[,4]< shadep # cells that will be shaded
  if (!interceptp){
    sigp[1] <- FALSE # intercept will only be shaded on explicit request
    # Replace small significances, discarding p-value for (Intercept)
    xtTab[1,4] = 1 # we do not show it anyway, easier formatting
  }
  pval <- format(zapsmall(xtTab[, 4],4))
  pval[as.double(pval) < 0.0001] <- "\\textless .0001"
  xtTab[, 4] <- pval
  xtTab[,"z value"] <- round(xtTab[,"z value"],1)
  if (any(wchLv <- (as.double(levels(xtTab[, 4])) == 0))) {
    levels(xtTab[, "4"])[wchLv] <- "\\textless .0001"
  }
  # extract formula
  if (is.null(form)) {
    form <- deparse(formula(object$terms))
  }
  if (is.null(label))
    label <- lmeLabel("contr",form)
  # remove I(..). Because finding matched parens is tricky, we leave the ()
  form <- gsub("I\\(","(",form)
  form <- paste(sub('~','$\\\\sim$ ',form),sep="")
  # Determine base level. TODO: check for numeric covariables
  lev <- list()
  for (i in  seq(along=object$xlevels)) {
    levs <- object$xlevels[i]
    lev[i] <- paste(names(levs),levs[[1]][1],sep=" = ")
  }
  levnames <- paste(lev,collapse=", ")
  
  if (is.null(caption)) # TODO: Allow %s substition
    caption <- paste("Generalize linear model (glm) contrast table for \\emph{",
                     parameter, "} (model ",form,
                     "). The value in row (Intercept) gives the reference value for ",
                     levnames,".", 
                     " The deviance is ",signif(xt$deviance,3), 
                     " at ", xt$df.residual," degrees of freedom.",
                     " The null deviance is ",signif(xt$null.deviance,3), 
                     " at ", xt$df.null," degrees of freedom.", sep='')
  caption.lot <- paste("Contrast table for ",parameter, " by ",
                       levnames)
  ndec <- pmax(round(1-log10(xtTab[,2]+0.000001)+moredec),0)
  xtTab[,1] <- formatC(round(xtTab[,1],ndec))
  xtTab[,2] <- formatC(round(xtTab[,2],ndec))
  names(xtTab) <- c("Value","StdErr","z","p")
  # Do not show Intercept p-values and t-value if not explicitely requeste
  if (!interceptp) {
    xtTab[1,3] <- NA
    xtTab[1,4] <- ''
  }
  cellTex <- matrix(rep("", NROW(xtTab) * NCOL(xtTab)), nrow=NROW(xtTab))
  cellTex[sigp,4] <- "cellcolor[gray]{0.9}"
  rowlabel <- ifelse(nchar(parameter) >9,"",parameter)
  # All I( in factors are replaced with (
  row.names(xtTab) <-  gsub("I\\(","(",row.names(xtTab))
  row.names(xtTab) <-  gsub("\\^2","\\\\texttwosuperior",row.names(xtTab))
  latex(xtTab,title=title, file=file, caption=caption,caption.lot=caption.lot,
        caption.loc="bottom", label=label, cellTexCmds = cellTex,
        rowlabel=rowlabel, ctable=ctable, where=where,
        booktabs = !ctable, numeric.dollar=FALSE,col.just=rep("r",5),...)
  # returns summary(object)
  invisible(xt)
}

#  counts <- c(18,17,15,20,10,20,25,13,12)
#  outcome <- gl(3,1,9)
#  treatment <- gl(3,3)
#  d.AD <- data.frame(treatment, outcome, counts)
#  glm.D <- glm(counts ~ outcome + treatment, family=poisson())
#  latex.glm(glm.D,parameter="severe")



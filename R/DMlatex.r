#' @title Dieter's special shorthand version of Hmisc/latex
#' @description  Outputs a data frame, in my personal latex style.  \code{label} and
#' \code{caption} are required, and \code{ctable} is the default setting. By
#' default, rownames are not included; uses \code{rownames = NULL} to include
#' these.
#'
#' @aliases DMlatex DMlatex.default DMlatex.summary.formula.reverse DMlatex.summary.formula.cross
#' @param object The object to be printed, usually a data frame
#' @param label Required label
#' @param caption Required caption
#' @param rowname Rownames are not shown by default.
#' @param longtable Uses LaTeX longtable with a very large value for
#' line.pages, effectively disabling this parameter.
#' @param ctable use package ctable for output.
#' @param file optional file name, by default output to console or Sweave
#' document
#' @param where positioning parameter for LaTeX
#' @param rowlabel Label in top left corner
#' @param mincapwidth for ctable, the minimum caption width in mm. Requires
#' ctable1.sty or a more recent update of ctable.
#' @param width for ctable, width of table. Should be given when column type
#' \code{X} is used.
#' @param cellTexCmds see latex(Hmisc) documentation
#' @param lines.page set to high value to avoid problem with longtable
#' @param \dots other parameters forwarded to \code{latex.default}. Use
#' rownames=NULL to include rownames.
#' @importFrom stats as.formula coefficients formula var
#' @export DMlatex
#' @export DMlatex.default
#' @export DMlatex.summary.formula.reverse
#' @export DMlatex.summary.formula.cross
#' @author Dieter Menne
#' @seealso \code{\link[Hmisc]{latex}}
#' @references Uses latex from Frank Harrel's Hmisc
#' @keywords print
#' @examples
#'
#'   DMlatex(warpbreaks[1:10,],"Warpbreak data","tab:warpbreak")
#'   set.seed(173) # so can replicate results
#'   sex =  factor(sample(c("m","f"), 500, rep=TRUE))
#'   age =  rnorm(500, 50, 5)
#'   treatment =  factor(sample(c("Drug","Placebo"), 500, rep=TRUE))
#'   x = summary(treatment~age+sex,method="reverse")
#'   DMlatex(x,caption="This is the reverse caption",
#'    label="tab.summary.formula.reverse")
#'
DMlatex = function(object,label,caption,
                   rowname = NULL, longtable = FALSE, ctable = FALSE, file = "",
                   where = "!htbp",
                   rowlabel = "",
                   mincapwidth, width,
                   cellTexCmds = NULL,
                   lines.page = 10000,
                   ...) {
  UseMethod("DMlatex",object)
}


DMlatex.default = function(object,label,caption,
                           rowname = NULL, longtable = FALSE, ctable = FALSE, file = "",
                           where = "!htbp", rowlabel = "",
                           mincapwidth,width, cellTexCmds = NULL, ...) {
  options(Hverbose = FALSE)
  
  # A reported bug in latex does not allow to use cgroup with cellTex
  # This bug reappeared with R 2.7.0
  dots = list(...)
  hascgroup =  length(grep("cgroup",names(dots))) > 0
  #  hascgroup = FALSE
  
  shadep = 0.05 # Columns with name p are shaded when value is < shadep
  pcol = substr(names(object),1,1) == "p" |
    substr(names(object),1,2) == "Pr"
  # Color for columns with name p
  cellTex = cellTexCmds
  if (!hascgroup && is.null(cellTex) && any(pcol) &&
      is.numeric(object[pcol][,1])) {
    nr = NROW(object)
    nc = NCOL(object)
    cellTex <- matrix(rep("", nr * nc), nrow = nr)
    sigp <-
      object[,pcol,drop = FALSE] < shadep # cells that will be shaded
    colnames(cellTex) = names(object)
    for (col in colnames(sigp)) {
      cellTex[sigp[,col],col]    =      "cellcolor[gray]{0.9}"
      anysmall =  as.double(object[, col])   < 0.0001
      # DM March 2009
      #      object[,col] = round(object[,col],2)
      object[,col] = signif(object[,col],2)
      if (any(anysmall)) {
        object[anysmall, col][] <- "\\textless .0001"
      }
    }
  }
  # ctable = FALSE # because of a bug in latex 3.8.1, giving additional lines
  # mincapwidth is a special feature in ctable(1) that forces
  # a minimum caption width. I use a dirty try via label to force it
  # into the right place.
  misscap = missing(mincapwidth)
  usemincap = ctable & (ncol(object) <= 3 | !misscap)
  if (usemincap & misscap)
    mincapwidth = 120
  if (usemincap)
    label = paste(label,",mincapwidth=",mincapwidth,"mm",sep = "")
  # same trick for width
  if (!missing(width) & ctable)
    label = paste(label,",width=",width,"mm",sep = "")
  if (is.matrix(object) | (!is.null(rowname) && rowname == TRUE)) {
    # with default row label
    latex(
      object,ctable = ctable,file = file, na.blank = TRUE,
      longtable = longtable, caption.loc = "bottom",label = label,
      rowlabel = rowlabel, cellTexCmds = cellTex,
      caption = caption, where = where,booktabs = !ctable,
      first.hline.double = FALSE,...
    )
  } else {
    # Required due to a bug latex 3.8-1 (corrected 3.8.2
    #    object = sapply(object,as.character)
    latex(
      object,ctable = ctable,file = file, rowname = rowname,na.blank = TRUE,
      longtable = longtable, caption.loc = "bottom",label = label,
      rowlabel = rowlabel,cellTexCmds = cellTex,
      caption = caption, where = where, booktabs = !ctable,
      first.hline.double = FALSE,...
    )
  }
}

DMlatex.summary.formula.reverse =
  function(object,label,caption,
           rowname = NULL, longtable = FALSE, ctable = FALSE, file = "",
           where = "!htbp",
           rowlabel = "",
           mincapwidth, width, cellTexCmds, lines.page = 10000, ...) {
    stopifnot(class(object) == "summary.formula.reverse")
    Hmisc::latex(
      object,ctable = ctable,file = file, na.blank = TRUE,
      longtable = longtable, caption.loc = "bottom",label = label,
      insert.bottom = FALSE,   booktabs = !ctable,
      caption = caption, lines.page = lines.page, where = where,
      first.hline.double = FALSE,width = width, ...
    )
  }

DMlatex.summary.formula.cross =
  function(object,label,caption,
           rowname = NULL, longtable = FALSE, ctable = FALSE, file = "",
           where = "!htbp",
           rowlabel = "",
           mincapwidth, width, cellTexCmds = NULL, lines.page = 10000, ...) {
    stopifnot(class(object) == "summary.formula.cross")
    Hmisc::latex(
      object, ctable = ctable,file = file, na.blank = TRUE,
      longtable = longtable, caption.loc = "bottom",label = label,
      caption = caption, lines.page = lines.page, where = where,
      first.hline.double = FALSE,width = width,booktabs = !ctable,...
    )
  }

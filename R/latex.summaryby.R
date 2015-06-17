#' @title Print LaTeX tables of summary statistics broken down by factors.
#'
#' @description LaTex output of summary statistics of a data frame. Prints one line for
#' minimum, maximum median, mean, quartiles, number of elements, standard
#' deviation and standard error for each numerical variable.  A separate table
#' is written for each unique combinations of factors. Labeling and generation
#' of captions is automatic.
#'
#' @name DMlatexSummaryby
#' @param object data frame with numerical and factor variables
#' @param title to be printed in upper left corner
#' @param INDICES a factor or list of factors for breakdown. Defaults to no
#' breakdown, resulting in one detail table only.
#' @param file output file name; default prints output to the standard output,
#' which is the choice for Sweave.
#' @param caption text for caption, with sprintf substitions.  Defaults to
#' factor combination, followed by explanation of the column headers.
#' @param label if no label given, label of table, defaults to
#' \code{tab:<factorname1><factor1>:<factornameN><factorN>.} If not "", and
#' there is no colon in the label, \code{tab:<label>.<factorname1>...} is used
#' as table label.  If there is a colon in the label, the label is used as is.
#' @param ctable Uses \code{ctable}-formatting of LaTeX by default.
#' @param rowlabel optional text for left table header cell.
#' @param moredec Show more decimals than by default. Fractional values, e.g.
#' 0.5 can be used here such that boundary case are rounded up instead of down
#' @param where positioning parameter for LaTeX
#' @param onetable If TRUE, force multiple tables into one.  If FALSE, print
#' one table per combination of factors.  Internally set to TRUE when there is
#' only one numerical factor.
#' @param lang Language, currently \code{en} (default) or \code{de}.
#' @param noQuantiles Suppress quantiles if TRUE
#' @param \dots Additional parameters passed to \code{latex} in \code{Hmisc}.
#' @return Returns the result of the call to \code{latex} in Hmisc.
#' @note Requires \code{ctable} in your Snw/tex file. In a
#' multi-table breakdown, only the caption of the first table has the
#' explanation of the headers.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link[Hmisc]{latex}}, \code{\link{by}}
#' @keywords print
#' @examples

#' # one table only
#' DMlatexSummaryby(warpbreaks)
#' # a table for each combination
#' DMlatexSummaryby(warpbreaks,INDICES=c("wool","tension"))
#' DMlatexSummaryby(warpbreaks,INDICES=c("wool","tension"),noQuantiles=TRUE,
#'           caption="Warpbreaks %s At the end", file="")
#' DMlatexSummaryby(warpbreaks,INDICES=c("wool","tension"),
#'           caption="Warpbreaks %s At the end", file="",onetable=TRUE)
#'

#' @export
"DMlatexSummaryby" <-
  function(object, title = "", INDICES = NULL, file = "", caption = "%s",
            label = "",   ctable = FALSE, rowlabel = "", moredec = 0,
            where = "!htbp", onetable = NULL, lang = "en", noQuantiles = FALSE, ...) {
    options(Hverbose = FALSE)
    # Remove unused factors
    ind <- sapply(object, is.factor)
    object[ind] <- lapply(object[ind], "[", drop = TRUE)
    
    numvars = unlist(lapply(object,is.numeric))
    numvars[INDICES] = FALSE # Force numerical in list to play factors
    
    if (is.null(onetable))
      onetable = !is.null(INDICES) & (sum(numvars) == 1)
    if (file != "")
      unlink(file)   # delete old tex file
    isfirst = TRUE # only print out detailed caption on first time
    if (is.null(INDICES))
      INDICES = ind
    z = by(object,object[INDICES],function(y) {
      byINDICES0 = y[1,INDICES,drop = FALSE] # isolate grouping
      if (is.null(INDICES)) {
        labINDICES = ""
        nam = ""
      } else {
        nam =  paste("\\emph{",names(byINDICES0),":}",sep = "")
        labINDICES = paste(names(byINDICES0),collapse = ".")
      }
      faknam <-  sapply(byINDICES0,
                        function(object)
                          (paste(levels(object)[object])))
      if (onetable)
        byINDICES <- ""
      else
        byINDICES <- paste(nam,faknam,sep = " ",collapse = ", ")
      labINDICES <-
        tolower(paste(labINDICES,faknam,sep = ".",collapse = ""))
      labINDICES <-
        paste(unique(unlist(strsplit(
          labINDICES,"(\\.+)"
        ))),
        collapse = ".")
      # When there is a colon in the label, we take it as is
      if (!length(grep(":", label)))
        if (label == "") {
          label = lmeLabel("by",labINDICES, TRUE)
        } else {
          label = lmeLabel(paste("by",tolower(label),sep = ""),labINDICES, TRUE)
        }
      # Integrate with caption; adds a %s if it is not in caption
      # TODO remove extra space before caption end.
      if (!length(grep("%s",caption)))
        caption = paste(caption," %s",sep = "")
      caption = paste(sprintf(caption,byINDICES),sep = ". ",collapse = "")
      if (isfirst) {
        if (lang == "en") {
          q = ifelse(noQuantiles,"", ". \\emph{Q1, Q3}: quartiles");
          caption = paste(
            caption, q,". \\emph{Med}: median; \\emph{StdDev}: standard deviation; \\emph{StdErr}: standard error of mean.", sep =
              ""
          )
        } else {
          q = ifelse(noQuantiles,"", ". \\emph{Q1, Q3}: Quartile");
          caption = paste(
            caption, q,". \\emph{Med}: Median; \\emph{StdAbw}: Standardabweichung; \\emph{StdFeh}: Fehler des Mittelwerts.", sep =
              ""
          )
        }
        assign("isfirst",FALSE,inherits = TRUE) ##eee.. ugly assign
      } # end isfirst
      
      y = y[,numvars,drop = FALSE]
      xt = lapply(y, function(object) {
        su = summary(object)[1:6] # Don't use the NA column
        su["n"] = sum(!is.na(object)) # add the number of items
        su["StdDev"] = sqrt(var(object,na.rm = TRUE))
        if (su["n"] > 1)
          su["StdErr"] = su["StdDev"] / (sqrt(su["n"] - 1))
        else
          su["StdErr"] = NA
        su
      })
      xt = do.call(rbind,xt)
      if (lang == "en") {
        nam = c("Min","Q1","Med","Mean","Q3","Max","n","StdDev","StdErr")
      } else {
        nam = c("Min","Q1","Med","Mittel","Q3","Max","n","StdAbw","StdFeh")
      }
      dimnames(xt)[[2]] = nam
      # determine row-wise best display from StdDev
      # in earlier Versions, used IQR instead of StdDev, but IQR can
      # degenerate to 0
      ndec = pmax(round(1 - log10(
        apply(xt,1,
              function(object)
                object[8]) + 0.000001
      ) + moredec),0)
      # standard error has more decimals
      ncol = ifelse(noQuantiles,4,6)
      dec = cbind(matrix(ndec,nrow = length(ndec),ncol = ncol),0,ndec,ndec +
                    1)
      if (onetable)     {
        list(cbind(byINDICES0,xt),dec,caption)
      } else {
        if (noQuantiles) {
          qcols = grep("^Q",colnames(xt))
          xt = xt[,-qcols,drop = FALSE]
        }
        lx = latex(
          xt,title = title,file = file,label = label,caption = caption,
          booktabs = !ctable,append = TRUE,
          caption.loc = "bottom",ctable = ctable,rowlabel = rowlabel,
          center = "centering",where = where,
          dec = dec,
          ...
        )
        if (file == "")
          invisible(xt)
        else
          invisible(lx)
      }
    })
    if (onetable) {
      # Merge rows
      # Maximal decimals per column
      cdec = c(rep(0,sum(!numvars)),apply(z[[1]][[2]],2,max))
      za = z[[1]][[1]]
      for (i in 2:length(z)) {
        za = rbind(za,z[[i]][[1]])
      }
      if (noQuantiles) {
        qcols = grep("^Q",colnames(za))
        cdec = cdec[-qcols]
        za = za[,-qcols,drop = FALSE]
      }
      lx = latex(
        za,title = title,file = file,label = label,caption = z[[1]][[3]],
        booktabs = !ctable,append = TRUE,rowlabel = rowlabel,
        caption.loc = "bottom",ctable = ctable,rowname = NULL,
        center = "centering",where = where,
        cdec = cdec,
        ...
      )
      # return table if for console output, otherwise dvi-able file
      if (file == "")
        invisible(za)
      else
        invisible(lx)
    } else {
      if (file == "")
        invisible(z)
      else
        invisible(z[[1]])
    }
  }

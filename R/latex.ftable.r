#' Print LaTeX contrast table for ftable
#' 
#' Prints an ftable result in latex; does not use Hmisc latex, but rather a
#' home-brew hack. Hmisc should be included for method dispatching.
#' 
#' @method latex ftable
#' @param object an ftable object
#' @param title label of table
#' @param caption caption used for table
#' @param label label used for table
#' @param pos positioning parameter for LaTeX
#' @param mincapwidth for ctable, the minimum caption width in mm.
#' @param \dots additional parameters; none used currentyle, kept in analogy to
#' Hmisc
#' @return No result
#' @export latex.ftable
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @keywords print
#' @examples
#' 
#' latex(ftable(Survived ~ ., data = Titanic),mincapwidth=100, label="tab:tit",
#'   caption="Titanic survivors")
#' 
latex.ftable = function(object,title="",caption,label,pos="!htbp",
  mincapwidth,...){
  ft = format(object,quote=FALSE)
  nr = nrow(ft)
  nc = ncol(ft)
  cv = attr(object,"col.vars")
  rv = attr(object,"row.vars")
  ncolvars = length(cv)
  nrowvars = length(rv)
  
  ft[ncolvars,1:nrowvars] = ft[ncolvars+1,1:nrowvars]
  mincap = ""
  if(!missing(mincapwidth)) 
    mincap = paste(", mincapwidth=",mincapwidth,"mm",sep="")
  align1 = paste(rep("l",ncolvars),collapse="")
  align2 = paste(rep("r",nc-ncolvars),collapse="")
  cat("\\ctable[ caption={",caption,"}, label=",label,",pos=",pos,
    mincap,", botcap]{",
    align1,align2,"}{} \n{\\FL\n", sep="")
  for (i in 1:ncolvars){
    head = paste("\\multicolumn{1}{c}{",ft[i,],"}",collapse="&\n",sep="")  
    if (i == ncolvars)
      cat(head, "\n\\ML\n") else
    cat(head, "\n\\NN\n")
      
  }
  for (i in (ncolvars+2):(nr)) { 
    cat(paste(ft[i,],collapse="&"))
    if (i != nr ) {
      if (substr(ft[i,1],1,1) ==' ' & substr(ft[i+1,1],1,1) !=' '){
        cat("\\ML\n")
      } else 
      cat("\\NN\n")
    }
  }
  cat("\n\\LL\n}\n")
}

#latex(ftable(Survived ~ ., data = Titanic),mincapwidth=100, label="tab:tit",
#  caption="Titanic survivors")

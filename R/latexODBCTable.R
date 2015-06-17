#' @title Retrieve database table and format it nicely for LaTeX
#'
#' @description Generates LaTeX table from data retrieved from an Excel or Access file.
#' The table is closed immediately after retrieval.
#'
#' @param object path of Excel or Access file. Currently, no other ODBC source
#' are supported, but changing this should be easy.
#' @param title to be printed in upper left corner
#' @param table name of table or query in database, or named range in Excel.
#' In theory, giving worksheet names for Excel should work, but results are
#' mixed, so better always use named ranges. Not used if sql is given.
#' @param caption optional caption. Default is \emph{Table <tablename>,
#' n=<number of rows>}
#' @param longtable use longtable; cannot be combinded with \code{ctable}
#' @param ctable Uses \code{ctable}-formatting of LaTeX by default.
#' @param label optional label. Default is \emph{tab:tablename}
#' @param file output file name; default prints output to the standard output,
#' which is the choice for \emph{Sweave}.
#' @param sql SQL query to retrieve data. Default is whole table.  Parameter
#' \code{table} is not used if query is given.
#' @param where positioning parameter for LaTeX
#' @param mincapwidth for ctable, the minimum caption width in mm. Requires
#' ctable1.sty or a more recent update of ctable.
#' @param width for ctable, width of table. Should be given when column type
#' \code{X} is used.
#' @param as.is passed to sqlQuery; if true, string are not converted to
#' factors.
#' @param \dots Additional parameters passed to \code{latex} in \code{Hmisc}.
#' @return Returns the table retrieved from the database. This is somewhat
#' inconsistent to \code{latex.lme} and friends that return the latex object,
#' but it's convenient if single items of the table are to be displayed in
#' Sweave.
#' @export latexODBCTable
#' @import RODBC
#' @note Requires \code{ctable} in your Snw/tex file if ctable = TRUE
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link{lme.value}}, \code{\link[Hmisc]{latex}},
#' \code{\link{latex.lme}}, \code{\link[nlme]{lme}}
#' @keywords print
#' @examples
#'
#' \dontrun{
#'    # tex output in Sweave
#'    database = system.file("extdata", "testtable.accdb", package = "Dlatex")
#'    tb <- latexODBCTable(database, table = "Subject")
#'
#'
#' }
#'
"latexODBCTable" <-
  function(object, title = "", table = NULL, caption = NULL, longtable = FALSE,
           ctable = FALSE, label = NULL, file = "", sql = paste("select * from",table),
           where = "!htbp", mincapwidth = 120, width, as.is = FALSE, ...)
  {
    options(Hverbose = FALSE)
    if (length(grep(".mdb|.accdb",tolower(object))))
      channel = odbcConnectAccess2007(object)
    else
      channel = odbcConnectExcel2007(object)
    if (is.null(table))
      table = sub("^.*from (.*) where.*$","\\1",sql)
    tb = sqlQuery(channel,sql,nullstring = "",as.is = as.is)
    odbcClose(channel)
    # mincapwidth is a special feature in ctable(1) that forces
    # a minimum caption width. I use a dirty trick via label to force it
    # into the right place.
    if (is.null(label))
      label = paste('tab:',table,sep = "")
    if (!is.null(mincapwidth) & ctable)
      label = paste(label,",mincapwidth=",mincapwidth,"mm",sep = "")
    # same trick for width
    if (!missing(width) & ctable)
      label = paste(label,",width=",width,"mm",sep = "")
    if (is.null(caption))
      caption = paste("Table: ",table,", n=",nrow(tb),sep = '')
    latex(
      tb, title = title,file = file, label = tolower(label), rowname = NULL,
      caption = caption, caption.loc = "bottom",
      longtable = longtable, ctable = ctable,
      booktabs = !ctable, na.blank = TRUE, where = where,
      center = "centering",...
    )
    invisible(tb)
  }

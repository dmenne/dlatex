#' @title Returns a standard label for LaTeX tables
#'
#' @description The standard label for LaTeX table references consist of a first part (e.g.
#' \code{tab:}, followed by the parameter \code{type} (e.g. \code{contr} for
#' contrasts, \code{by} for breakdown lists.  A list of parameters is appended,
#' with special characters, including spaces and I(), are removed, and the
#' whole string is lowercased.
#'
#'
#' @param type type of table, e.g. \code{contr} for contrasts.
#' @param form a formula or a multipart string.
#' @param spelldigits if TRUE, digits are converted to "one", "two", etc.
#' @return For example, \code{lmeLabel("contr","Press~Treat*Repeat")} returns
#' \code{tab:contr.press.treat.repeat}. Note lowercase in result.
#' @export lmeLabel
#' @note Used internally in \code{Dmisc}, but recommended for use by other
#' labelled tables to force consistency.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link{latex.lme}}, \code{\link{anova.lme}}
#' @keywords misc
#' @examples
#'
#' lmeLabel("contr","Press~Treat*Repeat+I(Repeat^2)")
#' # This shows how digits are converted
#' lmeLabel("contr","yield ~ I(nitro^2) * Variety + I(nitro^3)",TRUE)
#'
"lmeLabel" <-
  function(type,form,spelldigits = FALSE) {
    # Label of table is tab: with form items separated by .
    # remove all I(
    label <- gsub("I\\(","",form)
    label <- gsub("~","",form)
    # Spell out digits
    # How do I vectorize this?
    if (spelldigits) {
      label = gsub("1","one",label)
      label = gsub("2","two",label)
      label = gsub("3","three",label)
      label = gsub("4","four",label)
      label = gsub("5","five",label)
      label = gsub("6","six",label)
      label = gsub("7","seven",label)
      label = gsub("8","eight",label)
      label = gsub("9","nine",label)
      label = gsub("0","zero",label)
    }
    else
      # remove digits
      label = gsub("[[:digit:]]","",label)
    label <-
      tolower(gsub(" ","",label)) # remove all spaces and lowercase
    label <- gsub("\\+","pos",label) #
    label <- gsub("\\-","neg",label) #
    label <- gsub("\xE4","ae",label) # ?
    label <- gsub("\xDF","ss",label)  # ?
    label <- gsub("\xF6","oe",label)  # ?
    label <- gsub("\xFC","ue",label)  # ?
    label <- gsub("[^[:alpha:]]",".",label)
    # Remove repeating parts
    label <-
      paste(unique(unlist(strsplit(label,"(\\.+)"))),collapse = ".")
    label <- as.character(paste("tab:",type,".",label,sep = ""))
    label
  }
# lmeLabel("tab",form)

#lmeLabel("contr","yield? ~ I(n?tro^2) * Variety + I(n?tro^3)",TRUE)

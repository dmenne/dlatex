#' @title Lexically removes a function from a formula.
#'
#' @description Remove the I(..) or similar function from a formula. So
#' \code{effect ~ I(sqrt(nitro))*treat + I(nitro^2)}
#' becomes \code{effect ~ sqrt(nitro)*treat + nitro^2}
#'
#' @param formula valid formula object
#' @param func Formula to be remove, without the \code{()}
#' @return Returns the string without the formula part.
#' @export removeFormFunc
#' @author Thomas Lumley, Dieter Menne
#' @examples
#'
#' removeFormFunc(effect ~ I(sqrt(nitro))*treat + I(nitro^2))
#'
"removeFormFunc" <-
  function(formula,func = "I") {
    process <- function(expr) {
      if (length(expr) == 1)
        return(expr)
      if (length(expr) == 2)
        if (expr[[1]] == as.name(func))
          return(expr[[2]])
      else
        return(expr)
      expr[[2]] <- process(expr[[2]])
      expr[[3]] <- process(expr[[3]])
      return(expr)
    }
    formula[[3]] <- process(formula[[3]])
    formula
  }

#removeFormFunc(effect ~sqrt(nitro)*treat + I(nitro^2))

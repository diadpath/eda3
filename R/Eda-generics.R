#' Load the table of an EDA object from the database
#'
#' stores the result in one of the class slot
#'
#' @param object An [Eda-class] object.
#' @param ... Further arguments passed to methods.
#'
#' @return The [Eda-class] object with one or several slots filled.
#' @export
setGeneric("loaddb", function(object, ...) standardGeneric("loaddb"))

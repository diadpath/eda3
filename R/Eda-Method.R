#' @include Eda-Class.R Eda-generics.R Eda-utils.R
NULL

#' @rdname loaddb
#' @aliases loaddb,Eda-method
#' @examples
#' \dontrun{
#'eda <- new("Eda", base = list(database = "diaspara",
#'host = "localhost", port = 5432, user = "postgres", password= "postgres"
#' eda <- loaddb(eda)
#' }
#' @export
setMethod("loaddb", signature = signature("Eda"), definition = function(object, ...) {

})

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


#' Open a DBI connection from the credentials stored in an Eda object
#'
#' @param object an object inheriting from class `Eda`.
#' @return a DBIConnection (RPostgres). The caller must disconnect it.
#' @noRd
.eda_connect <- function(object) {
  stopifnot(is(object, "Eda"))
  b <- object@base
  DBI::dbConnect(
    drv      = RPostgres::Postgres(),
    host     = b$host,
    dbname   = b$database,
    port     = as.integer(b$port),
    user     = b$user,
    password = b$password
  )
}

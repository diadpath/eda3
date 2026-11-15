#' Run a function with a console progress bar (internal)
#' @noRd
.make_progress <- function(n) {
  pb <- utils::txtProgressBar(min = 0, max = max(n, 1), style = 3)
  list(
    tick  = function(i) utils::setTxtProgressBar(pb, i),
    close = function() close(pb)
  )
}

#' Open a DBI connection from the credentials stored in an Eda object
#'
#' @param object an object inheriting from class `Eda`.
#' @return a DBIConnection (RPostgres). The caller must disconnect it.
#' @export
eda_connect <- function(object) {
  stopifnot(is(object, "Eda")) # true for daughter class also
  b <- object@base
  con <- DBI::dbConnect(
    drv      = RPostgres::Postgres(),
    host     = b$host,
    dbname   = b$database,
    port     = as.integer(b$port),
    user     = b$user,
    password = b$password
  )
  return(con)
}

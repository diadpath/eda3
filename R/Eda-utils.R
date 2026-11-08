#' Run a function with a console progress bar (internal)
#' @noRd
.make_progress <- function(n) {
  pb <- utils::txtProgressBar(min = 0, max = max(n, 1), style = 3)
  list(
    tick  = function(i) utils::setTxtProgressBar(pb, i),
    close = function() close(pb)
  )
}

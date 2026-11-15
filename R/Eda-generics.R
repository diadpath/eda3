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
setGeneric("distance_sea",def=function(object,...) standardGeneric("distance_sea"))
setGeneric("distance_source",def=function(object,...) standardGeneric("distance_source"))
setGeneric("upstream_area",def=function(object,...) standardGeneric("upstream_area"))
setGeneric("upstream_segments",def=function(object,...) standardGeneric("upstream_segments"))
setGeneric("stream_order",def=function(object,...) standardGeneric("stream_order"))
setGeneric("bassin_area",def=function(object,...) standardGeneric("bassin_area"))
setGeneric("calculatedensities",def=function(object,...) standardGeneric("calculatedensities"))
setGeneric("distance_upstream_between_stations",def=function(object,...) standardGeneric("distance_upstream_between_stations"))
setGeneric("map",def=function(object,...) standardGeneric("map"))
setGeneric("loadshp",def=function(object,...) standardGeneric("loadshp"))
setGeneric("segment_mortality",def=function(object,...) standardGeneric("segment_mortality"))

#' @include Eda-Class.R Eda-generics.R Eda-utils.R
NULL


#' @title River class
#' @note Class inherited from `Eda` adapted to work with different river layers
#' @author Cedric Briand \email{cedric.briand@@eaux-et-vilane.bzh}
#' @slot base `list` inherited from Eda (database, host, port, user, password)
#' @slot root_path `character` inherited from Eda
#' @slot river `data.frame` containing the data loaded from the rivers
#' @slot country `character` A vector of country to limit the size of the loaded data
#' @slot river_network The river network used to build the object, "sudoang",
#' "hydrosheds" and "amber_salmon"
#' @slot area `character`A vector of area loaded from the diadromous database
#' @slot traveled_segments vector of all segments traveled when going upstream,
#' @examples
#' \dontrun{
#' # credentials read from credentials.yml at the project root
#' obj <- new("Rivers", country = "France")
#' # or supplied explicitly
#' obj <- new("Rivers",
#'   base = list(database = "sudoang", host = "localhost", port = 5432,
#'               user = "postgres", password = "secret"),#'
#'   country = "France",
#'   river_network = "sudoang")
#' obj <- loaddb(obj)
#' }
setClass(
  Class = "River",
  representation = representation(
    river_network = "character",
    country = "character",
    area = "character",
    shp = "ANY"
  ),
  prototype = prototype(
    river = data.frame(),
    river_network = "character",
    country = "character",
    area = "character",
    traveled_segments = vector()),
  validity = function(object) {
    msg <- character()
    if (length(object@river_network) != 1L)
      msg <- c(msg, "@river_network must have length 1")
      if (length(msg)) return(msg) else return(TRUE)
  },
  contains = "Eda"
)



#' TODO initialize method
#' Check consistency depending on river_network country and area
#'
# setMethod("initialize", "River",
#           function(.Object,
#                    river_network = as.character(NULL),
#                    country = as.character(NULL),
#                    area = as.character(NULL),
#                    ...) {
#             #browser()
#             .Object <-
#               callNextMethod(.Object, ...) #  It then results in a call to the first inherited method after the current method, with the arguments to the current method passed down to the next method
#             # check that area are existing and that area is called with hydroshed
#             # or that they are EMU if called with sudoang
#             if (length(area) > 0) {
#               requete <- new("RequeteDB")
#               base <- c(
#                 getOption("eda.dbname"),
#                 getOption("eda.host"),
#                 getOption("eda.port"),
#                 getOption("eda.user"),
#                 getOption("eda.password"))
#               requete@sql <-
#                 paste0("select distinct on (basin) basin from dbeel_rivers.rna")
#               requete <- stacomirtools::query(requete, base)
#               the_basins <- requete@query
#               if (!all(basin %in% the_basins$basin))
#                 stop(paste(
#                   "basin not found=",
#                   basin[!basin %in% the_basins$basin],
#                   " possible values ",
#                   paste(the_basins, collapse = ",")
#                 ))
#             }
#             # check that emu are existing
#             if (length(emu) > 0) {
#               if(!require(stacomirtools)) stop("stacomirtools needed to initialize BaseEdaRios")
#               requete <- new("RequeteDB")
#               base <- c(
#                 getOption("eda.dbname"),
#                 getOption("eda.host"),
#                 getOption("eda.port"),
#                 getOption("eda.user"),
#                 getOption("eda.password"))
#               requete@sql <-
#                 paste0("select distinct on (emu) emu from dbeel_rivers.rna")
#               requete <- stacomirtools::query(requete, base)
#               the_emus <- requete@query
#               if (!all(emu %in% the_emus$emu))
#                 stop(paste(
#                   "emu not found=",
#                   emu[!emu %in% the_emus$emu],
#                   " possible values ",
#                   paste(the_emus, collapse = ",")
#                 ))
#             }
#
#
#             schema <- .Object@schema
#             #      rep1 <- schema%in%c("spain","portugal","france","dbeel","dbeel_rivers")
#             #	  tx1 <- "You should be using spain or france or portugal, dbeel or dbeel_rivers schema"
#             #      if (!rep1) stop(tx1)
#             if (length(zonegeo) > 0) {
#               if (!all(zonegeo %in% c("all", "Spain", "France", "Portugal", "Sweden", "Finland", "Norway")))
#                 stop(
#                   "zonegeo should be one of 'all','Spain','France','Portugal', 'Sweden', 'Finland', 'Norway' "
#                 )
#               #if (length(zonegeo)>1) warning ("You are using multiple choices currently developped for emu, not at the national level")
#             }
#             #      if (length(basin)>0){
#             #      if (!(length(basin)==0) & !(length(emu)==0)) stop("Choose either basin or emu, both cannot be there")
#             #       }
#             .Object@zonegeo <- zonegeo
#             .Object@emu <- emu
#             .Object@basin <- basin
#             .Object@prkey <- prkey
#             .Object
#           })

#' @rdname River-class
#' @aliases show,River-method
#' @param object An object of class River
#' @export
setMethod("show", "River", function(object) {
  callNextMethod()
  cat("------\n")
  cat("<River>\n")
  cat("  river_network: ", object@river_network, "\n", sep = "")
  cat("  river dataset:\n")
  cat("    Number of rows: ", nrow(object@river), "\n", sep = "")
  invisible(NULL)
})


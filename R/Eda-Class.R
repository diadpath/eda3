#' Eda: base class for exploratory data analysis on database tables
#'
#' Mother class for EDA model objects. It stores the connection information
#' of a database table which is initialised from a credentials.yml at the root
#' of the project.
#'
#' @slot data `data.frame`. The data loaded from the table (see [loaddb()]).
#' @slot base `list`. A list with names `database`, `host`, `port`, `user`,
#' `password` that holds DBI connexion information.
#' @slot root_path `character`. The path to root, `credentials.yml` is searched
#' when `base` is not supplied.
#'#' @section Validity:
#' `base` must return a string of length 5 and contain database, host, port, user,
#'  and password.  and `port` must be numeric. `root_path` must have length 1.
#' @section credentials.yml:
#' Create a text file named `credentials.yml` at the root of the project:
#' ```
#' database: name_of_the_database
#' host: 127.0.0.1
#' port: 5432
#' user: myuser
#' password: mypassword
#' ```
#' Do not commit this file to git.
#' @author Cedric Briand \email{cedric.briand@@eaux_et_vilaine.bzh}
#' @seealso [loaddb()]
#'
#' @examples
#' # An example setting initial string
#' obj <- new("Eda", list("database" = "my_database", "host" = "localhost",
#' "port" = 5432,"user" = "postgres", "password" = "Monpetitsecret" ))
#' obj
#' # Reading a credentials file
#' if (requireNamespace("yaml", quietly = TRUE)) {
#' dir <- tempfile("eda")
#' dir.create(dir)
#'  base <- list(
#'    database = "my_database",
#'    host     = "localhost",
#'    port     = 5432,
#'    user     = "postgres",
#'    password = "mon_petit_secret_bien_garde"
#'    )
#'  yaml::write_yaml(base, "credentials.yml")
#'  obj <- new("Eda", root_path = "dir")
#'  print(obj) # shows the content of the object
#'  # An example which will search for credentials.yml at the root of the project
#'  }
#' @aliases Eda-class
#' @export
setClass(
  Class = "Eda",
  representation = representation(
    base = "list",
    root_path = "character"
  ),
  prototype = prototype(
    base = list(),
    root_path = character(0L)
  ),
  validity = function(object) {
    msg <- character()
    required <- c("database", "host", "port", "user", "password")
    missing <- setdiff(required, names(object@base))
    if (length(missing))
      msg <- c(msg, paste("@base is missing element(s):",
                          paste(missing, collapse = ", ")))
    if ("port" %in% names(object@base) && class(object@base$port) != "numeric")
      msg <- c(msg, "the port should be an integer or a numeric")
    if (length(object@root_path) > 1L)
      msg <- c(msg, "@root_path must have length 0 or 1")

  if (length(msg)) msg else TRUE
  }
)

#' Initialize an Eda object
#'
#' @param .Object An [Eda-class] object.
#' @param base `list`. base `list`. Named list with elements `database`, `host`, `port`,
#'   `user` and `password`. If empty (default), it is read
#'   from `credentials.yml` in `root_path`.#'
#' @param root_path `character(1)`. Project root. Defaults to [here::here()].
#' @param ... Passed to the next `initialize` method.
#' @rdname Eda-class
#' @aliases initialize,Eda-method
#' @export
setMethod(
  "initialize", "Eda",
  function(.Object, base = list(), root_path = character(0L), ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@base <- base
    .Object@root_path <- if (length(root_path) == 0L) here::here() else root_path


    if (length(.Object@base) == 0) {
      yaml_file <- file.path(.Object@root_path, "credentials.yml")
      if (!file.exists(yaml_file)) {
        stop(
          "No `base` supplied and no credentials file found at: ", yaml_file,
          "\nCreate a credentials.yml with fields database, host, port, ",
          "user, password.",
          call. = FALSE
        )
      }
      cred <- tryCatch(
        yaml::read_yaml(yaml_file),
        error = function(e) {
          stop("Invalid yaml: ", conditionMessage(e), call. = FALSE)
        }
        # the call should not be part of the error message, call. = FALSE
      )
      required <- c("host", "database", "user", "password", "port")
      missing <- setdiff(required, names(cred))
      if (length(missing)) {
        stop("credentials.yml is missing: ", paste(missing, collapse = ", "), call. = FALSE)
      }
      .Object@base <- as.list(cred)

    }
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname Eda-class
#' @aliases show,Eda-method
#' @export
setMethod("show", "Eda", function(object) {
  cat("<Eda>\n")
  cat("  root_path: ", object@root_path, "\n", sep = "")
  for (n in names(object@base)) {
    val <- if (n == "password") "****" else as.character(object@base[[n]])
    cat("  ", n, ": ", val, "\n", sep = "")
  }
  invisible(NULL)
})

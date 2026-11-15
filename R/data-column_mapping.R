#' River column names
#'
#' Provides the correspondence between EDA standard field names and
#' field names used in external hydrographic datasets.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{dataset}{The name of the dataset, "sudoang", "amber_salmon", "ccm"}
#'   \item{eda_name}{Column name for EDA - and later standard use in the package}
#'   \item{source_column}{Column name in the source dataset}
#'   \item{source_table}{Table name}
#'   \item{source_schema}{Schema name}
#'   \item{source_schema}{Database name}
#' }
#' @examples
#' column_mapping
"column_mapping"



#' Translate country ("France", "Spain", "Portugal", "all") into country codes (internal)
#' @return character vector of country codes, or character(0) for "all"
#' @noRd
.eda_country <- function(country) {
  if (length(country) == 0L || any(country == "all")) {
    return(character(0L))
  }
  level_key <- c("France" = "FR",
                 "Spain" = "SP",
                 "Portugal" = "PT",
                 "Germany" ="DE")
  unsupported <- setdiff(country, names(level_key))
  if (length(unsupported)) {
    stop(paste(unsupported, collapse = ", "), " not supported yet", call. = FALSE)
  }
  unname(level_key[country])
}


#' Translates the columns from different river_networks to the EDA format (internal)
#' @param con The DBI connection
#' @param selected_eda_columns A vector of selected columns to build the river datasets
#' @return a select statement that will translate names to the new format
#' @noRd
.build_select_mapping <- function(
    con,
    selected_eda_columns
) {
  # eda3 river_network
  if (!river_network %in% column_mapping$river_network)
    stop(sprintf("river_network %s not in column mapping, current available river_networks %S",
                 river_network,
                 unique(column_mapping$river_network)))

  # Restrict to requested columns and river_network
  mapping_subset <- column_mapping[column_mapping$river_network == river_network &
                                     column_mapping$eda_name %in% selected_eda_columns]
  # check if some are missing
  unmatched <-  !selected_eda_columns %in% column_mapping$eda_name
  # Build aliases
  select_query_part <- purrr::map_chr(
    seq_len(nrow(mapping_subset)),
    \(i) glue_sql(
      "{`mapping_subset$source_column[i]`} AS {`mapping_subset$eda_name[i]`}",
      .con = con
    )
  )
  return(select_query_part)
}

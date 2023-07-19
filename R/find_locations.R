### find location in description narrative

find_locations <- function(descriptions, locations) {
  matching_locations <- str_extract_all(descriptions,paste0("\\b",locations,"\\b", collapse = "|"))
  matching_locations <- unlist(matching_locations)
  return(matching_locations)
}

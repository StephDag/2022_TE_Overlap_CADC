

# small function to clean character strings
clean_string <- function(x){
  sapply(x, function(y){
    stringi::stri_trans_general(
      str = y, id = "Latin-ASCII") |>
      tolower() |>
      # gsub("-|_|\\.|/", " ", .)
      gsub('[[:punct:]]', " ", x=_) |>
      gsub("\\s+{2,}", " ", x=_) |>
      gsub("^\\s+|\\s+$", "",x=_) 
  }) |>
    as.vector()
}

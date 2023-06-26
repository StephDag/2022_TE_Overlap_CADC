# downlaod MPA and OECM protected areas
# wdpar


coastal_raw_MPA_OECM_data <- wdpa_fetch(
  coastal.ctr$alpha.3, wait = TRUE, download_dir = rappdirs::user_data_dir("data/raw-data"),force=TRUE)
)
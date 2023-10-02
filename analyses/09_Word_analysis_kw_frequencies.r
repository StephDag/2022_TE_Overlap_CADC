# Extract kewords frequencies, and context optionally
# Camille Coux
# August 2023

# load this short function to clean character strings
source("R/clean_string.R")

# copy paste the keyword list from the google doc here (this contains even the 
#  blue keywords for now)
rec_equ_kw <- unlist(strsplit(c("Rights, rights-based, traditional knowledge, respect, power, values, identities, respect, dignity, diverse groups, knowledge system, inclusiv*, Indigenous knowledge, Indigenous ecological knowledge, traditional ecological knowledge, fair*, equit*, justice, injustice, gender, Indigenous peoples, marginalize*, vulnerable peoples, women, youth, champions, leadership, ambassadors"), ","))

# cleaning the list of keywords, but see later on for additional options to 
# make selective choices about keywords
rec_equ_kw <- clean_string(x=rec_equ_kw)

# PDFs should be converted to text files using pdftotext.
# Download the Xpdf command line tools from here: https://www.xpdfreader.com/download.html

# PDFs can be converted rapidly, in batch, as follows :

# run in folder containing PDFs of interest, from (Git bash) terminal command line:
#   ls -1 ./ | \
# while read sample; do
# pdftotext $sample
# done

# list all the converted text files from the folder to check if it worked, 
# and for funky characters:
data_path <- here::here("outputs", "44000_P082492") # you'll probably need to adapt this

# make a vector of PDF file names
myfiles <- list.files(path = data_path, pattern = "pdf",  full.names = TRUE)

# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
lapply(myfiles, function(i) pdf_text,paste0('"', i, '"'), wait = FALSE)

#doc_list <- list.files(data_path, pattern = "\\.txt$")
#Extract the text from the PDF using the command 
txt_output <- pdf_text(myfiles[1])
#Clean up the extracted text using the command 
txt_output <- paste0(txt_output, collapse = " ") %>% 
  stringr::str_squish() %>% 
  clean_string()

# select project document
#txt_file <- doc_list[1] 

# extracts text, removes weird characters and puctuation
#doc <- read.table( paste0(data_path, "/", txt_output), sep = "\t", quote="") |>
#  unlist(x=_) |>
#  paste0(x=_, collapse = " ") |>
#  clean_string()


# COMPUTE FREQUENCIES: this is the clean way, that sticks precisely to the
# listed keywords
df <- dplyr::data_frame(text = txt_output) |> 
  dplyr::mutate(tokens = stringr::str_split(text, "\\s+")) |>
  tidyr::unnest(cols = c(tokens)) |>
  dplyr::filter(tokens %in% rec_equ_kw) |>
  dplyr::count(tokens)

# for a most extensive way, where you could easily sort the keywords afterwards,
# use this:
words <- dplyr::data_frame(text = txt_output) |> 
  dplyr::mutate(tokens = stringr::str_split(text, "\\s+")) |>
  tidyr::unnest(cols = c(tokens)) # keep this to extrcat context
words <- words$tokens

which(words =="power") # in this text, there is no exact match with power, but..
grep("power", words, value = TRUE) # there are a bunch of words that contain it!

# To get an extensive list of all possible matches from an "enriched list" of 
# keywords:
all_kw <- NA
count=1
for (i in rec_equ_kw){
  all_kw <- c(all_kw, unique(grep(paste(i), words, value=TRUE)))
}

all_kw <- c(all_kw[-1], rec_equ_kw) |> 
  unique() |> 
  sort() 

# re-run the previous pipeline using the new list:

df2 <- dplyr::data_frame(text = txt_output) |> 
  dplyr::mutate(tokens = stringr::str_split(text, "\\s+")) |>
  tidyr::unnest(cols = c(tokens)) |>
  dplyr::filter(tokens %in% all_kw) |>
  dplyr::count(tokens)

# and now you can easliy sort through these and ditch those you don't want.

# and once you've made your choice, let me know and we can automatize the process
# for the remaining documents. I'll going to need to know which format you
# would like the output to be like: sorted by document, or should we sum 
# frequencies across all documents and make one big dataframe ?

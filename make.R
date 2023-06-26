#' 2022_TE_Overlap_CADC: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Stephanie D'AGATA \email{stephanie.dagata@gmail.com}
#' 
#' @date 2023/05/29



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
source(here::here("analyses", "00_setup.R")) # setup libraries
source(here::here("analyses", "001_Coastal_countries.R")) # list of coastal countries
source(here::here("analyses", "01_IATA_DB.R")) # retrieve full database filtered by coastal countries
source(here::here("analyses", "02_Clean_GPS.R")) # retrieve missing GPS locations and correct gps locations
  # updates on the 26/06/2023
  





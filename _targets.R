
# LOAD LIBRARIES ----------------------------------------------------------

library(targets)
library(tidyverse)
library(sf)
library(here)
library(tidycensus)
library(tigris)
library(janitor)
library(readxl)
library(mapview)


# SET OPTIONS -------------------------------------------------------------

tar_option_set(
  packages = c("tidyverse",
               "tidyverse",
               "sf",
               "here",
               "tidycensus",
               "tigris",
               "janitor",
               "readxl")
)

# SOURCE R FUNCTIONS -----------------------------------------------------------

tar_source(files = here("R/functions.R"))


# TARGET PIPELINE: FILES --------------------------------------------------

pipeline_files <- list(
  tar_target(file_precincts_2016,
             here("data/Statewide_Prec_2016.zip"),
             format = "file"),
  tar_target(file_election_2016,
             here("data/2016-general-data.zip"),
             format = "file"),
  tar_target(file_latch_2016,
             here("data/Local_Area_Transportation_Characteristics_by_Household_20240326.csv"),
             format = "file")
  
)


# TARGET PIPELINE: DATA -------------------------------------------

pipeline_data <- list(
  tar_target(wa_pop_2010,
             make_wa_pop_2010()),
  tar_target(wa_blocks,
             make_wa_blocks()),
  tar_target(tracts, 
             make_tracts()),
  tar_target(hh_vmt_2012_2016,
             make_hh_vmt_2012_2016(file_latch_2016)),
  tar_target(i732, 
             make_i732(file_precincts_2016, file_election_2016)),
  tar_target(vote_pres_2016, 
             make_vote_pres_2016(file_precincts_2016, file_election_2016))
)

# PROJECT PIPELINE --------------------------------------------------------

list(
  pipeline_files,
  pipeline_data
)

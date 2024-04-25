
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
library(spdep)
library(spatialreg)


# SET OPTIONS -------------------------------------------------------------

tar_option_set(
  packages = c("tidyverse",
               "tidyverse",
               "sf",
               "here",
               "tidycensus",
               "tigris",
               "janitor",
               "readxl",
               "spdep",
               "spatialreg")
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
             format = "file"),
  tar_target(file_senate_dist_2016,
             here("data/tl_2016_53_sldu.zip"),
             format = "file")
  
)


# TARGET PIPELINE: DATA -------------------------------------------

pipeline_data <- list(
  tar_target(model_data,
             make_model_data(hh_vmt_2012_2016, tracts_vote_2016)),
  tar_target(drove_alone_2012_2016,
             make_drove_alone_2012_2016()),
  tar_target(tracts_vote_2016,
             make_tracts_vote_2016(wa_blocks,
                                   wa_pop_2010,
                                   vote_pres_2016, 
                                   i732,
                                   tracts)),
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
             make_vote_pres_2016(file_precincts_2016, file_election_2016)),
  tar_target(senate_dist_2016,
             make_senate_dist_2016(file_senate_dist_2016))
)


# TARGET PIPELINE: MODELS -------------------------------------------------

pipeline_models <- list(
  tar_target(model_spatial_lag,
             make_model_spatial_lag(model_data, model_lm_multivariate, model_spatial_weights)),
  tar_target(model_spatial_weights,
             make_model_spatial_weights(model_data)),
  tar_target(model_lm_multivariate,
             make_model_lm_multivariate(model_data)),
  tar_target(model_lm_univariate,
             make_model_lm_univariate(model_data))
)

# PROJECT PIPELINE --------------------------------------------------------

list(
  pipeline_files,
  pipeline_data,
  pipeline_models
)

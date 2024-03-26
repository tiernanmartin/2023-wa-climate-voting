
# TARGET FUNCTIONS --------------------------------------------------------

make_tracts <- function(){
  
  tracts <- tigris::tracts(state = "WA",
                           year = 2016,
                           cb = TRUE) |> 
    to_proj_crs() |> 
    clean_names()
  
  return(tracts)
}

make_hh_vmt_2012_2016 <- function(latch_fp){
  
  hh_vmt_2012_2016 <- read_csv(latch_fp) |> 
    clean_names() |> 
    select(geoid = geocode,
           hh_vmt = est_vmiles,
           contains("flag_"))
  
  return(hh_vmt_2012_2016)
  
  
}

make_i732 <- function(prec_fp, elec_fp){
  
  # Precincts (2016) 
  
  prec_2016 <- st_read(sprintf("/vsizip/%s", prec_fp),
                       layer = "Statewide_Prec_2016") |> 
    to_proj_crs() |> 
    clean_names()
  
  # 2016 Election results
  
  temp_dir <- tempdir()
  
  xlsx_fp <- "2016-general-data/2016Gen_Precinct_Results_GIS-Ready.xlsx"
  
  unzip(elec_fp, 
        files = xlsx_fp, 
        exdir = temp_dir)
  
  elec_2016 <- read_excel(file.path(temp_dir, xlsx_fp)) |> 
    clean_names() |> 
    select(precinct_code,
           contains("i0732"))
  
  # Join
  
  i732 <- prec_2016 |> 
    select(county, 
           preccode, 
           precname,
           precinct_code = st_code) |> 
    left_join(elec_2016, by = "precinct_code")
  
  
  return(i732)
}

make_vote_pres <- function(prec_fp, elec_fp){
  
  # prec_fp <- here("data/Statewide_Prec_2016.zip")
  # 
  # elec_fp <- here("data/2016-general-data.zip")
  
  
  # Precincts (2016) 
  
  prec_2016 <- st_read(sprintf("/vsizip/%s", prec_fp),
                       layer = "Statewide_Prec_2016") |> 
    to_proj_crs() |> 
    clean_names()
  
  # 2016 Election results
  
  temp_dir <- tempdir()
  
  xlsx_fp <- "2016-general-data/2016Gen_Precinct_Results_GIS-Ready.xlsx"
  
  unzip(elec_fp, 
        files = xlsx_fp, 
        exdir = temp_dir)
  
  elec_2016 <- read_excel(file.path(temp_dir, xlsx_fp)) |> 
    clean_names() 
  
  
  vote_pres <- elec_2016|> 
    select(precinct_code,
           contains("g16pr")) # 'g16pr; is the code for presidential election results
  
  # Join
  
  vote_pres <- prec_2016 |> 
    select(county, 
           preccode, 
           precname,
           precinct_code = st_code) |> 
    left_join(vote_pres, by = "precinct_code")
  
  
  return(vote_pres)
}


make_medianinc_2012_2016 <- function(year = 2016){
  medianinc_2012_2016 <- get_acs(
    geography = "tract",
    variables = "B19013_001", 
    state = "WA",
    year = year, 
    geometry = TRUE 
  ) |> 
    to_proj_crs() |> 
    clean_names() |> 
    tibble() |> 
    st_sf()
  
  return(medianinc_2012_2016)
}

# UTILITY FUNCTIONS -------------------------------------------------------

to_proj_crs <- function(sf){
  st_transform(sf, 6152)
}


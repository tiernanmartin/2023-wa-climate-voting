# source: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2016&layergroup=State+Legislative+Districts

senate_dist_2016_fp <- here("data/tl_2016_53_sldu.zip")

senate_dist_2016 <- st_read(sprintf("/vsizip/%s", senate_dist_2016_fp)) |> 
  to_proj_crs() |> 
  clean_names() |> 
  st_make_valid()

house_dist_2016_fp <- here("data/tl_2016_53_sldl.zip")

house_dist_2016 <- st_read(sprintf("/vsizip/%s", house_dist_2016_fp)) |> 
  to_proj_crs() |> 
  clean_names() |> 
  st_make_valid()

list(senate_dist_2016, house_dist_2016) |> mapview(alpha.regions = 0,color = list("navy",
                                                                                  "tomato"))

# Nearly identical, but Senate legislative districts doesn't include the weird
# boundary situation south of Bellevue (Eastgate and Cougar Hills)

prec_fp <- here("data/Statewide_Prec_2016.zip")

prec_2016 <- st_read(sprintf("/vsizip/%s", prec_fp),
                       layer = "Statewide_Prec_2016") |> 
    to_proj_crs() |> 
    clean_names() |> 
    st_make_valid()

list(senate_dist_2016, prec_2016) |> mapview(alpha.regions = 0,color = list("black",
                                                                    "tomato"))
tar_load(tracts)

list(senate_dist_2016, tracts) |> mapview(alpha.regions = 0,color = list("black",
                                                                            "tomato"))

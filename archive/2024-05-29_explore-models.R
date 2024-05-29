
# SETUP -------------------------------------------------------------------



library(parameters)
library(performance)

tar_load("model_i732_lm_univariate")
tar_load("model_i1631_lm_univariate")
tar_load("model_i732_lm_multivariate")
tar_load("model_i1631_lm_multivariate")
tar_load("model_i732_spatial_lag")
tar_load("model_i1631_spatial_lag")


# MODEL SUMMARIES -----------------------------------------------------------

model_list <- list(
  "i732_uni" = model_i732_lm_univariate,
  "i1631_uni" =   model_i1631_lm_univariate,
  "i732_multi" =    model_i732_lm_multivariate,
  "i1631_multi" =   model_i1631_lm_multivariate,
  "i732_spatial" =   model_i732_spatial_lag,
  "i1631_spatial" =   model_i1631_spatial_lag)

map(model_list, summary, Nagelkerke = TRUE)



# MODEL COEFFICIENTS ------------------------------------------------------


compare_parameters(model_list, 
                   pretty_names = "labels",
                   select = "{estimate}{stars}") 


# MODEL R2 ----------------------------------------------------------------


model_i732_lm_univariate_r2_adj <- r2(model_i732_lm_univariate) |> pluck("R2_adjusted")

model_i732_lm_multivariate_r2_adj <- r2(model_i732_lm_multivariate) |> pluck("R2_adjusted")

model_i732_spatial_lag_r2_adj <- model_i732_spatial_lag |> 
  summary(Nagelkerke = TRUE) |> 
  pluck("NK")

model_i1631_lm_univariate_r2_adj <- r2(model_i1631_lm_univariate) |> pluck("R2_adjusted")

model_i1631_lm_multivariate_r2_adj <- r2(model_i1631_lm_multivariate) |> pluck("R2_adjusted")

model_i1631_spatial_lag_r2_adj <- model_i1631_spatial_lag |> 
  summary(Nagelkerke = TRUE) |> 
  pluck("NK")

tibble(
  "i732_uni" = model_i732_lm_univariate_r2_adj,
  "i1631_uni" = model_i1631_lm_univariate_r2_adj,
  "i732_multi" = model_i732_lm_multivariate_r2_adj,
  "i1631_multi" = model_i1631_lm_multivariate_r2_adj,
  "i732_spatial" = model_i732_spatial_lag_r2_adj,
  "i1631_spatial" = model_i1631_spatial_lag_r2_adj
) 

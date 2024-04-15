tar_load(hh_vmt_2012_2016)
tar_load(tracts_vote_2016)

tmp <- left_join(tracts_vote_2016, hh_vmt_2012_2016) |> 
  transmute(
    geoid,
    hh_vmt,
    vote_rep_pct = vote_rep_pct * 100,
    vote_i0732n_pct = vote_i0732n_pct * 100) |> 
  drop_na()

tmp |> 
  mapview(zcol = "hh_vmt")

tmp |> 
  mapview(zcol = "vote_i0732n_pct")


tmp |> 
  st_drop_geometry() |> 
  pivot_longer(cols = where(is.numeric)) |> 
  mutate(name = factor(name)) |> 
  ggplot() +
  aes(x = value, y = after_stat(density)) +
  geom_histogram() +
  facet_wrap(~name,nrow = 1, scales = "free_x")

tmp |> 
  ggplot() +
  aes(x = hh_vmt) +
  geom_histogram()

tmp |> 
  ggplot() +
  aes(x = vote_i0732n_pct) +
  geom_histogram()

tmp |> 
  ggplot() +
  aes(x = vote_rep_pct) +
  geom_histogram()


tmp |> 
  ggplot() +
  aes(x = vote_i0732n_pct, y = hh_vmt) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

tmp |> 
  ggplot() +
  aes(x = vote_rep_pct, y = hh_vmt) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")




summary(model_lm)

tmp$.resid <- residuals(model_lm)


model_lm |> 
broom::augment() |> 
  ggplot() +
  aes(x = .resid) +
  geom_histogram()

#Cohen's f^2: 0.47
effect_size <- summary(model_lm) |> pluck("r.squared") / (1 - summary(model_lm) |> pluck("r.squared"))


broom::tidy(model_lm)

resid <- model_lm |> broom::augment() |> pluck(".resid")

library(spdep)

wts <- tmp |> 
  poly2nb()  |> 
  nb2listw(zero.policy = TRUE)

moran.test(tmp$.resid, wts)

library(spatialreg)

lag_model <- lagsarlm(
  formula = model_lm, 
  data = tmp, 
  listw = wts,
  zero.policy = TRUE
)

summary(lag_model, Nagelkerke = TRUE)

moran.test(lag_model$residuals, wts)

tmp$.resid_lag <- lag_model$residuals

tmp |> 
  mutate(.resid_lag = abs(.resid_lag)) |>  
mapview(zcol = ".resid_lag")

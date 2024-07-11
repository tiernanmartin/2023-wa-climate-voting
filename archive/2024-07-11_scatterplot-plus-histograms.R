
```{r}
#| label: fig-scatplots

p1 <- model_data_i732 |> 
  drop_na() |> 
  st_drop_geometry() |> 
  mutate(vote_party = case_when(
    vote_rep_pct <= 50 ~ "Democrat",
    vote_rep_pct > 50 ~ "Republican",
    TRUE ~ "Other"
  )) |> 
  mutate(vote_party = factor(vote_party,
                             levels = c("Democrat",
                                        "Republican",
                                        "Other"))) |> 
  ggplot() +
  aes(y = vote_i0732n_pct/100, 
      x = hh_vmt)+
  geom_point(aes(fill = vote_party,
                 color = vote_party),
             alpha = 0.5) +
  geom_smooth(method = "lm",
              se = TRUE,
              color = "black",
              linetype = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("cornflowerblue","tomato")) +
  theme_minimal() +
  guides(fill = FALSE) +
  ylab("Share of 'No' Votes, I-732") +
  xlab("Average Daily Vehicle Miles Traveled Per Household") +
  labs(color = "2016\nPresidential\nElection\nWinner") +
  theme(plot.caption = element_text(hjust = 0))

custom_labels <- c("hh_vmt" = "'No' on I-732 ~ Avg. HH VMT", 
                   "vote_rep_pct" = "'No' on I-732 ~ Pres. Vote Rep."
)

labeller_func <- as_labeller(custom_labels)

p2 <- model_data_i732 |> 
  drop_na() |> 
  st_drop_geometry() |> 
  pivot_longer(cols = c(hh_vmt, vote_rep_pct)) |> 
  mutate(name = factor(name)) |> 
  ggplot() +
  aes(x = value, y = vote_i0732n_pct/100) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  facet_wrap(~name, nrow = 1, scales = "free_x",labeller = labeller_func) + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() + 
  ylab("") +
  xlab("")


p1 + p2 + plot_layout(ncol = 1,
                      heights = c(2,1))
```

library(patchwork)

tar_load("model_data_i732")
tar_load("model_data_i1631")

combined_models <- model_data_i732 |> 
  st_drop_geometry() |> 
  select(-vote_i0732n_pct) |> 
  mutate(initiative = "I-732") |> 
  bind_rows(
    model_data_i1631 |> 
      st_drop_geometry() |> 
      select(-vote_i1631n_pct) |> 
      mutate(initiative = "I-1631")
  ) |> 
  drop_na() |> 
  mutate(vote_party = case_when(
    vote_rep_pct <= 50 ~ "Democrat",
    vote_rep_pct > 50 ~ "Republican",
    TRUE ~ "Other"
  )) |> 
  mutate(initiave = factor(initiative,
                           levels = c("I-1631",
                                      "I-732")),
         vote_party = factor(vote_party,
                             levels = c("Democrat",
                                        "Republican",
                                        "Other")))
  
  
combined_models |> 
  ggplot() +
  aes(y = vote_no_pct/100, 
      x = hh_vmt,
      group = initiative) +
  geom_point(aes(fill = vote_party,
                 color = vote_party),
             alpha = 0.5) +
  # geom_smooth(aes(color = vote_party, group = vote_party),
  #             method = "lm",
  #             se = TRUE,
  #             # color = "black",
  #             linetype = 1) +
  geom_smooth(aes(group = vote_party, fill = vote_party),
              method = "lm",
              se = TRUE,
              color = "black",
              linewidth = 0.66,
              linetype = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("cornflowerblue","tomato")) +
  scale_fill_manual(values = c("cornflowerblue","tomato")) +
  facet_wrap(~ fct_rev(initiative), nrow = 1) +
  theme_bw() +
  guides(fill = FALSE) +
  ylab("Share of 'No' Votes") +
  xlab("Average Daily Vehicle Miles Traveled Per Household") +
  labs(color = "Partisanship",
       caption = "Sources: Local Area Transportation Characteristics for Households, 2017;\nWashington Secretary of State: Election Division, 2016 and 2018;\nFuturewise, 2024") +
  theme(plot.background = element_rect(fill = "white",color = "white"),
        plot.caption = element_text(hjust = 0))
  
ggsave("plots/2024-05-17_scatterplots-both-intiatives.png",scale = 2,
       width = 4.5, units = "in")




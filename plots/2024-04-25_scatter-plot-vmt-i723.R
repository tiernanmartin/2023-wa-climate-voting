tar_load(model_data)

model_data |> 
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
  theme_bw() +
  guides(fill = FALSE) +
  ylab("Share of 'No' Votes, I-0732") +
  xlab("Average Daily Vehicle Miles Traveled Per Household") +
  labs(title = "Positive Relationship Between Daily VMT and\nOpposition to Initiative 732",
       subtitle = "Census Tracts in Washington State",
       color = "2016\nPresidential\nElection\nWinner",
       caption = "Sources: Local Area Transportation Characteristics for Households, 2017;\nWashington Secretary of State: Elections Division, 2016; Futurewise, 2024") +
  theme(plot.caption = element_text(hjust = 0))

ggsave("plots/2024-04-25_scatter-plot-vmt-i723.png",
       dpi = 300,
       width = 605,height = 516, units = "px")

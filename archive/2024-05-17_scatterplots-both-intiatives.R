source("_targets.R")

library(viridis)

tar_load("model_data_i732")
tar_load("model_data_i1631")

# Define the bins and labels
bins <- c(-Inf, 20, 30, 40, 50, 60, 70, Inf)

levels <- factor()

labels <- c("Dem +30","Dem +20", "Dem +10", "Close", "Rep +10", "Rep +20","Rep +30")

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
                                        "Other")),
         vote_partisan = vote_rep_pct - 50,
         vote_grouped = case_when(
           vote_partisan > 30 ~ "Rep +30",
           vote_partisan > 20 ~ "Rep +20",
           vote_partisan > 10 ~ "Rep +10",
           vote_partisan >= -10 & vote_partisan <= 10 ~ "Close",
           vote_partisan > -20 ~ "Dem +10",
           vote_partisan > -30 ~ "Dem +20",
           TRUE ~ "Dem +30"
         ),
         vote_grouped = factor(vote_grouped,levels = labels, ordered = TRUE)
         )
  
# Define custom discrete colors

palette_function <- colorRampPalette(c("cornflowerblue", "tomato"))

redblue_palette <- palette_function(7)

combined_models |> 
  ggplot() +
  aes(y = vote_no_pct/100, 
      x = hh_vmt,
      group = initiative) +
  geom_point(aes(color = vote_grouped,
                 fill = vote_grouped),
             shape = 21,
             alpha = 0.5) + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = redblue_palette,
                    guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = redblue_palette,
                    guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ fct_rev(initiative), nrow = 1) +
  theme_bw() +
  guides(color = FALSE) +
  ylab("Initiative 'No' Votes") +
  xlab("Average Daily Vehicle Miles Traveled Per Household") +
  labs(fill = "Partisanship",
       caption = "\nData points are census tracts in Washington State.\nPartisanship is measured by the 2016 Presidential election results for I-732\nand the 2018 Congressional Senate results for I-1631.\n\nSources: Local Area Transportation Characteristics for Households, 2017;\nWashington Secretary of State: Election Division, 2016 and 2018;\nFuturewise, 2024") +
  theme(plot.background = element_rect(fill = "white",color = "white"),
        plot.caption = element_text(hjust = 0))
  
ggsave("plots/2024-06-05_scatterplots-both-intiatives.png",
       scale = 1.5, width = 5, height = 3, units = "in")

combined_models |> 
  ggplot() +
  aes(y = vote_no_pct/100, 
      x = hh_vmt,
      group = initiative) +
  facet_wrap(~ fct_rev(initiative), nrow = 1) +
  geom_density2d(color = "black", alpha = 1,linetype = 1) +
  geom_point(aes(color = vote_grouped), alpha = 0.25) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = redblue_palette,
                     guide = guide_legend(reverse = TRUE)) + 
  theme_bw() +
  guides() +
  ylab("Initiative 'No' Votes") +
  xlab("Average Daily Vehicle Miles Traveled Per Household") +
  labs(color = "Political\nPartisanship",
       caption = "\nThe hexagons represent aggregated groups of census tracts,\nwith each hexagon's color intensity indicating the count of census tracts within that group.\nThis study includes all populated census tracts in Washington State.\n\nSources: Local Area Transportation Characteristics for Households, 2017;\nWashington Secretary of State: Election Division, 2016 and 2018;\nFuturewise, 2024") +
  theme(plot.background = element_rect(fill = "white",color = "white"),
        plot.caption = element_text(hjust = 0))



combined_models |> 
  ggplot() +
  aes(y = vote_no_pct/100, 
      x = hh_vmt,
      group = initiative) +
  geom_hex(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(option="magma", direction = -1) + 
  facet_wrap(~ fct_rev(initiative), nrow = 1) +
  theme_bw() +
  guides() +
  ylab("Initiative 'No' Votes") +
  xlab("Average Daily Vehicle Miles Traveled Per Household") +
  labs(fill = "Count of\nCensus\nTracts",
       caption = "\nThe hexagons represent aggregated groups of census tracts,\nwith each hexagon's color intensity indicating the count of census tracts within that group.\nThis study includes all populated census tracts in Washington State.\n\nSources: Local Area Transportation Characteristics for Households, 2017;\nWashington Secretary of State: Election Division, 2016 and 2018;\nFuturewise, 2024") +
  theme(plot.background = element_rect(fill = "white",color = "white"),
        plot.caption = element_text(hjust = 0))

ggsave("plots/2024-06-05_hexbin-both-intiatives.png",
       scale = 1.5, width = 5, height = 3, units = "in")

combined_models |> 
  ggplot() +
  aes(y = vote_no_pct/100, 
      x = vote_rep_pct/100,
      group = initiative) +
  geom_hex(color = "black") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(option="magma", direction = -1) + 
  facet_wrap(~ fct_rev(initiative), nrow = 1) +
  theme_bw() +
  guides() +
  ylab("Initiative 'No' Votes") +
  xlab("Share of Votes for Republican Candidate") +
  labs(fill = "Count of\nCensus\nTracts",
       caption = "\nThe hexagons represent aggregated groups of census tracts,\nwith each hexagon's color intensity indicating the count of census tracts within that group.\nThis study includes all populated census tracts in Washington State.\n\nSources: Local Area Transportation Characteristics for Households, 2017;\nWashington Secretary of State: Election Division, 2016 and 2018;\nFuturewise, 2024") +
  theme(plot.background = element_rect(fill = "white",color = "white"),
        plot.caption = element_text(hjust = 0))

ggsave("plots/2024-06-05_hexbin-both-intiatives-partisanship.png",
       scale = 1.5, width = 5, height = 3, units = "in")

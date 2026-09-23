tribble(
  ~district, ~municipality, ~individuals_reached,
  "Rasuwa",	"Aamachhodingmo",	4020,
  "Rasuwa",	"Gosaikunda",	10178,
  "Rasuwa",	"Kalika",	1948,
  "Rasuwa",	"Uttargaya",	6172,
  "Nuwakot",	"Belkotgadhi",	1665,
  "Nuwakot",	"Bidur",	20148,
  "Nuwakot",	"Kispang",	4293,
  "Nuwakot",	"Tarakeshwor",	1408,
  "Dhading",	"Benighat Rorang",	5000,
  "Dhading",	"Gajuri",	2454,
  "Dhading",	"Galchhi",	5899,
  "Dhading",	"Siddhalek",	1078
) |> 
  left_join(
    nepal_admin |> 
      distinct(adm2_name, adm2_pcode, adm3_name, adm3_pcode), 
    by = c("district" = "adm2_name", 
           "municipality" = "adm3_name")
  ) |> 
  left_join(
    municipal_damage_losses |> 
      filter(!is.na(impacted_buildings)) |> 
      select(adm3_pcode, impacted_buildings), 
    by = "adm3_pcode"
  ) |> 
  relocate(impacted_buildings, .before = individuals_reached) |> 
  pivot_longer(cols = impacted_buildings:individuals_reached, 
               names_to = "variable",
               values_to = "value") |> 
  mutate(variable = case_when(
    variable == "estimated_affected_population" ~ "Affected Population (Flash Appeal)", 
    variable == "impacted_buildings" ~ "Impacted Buildings",
    variable == "individuals_reached" ~ "Individuals Reached (5Ws)", 
    TRUE ~ NA_character_
  )) |> 
  mutate(variable = fct_relevel(variable, c("Impacted Buildings", 
                                            "Affected Population (Flash Appeal)", 
                                            "Individuals Reached (5Ws)"))) |> 
  filter(variable != "Affected Population (Flash Appeal)") |> 
  ggplot(aes(x = fct_relevel(municipality, 
                             c("Bidur", "Gosaikunda", "Benighat Rorang", "Kispang", "Uttargaya",
                               "Gandaki", "Siddhalek", "Belkotgadhi", "Kalika", "Gajuri",
                               "Ichchha Kamana", "Galchhi", "Aamachhodingmo", "Tarakeshwor", 
                               "Shahid Lakhan", "Devghat", "Aanbu Khaireni", "Other")), 
             y = value)) +
  geom_col(aes(fill = district)) + 
  geom_text(aes(label = comma(value)), 
            vjust = "inward",
            size = 3) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(
    "Chitawan" = "#cee3a0", 
    "Dhading" = "#72bf44", 
    "Gorkha" = "#e6efd0", 
    "Nuwakot" = "#64bdea", 
    "Rasuwa" = "#c5dfef", 
    "Tanahu" = "#0074b7")
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1), 
        strip.background = element_rect(fill = "black")) + 
  facet_wrap(~ variable, nrow = 3, scales = "free_y") +
  labs(x = "", y = "", fill = "", 
       title = "Comparison between Affected Municipalities", 
       caption = "Source: NDRRMA RDNA; UN Nepal and its humanitarian partners; 5Ws 20260923.")
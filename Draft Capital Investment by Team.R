
setwd("C:/Users/sethl/OneDrive/Excel Files/Football/Draft/WAR")

Draft_WAR_add <- read_xlsx("NFL WAR Draft Chart.xlsm")

draftpicks <- load_draft_picks(2007:2023)%>% 
  left_join(Draft_WAR_add, by = c("pick" = "Pick Number")) %>% 
  mutate(team = case_when(team == "TAM" ~ "TB",
                          team == "KAN" ~ "KC",
                          team == "GNB" ~ "GB",
                          team == "LAR" ~ "LA",
                          team == "STL" ~ "LA",
                          team == "LVR" ~ "LV",
                          team == "OAK" ~ "LV",
                          team == "SDG" ~ "LAC",
                          team == "SFO" ~ "SF",
                          team == "NWE" ~ "NE",
                          team == "NOR" ~ "NO",
                          T ~ team))

InvestmentByTeam <- draftpicks %>% 
  group_by(season, team, category) %>% 
  summarise(Capital = sum(`Regressed Mean 4-Year WAR`, na.rm = T)) %>% 
  mutate(Season = substr(season, 3, 4)) %>% 
  left_join(nfl_colors, by = c("team" = "Code")) %>% 
  left_join(nfl_logos, by = c("team" = "team_code"))


ggplot(InvestmentByTeam %>% 
         filter(team == "TB"), aes(x = Season, y = Capital))+
  facet_wrap(~category)+
  geom_col(aes(color = secondary, fill = primary))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.4)+
  geom_hline(yintercept = 1.2, linetype = "dashed", alpha = 0.4)+
  theme_reach()+
  theme(axis.text.x = element_text(size=8))+
  scale_color_identity(aesthetics = c("color", "fill"))+
  #scale_x_continuous(breaks = seq(2007,2030, 1))+
  labs(title = paste0(
    # input$seasons3,
    # " ",
    # input$posgroup3,
    " ", "Wins Above Replacement"),
    subtitle = "Sum of Wins Above Replacement by position group, min 150 snaps",
    y = "WAR",
    x = "Team",
    caption = "@SethDataScience"
  )

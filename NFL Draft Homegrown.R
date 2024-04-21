
# Homegrown Talent Percent ------------------------------------------------

Draft_Picks <- Draft_Picks_2000_2020 %>% 
  full_join(X2021_Draft_Picks) %>% 
mutate(DraftTeam = case_when(DraftTeam == "HST" ~ "HOU",
                             DraftTeam == "CLV" ~ "CLE",
                             DraftTeam == "SD" ~ "LAC",
                             DraftTeam == "BLT" ~ "BAL",
                             DraftTeam == "OAK" ~ "LV",
                             DraftTeam == "ARZ" ~ "ARI",
                             DraftTeam == "SL" ~ "LA",
                             DraftTeam == "WAS" ~ "WAS",
                             T ~ DraftTeam),
       Age = case_when(is.na(Age) == 1 ~ 22.08,
                       T ~ Age)) 

Homegrown <- NFLWARDraftData %>% 
  left_join(Draft_Picks, by = c("player_id")) %>% 
  left_join(CoachingChanges1999, by = c("season", "team_name" = "Team")) %>% 
  group_by(season, team_name) %>% 
  mutate(DraftedbyTeam = case_when(team_name == DraftTeam ~ 1,
                                   team_name != DraftTeam ~ -1,
                                   is.na(DraftTeam) ~ 0.5)) %>% 
  summarise(Count = n(),
            WAR = sum(PosWAR, na.rm = T),
            NonQBWAR = sum(PosWAR[position != "QB"], na.rm = T),
            Drafted = sum(DraftedbyTeam[DraftedbyTeam == 1], na.rm = T),
            DraftedPerc = Drafted/Count,
            DraftedWAR = sum(PosWAR[DraftedbyTeam == 1], na.rm = T),
            DraftedAge = mean(Age, na.rm = T),
            NonQBDraftedWAR = sum(PosWAR[position != "QB" & DraftedbyTeam == 1], na.rm = T),
            NonDrafted = -1*sum(DraftedbyTeam[DraftedbyTeam == -1], na.rm = T),
            NonDraftedPerc = NonDrafted/Count,
            NonDraftedWAR = sum(PosWAR[DraftedbyTeam == -1], na.rm = T),
            NonQBNonDraftedWAR = sum(PosWAR[DraftedbyTeam == -1 & position != "QB"], na.rm = T))


HomegrownGM <- NFLWARDraftData %>% 
  left_join(Draft_Picks, by = c("player_id")) %>% 
  left_join(CoachingChanges1999, by = c("season", "team_name" = "Team")) %>% 
  group_by(season, team_name) %>% 
  mutate(DraftedbyTeam = case_when(team_name == DraftTeam ~ 1,
                                   team_name != DraftTeam ~ -1,
                                   is.na(DraftTeam) ~ 0.5)) %>% 
  group_by(GM) %>% 
  summarise(LastTeam = tail(team_name, 1),
            LastSeason = tail(season, 1),
            Count = n(),
            WAR = mean(PosWAR, na.rm = T),
            NonQBWAR = mean(PosWAR[position != "QB"], na.rm = T),
            Drafted = sum(DraftedbyTeam[DraftedbyTeam == 1], na.rm = T),
            DraftedPerc = Drafted/Count,
            DraftedWAR = mean(PosWAR[DraftedbyTeam == 1], na.rm = T),
            DraftedAge = mean(Age, na.rm = T),
            NonQBDraftedWAR = mean(PosWAR[position != "QB" & DraftedbyTeam == 1], na.rm = T),
            NonDrafted = -1*sum(DraftedbyTeam[DraftedbyTeam == -1], na.rm = T),
            NonDraftedPerc = NonDrafted/Count,
            NonDraftedWAR = mean(PosWAR[DraftedbyTeam == -1], na.rm = T),
            NonQBNonDraftedWAR = mean(PosWAR[DraftedbyTeam == -1 & position != "QB"], na.rm = T))


write_xlsx(HomegrownGM, path = "HomegrownGM.xlsx")

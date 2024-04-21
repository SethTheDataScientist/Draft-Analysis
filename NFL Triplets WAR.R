

NFLWARTripletsWAR <- DefWARJoin %>% 
  full_join(OLWARJoin) %>% 
  full_join(WRWARJoin) %>% 
  full_join(RBWARJoin) %>% 
  full_join(QBWARJoin) %>% 
  filter(Snaps >= 150) %>% 
  group_by(season, player_id) %>% 
  mutate(PosWAR = sum(WAR)) %>% 
  slice_head(n = 1) %>% 
  select(season, team_name, position, player, Snaps, player_id, PosWAR) %>% 
  group_by(season, team_name) %>% 
  filter(position == "HB" |
           position == "WR" | 
           position == "TE") %>% 
  arrange(desc(PosWAR)) %>% 
  mutate(TotalOffValue = sum(PosWAR)) %>% 
  slice_head(n = 3) %>% 
  summarise(TotalOffValue = head(TotalOffValue, 1),
            Value = sum(PosWAR),
            Ratio = Value/TotalOffValue,
            TotalSnaps = sum(Snaps),
            ValuePerSnap = Value/TotalSnaps,
            Player1 = player[1],
            Player1WAR = PosWAR[1],
            Player1Snaps = Snaps[1],
            Player2 = player[2],
            Player2WAR = PosWAR[2],
            Player2Snaps = Snaps[2],
            Player3 = player[3],
            Player3WAR = PosWAR[3],
            Player3Snaps = Snaps[3]) %>% 
  filter(Ratio >= 0) %>% 
  group_by() %>% 
  mutate(TotalPR = percent_rank(TotalOffValue),
         ValuePR = percent_rank(Value),
         RatioPR = 1-percent_rank(Ratio),
         ValuePerSnapPR = percent_rank(ValuePerSnap),
         BestSkillGroup = (TotalPR*2 + ValuePR*2 + RatioPR + ValuePerSnapPR)/6) %>% 
  group_by(team_name) %>% 
  mutate(BestWeapons = mean(BestSkillGroup),
         Recent18BestWeapons = mean(BestSkillGroup[season >= 2018]),
         Recent20BestWeapons = mean(BestSkillGroup[season >= 2020]))

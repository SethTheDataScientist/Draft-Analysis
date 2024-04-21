
# NFL Height per Receiving Option by Team -------------------------------
library(nflreadr)

NFLPlayers <- nflreadr::load_players(seasons = 2011:2022)

NFLRosters <- nflreadr::load_rosters(seasons = 2011:2022)

RosterHeight <- NFLRosters %>% 
  filter(status == "Active") %>% 
  group_by(season, team) %>% 
  mutate(Height = as.numeric(substr(height, 1, 1))*12 + 
           as.numeric(substr(height, grepl("-", height)+2, 30)),
         Density = as.numeric(weight)/Height,
         Posgroup = case_when(position == "WR" |
                                position == "TE" ~ "REC",
                              T ~ position)) %>% 
  group_by(season, team) %>% 
  summarise(Count = n(),
            Height = mean(Height, na.rm = T),
            Density = mean(Density, na.rm = T))

# NFL War per Dollar by Team ----------------------------------------------
NFL_Conversion_Names <- read_excel("C:/Users/sethl/OneDrive/Excel Files/Football/Data Csv/NFL Conversion Names.xlsm")

NFL_Conv_Short <- NFL_Conversion_Names %>% 
  filter(Alternate != "OAK", Alternate != "STL", Alternate != "SL",
         Alternate != "SDG") %>% 
  mutate(Team = case_when(Team == "Football Team" ~ "Commanders",
                          T ~ Team))

##NFL TRADES from nflreadr might be able to tell me direction of trades
NFLtrades <- nflreadr::load_trades() %>% 
  arrange(desc(season)) %>% 
  group_by(pfr_name) %>% 
  slice_head(n = 1) %>% 
  select(received, pfr_name) %>% 
  mutate(received = case_when(received == "OAK" ~ "LV",
                              received == "SD" ~ "LAC",
                              received == "SL" ~ "LA",
                              received == "STL" ~ "LA",
                              received == "LAR" ~ "LA"))

SalaryCap <- NFL_Salary_Cap_per_Year %>% 
  mutate(CapPerc = max(Cap)/Cap)

NFLContracts <- nflreadr::load_contracts() 

NFLContracts1 <- NFLContracts %>% 
  arrange(desc(year_signed)) %>% 
  group_by(player) %>% 
  slice_head(n = 1) %>% 
  unnest(cols = c(cols), names_repair = "unique") %>% 
  mutate(player = ifelse(player == "Shaquille Mason", "Shaq Mason", player),
         team = `team...25`) %>% 
  left_join(NFLtrades, by = c("player" = "pfr_name"))  %>% 
  left_join(NFL_Conv_Short, by = c("team" = "Team")) %>% 
  mutate(team = case_when(is.na(Code) == 1 ~ received,
                          T ~ Code)) %>% 
  filter(!is.na(team), year != "Total", between(year, 2011, 2022))  %>% 
  mutate(year = as.numeric(year)) %>% 
  distinct() %>% 
  left_join(SalaryCap, by =c ("year")) %>% 
  mutate(QB = case_when(position == "QB" ~ 1, 
                        T ~ 0),
         Off = case_when(position == "QB" |
                           position == "OG" |
                           position == "G" |
                           position == "C" |
                           position == "T" |
                           position == "OT" |
                           position == "WR" |
                           position == "TE" |
                           position == "RB" |
                           position == "FB"  ~ 1,
                         T ~ 0),
         NonQBOff = case_when(position == "OG" |
                                position == "G" |
                                position == "C" |
                                position == "T" |
                                position == "OT" |
                                position == "WR" |
                                position == "TE" |
                                position == "RB" |
                                position == "FB"  ~ 1,
                              T ~ 0),
         Def = case_when(position == "DT" |
                           position == "DI" |
                           position == "ED" |
                           position == "DE" |
                           position == "LB" |
                           position == "OLB" |
                           position == "CB" |
                           position == "S" |
                           position == "DB" |
                           position == "SS" |
                           position == "SCB" |
                           position == "NCB" |
                           position == "FS"  ~ 1,
                         T ~ 0),
         position = case_when(position == "RB" ~ "HB",
                              position == "IDL" ~ "DI",
                              position == "LT" ~ "T",
                              position == "LG" ~ "G", 
                              position == "RT" ~ "T",
                              position == "RG" ~ "G", 
                              T ~ position),
         Cap = cap_number * CapPerc)  %>% 
  group_by(year, team) %>% 
  summarise(TeamCap = sum(Cap),
            QBCap = sum(Cap[QB == 1]),
            OffCap = sum(Cap[Off == 1]),
            NonQBOffCap = sum(Cap[NonQBOff == 1]),
            DefCap = sum(Cap[Def == 1])) %>% 
  left_join(NFLWAR, by = c("team" = "team_name", 
                           "year" = "season")) %>% 
  left_join(nfl_logos, by =c("team" = "team_code")) %>% 
  mutate(TeamWarperDollar = TeamWAR/TeamCap,
         QBWarperDollar = QBWAR/QBCap,
         OffWarperDollar = OffWAR/OffCap,
         NonQBOffWarperDollar = NonQBOffWAR/NonQBOffCap,
         DefWarperDollar = DefWAR/DefCap,
  )

nflfill <- NFLWARFill %>% 
  select(position) %>% 
  distinct()

nflcontractfill <- NFLContracts %>% 
  select(position) %>% 
  distinct()

NFLWAR <- NFLWARFill %>% 
  mutate(QB = case_when(position == "QB" ~ 1, 
                      T ~ 0),
         Off = case_when(position == "QB" |
                                position == "OG" |
                                position == "G" |
                                position == "C" |
                                position == "T" |
                                position == "OT" |
                                position == "WR" |
                                position == "TE" |
                                position == "RB" |
                                position == "FB"  ~ 1,
                              T ~ 0),
         NonQBOff = case_when(position == "OG" |
                           position == "G" |
                           position == "C" |
                           position == "T" |
                           position == "OT" |
                           position == "WR" |
                           position == "TE" |
                           position == "RB" |
                           position == "FB"  ~ 1,
                         T ~ 0),
         Def = case_when(position == "DT" |
                       position == "DI" |
                       position == "ED" |
                       position == "DE" |
                       position == "LB" |
                       position == "OLB" |
                       position == "CB" |
                       position == "S" |
                       position == "DB" |
                       position == "SS" |
                       position == "SCB" |
                       position == "NCB" |
                       position == "FS"  ~ 1,
                     T ~ 0),
         position = ) %>% 
  group_by(season, team_name) %>% 
  summarise(TeamWAR = sum(WAR),
            QBWAR = sum(WAR[QB == 1]),
            OffWAR = sum(WAR[Off == 1]),
            NonQBOffWAR = sum(WAR[NonQBOff == 1]),
            DefWAR = sum(WAR[Def == 1])) 


##Want to change to make it be able to reflect by position. Would need to adjust both the NFLWARFill and the NFLContracts to match position labels. Then in the below ggplot, simple filter out to be team/season/position combination you are interested in.


ggplot(NFLContracts1 %>% 
         filter(year == 2020),
       aes(x = TeamCap, y = TeamWAR))+
  geom_image(aes(image = url))+
  geom_smooth(method = "lm", se = F)+
  geom_hline(yintercept = mean(NFLContracts1$TeamWAR[NFLContracts1$year == 2021], na.rm = T), linetype = "dashed")+
  geom_vline(xintercept = mean(NFLContracts1$TeamCap[NFLContracts1$year == 2021], na.rm = T), linetype = "dashed")+
  # scale_x_continuous(breaks = seq()) +
  # scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
  #                    labels = scales::percent_format(accuracy = 1))+
  labs(
    title= 2021,
    caption = "@SethDataScience"
  ) +
  theme_reach()  
  # ggrepel::geom_text_repel(aes(label = year), size = 4, box.padding = 0.2,
  #                          force = 10, max.overlaps = Inf,
  #                          min.segment.length = 0)
  
  
test <- NFLContracts1 %>% 
  group_by(team) %>% 
  filter(year >= 2017) %>% 
  summarise(mean(TeamCap, na.rm = T))

# NFL Defenders in the box ------------------------------------------------

NFLParticipation <- nflreadr::load_participation()


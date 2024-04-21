RAS_Scores <- read_csv("C:/Users/sethl/OneDrive/Excel Files/Football/Data Csv/Draft Data/RAS Scores.csv")


RAS <- RAS_Scores %>% 
  mutate(AltName = tolower(gsub("'", "", Name)),
         AltName = gsub("[''[:punct:]]", "", AltName),
         AltName = gsub(" jr", "", AltName),
         AltName = gsub(" sr", "", AltName),
         AltName = gsub(" iii", "", AltName),
         AltName = gsub(" ii", "", AltName),
         AltName = gsub(" iv", "", AltName),
         AltName = gsub(" v", "", AltName)) %>% 
  separate(AltName, c("Altfirst_name", "Altlast_name"), sep = " ", remove = F) 

convert_to_inches <- function(string) {
  parts <- strsplit(string, "-")[[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- (feet * 12) + inches
  return(total_inches)
}

size <- load_combine() %>% 
  mutate(height = sapply(ht, convert_to_inches)) %>% 
  select(pfr_id, height, wt)

college_avg <- CollegeWARJoin%>% 
  group_by(player_id) %>% 
  arrange(desc(season)) %>% 
  mutate(careerAvg = mean(WAR, na.rm = T))%>% 
  mutate(team_name = case_when(player_id == 10658 & season == 2022 ~ "PIT",
                               T ~ team_name),
         player = case_when(player == "Sean MurphyBunting" ~ "Sean Bunting",
                            player == "Tank Dell" ~ "Nathaniel Dell",
                            player == "Mitch Trubisky" ~ "Mitchell Trubisky",
                            
                            
                            
                            
                            player == "Mike Onwenu" ~ "Michael Onwenu",
                            player == "Cam Fleming" ~ "Cameron Fleming",
                            player == "Ogbo Okoronkwo" ~ "Ogbonnia Okoronkwo",
                            player == "Matthew Slater Trubisky" ~ "Matt Slater",
                            player == "Joe Noteboom" ~ "Joseph Noteboom",
                            player == "Patrick OConnor" ~ "Pat OConnor",
                            player == "Cam Lewis" ~ "Cameron Lewis",
                            player == "Sua Opeta" ~ "Iosua Opeta",
                            player == "Trent Scott" ~ "Trenton Scott",
                            player == "Nick Williams" ~ "Nicholas Williams",
                            player == "CJ GardnerJohnson" ~ "Chauncey GardnerJohnson",
                            
                            
                            
                            
                            
                            player == "Scotty Miller" ~ "Scott Miller",
                            player == "Cam Erving" ~ "Cameron Erving",
                            player == "Jeff Wilson Jr." ~ "Jeffery Wilson",
                            player == "Ugo Amadi" ~ "Ugochukwu Amadi",
                            player == "Yosh Nijman" ~ "Yosuah Nijman",
                            player == "Trent Brown" ~ "Trenton Brown",
                            player == "Jacob Martin" ~ "Jake Martin",
                            player_id == 50532 ~ "LJ2",
                            T ~ player) ,
         AltName = tolower(gsub("'", "", player)),
         AltName = gsub("[''[:punct:]]", "", AltName),
         AltName = gsub(" jr", "", AltName),
         AltName = gsub(" sr", "", AltName),
         AltName = gsub(" iii", "", AltName),
         AltName = gsub(" ii", "", AltName),
         AltName = gsub(" iv", "", AltName),
         AltName = gsub(" v", "", AltName)) %>% 
  separate(AltName, c("Altfirst_name", "Altlast_name"), sep = " ", remove = F)  %>% 
  select(player_id, AltName, collegeAvg) %>% 
  distinct()


player_avg <- IdivWARData %>% 
  group_by(player_id) %>% 
  arrange(desc(season)) %>% 
  mutate(careerAvg = mean(WAR, na.rm = T))%>% 
  mutate(team_name = case_when(player_id == 10658 & season == 2022 ~ "PIT",
                               T ~ team_name),
         player = case_when(player == "Sean MurphyBunting" ~ "Sean Bunting",
                            player == "Tank Dell" ~ "Nathaniel Dell",
                            player == "Mitch Trubisky" ~ "Mitchell Trubisky",
                            
                            
                            
                            
                            player == "Mike Onwenu" ~ "Michael Onwenu",
                            player == "Cam Fleming" ~ "Cameron Fleming",
                            player == "Ogbo Okoronkwo" ~ "Ogbonnia Okoronkwo",
                            player == "Matthew Slater Trubisky" ~ "Matt Slater",
                            player == "Joe Noteboom" ~ "Joseph Noteboom",
                            player == "Patrick OConnor" ~ "Pat OConnor",
                            player == "Cam Lewis" ~ "Cameron Lewis",
                            player == "Sua Opeta" ~ "Iosua Opeta",
                            player == "Trent Scott" ~ "Trenton Scott",
                            player == "Nick Williams" ~ "Nicholas Williams",
                            player == "CJ GardnerJohnson" ~ "Chauncey GardnerJohnson",
                            
                            
                            
                            
                            
                            player == "Scotty Miller" ~ "Scott Miller",
                            player == "Cam Erving" ~ "Cameron Erving",
                            player == "Jeff Wilson Jr." ~ "Jeffery Wilson",
                            player == "Ugo Amadi" ~ "Ugochukwu Amadi",
                            player == "Yosh Nijman" ~ "Yosuah Nijman",
                            player == "Trent Brown" ~ "Trenton Brown",
                            player == "Jacob Martin" ~ "Jake Martin",
                            player_id == 50532 ~ "LJ2",
                            T ~ player) ,
         AltName = tolower(gsub("'", "", player)),
         AltName = gsub("[''[:punct:]]", "", AltName),
         AltName = gsub(" jr", "", AltName),
         AltName = gsub(" sr", "", AltName),
         AltName = gsub(" iii", "", AltName),
         AltName = gsub(" ii", "", AltName),
         AltName = gsub(" iv", "", AltName),
         AltName = gsub(" v", "", AltName)) %>% 
  separate(AltName, c("Altfirst_name", "Altlast_name"), sep = " ", remove = F)  %>% 
  select(player_id, AltName, careerAvg) %>% 
  distinct()

draft_picks <- load_draft_picks() %>% 
  filter(season >= 2011,
         round < 4) %>% 
  mutate(PFR_Altname = tolower(gsub("'", "", pfr_player_name)),
         PFR_Altname = gsub("[''[:punct:]]", "", PFR_Altname),
         PFR_Altname = gsub(" jr", "", PFR_Altname),
         PFR_Altname = gsub(" sr", "", PFR_Altname),
         PFR_Altname = gsub(" iii", "", PFR_Altname),
         PFR_Altname = gsub(" ii", "", PFR_Altname),
         PFR_Altname = gsub(" iv", "", PFR_Altname),
         PFR_Altname = gsub(" v", "", PFR_Altname)) %>% 
  separate(PFR_Altname, c("playerfirst_name", "playerlast_name"), sep = " ", remove = F) %>% 
  left_join(RAS, by = c("PFR_Altname" = "AltName",
                        "position" = "Pos",
                        "college" = "College",
                        "season" = "Year")) %>% 
  left_join(RAS, by = c("playerlast_name" = "Altlast_name",
                        "position" = "Pos",
                        "college" = "College",
                        "season" = "Year")) %>% 
  left_join(RAS, by = c("pfr_player_name" = "Name")) %>% 
  left_join(RAS, by = c("PFR_Altname" = "Name")) %>% 
  left_join(RAS, by = c("PFR_Altname" = "AltName")) %>% 
  mutate(Pos = coalesce(Pos.x, Pos.y, Pos),
         College = coalesce(College.x, College.y, College),
         AllTime = coalesce(AllTime.x, AllTime.y, AllTime.x.x, AllTime.y.y, AllTime)) %>% 
  left_join(size, by = c('pfr_player_id' = 'pfr_id')) %>% 
  left_join(player_avg, by = c('PFR_Altname' = 'AltName')) %>% 
  left_join(college_avg, by = c('PFR_Altname' = 'AltName')) %>% 
  select(pfr_player_name, height, wt, College, Pos, AllTime, season, round, pick, collegeAvg, careerAvg)  %>% 
  filter(Pos == "DE" | Pos == "LB") %>% 
  distinct()

write_xlsx(draft_picks, "edge rusher hit rate.xlsx")

current_edges <- CollegeWARJoinShort %>% 
  filter(position == "ED", Seasons >= 2) %>% 
  arrange(desc(season)) %>% 
  group_by(player_id) %>% 
  slice_head(n = 1) %>% 
  filter(season == 2023) %>% 
  arrange(desc(AvgWAR))


write_xlsx(current_edges, "Current Edge Rushers.xlsx")


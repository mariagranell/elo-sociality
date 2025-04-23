library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')

# orginal script made by Josie
#https://mail.google.com/mail/u/0/#search/josefien/FMfcgzQZTpwsZCbSwnPLFPHMBmGgWGKh

# parameters ------------------
groups <- c("NH", "AK", "BD", "KB", "LT")
years <- 2022:2024 # for these years
{
# Define seasons and their month ranges
seasons <- data.frame(
  Season = c("Summer", "Mating", "Winter", "Baby"),
  StartMonth = c(1, 4, 7, 10),
  EndMonth = c(3, 6, 9, 12)
)

# Correctly expand the dataframe to include all years and seasons
season_df <- crossing(Year = years, seasons) %>%
  mutate(
    MSStartDate = as.Date(paste(Year, StartMonth, "01", sep = "-")),
    MSEndDate = as.Date(paste(Year, EndMonth, "01", sep = "-")) + months(1) - days(1)
  ) %>%
  dplyr::select(Season, MSStartDate, MSEndDate) %>%
  arrange(MSStartDate)
  rm(seasons, years)
} # define seasons

# data ------------
{
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
females_names <- lh %>% filter(Sex == "F") %>% pull(AnimalCode)
males_names <- lh %>% filter(Sex == "M") %>% pull(AnimalCode)
pd <- read.csv("/Users/mariagranell/Repositories/data/Oct2021-May2022/Sexual_10.2021-05.2022.csv") %>%
  dplyr::select(Date, Group, Initiator, InitiatorBehaviour, Receptor, ReceptorBehaviour) %>% 
  rename(date = Date,
         group = Group,
         idindividual1 = Initiator,
         behaviourindiv1 = InitiatorBehaviour,
         idindividual2 = Receptor,
         behaviourindiv2 = ReceptorBehaviour) %>% 
  mutate(complete = NA,
         date = as.Date(date, format = "%m/%d/%Y"))
pd2 <- read.csv("/Users/mariagranell/Repositories/data/acess_data/OutputData/sexual_access.csv")%>%
  dplyr::select(Date, Group, IDIndividual1, BehaviourIndiv1, IDIndividual2, BehaviourIndiv2, Complete) %>%
  rename_with(tolower)

pd3 <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/CleanFiles/sexual_cybertracker.csv") %>%
  dplyr::select(Date, Group, IDIndividual1, BehaviourIndiv1, IDIndividual2, BehaviourIndiv2, Complete) %>%
  rename_with(tolower)

focal <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/OriginalFiles/Focal_cybertracker.csv") %>%
   rename_with(tolower) %>%
  filter(behaviour == "Sexual",
         #interobs == "No"
  ) %>%
  dplyr::select(date, group, idindividual1, behaviourfocal, idindividual2, complete) %>%
  rename(behaviourindiv1 = behaviourfocal) %>% 
  mutate(behaviourindiv2 = NA,
         date = as.Date(date))
}

rbind(pd, pd2, pd3, focal) %>% distinct() %>%
  add_season(.,"date") %>%
  plot_weekly_summary(., "data", "date")

sex_combination <- rbind(pd, pd2, focal) %>% distinct() %>%
  mutate(date = ymd(date)) %>%
  mutate(
    across(behaviourindiv1, tolower),
    complete = case_when(
      complete %in% c("No", "Unknown", "Yes", "Refused") ~ complete,
      grepl("mu|mo", behaviourindiv1) ~ "Unknown",
      grepl("ahg|av|rf|amu|ja|le|st|hi|amo", behaviourindiv1) ~ "Refused",
      TRUE ~ complete
    )
  ) %>%
  filter(complete %in% c("Yes", "No", "Unknown", "Interrupted")) %>%               # Keep only rows where mount was accepted
  filter(idindividual1 != "",
         idindividual2 != "",
         !is.na(idindividual2)) %>%                        # Remove empty IDs
  integrate_otherid(idindividual1, idindividual2) %>%
  mutate(FemaleID = case_when(
    idindividual2 %in% females_names ~ idindividual2,
    idindividual1 %in% females_names ~ idindividual1,
    TRUE ~ NA),
    MaleID = case_when(
      idindividual1 %in% males_names ~ idindividual1,
      idindividual2 %in% males_names ~ idindividual2,
      TRUE ~ NA_character_)
  ) %>% 
  distinct()


sex <- change_group_names(sex_combination, "group") %>%
  filter(group %in% groups,
         !is.na(MaleID),
         !is.na(FemaleID)) %>% 
  dplyr::select(date, MaleID, FemaleID, group) %>%
  correct_pru_que_mess("MaleID", "date", "group")

#write.csv(sex, "IVP DATA/Crossings/Sex_csi.csv", row.names = F)

## Separate the dataframes by group
gp = "AK"
SEQ_list <- list()
for (gp in groups) {
# Now create separate files for ak, bd and nh
individuals_in_gp <- lh %>% filter(Group_mb == gp, !is.na(AnimalCode)) %>% pull(AnimalCode) %>% unique()
sex_gp <- sex %>%
  filter(group == gp) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!MaleID %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !FemaleID %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         MaleID %in% individuals_in_gp,
         FemaleID %in% individuals_in_gp)

  #hist(sex_gp$date, breaks = "months",main = paste("Histogram for group:", gp))
  SEQ_list[[gp]] <- sex_gp
}
SEQ <- bind_rows(SEQ_list)

## Mount count -------

# parameters refine ------------
# SELECT MANUALLY check that you have enough range for the season calulcation
range(sex$date)
season_df_possible <- season_df[3:8,] # only these seasons can be calculated

MSStartDate = "2023-10-01"
MSEndDate = "2023-12-31"

combined_list <- list()

for (i in seq_len(nrow(season_df_possible))) {

MSStartDate = as.character(season_df_possible[i,"MSStartDate"][[1]])
MSEndDate = as.character(season_df_possible[i,"MSEndDate"][[1]])

# Define the date range: past 12 months (you can also use Date - 365 if you prefer)
start_date_last = as.Date(MSEndDate) %m-% months(12); end_date_last = as.Date(MSEndDate)
mount_last <- SEQ %>%
  filter(date >= start_date_last, date < end_date_last) %>%
  group_by(MaleID, group) %>%
  dplyr::summarize(mount_last12 = n()) %>%
  left_join(lh %>% filter(StartDate_mb < end_date_last, EndDate_mb > start_date_last) %>%
              dplyr::select(AnimalCode, Group_mb,StartDate_mb, EndDate_mb), by = c("MaleID" = "AnimalCode", "group" = "Group_mb")) %>%
  mutate(AllStay_last = ifelse(start_date_last > StartDate_mb & end_date_last < EndDate_mb, "yes", "no"),
         Stay_last = end_date_last - as.Date(StartDate_mb))

start_date_coming = as.Date(MSEndDate) ; end_date_coming = as.Date(MSEndDate) + months(12)
mount_coming <- SEQ %>%
  filter(date >= start_date_coming, date < end_date_coming) %>%
  group_by(MaleID, group) %>%
  dplyr::summarize(mount_coming12 = n()) %>%
  left_join(lh %>% filter(StartDate_mb < end_date_coming, EndDate_mb > start_date_coming) %>%
              dplyr::select(AnimalCode, Group_mb,StartDate_mb, EndDate_mb), by = c("MaleID" = "AnimalCode", "group" = "Group_mb")) %>%
  mutate(AllStay_coming = ifelse(start_date_coming > StartDate_mb & end_date_coming < EndDate_mb, "yes", "no"),
         Stay_coming = as.Date(EndDate_mb) - start_date_coming)

combined_df <- full_join(mount_last, mount_coming, by = c("MaleID", "group", "StartDate_mb", "EndDate_mb")) %>%
  mutate(ReferencedDate = MSEndDate,
         Stay_last = as.numeric(Stay_last, units = "days"),
         Mount_prop_last = ifelse(AllStay_last == "yes", mount_last12, mount_last12 * (365/Stay_last)),
         Stay_coming = as.numeric(Stay_coming, units = "days"),
         Mount_prop_coming = ifelse(AllStay_coming == "yes", mount_coming12, mount_coming12 * (365/Stay_coming)),
         )
  combined_list[[i]] <- combined_df
}

sexualinteractions_df <- bind_rows(combined_list)

#write.csv(sexualinteractions_df, "/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/SexualInteractions_MS.csv", row.names = F)
library(dplyr)
library(lubridate)

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

# ask josie about mounts data. Is very poor

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
  dplyr::select(date, MaleID, FemaleID, group)

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

  hist(sex_gp$date, breaks = "months",main = paste("Histogram for group:", gp))
  SEQ_list[[gp]] <- sex_gp
}
SEQ <- bind_rows(SEQ_list)

## Mount count -------

# parameters refine ------------
# SELECT MANUALLY check that you have enough range for the season calulcation
range(sex$date)
season_df_possible <- season_df[3:8,] # only these seasons can be calculated

MSStartDate = "2023-04-01"
MSEndDate = "2023-06-30"

# todo control for tenure. Josie divided the n_mounts by the months present
mount_last <- SEQ %>%
  mutate(
      # Define the date range: past 12 months (you can also use Date - 365 if you prefer)
      start_date =as.Date(MSEndDate) %m-% months(12),
      end_date= as.Date(MSEndDate)
  ) %>%
  filter(date >= start_date, date < end_date) %>%
  group_by(MaleID, group) %>%
  summarize(mount_last12 = n()) %>%
  left_join(lh %>% dplyr::select(AnimalCode, Group_mb,StartDate_mb, EndDate_mb), by = c("MaleID" = "AnimalCode", "group" = "Group_mb")) %>%
  mutate(AllStay = ifelse((as.Date(MSEndDate) %m-% months(12))< StartDate_mb, "yes", "no"),
         Stay = as.Date(MSEndDate) - as.Date(StartDate_mb))

mount_coming <- SEQ %>%
  mutate(
    # Define the date range: next 12 months (you can also use Date + 365 if you prefer)
      start_date = as.Date(MSEndDate),
      end_date = as.Date(MSEndDate) + months(12)
  ) %>%
  filter(date >= start_date, date < end_date) %>%
  group_by(MaleID, group) %>%
  summarize(mount_coming12 = n())

combined_df <- full_join(mount_last, mount_coming, by = c("MaleID", "group")) %>%
  mutate(ReferencedDate = MSEndDate)

# Now, add a column to cros_sex that counts, for each row, the number of times
# that individual's name (in the column ID) appears as 'focal' in the corresponding
# group-specific dataframe (from SEQ_list) within the past 12 months.
cros_sex <- cros_sex %>%
  rowwise() %>%  # Process row-by-row
  mutate(Date = as.Date(Date)) %>%
  mutate(
    mount_last12 = {
      # Get the dataframe for the corresponding group.
      # Make sure the group name in cros_sex matches the names in SEQ_list.
      sex_df <- SEQ_list[[Group]]

      # Define the date range: past 12 months (you can also use Date - 365 if you prefer)
      start_date <- Date %m-% months(12)
      end_date <- Date

      # Filter the dataframe:
      # - Only consider rows where 'date' is within the last 12 months.
      # - Only count rows where the focal individual's name equals the ID.
      # (Change "focal" to another column if needed.)
      count <- sex_df %>%
        filter(date >= start_date, date < end_date, focal == ID) %>%
        nrow()

      count
    },
    mount_coming12 = {
      # Get the dataframe for the corresponding group.
      # Make sure the group name in cros_sex matches the names in SEQ_list.
      sex_df <- SEQ_list[[Group]]

      # Define the date range: past 12 months (you can also use Date - 365 if you prefer)
      start_date <- Date
      end_date <- Date + 365

      # Filter the dataframe:
      # - Only consider rows where 'date' is within the last 12 months.
      # - Only count rows where the focal individual's name equals the ID.
      # (Change "focal" to another column if needed.)
      count <- sex_df %>%
        filter(date >= start_date, date < end_date, focal == ID) %>%
        nrow()

      count
    }
  ) %>%
  ungroup() %>%
  mutate(Mount = case_when(
    TenureYears < 1 ~ mount_last12/TenureYears,
    T ~ mount_last12
  ),
  Mount = ifelse(is.na(Mount), 0, Mount))

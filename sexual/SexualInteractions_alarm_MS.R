library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')

# data ------------------
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
sex <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/Sex_csi_combined_date.csv")

# parameters ------------------
MSgroups <- c("NH", "AK", "BD", "KB", "LT")
years <- 2021:2025 # for these years

range(sex$date)
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

## Separate the dataframes by group
gp = "AK"
SEQ_list <- list()
for (gp in MSgroups) {
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
#SEQ <- bind_rows(SEQ_list)

## Mount count -------
# dataframe where I want the calculation
cros_sex <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/alarm_maleservices_basedf.csv") %>%
  filter(Sex == "M", Age_class == "adult", Group %in% MSgroups) %>%
  mutate(Date = ymd(Date),
         TenureYears = as.numeric(ymd(Date) - ymd(StartDate_mb)) / 365.25) %>%
  distinct()

## Mount count -------
# List of SEQ, OT, and PRES data frames for each group
#SEQ_list <- list(AK = sex_ak, BD = sex_bd, NH = sex_nh, KB = sex_kb)

# Now, add a column to cros_sex that counts, for each row, the number of times
# that individual's name (in the column ID) appears as 'focal' in the corresponding
# group-specific dataframe (from SEQ_list) within the past 12 months.
cros_sex <- cros_sex %>%
  rowwise() %>%  # Process row-by-row
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
        filter(date >= start_date, date < end_date, AnimalCode == MaleID) %>%
        nrow()

      count
    },
    Date = as.Date(Date),
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
        filter(date >= start_date, date < end_date, AnimalCode == MaleID) %>%
        nrow()

      count
    }
  ) %>%
  ungroup() %>%
  mutate(Mount = case_when(
    TenureYears < 1 ~ mount_last12/TenureYears,
    T ~ mount_last12
  ),
  Mount = ifelse(is.na(Mount), 0, Mount)) %>%
  distinct()

sexualinteractions_df <- cros_sex %>% dplyr::select(Date, Group, AnimalCode, mount_last12, mount_coming12, Mount) %>% distinct()

write.csv(sexualinteractions_df, "/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/SexualInteractions_alarm_MS.csv", row.names = F)
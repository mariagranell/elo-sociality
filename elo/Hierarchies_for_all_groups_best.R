# ---------------
# Title: Hierarchies for all groups
# Date: 11 mar 2025
# Author: mgranellruiz
# Goal: This is a code that generated all hierarchies for evething you would need ever.
# the idea is that you calulate the herarchy here and then you export the values you are intrested in for
# the different dates in the different projects

# MALES AND FEMALES HIERARCHY SHOULD AND HAVE TO BE CALUCLATED SEPARETEDLY
# they compete over different stuff so you should never combine them!
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')

library(EloRating)

# path ------------------------
setwd()

# data ------------------------
{# data given to me by josie, it includes logbook and scan
winnerloser <- read.csv("/Users/mariagranell/Repositories/data/elo_data/WinnerLoser.csv") %>% change_group_names("Group") %>%
    correct_pru_que_mess("winner", "Date", "Group") %>% correct_pru_que_mess("loser","Date", "Group") %>% integrate_otherid(winner, loser)
# presence calculated in here: /Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/Presence_dplyr.R
presence_list <- list(
  KB = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhKB2020-2024.csv"),
  AK = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhAK2020-2024.csv"),
  NH = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhNH2020-2024.csv"),
  LT = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhLT2020-2024.csv"),
  BD = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhBD2020-2024.csv")
)
  lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>% filter(!is.na(AnimalCode))
}

# cleaning of winner loser, this step is done by trial and error seeing the mistakes gotten from the elo.seq() function and the seqcheck() function
winnerloser_clean <- winnerloser %>% filter(winner != loser) %>% distinct() %>%
  # Yubb was worngly named, one you update lh, it will be automatically corrected
  mutate(Date = as.Date(Date), winner = ifelse(winner == "Yubb", "Yub", winner), loser = ifelse(loser == "Yubb", "Yub", loser)) %>%
  # Small deletions following lh file
  filter(!(winner == "Gub" & Date >= as.Date(lh[lh$AnimalCode == "Gub", "StartDate_mb"], format = "%Y-%m-%d") | loser == "Gub" & Date >= as.Date(lh[lh$AnimalCode == "Gub", "StartDate_mb"], format = "%Y-%m-%d")),
         !(winner == "Naa" & Date >= as.Date(lh[lh$AnimalCode == "Naa", "StartDate_mb"], format = "%Y-%m-%d") | loser == "Naa" & Date >= as.Date(lh[lh$AnimalCode == "Naa", "StartDate_mb"], format = "%Y-%m-%d"))) %>%
  # too large of a correction we do not care much about
  filter(
    !(loser %in% c("Amg", "Mhao", "Nug", "Add", "Potj", "Rat", "Loki", "Alc", "Her", "Ree")),
    !(winner %in% c("Amg", "Mhao", "Nug", "Add", "Potj", "Rat", "Loki", "Alc", "Her", "Ree"))
  )

# the groups
groups <- c("NH", "AK", "BD", "KB", "LT")

ELO_list_females <- lapply(groups, function(grp) {

  # Extract and prepare presence data for the group
  pres <- presence_list[[grp]] %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

  # Filter win/loss data for the group and sex
  winlos_females <- winnerloser_clean %>% filter(Group == grp, AgeClassLoser == "AF", AgeClassWinner == "AF") %>% drop_na()

  # Optionally, run the sequence check to verify dates, etc.
  EloRating::seqcheck(
    winner = winlos_females$winner,
    loser = winlos_females$loser,
    Date = winlos_females$Date,
    draw = NULL,
    presence = pres
  )

  # Calculate the ELO sequence for the group and sex
  elo_result_females <- elo.seq(
    winner = winlos_females$winner,
    loser = winlos_females$loser,
    Date = winlos_females$Date,
    presence = pres,
    runcheck = FALSE
  )

  return(elo_result_females)
})
ELO_list_males <- lapply(groups, function(grp) {

  # Extract and prepare presence data for the group
  pres <- presence_list[[grp]] %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

  # Filter win/loss data for the group and sex
  winlos_males <- winnerloser_clean %>% filter(Group == grp, AgeClassLoser == "AM", AgeClassWinner == "AM") %>% drop_na()

  # Optionally, run the sequence check to verify dates, etc.
  EloRating::seqcheck(
    winner = winlos_males$winner,
    loser = winlos_males$loser,
    Date = winlos_males$Date,
    draw = NULL,
    presence = pres
  )

  elo_result_males <- elo.seq(
    winner = winlos_males$winner,
    loser = winlos_males$loser,
    Date = winlos_males$Date,
    presence = pres,
    runcheck = FALSE
  )

  return(elo_result_males)
})

# Name the list elements by the group names
names(ELO_list_females) <- groups; names(ELO_list_males) <- groups

# FINISHED -------

# extract ELO for the different projects

aa <- extract_elo(ELO_list_males$NH, extractdate = "2023-12-31", standarize = TRUE)

# MALE SERVICES PUBLICATION ------------------ # /Users/mariagranell/Repositories/male_services_index/MSpublication
#
# parameters ------------------
MSGroups <- c("NH", "AK", "BD", "KB", "LT")
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
season_df_possible <- season_df[3:8,]

# Initialize an empty list to store the results
result_list <- list()
counter <- 1

grp = "NH"
MSStartDate = ymd("2023-04-01")
MSEndDate = ymd("2023-06-30")
j = 1
# Outer loop: iterate over groups
for (grp in MSGroups) {
  # Inner loop: iterate over each date in season_df
  for (j in seq_len(nrow(season_df_possible))) {
    date <- season_df_possible$MSEndDate[j]

    # check if the date we are asking is out of range
    maxdate_males <- max(ELO_list_males[[grp]]$truedates)

    if (date > maxdate_males){
      temp_df <- data.frame(AnimalCode = NA, elo = "error max date reached for males", Group = grp, Date = date)
    }
    else{
    # Extract the ELO vector for the current group and date
    elo_female <- extract_elo(ELO_list_females[[grp]], extractdate = date, standardize = TRUE)
    elo_male <- extract_elo(ELO_list_males[[grp]], extractdate = date, standardize = TRUE)

    # Convert the named vector 'aa' into a data frame with columns for AnimalCode, elo, Group, and Date
    temp_df_female <- data.frame(
      AnimalCode = names(elo_female),
      elo = as.numeric(elo_female),
      Group = grp,
      Date = date
    )
    temp_df_male <- data.frame(
      AnimalCode = names(elo_male),
      elo = as.numeric(elo_male),
      Group = grp,
      Date = date
    )
    temp_df <- rbind(temp_df_female, temp_df_male)
    }
    # Save the temporary data frame in the list
    result_list[[counter]] <- temp_df
    counter <- counter + 1
  }
}

# Combine all the individual data frames into one data frame
result_ELO <- do.call(rbind, result_list)
rm(grp,i,j,date,maxdate,temp_df,aa,result_list,counter)

#write.csv(result_ELO, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_maleservices.csv", row.names = FALSE)


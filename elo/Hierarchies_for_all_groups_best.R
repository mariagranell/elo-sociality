# ---------------
# Title: Hierarchies for all groups
# Date: 11 mar 2025
# Author: mgranellruiz
# Goal: This is a code that generates all hierarchies for evething you would need ever.
# the idea is that you calulate the hierarchy here and then you export the values you are intrested in for
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

# # data ------------------------
{# data given to me by josie, it includes logbook and scan
winnerloser <- read.csv("/Users/mariagranell/Repositories/data/elo_data/WinnerLoser_allmydata.csv")
# presence calculated in here: /Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/Presence_dplyr.R
presence_list_old <- list(
  KB = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhKB2020-2024.csv"),
  AK = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhAK2020-2024.csv"),
  NH = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhNH2020-2024.csv"),
  LT = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhLT2020-2024.csv"),
  BD = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_2020-2023_lhBD2020-2024.csv")
)
  presence_list <- list(
  KB = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_KB2020-2025.csv"),
  AK = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_AK2020-2025.csv"),
  NH = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_NH2020-2025.csv"),
  LT = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_LT2020-2025.csv"),
  BD = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_BD2020-2025.csv")
)
  lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>% filter(!is.na(AnimalCode))
}

# cleaning of winner loser, this step is done by trial and error seeing the mistakes gotten from the elo.seq() function and the seqcheck() function
winnerloser_clean <- winnerloser %>% filter(winner != loser) %>% distinct() %>%
  # plain jane is considered a baby, that is wrong.
  mutate(AgeClassWinner = ifelse(winner == "PlainJane", "AM", AgeClassWinner),
         AgeClassLoser = ifelse(loser == "PlainJane", "AM", AgeClassLoser)) %>%
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
grp = "AK"

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

# Results
eloplot(ELO_list_males$NH)
eloplot(ELO_list_males$AK)
eloplot(ELO_list_males$BD)
eloplot(ELO_list_males$KB)

# FINISHED -------

# extract ELO for the different projects
aa <- extract_elo(ELO_list_males$KB, extractdate = "2022-06-28", standardize = TRUE)
aa <- extract_elo(ELO_list_males$NH, extractdate = "2025-08-01", standardize = TRUE)

# MALE SERVICES PUBLICATION ------------------ # /Users/mariagranell/Repositories/male_services_index/MSpublication
{
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
result_ELO <- do.call(rbind, result_list) %>% distinct()
rm(grp,i,j,date,maxdate,temp_df,aa,result_list,counter)
write.csv(result_ELO, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_maleservices.csv", row.names = FALSE)
}

# WHATS IN SALIVA ----------------------------- # /Users/mariagranell/Repositories/hormones/hormone_saliva/What-s-in-saliva-
{# dataframe of interest ------------------
saliva <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_saliva/What-s-in-saliva-/Data/OutputFiles/Data_Modelling.csv") %>%
  filter(!is.na(age)) %>%
  dplyr::select(id, group, date, sex, age)

# Initialize a new column "ELO" with NA for all rows
saliva$ELO <- NA
# Loop through each row of the dataframe
for (i in seq_len(nrow(saliva))) {
  # Only calculate Elo if the individual is an adult
  if (saliva$age[i] == "ad") {
    # Extract individual variables
    indv <- saliva$id[i]
    date <- saliva$date[i]
    gp <- saliva$group[i]

    # Choose the Elo list based on the individual's sex
    if (saliva$sex[i] == "male") {
      elo_list <- ELO_list_males[[gp]]
    } else if (saliva$sex[i] == "female") {
      elo_list <- ELO_list_females[[gp]]
    } else {
      elo_list <- NULL
    }

    # If an appropriate Elo list is found, extract the Elo rating
    if (!is.null(elo_list)) {
      elo_date <- extract_elo(elo_list, extractdate = date, standardize = TRUE)
      # Extract the Elo for the individual (assuming elo_date is a named vector)
      saliva$ELO[i] <- elo_date[[indv]]
    }
  } else {
    # If not an adult, ensure the value remains NA
    saliva$ELO[i] <- NA
  }
}
  saliva <- saliva %>% distinct()

write.csv(saliva, "/Users/mariagranell/Repositories/hormones/hormone_saliva/What-s-in-saliva-/Data/OutputFiles/ELO_whatsinsaliva.csv", row.names = FALSE)
}

# VIGILANCE - MALE SERVICES PUBLICATION ----------------------------- # /Users/mariagranell/Repositories/male_services_index/MSpublication
{# dataframe of interest ------------------
vigilance <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/vigilance_allmyfiles.csv") %>%
  left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("IDIndividual1" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(Date > StartDate_mb & Date < EndDate_mb) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(Age_class %in% "adult", Group %in% c("AK", "BD", "KB", "NH")) %>%
  dplyr::select(IDIndividual1, Group, Date, Sex, Age_class) %>%
  # not enough data for these males
  filter(IDIndividual1 != "Oti",
         !(IDIndividual1 == "Guz" & Group == "KB"),
         !(Date > "2024-05-26" & Group == "KB")
  ) %>%
  drop_na()

range(vigilance$Date)

# Initialize a new column "ELO" with NA for all rows
vigilance$ELO <- NA
  #vigilance <- head(vigilance)
# Loop through each row of the dataframe
for (i in seq_len(nrow(vigilance))) {
  if (vigilance$Age_class[i] == "adult") {
    indv <- vigilance$IDIndividual1[i]
    date <- vigilance$Date[i]
    gp <- vigilance$Group[i]

    if (vigilance$Sex[i] == "M") {
      elo_list <- ELO_list_males[[gp]]
    } else if (vigilance$Sex[i] == "F") {
      elo_list <- ELO_list_females[[gp]]
    } else {
      elo_list <- NULL
    }

    if (!is.null(elo_list)) {
      if (date %in% elo_list$truedates) {
        elo_date <- extract_elo(elo_list, extractdate = date, standardize = TRUE)

        if (indv %in% names(elo_date)) {
          vigilance$ELO[i] <- elo_date[[indv]]
        } else {
          vigilance$ELO[i] <- paste0("Individual ", indv, " not found at date")
        }
      } else {
        vigilance$ELO[i] <- "Date out of bounds"
      }
    } else {
      vigilance$ELO[i] <- NA
    }
  } else {
    vigilance$ELO[i] <- NA
  }
}

vigilance <- vigilance %>% distinct()

# In AK there was a long period in where Sho was the only adult male, making him the dominant.
  vigilance_corrected <- vigilance %>% add_group_composition( "Group", "Date") %>%
    mutate(
      ELO = case_when(
        AM == 1 ~ "1",
        # particular case,
        IDIndividual1 == "Sho" & Date == "2024-08-05" ~ "0", # during this time there were two males in AK, buk and sho, buk was the dominant even if there is no aggression data, until he died due to predation.
        IDIndividual1 == "Sho" & Date == "2024-08-23" ~ "0", # during this time there were two males in AK, buk and sho, buk was the dominant even if there is no aggression data, until he died due to predation.
        TRUE ~ ELO
      )
    ) %>% dplyr::select(IDIndividual1, Group, Date, Sex, Age_class, ELO) %>% distinct()

write.csv(vigilance_corrected, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_vigilance_maleservices.csv", row.names = FALSE)
}

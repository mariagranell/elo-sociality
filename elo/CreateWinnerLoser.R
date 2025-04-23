## Create WinnerLoser.csv
library(dplyr)
library(tidyr)
library(stringr)
source('/Users/mariagranell/Repositories/data/functions.R')

# MR: I combined the scrips of josie of FinalAgonistic and WinnerLosser

# ------------------------------------------------------------------------------
# Title: Create FinalAgonistic
# Author: Josefien Tankink
# Contact: josefien.tankink@unine.ch
# ------------------------------------------------------------------------------

# --- LOAD & PREPARE OLD AGONISTIC DATA ---
{old <- read.csv("/Users/mariagranell/Repositories/elo-sociality/elo/AllJosieFilesExplained/Agonistic2016-2023.csv") %>%
# This dataframe contains agonistic ad lib data from 2016 to May 2023.
  mutate(Date = ymd(Date)) %>% # convert Date
  add_season("Date")

# Visualise the data distribution and check that there are no gaps
plot_weekly_summary(old, "Data", "Date")

# Check if the latest agonistic csv file of cybertracker has data that goes back to May 2023!
# Otherwise, make sure to include that back up that contains data from that period until the latest
# file of cybertracker

# You can add that potential backup to this file the same way as you would add the newest file
}

# --- LOAD & PREPARE NEW AGONISTIC DATA ---
# Load the newest agonistic ad lib data
{new <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/agonistic_allmyfiles.csv",1) %>%

# We only use one on one interactions -- interactions with more than one individual are not reliable for dominance rank calculations (see EloRating tutorial)
# So omit all interactions with a supporter

  # Remove interactions with supporters
  filter(IDSupporters != "") %>%
  # Select columns of interest
  dplyr::select(Date, Time, Group, IDIndividual1, BehaviourIndiv1, IDIndividual2, BehaviourIndiv2, Data) %>%
  # Rename columns for clarity
  rename(
    Aggressor = IDIndividual1,
    AggressorBehaviour = BehaviourIndiv1,
    Victim = IDIndividual2,
    VictimBehaviour = BehaviourIndiv2
  ) %>%
  # Format Time and Date correctly
  mutate(
    Time = format(as.POSIXct(Time, format = "%H:%M:%S"), "%H:%M:%S"),
    Date = ymd(Date)
  ) %>%
  add_season("Date")

plot_weekly_summary(new, "Data", "Date")
}

new %>% filter(Group == "AK") %>%
plot_weekly_summary(., "Data", "Date")

# Combine the old and new dataframes:
elo <- bind_rows(old, new) %>% distinct() %>%
  filter(!is.na(Date)) %>%
  change_group_names("Group") %>%
  integrate_otherid(Aggressor, Victim) %>%
  correct_pru_que_mess("Aggressor", "Date", "Group") %>%
  correct_pru_que_mess("Victim", "Date", "Group")

# Visualise and look for gaps
plot_weekly_summary(elo, "Data", "Date")


# This file now should contain all ad lib recorded interactions that had no support

# --- LOAD & PREPARE FOCAL AGONISTIC DATA ---
{# Now we do the same for focals
# I don´t have old focal
#foc_old <- read.csv("IVP DATA/Dominance hierarchy/Focal_agonistic2022-2023.csv")  %>% mutate(Date = as.Date(Date))

# This file contains all the agonistic interactions recorded during focals without support
# Check the time period:
#hist(foc_old$Date, breaks = "weeks")
# So this file is from May 2022 (when we started focalling) until May 2023 basically

# Now we should load the newest file or the newest and the file until May 2023
ff <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/Cleaned_focal_allmyfiles.csv") %>%
  filter(Behaviour == "Agonistic") %>%
  mutate(Date = ymd(Date)) %>%
  # Exclude interactions with support
  filter(!AgonisticInteraction %in% c("Support to Focal", "Support to Partner", "Focal Supports")) %>%
  # Select relevant columns and create an empty VictimBehaviour column
  dplyr::select(Date, Time, Group, IDIndividual1, BehaviourFocal, IDIndividual2) %>%
  mutate(VictimBehaviour = "") %>%
  # The difference between focal and ad lib data is that all behaviours are written from the perspective of the focal (in focals)
  # That means that if ID2 does something to ID1, it is written as the passive form ("being" groomed) for ID1
  # We want to have the behaviours for ID1 and ID2 separate to determine who is the "winner" and "loser"
  # Extract VictimBehaviour from BehaviourFocal and clean it out from Aggressor's behaviour:
  mutate(
    VictimBehaviour = str_extract_all(BehaviourFocal, "(?<=\\b[bB])(\\w{2,4})"),
    VictimBehaviour = ifelse(lengths(VictimBehaviour) > 0, sapply(VictimBehaviour, paste, collapse = " "), NA),
    BehaviourFocal = str_replace_all(BehaviourFocal, "\\b[bB]\\w{2,4}\\b", "")
  ) %>%
  # Rename columns for consistency
  rename(
    Aggressor = IDIndividual1,
    AggressorBehaviour = BehaviourFocal,
    Victim = IDIndividual2
  )
}

# we bind both focals together
# I don´t have old focals
#focals <- bind_rows(foc_old, ff) %>% distinct()
focals <- ff %>% distinct()

# Now we bind both ad lib and focals together
final <- bind_rows(elo, focals) %>%
  mutate(Date = as.Date(Date)) %>%
  change_group_names("Group") %>%
  integrate_otherid(Aggressor, Victim) %>%
  correct_pru_que_mess("Aggressor", "Date", "Group") %>%
  correct_pru_que_mess("Victim", "Date", "Group")

# Check data:
final %>% mutate(Data = "Elo") %>% add_season("Date") %>%
plot_weekly_summary("Data","Date")

# no need to save the data because we combined the dataframes
#--- SAVE THE FINAL DATA ---
#write.csv(final, "IVP DATA/Dominance hierarchy/FinalAgonistic.csv", row.names = F)

# However, although the column names are Aggressor and Victim, we do not know yet which
# individual won the interaction, and which one lost.
# Go to Create WinnerLoser to calculate the winners and losers :)

# ------------------------------------------------------------------------------
# Title: Create WinnerLoser
# Author: Josefien Tankink
# Contact: josefien.tankink@unine.ch
# Adapted from: script by Stephanie Mercier
# ------------------------------------------------------------------------------

# Load FinalAgonistic made with Create FinalAgonistic.R script
final_data <- final %>% mutate(Data = "WinnerLoser")

## Assumptions Elo-rating package:
# File is ordered by date
# There are no empty cells
# IDs occur more than once
# Loser ID is different from winner ID

## ---- PREPARE THE DATA ----
d <- final_data %>%
  # Order by date
  arrange(Date) %>%
  # Remove rows with any NA
  na.omit() %>%
  # Set behaviour columns to lower case
  mutate(
    AggressorBehaviour = tolower(AggressorBehaviour),
    VictimBehaviour = tolower(VictimBehaviour)
  ) %>%
  # Exclude interactions before 2020-01-01. This is in acordance with the presence matrix
  filter(as.Date(Date) >= as.Date("2020-01-01"))

range(d$Date)
## ---- DEFINE WINNER/LOSER DECISION FUNCTIONS ----
## Based on script by Stephanie Mercier ##

# In order to add the social rank of initiators & targets and thus get their social rank differences, 
# we first need to create the "fight.data" from data which include all agonistic interactions in which 
# there is a clear winner (defined as the individual being the most aggressive, i.e. who used the most 
# intense aggressive behaviour 
# Define the submissive behaviours (for victims)
ret_beh <- c('fl', 'rt', 'av', 'ja', 'cr', 'ss', 'gu')

# Define aggressive categories from least (cat_1) to most (cat_3)
agg_cat <- list(
  cat_3 = c('bi', 'gb', 'hi', 'fi', 'hh', 'so'), 
  cat_2 = 'ch', 
  cat_1 = c('ac', 'at', 'dp', 'tp', 'st', 'su', 'fh', 'sf', 'hb', 'bd')
)

decide.win <- function(beh_x, beh_y) {
  x <- 0
  y <- 0
  for(beh_ in ret_beh) {
    x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
    y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
  }
  if(x < y) return(1)
  if(y < x) return(2)
  
  for(cat_ in agg_cat) {
    x <- 0
    y <- 0
    for(beh_ in cat_) {
      x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
      y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
    }
    if(x > y) return(1)
    if(y > x) return(2)
  }
  return(0)
}

## ---- ASSIGN WINNER/LOSER AT THE OBSERVATION LEVEL ----
fight.data <- d %>%
  # Calculate win_lose using mapply over the behaviour columns
  mutate(win_lose = mapply(decide.win, AggressorBehaviour, VictimBehaviour)) %>%
  # Assign winner/loser and corresponding behaviours using vectorized if_else
  mutate(
    winner       = case_when(win_lose == 1 ~ Aggressor,
                             win_lose == 2 ~ Victim,
                             TRUE ~ NA_character_),
    BehaviourW   = case_when(win_lose == 1 ~ AggressorBehaviour,
                             win_lose == 2 ~ VictimBehaviour,
                             TRUE ~ NA_character_),
    loser        = case_when(win_lose == 1 ~ Victim,
                             win_lose == 2 ~ Aggressor,
                             TRUE ~ NA_character_),
    BehaviourL   = case_when(win_lose == 1 ~ VictimBehaviour,
                             win_lose == 2 ~ AggressorBehaviour,
                             TRUE ~ NA_character_)
  ) %>%
  # Add a column indicating a draw if win_lose equals 0
  mutate(Draw = if_else(win_lose == 0, TRUE, FALSE))

# write the function which allows to calculate for each sequence of obs between two individuals, 
# how many aggressive behaviours of the different severity each of them performed 
# (starting with the most intense one, i.e., involving physical contact), and as soon as one of the individuals
# has a greater number of aggressive behaviours performed, we declare this individual as the winner 
# (ignoring the rest of the sequence, but making sure that this one is not the individual also ending up the conflict by moving away...)

## ---- ADD INTENSITY COLUMN ----
# Determine the intensity based on the winner's behaviour
fight.data <- fight.data %>%
  mutate(
    intensity = case_when(
      grepl("bi|gb|hi|fi|hh|so", BehaviourW) ~ "severe",
      grepl("ch", BehaviourW) ~ "chase",
      grepl("ap|ac|ag|at|dp|tp|st|vo|gu|sc|ap0|ap2|ap10|hb", BehaviourW) ~ "mild",
      TRUE ~ NA_character_
    )
  ) %>%
  # Exclude interactions with no clear intensity
  filter(!is.na(intensity))

## ---- REMOVE ROW NAMES ----
fight.data <- fight.data %>% 
  mutate(row_id = row_number()) %>%
  dplyr::select(-row_id)

## ---- CORRECT NAME MISTAKES ----
# Replace names using mutate and recode (or case_when)
# already corrected with integrateotherid()
#fight.data <- fight.data %>%
#  mutate(
#    winner = recode(winner,
#                    "Cheseled nose" = "Bra",
#                    "White dot" = "Tot",
#                    "Fluffy" = "Win",
#                    "Knee scab" = "Dal",
#                    "Round lips" = "Bob",
#                    "Kap" = "PlainJane"),
#    loser = recode(loser,
#                   "Cheseled nose" = "Bra",
#                   "White dot" = "Tot",
#                   "Fluffy" = "Win",
#                   "Knee scab" = "Dal",
#                   "Round lips" = "Bob",
#                   "Kap" = "PlainJane")
#  )

# It is a good idea at this point to check whether you agree with who is calculated as the winner or the loser of a conflict, manually
# As said, this script decides the winner based on the individual that has the most "severe" aggressive behaviours
# and the loser as the individual that has the most "retreating" behaviours
# For a list of the behaviours, see ret_beh and agg_cat


## ---- LINK LIFE HISTORY DATA ----
# For this part you need the functions from Age and Ageclass.R

# Load life history data; choose appropriate file path:
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>% filter(!is.na(AnimalCode)) %>%
  dplyr::select(AnimalCode,Group_mb, Sex, DOB_estimate, Tenure_type) %>% distinct()

## ---- CALCULATE WINNER AGE & SEX ----
# You can ignore the warning here
# Please note: the age of the male is an approximation - usually we do not know when they were born if they were not born in IVP
d_win <- fight.data %>%
  left_join(lh, by = c("winner" = "AnimalCode", "Group" = "Group_mb"), relationship ="many-to-many") %>%
  rowwise() %>%
  mutate(WinnerAge = add_age(DOB_estimate, Date, "Years"),
         AgeClassWinner = get_age_class(Sex,WinnerAge)) %>%
  ungroup() %>%
  dplyr::select(-DOB_estimate, -Tenure_type) %>%
  rename(WinnerSex = Sex) %>% distinct()

## ---- CALCULATE LOSER AGE & SEX ----
d_full <- d_win %>%
  left_join(lh, by = c("loser" = "AnimalCode", "Group" = "Group_mb"), relationship ="many-to-many") %>%
  rowwise() %>%
  mutate(LoserAge = add_age(DOB_estimate, Date, "Years"),
         AgeClassLoser = get_age_class(Sex,LoserAge)) %>%
  ungroup() %>%
  dplyr::select(-DOB_estimate, -Tenure_type) %>%
  rename(LoserSex = Sex) %>% distinct()

## ---- SELECT FINAL COLUMNS & ADD AGE CLASS ----

# check if there are things missing. Most Unk, all good
nrow(d_full %>% filter(is.na(AgeClassLoser)))
nrow(d_full %>% filter(is.na(AgeClassWinner)))

# Assume get_age_class is available
winner_loser <- d_full %>%
  mutate(across(where(is.character), ~ ifelse(. %in% c("", "NA"), NA, .))) %>%
  # Select columns of interest
  dplyr::select(Date, Time, Group, winner, BehaviourW, loser, BehaviourL,
         WinnerSex, WinnerAge, LoserSex, LoserAge, AgeClassWinner, AgeClassLoser) %>%
  na.omit() %>% distinct()
nrow(winner_loser)

aa <- read.csv("/Users/mariagranell/Repositories/data/elo_data/WinnerLoser.csv") %>%
  change_group_names("Group")

bb <- rbind(winner_loser, aa) %>% distinct() %>% dplyr::select(-WinnerAge, -LoserAge) %>% distinct()

bb %>% mutate( Data = "all") %>% add_season("Date") %>%
plot_weekly_summary("Data", "Date" ) +
   scale_y_continuous(limits = c(0, 900))

winner_loser %>% mutate( Data = "mine") %>% add_season("Date") %>%
plot_weekly_summary("Data", "Date" ) +
   scale_y_continuous(limits = c(0, 900))

aa %>% mutate( Data = "josie") %>% add_season("Date") %>% distinct() %>%
plot_weekly_summary("Data", "Date" ) +
   scale_y_continuous(limits = c(0, 900))

## ---- WRITE THE FINAL CSV ----
write.csv(winner_loser, "/Users/mariagranell/Repositories/data/elo_data/WinnerLoser_allmydata.csv", row.names = FALSE)
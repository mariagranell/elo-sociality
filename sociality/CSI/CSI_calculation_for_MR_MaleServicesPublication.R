# ---------------
# Title: Sociality CSI
# Date: 18 Sep 2023
# Updated: 12 Mar 2025
# Author: mgranellruiz
# Disclaimer: This script was created follwing two scrips created by Josefien Tankink
# Goal: Calculate centrality of individuals in the social network. That is NOT about DSI or particular friendships.
# ultimately find a single number (or two) that represents the social integration of an individual in a group.

# This script is also modified to take into account the presence of individulas in the group. For that individuals are subsetted
# into the time presented and group by a dataframe of choice.

# The data that it gives, has been calculated over 6 months of behavioural data and then gives the CSI for that date. Using less
# behaviour than 6 months is unavisable since the data would be too wonky. However, in most cases there will be a correlation in results
# if the dates that you ant are not separated by 6 months. Thus make sure that you include colineraity in you mondels when using this data.
# ---------------


# library ---------------------
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
source('/Users/mariagranell/Repositories/data/functions.R')
library(socialindices2)

setwd("/Users/mariagranell/Repositories/elo-sociality/sociality/CSI")

# BASIC DATA : -----------------------
# dsi is the social data, you need a Date, Focal, Actor, Receiver, BehaviourFocal, Duration, Group
{dsi <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sociality/data/Social_data_20211002_20240907.csv") %>%
  mutate(Focal = Actor, BehaviourFocal = BehaviourActor, Duration = NA)
OT <- read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_ot_2021-2024_logbook/OutputFiles/OT_LB.csv")
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

# check data with plot
dsi %>% add_season("Date") %>% mutate(Data = "ot") %>%
  filter(Group == "KB") %>%
  plot_weekly_summary("Data", "Date")

# Rename collumns for the loop
# date focal actor receiver beh dur
dsi <- dsi %>% rename(date = Date, focal = Focal, actor = Actor, receiver = Receiver, beh = BehaviourFocal, dur = Duration)
OT <- OT %>% rename(focal = Focal, date = Date)

# FILE WHERE YOU WANT YOUR CSI CALCULATED ----------- MALE SERVICES PUBLICATION /Users/mariagranell/Repositories/male_services_index/MSpublication
# should have AnimalCode, Group and Date for all the dates you´d like
darting_sheet <- read.csv("/Users/mariagranell/Repositories/combination_darting/dating_combination/OutputFiles/Darting_sheets.csv") %>%
  dplyr::select(AnimalCode, DartingSeason, Group, Date)

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

# STEP 1. ADD EMPTY OT -----------------
# Identify dates and focals in `dsi` but not in `OT`, otherwise, assign the average OT
{# I want to check whether there are days where there've been interactions observed but no logbook
# was created (therefore OT is 0)
missing_focals <- dsi %>%
  dplyr::select(focal, date, Group) %>%
  distinct() %>%
  anti_join(
    OT %>% dplyr::select(focal, date) %>% distinct(),
    by = c("focal", "date")
  )

# OK so there's a shitton of observations on days where there was no logbook
# Great
# Soooo I'm gonna average the observation time per group and assign that for individuals on 
# days where the logbook is missing

# Step 1: Calculate mean OT per group
mean_OT_per_group <- OT %>%
  group_by(Group) %>%
  summarize(mean_OT = mean(OT, na.rm = TRUE))

# Step 2: Merge missing records with mean OT
missing_records_with_OT <- missing_focals %>%
  left_join(mean_OT_per_group, by = "Group") %>%
  rename(OT = mean_OT)

# Step 3: Combine with the original OT dataframe
updated_OT <- bind_rows(OT, missing_records_with_OT) %>%
  mutate(date = as.Date(date))
}

# STEP 2. SELECT AFFILIATIVE BEHAVIOURS -----------------
# Select only applicable behaviours that are present in your social file
table(dsi$beh)
# It is apparently to keep different categories for the calculation.
# If the there is not enpugh of those behaviours for the categories you will get NaN in the CSI.
# if that happens either take more data, or simplify the categories.
dsi <- dsi %>%
  mutate(beh = ifelse(beh == "Contact.focal", "Contact", beh),
        beh = ifelse(beh == "Groom.focal", "Groom", beh),
         beh = ifelse(beh %in% c("Bgr", "Gr", "gr", "grgr", "bgr", "bgri", "grgr"), "Groom", beh),
         beh = ifelse(beh %in% c("bto", "em", "bem", "Sg", "SG", "bsg","sg","sw"), "Contact", beh)) %>%
  filter(beh %in% c("Groom", "Contact", "Two", "Five"))

# STEP 3. SUBSET THE DATAFRAMES ------------------
#if you want to subset the dates
date1 <- max(dsi$date)
date2 <- min(dsi$date)

{
SEQ_list <- list()
OT_list <- list()
pres_list <- list()
range(dsi$date)

for (gp in MSGroups) {

  # determine individuals
  individuals <-  presence_list[[gp]] %>% #filter(Group == gp) %>%
    dplyr::select(-Date) %>%
    dplyr::select(where(~ !all(. == 0))) %>%
                       colnames(.)

  # dsi
  dsi_subset <- dsi %>%
    filter(Group == gp,
           date < date1,
           date > date2)%>%
    dplyr::select(!Group) %>%
    mutate(date = ymd(date)) %>%
    filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
           !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
           actor %in% individuals,
           receiver %in% individuals,
           focal %in% individuals)

  # ot
  ot_subset <- updated_OT %>%
    filter(Group == gp,
           date < date1,
           date > date2) %>%
    dplyr::select(!Group) %>%
    mutate(date = ymd(date))

  # presence
  pres_subset <- presence_list[[gp]] %>%  rename(date = Date) %>% mutate(date = ymd(date)) %>%
    filter(#Group == gp,
           date < date1,
           date > date2) %>%
    dplyr::select(where(~ !all(. == 0))) # remove indv that have neven been present

  # Store in lists
  SEQ_list[[gp]] <- dsi_subset
  OT_list[[gp]] <- ot_subset
  pres_list[[gp]] <- pres_subset

}
rm(pres_subset, ot_subset, dsi_subset, individuals)
}

# STEP 4. PREPARE DF TO CONTAIN CSI ---------
df_csi_results <- expand_grid(season_df_possible, Group = MSGroups) %>%
    mutate(Start_CSIdaterange =  ymd(MSEndDate) %m-% weeks(2),   # define X time as the limit of social interactions you allow for, in this case 2 weeks
           End_CSIdaterange = ymd(MSEndDate))

group = "KB"
Start_CSIdaterange = "2022-07-01"
End_CSIdaterange = "2022-12-31"

# STEP 5. FUNCTION ---------
# Subset the data to include only the last 7 months
calculate_csi <- function(group, Start_CSIdaterange, End_CSIdaterange) {
  # Parse dates
  end_date <- as.Date(End_CSIdaterange, format = "%Y-%m-%d")
  Start_CSIdaterange <- as.Date(Start_CSIdaterange, format = "%Y-%m-%d")

  max_attempts <- 6  # Limit the number of iterations for going forward
  attempt <- 0
  actual_date <- NA  # Initialize ActualDate
  going_back <- FALSE  # Flag to indicate switching to backward search

  repeat {
    # Increment the attempt counter
    attempt <- attempt + 1

    # Calculate start date for the 6-month window
    start_date <- floor_date(end_date %m-% months(6), unit = "month") + days(day(end_date) - 1)

    # Filter the SEQ, OT, and presence data for the applicable group
    SEQ <- SEQ_list[[group]]
    OT <- OT_list[[group]]
    pres <- pres_list[[group]]

    SEQ_subset <- SEQ[SEQ$date >= start_date & SEQ$date <= end_date, ]
    OT_subset <- OT[OT$date >= start_date & OT$date <= end_date, ]

    # Check presence on the End_CSIdaterange date
    if (!end_date %in% pres$date & attempt < 6) {
      warning(paste("End date", end_date, "not found in presence data for group:", group))
      #return(c(NA, NA, NA))  # If presence data is missing, return NA (include ActualDate)

      # if end date is not found the the previous date that is
      end_date <- max(pres$date[pres$date < end_date], na.rm = TRUE)
    }

    # Get individuals present on End_CSIdaterange
    present_row <- pres[pres$date == end_date, ]
    present_individuals <- names(present_row)[which(present_row == 1)]  # Exclude date column

    # Debugging: Check presence data
    print(paste("Start analysis for group:", group))
    print(paste("Individuals present on", end_date, ":", paste(present_individuals, collapse = ", ")))

    # Filter SEQ_subset to retain only interactions involving present individuals
    SEQ_subset <- SEQ_subset[
      SEQ_subset$focal %in% present_individuals & SEQ_subset$actor %in% present_individuals, ]

    OT_subset <- OT_subset[
      OT_subset$focal %in% present_individuals, ]

    # Debugging: Check filtered SEQ_subset
    print(paste("Attempt:", attempt, "Start date:", start_date, "End date:", end_date))
    #print("SEQ_subset after filtering:")
    #print(head(SEQ_subset))
    #print("OT_subset:")
    #print(head(OT_subset))

    # Check for empty subsets
    if (nrow(SEQ_subset) == 0 || nrow(OT_subset) == 0) {
      warning(paste("Empty subset for group:", group, "between", start_date, "and", end_date))

    } else {
      # Calculate CSI and zCSI
      csi_results <- CSI(SEQ_subset, ot.source = OT_subset, duration.NA.treatm = "count")

      # Retrieve CSI for the focal indiv
      #focal <- paste(indiv)
      #result <- csi_results[csi_results$focal == focal, c("CSI", "zCSI")]

      if (nrow(csi_results) > 0) {
        # CSI score is found
        #csi_score <- as.numeric(result[1, ])
        actual_date <- as.character(ymd(end_date))  # Store the actual date used
        result <- csi_results %>% mutate(Actual_date = actual_date, Group = group, Asked_date = End_CSIdaterange) %>%
          dplyr::select(AnimalCode = focal,Asked_date, Actual_date, Group, CSI, zCSI)

        print(paste("Success! CSI found for", group, "on", actual_date))

        return( result
          #c(csi_score, actual_date)
        )  # **Exit loop successfully**
      }

    }
    
    # If max_attempts is reached, start going back in days
    if (attempt >= max_attempts) {
      if (!going_back) {
        going_back <- TRUE
        warning(paste("Switching to backward search for group:", group))
      }
      end_date <- end_date - days(1)  # Start decrementing the date
    } else {
      end_date <- end_date + days(1)  # Increment the date during the forward search
    }
    
    # Break the loop if the date moves too far out of range
    if (going_back && end_date < Start_CSIdaterange) {
      warning(paste("No valid score found after full search for group:", group))
      return(c(NA, NA, NA))  # Include NA for ActualDate
    }
  }
}

# STEP 6. APPLY FUNCTION ------
# Initialize an empty list to store results from each row
results_list <- vector("list", nrow(df_csi_results))
i =1
for (i in seq_len(nrow(df_csi_results))) {
  # Extract parameters for this iteration
  group <- df_csi_results$Group[i]
  start_range <- df_csi_results$Start_CSIdaterange[i]
  end_range   <- df_csi_results$End_CSIdaterange[i]

  # Call your function to get a data frame of results
  temp_result <- calculate_csi(group, start_range, end_range)

  # Optionally, if you need to add any columns (like which row of df_csi_results produced these results)
  # temp_result$source_row <- i

  # Save the data frame in the list
  results_list[[i]] <- temp_result
}

# Combine all returned data frames into one data frame
combined_results_all <- do.call(rbind, results_list)

# STEP 7. QUALITY CHECK -------
# a 0 csi is not a problem.
df_csi_results_checked <- combined_results_all %>%
  mutate(Warning = ifelse(ymd(Actual_date) > (ymd(Asked_date) + days(3)), "Investigate", NA),
         CSI = ifelse(CSI %in% c("Inf", "NaN"), NA, CSI),
         zCSI = ifelse(zCSI %in% c("Inf", "NaN"), NA, zCSI),
  )

 rm(date1,date2,dsi,KeyOtherID)
 rm(mean_OT_per_group, missing_records_with_OT, missing_focals)
 rm(gp, OT, presence_data, updated_OT,i)

# STEP 8. SAVE
write.csv(df_csi_results_checked,"/Users/mariagranell/Repositories/elo-sociality/sociality/CSI/OutputData/maleservicespublicationCSI.csv", row.names = FALSE)
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
{
# dsi is the social data, you need a Date, Focal, Actor, Receiver, BehaviourFocal, Duration, Group
dsi <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sociality/data/Social_data_20211001_20250327.csv") %>%
  mutate(Focal = Actor, BehaviourFocal = Behaviour, Duration = NA) %>% distinct() %>%
  dplyr::select(Group, Date, Time, Obs.nr, IntOrder = Source, Actor, Receiver, BehaviourActor = Behaviour, Focal, BehaviourFocal, Duration)

OT <- read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_ot_2021-2025_logbook/OutputFiles/OT_LB.csv")
# presence calculated in here: /Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/Presence_dplyr.R
presence_list <- list(
  KB = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_KB2020-2025.csv"),
  AK = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_AK2020-2025.csv"),
  NH = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_NH2020-2025.csv"),
  LT = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_LT2020-2025.csv"),
  BD = read.csv("/Users/mariagranell/Repositories/data/presence_ot/PresenceData_lh/PresenceData_BD2020-2025.csv")
)
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>% filter(!is.na(AnimalCode))
}

# check data with plot
dsi %>% add_season("Date") %>% mutate(Data = "ot") %>%
  filter(Group == "BD") %>%
  plot_weekly_summary("Data", "Date")

# Rename collumns for the loop
# date focal actor receiver beh dur
dsi <- dsi %>% rename(date = Date, focal = Focal, actor = Actor, receiver = Receiver, beh = BehaviourFocal, dur = Duration)
OT <- OT %>% rename(focal = Focal, date = Date)

# FILE WHERE YOU WANT YOUR CSI CALCULATED -----------
# should have AnimalCode, Group and Date for all the dates you´d like
alarm <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/alarm_maleservices_basedf.csv") %>%
  filter(Age_class %in% "adult", Group %in% c("AK", "BD", "KB", "NH")) %>%
  dplyr::select(AnimalCode, Group = Group, Date = Date) %>%
  drop_na() %>% distinct()

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
  dplyr::summarize(mean_OT = mean(OT, na.rm = TRUE))

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
         beh = ifelse(beh %in% c("Bgr", "Gr", "gr", "grgr", "bgr", "bgri"), "Groom", beh),
         beh = ifelse(beh %in% c("bto", "em", "bem", "Sg", "SG", "bsg","sg","sw"), "Contact", beh)) %>%
  filter(beh %in% c("Groom", "Contact", "Two", "Five"))

# STEP 3. SUBSET THE DATAFRAMES ------------------
habituated_groups <- c("AK", "NH", "BD", "KB", "LT") # For these groups
#if you want to subset the dates
date1 <- max(dsi$date)
date2 <- min(dsi$date)
{
SEQ_list <- list()
OT_list <- list()
pres_list <- list()
range(dsi$date)

for (gp in habituated_groups) {

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
df_csi_results <- alarm %>% #darting_sheet %>%
  mutate(Start_CSIdaterange =  ymd(Date) %m-% days(6),   # define X time as the limit of social interactions you allow for, in this case 6 month
         End_CSIdaterange = ymd(Date)) %>%
  filter(Group %in% habituated_groups)

# STEP 5. FUNCTION ---------
# Subset the data to include only the last 7 months
calculate_csi <- function(indiv, group, Start_CSIdaterange, End_CSIdaterange) {
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

    # Calculate start date for the 7-month window
    start_date <- floor_date(end_date %m-% months(7), unit = "month") + days(day(end_date) - 1)

    # Filter the SEQ, OT, and presence data for the applicable group
    SEQ <- SEQ_list[[group]]
    OT <- OT_list[[group]]
    pres <- pres_list[[group]]

    SEQ_subset <- SEQ[SEQ$date >= start_date & SEQ$date <= end_date, ] %>% rename_with(tolower)
    OT_subset <- OT[OT$date >= start_date & OT$date <= end_date, ]

    # Check presence on the End_CSIdaterange date
    if (!end_date %in% pres$date & attempt < 6) {
      #warning(paste("End date", end_date, "not found in presence data for group:", group))
      #return(c(NA, NA, NA))  # If presence data is missing, return NA (include ActualDate)

      # if end date is not found the the previous date that is
      end_date <- max(pres$date[pres$date < end_date], na.rm = TRUE)
    }

    # Get individuals present on End_CSIdaterange
    present_row <- pres[pres$date == end_date, ]
    present_individuals <- names(present_row)[which(present_row == 1)]  # Exclude date column

    # Debugging: Check presence data
    print(paste("Start analysis for individual:", indiv))
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
      focal <- paste(indiv)
      result <- csi_results[csi_results$focal == focal, c("CSI", "zCSI")]

      if (nrow(result) > 0) {
        # CSI score is found
        csi_score <- as.numeric(result[1, ])
        actual_date <- as.character(ymd(end_date))  # Store the actual date used

        print(paste("Success! CSI found for", indiv, "on", actual_date))

        return(c(csi_score, actual_date))  # **Exit loop successfully**
      }

    }
    
    # If max_attempts is reached, start going back in days
    if (attempt >= max_attempts) {
      if (!going_back) {
        going_back <- TRUE
        warning(paste("Switching to backward search for indiv:", indiv, "in group:", group))
      }
      end_date <- end_date - days(1)  # Start decrementing the date
    } else {
      end_date <- end_date + days(1)  # Increment the date during the forward search
    }
    
    # Break the loop if the date moves too far out of range
    if (going_back && end_date < Start_CSIdaterange) {
      warning(paste("No valid score found after full search for indiv:", indiv, "in group:", group))
      return(c(NA, NA, NA))  # Include NA for ActualDate
    }
  }
}

# STEP 6. APPLY FUNCTION ------
df_csi_results$CSI <- NA
df_csi_results$zCSI <- NA
df_csi_results$ActualDate <- NA

for (i in seq_len(nrow(df_csi_results))) {
  # Check if DSI or zDSI are NA
  if (is.na(df_csi_results$CSI[i]) | is.na(df_csi_results$zCSI[i])) {
    indiv <- df_csi_results$AnimalCode[i]
    group <- df_csi_results$Group[i]
    Start_CSIdaterange <- df_csi_results$Start_CSIdaterange[i]
    End_CSIdaterange <- df_csi_results$End_CSIdaterange[i]

    scores <- calculate_csi(indiv, group, Start_CSIdaterange, End_CSIdaterange)
    df_csi_results$CSI[i] <- scores[1]
    df_csi_results$zCSI[i] <- scores[2]
    df_csi_results$ActualDate[i] <- scores[3]
  }
}

# STEP 7. QUALITY CHECK -------
# a 0 csi is not a problem.
df_csi_results_checked <- df_csi_results %>%
  mutate(Warning = ifelse(ymd(ActualDate) > (ymd(Date) + days(3)), "Investigate", NA),
         CSI = ifelse(CSI %in% c("Inf", "NaN"), NA, CSI),
         zCSI = ifelse(zCSI %in% c("Inf", "NaN"), NA, zCSI),
  ) %>% distinct()

 rm(date1,date2,dsi,habituated_groups,KeyOtherID)
 rm(mean_OT_per_group, missing_records_with_OT, missing_focals)
 rm(gp, OT, presence_data, updated_OT)

# STEP 8. SAVE
write.csv(df_csi_results_checked,"/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/CSI_alarm_maleservices.csv", row.names = FALSE)
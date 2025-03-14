# Laptop:
dsi <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/File_DSI.csv")
OT <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/OT_LB.csv")
pres_ak <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Ankhase_presence_matrix.csv")
pres_bd <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Baie Dankie_presence_matrix.csv")
pres_nh <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Noha_presence_matrix.csv")

# Properly name columns
# date focal actor receiver beh dur
dsi <- dsi %>% 
  rename(date = Date, focal = Focal, actor = Actor, receiver = Receiver, beh = BehaviourFocal, dur = Duration)

OT <- OT %>%
  rename(focal = Focal, date = Date)

# I want to check whether there are days where there've been interactions observed but no logbook
# was created (therefore OT is 0)
# Identify dates and focals in `dsi` but not in `OT`
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

# Select only applicable behaviours
dsi <- dsi %>%
  mutate(beh = ifelse(beh == "Contact.focal", "Contact", beh),
         beh = ifelse(beh == "Groom.focal", "Groom", beh),
         beh = ifelse(beh %in% c("Bgr", "Gr"), "Groom", beh),
         beh = ifelse(beh %in% c("bto", "em", "bem", "Sg", "SG"), "Contact", beh)) %>%
  filter(beh %in% c("Groom", "Contact", "Two", "Five"))

# TODO not applicable
# Create dsi without experimental dyad interactions
dsiF <- dsi %>%
  filter(
    !(actor == "Xin" & receiver == "Ouli") & 
      !(actor == "Ouli" & receiver == "Xin") & 
      !(actor == "Xia" & receiver == "Piep") & 
      !(actor == "Piep" & receiver == "Xia") & 
      !(actor == "Sey" & receiver == "Sirk") & 
      !(actor == "Sirk" & receiver == "Sey") & 
      !(actor == "Nge" & receiver == "Oerw") & 
      !(actor == "Oerw" & receiver == "Nge") & 
      !(actor == "Kom" & receiver == "Oort") & 
      !(actor == "Oort" & receiver == "Kom") & 
      !(actor == "Sho" & receiver == "Ginq") & 
      !(actor == "Ginq" & receiver == "Sho") & 
      !(actor == "Buk" & receiver == "Ndaw") & 
      !(actor == "Ndaw" & receiver == "Buk") & 
      !(actor == "Pom" & receiver == "Xian") & 
      !(actor == "Xian" & receiver == "Pom")
  )


# Extract the column names from each presence matrix except for the first one (Date)
individuals_ak <- colnames(pres_ak)[-1]
individuals_bd <- colnames(pres_bd)[-1]
individuals_nh <- colnames(pres_nh)[-1]

# Now create separate files for ak, bd and nh
dsi_ak <- dsiF %>% 
  filter(Group == "Ankhase",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         actor %in% individuals_ak,
         receiver %in% individuals_ak,
         focal %in% individuals_ak)
dsi_bd <- dsiF %>% 
  filter(Group == "Baie Dankie",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         actor %in% individuals_bd,
         receiver %in% individuals_bd,
         focal %in% individuals_bd)
dsi_nh <- dsiF %>% 
  filter(Group == "Noha",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         actor %in% individuals_nh,
         receiver %in% individuals_nh,
         focal %in% individuals_nh)

# Create separate files for ak, bd and nh of OT
ot_ak <- updated_OT %>% 
  filter(Group == "Ankhase",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         focal = ifelse(focal == "GIl", "Gil", focal))
ot_bd <- updated_OT %>% 
  filter(Group == "Baie Dankie",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
ot_nh <- updated_OT %>% 
  filter(Group == "Noha",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# TODO AK press lifehistory
# Rename columns pres
AKpres <- AKpres %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date < "2023-11-20",
         date > "2021-06-02")
BDpres <- BDpres %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date < "2023-11-20",
         date > "2021-06-02")
NHpres <- NHpres %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date < "2023-11-20",
         date > "2021-06-02")

# todo file where you want to implemet my CSI. ID, date darted, we want CSI zCSI
female_results <- female_results %>%
  mutate(Group = case_when(
    FemaleID %in% individuals_ak ~ "Ankhase",
    FemaleID %in% individuals_bd ~ "BaieDankie",
    FemaleID %in% individuals_nh ~ "Noha",
    TRUE ~ NA_character_
  ))

# List of SEQ, OT, and PRES data frames for each group
SEQ_list <- list(Ankhase = dsi_ak, BaieDankie = dsi_bd, Noha = dsi_nh)
OT_list <- list(Ankhase = ot_ak, BaieDankie = ot_bd, Noha = ot_nh)
pres_list <- list(Ankhase = AKpres, BaieDankie = BDpres, Noha = NHpres)

# TODO edTwoweeks importat date
female_results <- female_results %>%
  mutate(Start_TwoWeeks = as.Date(format(as.POSIXct(Start_TwoWeeks, format = "%Y-%m-%d"), "%Y-%m-%d")),
         End_TwoWeeks = as.Date(format(as.POSIXct(End_TwoWeeks, format = "%Y-%m-%d"), "%Y-%m-%d")))

# Subset the data to include only the last 6 months
# TODO start two weeks is not used.
calculate_csi <- function(female, group, Start_TwoWeeks, End_TwoWeeks, Exp_Period) {
  # Parse dates
  end_date <- as.Date(End_TwoWeeks, format = "%Y-%m-%d")
  start_twoweeks <- as.Date(Start_TwoWeeks, format = "%Y-%m-%d")
  
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
    
    # Check presence on the End_TwoWeeks date
    if (!end_date %in% pres$date) {
      warning(paste("End date", end_date, "not found in presence data for group:", group))
      return(c(NA, NA, NA))  # If presence data is missing, return NA (include ActualDate)
    }
    
    # Get individuals present on End_TwoWeeks
    present_row <- pres[pres$date == end_date, ]
    present_individuals <- names(present_row)[which(present_row == 1)]  # Exclude date column
    
    # Debugging: Check presence data
    print(paste("Individuals present on", end_date, ":", paste(present_individuals, collapse = ", ")))
    
    # Filter SEQ_subset to retain only interactions involving present individuals
    SEQ_subset <- SEQ_subset[
      SEQ_subset$focal %in% present_individuals & SEQ_subset$actor %in% present_individuals, ]
    
    OT_subset <- OT_subset[
      OT_subset$focal %in% present_individuals, ]
    
    # Debugging: Check filtered SEQ_subset
    print(paste("Attempt:", attempt, "Start date:", start_date, "End date:", end_date))
    print("SEQ_subset after filtering:")
    print(head(SEQ_subset))
    print("OT_subset:")
    print(head(OT_subset))
    
    # Check for empty subsets
    if (nrow(SEQ_subset) == 0 || nrow(OT_subset) == 0) {
      warning(paste("Empty subset for group:", group, "between", start_date, "and", end_date))
    } else {
      # Calculate CSI and zCSI
      csi_results <- CSI(SEQ_subset, ot.source = OT_subset, duration.NA.treatm = "count")
      
      # Retrieve CSI for the focal female
      focal <- paste(female)
      result <- csi_results[csi_results$focal == focal, c("CSI", "zCSI")]
      
      # Initialize CSI scores to NA if not found
      csi_score <- as.numeric(result[1, ])

      # TODO for controlfemales for Josie, not applicable
      # If valid scores are found, set ActualDate and add other individuals
      if (!any(is.na(csi_score)) && !any(is.nan(csi_score)) && !any(is.infinite(csi_score))) {
        actual_date <- as.Date(end_date)  # Set ActualDate to the final attempt date
        
        # Check for other four-letter coded females in the results
        control_females <- csi_results$focal[nchar(csi_results$focal) == 4]
        
        # Exclude the current focal female from controls for this calculation
        control_females <- setdiff(control_females, focal)
        
        # Add control females to female_results for this specific calculation period
        control_entries <- lapply(control_females, function(control_female) {
          control_result <- csi_results[csi_results$focal == control_female, c("CSI", "zCSI")]
          control_csi <- if (nrow(control_result) > 0) {
            as.numeric(control_result[1, ])
          } else {
            c(NA, NA)
          }
          data.frame(
            FemaleID = control_female,
            Start_TwoWeeks = start_twoweeks,
            End_TwoWeeks = end_date,
            Exp_Period = Exp_Period,
            Group = group,
            CSI = control_csi[1],
            zCSI = control_csi[2],
            Nature = "Control",
            stringsAsFactors = FALSE
          )
        })
        
        # Append these new control entries to the global female_results dataframe
        if (!is.null(control_entries) && length(control_entries) > 0) {
          female_results <<- rbind(female_results, do.call(rbind, control_entries))
        }
        
        return(c(csi_score, actual_date))
      }
    }
    
    # If max_attempts is reached, start going back in days
    if (attempt >= max_attempts) {
      if (!going_back) {
        going_back <- TRUE
        warning(paste("Switching to backward search for female:", female, "in group:", group))
      }
      end_date <- end_date - days(1)  # Start decrementing the date
    } else {
      end_date <- end_date + days(1)  # Increment the date during the forward search
    }
    
    # Break the loop if the date moves too far out of range
    if (going_back && end_date < start_twoweeks) {
      warning(paste("No valid score found after full search for female:", female, "in group:", group))
      return(c(NA, NA, NA))  # Include NA for ActualDate
    }
  }
}


female_results$CSI <- NA
female_results$zCSI <- NA
female_results$ActualDate <- NA

for (i in 1:nrow(female_results)) {
  # Check if DSI or zDSI are NA
  if (is.na(female_results$CSI[i]) | is.na(female_results$zCSI[i])) {
    female <- female_results$FemaleID[i]
    group <- female_results$Group[i]
    Start_TwoWeeks <- female_results$Start_TwoWeeks[i]
    End_TwoWeeks <- female_results$End_TwoWeeks[i]
    Exp_period <- female_results$Exp_Period[i]  # Make sure this is being retrieved correctly
    
    
    scores <- calculate_csi(female, group, Start_TwoWeeks, End_TwoWeeks, Exp_period)
    female_results$CSI[i] <- scores[1]
    female_results$zCSI[i] <- scores[2]
    female_results$ActualDate[i] <- as.Date(scores[3])
  }
}

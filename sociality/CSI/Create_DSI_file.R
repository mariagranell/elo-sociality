## Create DSI file

# Using ad-lib, scan and focal
library(dplyr)
library(readr)
library(tidyr)
library(splitstackshape)
library(purrr)
source('/Users/mariagranell/Repositories/data/functions.R')

# Parameters
MSGroups = c("AK", "BD", "KB", "NH", "LT")

# Create latest files ####
# data -----------
{
# Focal
#f_new <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Cleaned_focal.csv")
f_new <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/Cleaned_focal_allmyfiles.csv") %>%
  mutate(Date = ymd(Date))

# Ad lib
#a_new <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Affiliative.CSV")
a_new <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/affiliative_allmyfiles.csv") %>%
  mutate(Date = ymd(Date))

# Scan
#s_new <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Scan.CSV")
s_new <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/scan_allmyfiles.csv")%>%
  mutate(Date = ymd(Date))

}

## CREATING *FOCAL* DATA FILE DSI ####
# Remove spaces before behaviours
# Replace empty Behaviours with "Unknown"
# Only select applicable groups
f_sb <- f_new %>% 
  filter(Group %in% MSGroups) %>%
  mutate(Behaviour = sub(" ", "", Behaviour),
         Behaviour = ifelse(nchar(Behaviour) == 0, "Unknown", Behaviour))

# Select affiliative only interactions
# Create a new row for every ".", warnings are ok
f <- f_sb %>%
  filter(Behaviour %in% "Affiliative") %>%
  cSplit(., "BehaviourFocal", ".", "long") %>%
  cSplit(., "BehaviourFocal", " ", "long") %>%
  mutate(BehaviourFocal = sub(" ", "", BehaviourFocal))

# Define active behaviours
active_beh <- c('gr', 'mc', 'pr', 'sg', 'ap', 'ag', 'le', 'ig', 'sm', 'fo', 'is', 'to', 'ls',
                'sw', 'nu', 'vo', 'wb', 'amc', 'bp', 'SG', 'ge', 'hh', 'mu', 'hg', 'tc', 'ato')

# Determine Actor and Receiver
f <- f %>%
  mutate(
    Actor = ifelse(BehaviourFocal %in% active_beh, IDIndividual1, IDIndividual2),
    Receiver = ifelse(BehaviourFocal %in% active_beh, IDIndividual2, IDIndividual1)
  )

# Put in the right format and remove rows with missing values
f <- f %>%
  filter(IDIndividual1 != "", IDIndividual2 != "", BehaviourFocal != "") %>%
  dplyr::select(Date, Focal = IDIndividual1, Actor, Receiver, Behaviour = BehaviourFocal, Duration, Group) %>%
  mutate(Duration = ifelse(!Behaviour %in% c('gr', 'bgr', 'sg', 'sw', 'bsg', 'bsw'), NA, Duration)) %>%
  mutate(Behaviour = ifelse(Behaviour %in% c('gr', 'bgr'), "Groom.focal", Behaviour)) %>%
  mutate(Behaviour = ifelse(Behaviour %in% c('sg', 'sw', 'bsg', 'bsw'), "Contact.focal", Behaviour)) %>%
  mutate(Behaviour = ifelse(Behaviour %in% c('ap', 'bap'), "Approach", Behaviour))

#write.csv(f, "G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Focal_DSI.CSV", row.names = F)
  
#### CREATING *AD-LIB* DSI FILE ####
#old <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Old_Affiliative_cleaned.CSV")
#old <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Old_Affiliative_cleaned.CSV")
old <- read.csv("/Users/mariagranell/Repositories/data/Oct2021-May2022/Social_10.2021-05.2022.csv") %>%
  dplyr::select(Date, Group, Context,
                IDIndividual1 = Individual1, IDIndividual2 = Individual2, BehaviourIndiv1 = BehaviourIndividual1, BehaviourIndiv2 = BehaviourIndividual2,
                IDIndividual3 = Individual3, IDIndividual4 = Individual4, BehaviourIndiv3 = BehaviourIndividual3, BehaviourIndiv4 = BehaviourIndividual4,
                IDIndividual5 = Individual5, IDIndividual6 = Individual6, BehaviourIndiv5 = BehaviourIndividual5, BehaviourIndiv6 = BehaviourIndividual6) %>%
  change_group_names(.,"Group") %>%
  mutate(Date = mdy(Date))

# Remove unusable contexts
iffy_contexts <- c("BGE", "bge", "Experiment", "BoxExperiment", "Agonistic", "Other", "alarm call, raups", "bgd", "see conflict form at this time")
a_new2 <- a_new %>%
  dplyr::select(Date, Group, Context,
                IDIndividual1, IDIndividual2, BehaviourIndiv1, BehaviourIndiv2,
                IDIndividual3, IDIndividual4, BehaviourIndiv3, BehaviourIndiv4,
                IDIndividual5, IDIndividual6, BehaviourIndiv5, BehaviourIndiv6) %>%
  rbind(.,old) %>%
  filter(!Context %in% iffy_contexts)

# Keep interactions with ID3 etc
int1 <- a_new2 %>%
  dplyr::select(Date, Group, IDIndividual1, IDIndividual2, BehaviourIndiv1, BehaviourIndiv2) %>%
  filter(!IDIndividual1 == "", !IDIndividual2 == "")

int2 <- a_new2 %>%
  dplyr::select(Date, Group, IDIndividual3, IDIndividual4, BehaviourIndiv3, BehaviourIndiv4) %>%
  filter(!IDIndividual3 == "", !IDIndividual4 == "") %>%
  rename(IDIndividual1 = IDIndividual3, 
         BehaviourIndiv1 = BehaviourIndiv3, 
         IDIndividual2 = IDIndividual4, 
         BehaviourIndiv2 = BehaviourIndiv4)

int3 <- a_new2 %>%
  dplyr::select(Date, Group, IDIndividual5, IDIndividual6, BehaviourIndiv5, BehaviourIndiv6) %>%
  filter(!IDIndividual5 == "", !IDIndividual6 == "") %>%
  rename(IDIndividual1 = IDIndividual5, 
         BehaviourIndiv1 = BehaviourIndiv5, 
         IDIndividual2 = IDIndividual6, 
         BehaviourIndiv2 = BehaviourIndiv6)

combined <- rbind(int1, int2, int3)

a <- combined %>% 
  filter(Group %in% MSGroups,
         Date > "2017-01-01")

# Create a new row for every "."
a <- a %>%
  cSplit("BehaviourIndiv1", ".", "long") %>%
  cSplit("BehaviourIndiv1", " ", "long") %>%
  cSplit("BehaviourIndiv2", ".", "long") %>%
  cSplit("BehaviourIndiv2", " ", "long")

# Select only interactions of interest
interested_behaviours <- c("gr", "sg", "sw", "em", "pl", "ap")
a <- a %>%
  filter(BehaviourIndiv1 %in% interested_behaviours | BehaviourIndiv2 %in% interested_behaviours)

# Select who was the actor and who the receiver
active_beh <- c('gr', 'sg', 'sw', 'em', 'pl', 'ap')
a <- a %>%
  mutate(
    Actor = ifelse(BehaviourIndiv1 %in% active_beh, IDIndividual1, IDIndividual2),
    Receiver = ifelse(BehaviourIndiv1 %in% active_beh, IDIndividual2, IDIndividual1)
  )

# Select only the "active" behaviour
a <- a %>%
  mutate(
    BehaviourFocal = ifelse(IDIndividual1 == Actor, BehaviourIndiv1, BehaviourIndiv2)
  )

# Select only columns of interest and remove rows with missing values
a <- a %>%
  dplyr::select(Date, IDIndividual1, Focal = IDIndividual1, Actor, Receiver, BehaviourFocal, Group) %>%
  filter(Actor != "", Receiver != "")

# Add Duration column with NA values
a <- a %>%
  mutate(Duration = NA)  %>%
  mutate(BehaviourFocal = ifelse(BehaviourFocal %in% c("gr", "bgr"), "Groom", BehaviourFocal)) %>%
  mutate(BehaviourFocal = ifelse(BehaviourFocal %in% c("sg", "sw"), "Contact", BehaviourFocal)) %>%
  mutate(BehaviourFocal = ifelse(BehaviourFocal %in% c("ap", "bap"), "Approach", BehaviourFocal))

#write.csv(a, "G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Affiliative_DSI.CSV", row.names = F)
#write.csv(a, "C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Affiliative_DSI.CSV", row.names = F)

#### CREATING *PROXIMITY* DSI FILE ####

# Format: date focal actor receiver beh dur 
# getting rid of doubles!
# if (ID1[0] == ID2[-1] && ID1[-1] == ID2[0]) 
#   delete row

# use both behaviour and distance

s <- s_new %>% 
  filter(Group %in% MSGroups)

# Function to clean nearest neighbors data
clean_nearest_neighbors <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      IndArmLength = ifelse(DistanceNNA %in% c("Contact", "Arm length") & !(NNAdult %in% strsplit(IndArmLength, "[; ]")[[1]]),
                            paste(IndArmLength, NNAdult, sep = ifelse(!is.na(IndArmLength) & nchar(IndArmLength) > 0, "; ", "")), IndArmLength),
      IndArmLength = ifelse(DistanceNNJ %in% c("Contact", "Arm length") & !(NNJuvenile %in% strsplit(IndArmLength, "[; ]")[[1]]),
                            paste(IndArmLength, NNJuvenile, sep = ifelse(!is.na(IndArmLength) & nchar(IndArmLength) > 0, "; ", "")), IndArmLength),
      Ind2m = ifelse(DistanceNNA %in% c("1m", "2m") & !(NNAdult %in% strsplit(Ind2m, "[; ]")[[1]]),
                     paste(Ind2m, NNAdult, sep = ifelse(!is.na(Ind2m) & nchar(Ind2m) > 0, "; ", "")), Ind2m),
      Ind2m = ifelse(DistanceNNJ %in% c("1m", "2m") & !(NNJuvenile %in% strsplit(Ind2m, "[; ]")[[1]]),
                     paste(Ind2m, NNJuvenile, sep = ifelse(!is.na(Ind2m) & nchar(Ind2m) > 0, "; ", "")), Ind2m),
      Ind5m = ifelse(DistanceNNA %in% c("3m", "4m", "5m") & !(NNAdult %in% strsplit(Ind5m, "[; ]")[[1]]),
                     paste(Ind5m, NNAdult, sep = ifelse(!is.na(Ind5m) & nchar(Ind5m) > 0, "; ", "")), Ind5m),
      Ind5m = ifelse(DistanceNNJ %in% c("3m", "4m", "5m") & !(NNJuvenile %in% strsplit(Ind5m, "[; ]")[[1]]),
                     paste(Ind5m, NNJuvenile, sep = ifelse(!is.na(Ind5m) & nchar(Ind5m) > 0, "; ", "")), Ind5m)
    ) %>%
    dplyr::ungroup()
}


# Apply the cleaning function to the dataframe
s <- clean_nearest_neighbors(s)

# Remove interactions with "InterObs" == "Yes" and select relevant columns
s1 <- s %>%
  dplyr::select(Date, Group, Time, IDIndividual1, Behaviour, BehaviourType, IDPartners, IndArmLength, Ind2m, Ind5m) %>%
  mutate(Time = format(as.POSIXct(Time, format = "%H:%M:%S"), "%H:%M:%S")) %>%
  filter(!is.na(IDIndividual1), IDIndividual1 != "")

# Load in old data and merge
#s_old <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Scan_old_cleaned.CSV")
# too annoying to do the s_old into scan data. I will just merge what I already have from josie
#s_old <- read.csv("/Users/mariagranell/Repositories/data/Oct2021-May2022/Scan_10.2021-05.2022.csv")

s_complete <- s #rbind(s, s_old)

# Select affiliative interactions for behaviour data
b <- s_complete %>%
  filter(Behaviour %in% c("Affiliative", "Social")) %>%
  mutate(
    IDIndividual1 = as.factor(IDIndividual1),
    IDPartners = as.factor(IDPartners)
  ) %>%
  filter(!is.na(IDPartners), IDPartners != "")

# Remove duplicate interactions
b <- b %>%
  mutate(pair = pmap_chr(list(IDIndividual1, IDPartners), ~paste(sort(c(..1, ..2)), collapse = "-"))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  dplyr::select(-pair)

# Change old behaviours into new ones
b <- b %>%
  mutate(
    BehaviourType = case_when(
      BehaviourType == "Being groomed" ~ "bgr",
      BehaviourType == "Groom" ~ "gr",
      BehaviourType == "Infant handle" ~ "ih", 
      BehaviourType == "Mouth to mouth" ~ "mc", 
      BehaviourType == "Nurse" ~ "nu",
      BehaviourType == "Play (other)" ~ "pl",
      BehaviourType == "Present" ~ "pr", 
      TRUE ~ BehaviourType  # Keep the original value if no match is found
    )
  )

# Determine Actor and Receiver based on BehaviourType, select relevant columns and add duration
active_beh <- c('gr', 'sg', 'sw', 'em', 'pl')
b <- b %>%
  mutate(
    Actor = ifelse(BehaviourType %in% active_beh, as.character(IDIndividual1), as.character(IDPartners)),
    Receiver = ifelse(BehaviourType %in% active_beh, as.character(IDPartners), as.character(IDIndividual1))
  ) %>%
  dplyr::select(Date, Focal = IDIndividual1, Actor, Receiver, BehaviourFocal = BehaviourType, Group) %>%
  mutate(Duration = NA) 

## Proximity ##

# Assign group scan observation numbers
# Check for and remove rows with non-numeric Time values
s <- s_complete %>%
  dplyr::select(Date, Time, IDIndividual1, IDPartners, Group, Behaviour, BehaviourType, everything()) %>%
  mutate(Time_POSIXct = as.POSIXct(Time, format = "%H:%M:%S")) %>%
  filter(!is.na(IDIndividual1), IDIndividual1 != "",
         !is.na(as.POSIXct(Time, format = "%H:%M:%S")))

# Assign group scan observation numbers
s <- s %>%
  arrange(Date, Group, Time_POSIXct) %>%
  mutate(
    Obs.nr = cumsum(
      Date != lag(Date, default = first(Date)) |
        Group != lag(Group, default = first(Group)) |
        as.numeric(Time_POSIXct) - as.numeric(lag(Time_POSIXct, default = first(Time_POSIXct))) > 600
    )
  )

# Split the data for different distances
arm <- s %>%
  dplyr::select(Date, Focal = IDIndividual1, IDPartners, Time, Group, Obs.nr, IndArmLength, Behaviour) %>%
  separate_rows(IndArmLength, sep = "[; ]")

two <- s %>%
  dplyr::select(Date, Focal = IDIndividual1, IDPartners, Time, Group, Obs.nr, Ind2m) %>%
  separate_rows(Ind2m, sep = "[; ]")

five <- s %>%
  dplyr::select(Date, Focal = IDIndividual1, IDPartners, Time, Group, Obs.nr, Ind5m) %>%
  separate_rows(Ind5m, sep = "[; ]")

# Remove duplicate entries for arm length
arm <- arm %>%
  filter(!(Behaviour %in% c("Affiliative", "Social") & IDPartners == IndArmLength))

# Function to remove duplicate dyads and count deletions
remove_duplicates <- function(df, focal_col, partner_col) {
  initial_rows <- nrow(df)
  
  df_clean <- df %>%
    mutate(dyad = map2_chr(!!sym(focal_col), !!sym(partner_col), ~paste(sort(c(.x, .y)), collapse = "-"))) %>%
    group_by(Obs.nr, dyad) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    dplyr::select(-dyad)
  
  final_rows <- nrow(df_clean)
  rows_deleted <- initial_rows - final_rows
  
  return(list(df_clean = df_clean, rows_deleted = rows_deleted))
}

# Remove duplicates and count deletions for arm, two, and five dataframes
arm_result <- remove_duplicates(arm, "Focal", "IndArmLength")
two_result <- remove_duplicates(two, "Focal", "Ind2m")
five_result <- remove_duplicates(five, "Focal", "Ind5m")

arm <- arm_result$df_clean
two <- two_result$df_clean
five <- five_result$df_clean

# Print the number of rows deleted
cat("Number of rows deleted in arm:", arm_result$rows_deleted, "\n")
cat("Number of rows deleted in two:", two_result$rows_deleted, "\n")
cat("Number of rows deleted in five:", five_result$rows_deleted, "\n")

# Add BehaviourFocal and Duration columns and rename columns
arm <- arm %>%
  mutate(BehaviourFocal = "Contact", Duration = NA, Actor = Focal) %>%
  dplyr::select(Date, Focal, Receiver = IndArmLength, BehaviourFocal, Duration, Actor, Group)

two <- two %>%
  mutate(BehaviourFocal = "Two", Duration = NA, Actor = Focal) %>%
  dplyr::select(Date, Focal, Receiver = Ind2m, BehaviourFocal, Duration, Actor, Group)

five <- five %>%
  mutate(BehaviourFocal = "Five", Duration = NA, Actor = Focal) %>%
  dplyr::select(Date, Focal, Receiver = Ind5m, BehaviourFocal, Duration, Actor, Group)

# Bind all dataframes together
prox <- bind_rows(b, arm, two, five) %>%
  dplyr::select(Date, Focal, Actor, Receiver, BehaviourFocal, Duration, Group) %>%
  filter(!Receiver %in% "",
         Date > "2021-01-01") %>%
  mutate(BehaviourFocal = ifelse(BehaviourFocal %in% c("gr", "bgr"), "Groom", BehaviourFocal)) %>%
  mutate(BehaviourFocal = ifelse(BehaviourFocal %in% c("sg", "sw"), "Contact", BehaviourFocal)) %>%
  mutate(BehaviourFocal = ifelse(BehaviourFocal %in% c("ap", "bap"), "Approach", BehaviourFocal))

# Print the final dataframe structure
str(prox)



write.csv(prox, "G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Scan_DSI.CSV", row.names = F)

### Merge all three files ####
#f <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Focal_DSI.CSV")
#a <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Affiliative_DSI.CSV")
#prox <- read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/Scan_DSI.CSV")

#f <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Focal_DSI.CSV")
#a <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Affiliative_DSI.CSV")
#prox <- read.csv("G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/Scan_DSI.CSV")

# First make sure that in ad lib and scans our experimental individuals are always focals: 
# Focals are experimental individuals 

# Convert Date to proper format and filter data
ind <- ind %>%
  mutate(Date = as.Date(Date, format = "%Y/%m/%d")) %>%
  filter(Date < "2024-01-01", !FemaleID %in% c("Guat", "", "Ghid"))

# Combine FemaleID and MaleID, then get unique IDs
individuals <- unique(c(ind$FemaleID, ind$MaleID))

# Function to ensure specified individuals are always focal
ensure_focal_individuals <- function(df, focal_list) {
  df %>%
    rowwise() %>%
    mutate(
      Focal = if_else(Actor %in% focal_list, Actor, 
                      if_else(Receiver %in% focal_list, Receiver, Focal))
    ) %>%
    ungroup()
}

# Apply the function to the dataframe 'a'
a <- ensure_focal_individuals(a, individuals)

# Apply the function to the dataframe 'prox'
prox <- ensure_focal_individuals(prox, individuals)

# Rename columns so they align
colnames(f)[5] <- "BehaviourFocal"
all <- rbind(f, a, prox) %>%
  filter(Date > "2022-01-01")



write.csv(all, "G:/Other computers/New laptop/PhD/IVP DATA/Shared stakes/File_DSI.CSV", row.names = F)
write.csv(all, "C:/Users/josef/Documents/PhD/IVP DATA/Shared stakes/File_DSI.CSV", row.names = F)

# Script to calculate the dominance hierarchy
# to do this scrip you need a life history file and
# agoinistic data

# Changes madre from the original script
# Behaviours added in the categores aggresive and retreat
# Changed the decide.sin funciton to acommodate for passive actions like "being hit"

library(EloRating)
library(dplyr)
library(lubridate)
source("/Users/mariagranell/Repositories/data/functions.R")

#### CREATING *FOCAL* ELO FILE ####
focal <- read.csv("/Users/mariagranell/Repositories/elo-sociality/data/darting2023/combinedFocal_2022-06_2023-5.csv")

fo <- focal %>%
  mutate(Date = as.character(ymd(Date))) %>%
  mutate_all(~na_if(., "")) %>%
  filter(
    # Select the study period of the second darting
    Date > "2022-07-01" & Date < "2023-02-01",
    # remove when is in context interobs
    Interobs != "Yes",
    # select only affiliative interactions
    Behaviour == "Agonistic"
    )
# ignore obs.nr

# Make the focal data into individual df -------

# The dataframe fo12 only has 1 collumn of behaviour
# thus is different how to decide the winner and loser
fo12 <- fo %>% dplyr::select(Date, Time, Group, IDIndividual1, BehaviourFocal,IDIndividual2,Remarks)

# however all these combinations of behaviours can be united with the regular adlib agonistic file, check below
{fo1red <- fo %>% # focal redirects to someone
  filter(AgonisticInteraction == "Focal Redirects") %>%
  dplyr::select(Date, Time, Group, IDIndividual1, RedirectionBehaviour,IDRedirection, VictimRedResponse,Remarks) %>%
  rename(BehaviourIndiv1 = RedirectionBehaviour, IDIndividual2 = IDRedirection, BehaviourIndv2 = VictimRedResponse)
foSupFoc <- fo %>% # A supporter aids the focal
  filter(AgonisticInteraction == "Support to Focal") %>%
  dplyr::select(Date, Time, Group, IDSupporters, SupportersBehaviour,IDIndividual2, VictimSupResponse,Remarks) %>%
  rename(IDIndividual1 = IDSupporters, BehaviourIndiv1 = SupportersBehaviour, BehaviourIndv2 = VictimSupResponse) %>%
  split_behaviours("IDIndividual1", ";") %>% distinct(.) # ignore warnings
foSupPar <- fo %>% # A supporter aids the parner
  filter(AgonisticInteraction == "Support to Partner") %>%
  dplyr::select(Date, Time, Group, IDSupporters, SupportersBehaviour,IDIndividual1, VictimSupResponse,Remarks) %>%
  rename(IDIndividual1 = IDSupporters, BehaviourIndiv1 = SupportersBehaviour, IDIndividual2 = IDIndividual1, BehaviourIndv2 = VictimSupResponse) %>%
  split_behaviours("IDIndividual1", "; ") %>% distinct(.) # ignore warnings

fo_2behav <- rbind(fo1red,foSupFoc,foSupPar) %>%
  # remove interactions with lac of information
  filter(!is.na(IDIndividual1), !is.na(IDIndividual2),
         !str_detect(IDIndividual1, regex("unk", ignore_case = TRUE)),
         !str_detect(IDIndividual2, regex("unk", ignore_case = TRUE)))

rm(fo1red,foSupFoc,foSupPar,foa)}

# scans data -------------------
scans <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/darting2023/combinedScan_2022-06_2023-5.csv')

# Select the study period of the first darting
sc <- scans %>%
  mutate(Date = as.character(ymd(Date))) %>%
  filter(
    # Select the study period of the second darting
    Date > "2022-07-01" & Date < "2023-02-01",
    # Select only agonistic behaviours
    Behaviour == "Agonistic") %>%
  # select the same collumns as fo12 to merge
  rename(BehaviourFocal = BehaviourType, IDIndividual2 = IDPartners) %>%
  dplyr::select(Date,Time,Group, IDIndividual1,BehaviourFocal,IDIndividual2,Remarks)


#### Winner/loser for Focal and Scan data ####

# this file has to have Date,Time,Group, IDIndividual1,BehaviourFocal,IDIndividual2,Remarks
df_fosc <- rbind(fo12,sc) %>%
  mutate_all(~ na_if(., "")) %>%  # make sure empty means NA
  filter(!is.na(IDIndividual1), !is.na(IDIndividual2), !is.na(BehaviourFocal)) # remove all NA from these collumns

# In order to add the social rank of initiators & targets and thus get their social rank differences,
# we first need decide a clear winner (defined as the individual being the most aggressive, i.e. who used the most
# intense aggressive behaviour and a clear loser (defined as the individual showing the most submissive behaviours and/or
# ending the conflict by moving away from the opponent.

# First definitions of the beahviours:
focal_loser_beh <- c('fl', 'rt', 'av', 'ja', 'cr', 'ss', 'gu', 'le','sc')
partner_loser_beh <- paste0("b", focal_loser_beh)
focal_winner_beh <- list(cat_3=c('bi', 'gb', 'hi', 'fi', 'hh', 'so'), # most aggressive
                         cat_2=c('ch', 'st', 'tp','at'),
                         cat_1=c('ac', 'dp', 'su', 'fh', 'sf', 'bd', 'hb',
                                 'ap', 'ap0', 'ap1', 'ap2', 'ap3', 'ap5', # we consider ap and wb from 5 to 0
                                 'wb', 'wb0', 'wb1', 'wb2', 'wb3', 'wb5')) # least aggressive
partner_winner_beh <- lapply(focal_winner_beh, function(x) paste0("b", x))

### Write a function to decide for each obs who is the winner (decide_winner) based on categories of behaviours specific to vic or agg
# to determine a winner we will do it based on instensity. That is, Higher intensity (cat_3) behaviors have the first opportunity to determine the winner.
# If the counts are equal in cat_3, we will consider the counts in cat_2 until we reach retreat behaviours.
# This gives a natural precedence to more intense behaviors.
# In the end we want to end up with Date, Time, Group, winner, loser columns

# Sample data. Useful to understand the code
#df_fosc <- data.frame(strings = c("bhi.fl.rt bch", "ch.hi.at brt.ble", "fl.bfl bi.bst", "bi bch.bat", "le", "bch.ch"),
#                 IDIndividual1 = "a", IDIndividual2 = "b")

# split the behaviours in a list
df_fosc$splitbehaviour <- lapply(df_fosc$BehaviourFocal, function(x) unlist(strsplit(x, split = "[. ]")))

# Function to count occurrences of behaviours in the list of winner or loser
count_occurrences <- function(list_elem, focal_list) {
  sum(sapply(list_elem, function(x) x %in% focal_list))
}

# Function to determine the winner based on hierarchical behavior categories
decide_winner <- function(behaviors) {
  behaviors <- unlist(behaviors)  # Ensure behaviors is a vector
  points_focal <- 0
  points_partner <- 0

  # Evaluate categories in order of intensity
  for (cat in c("cat_3", "cat_2", "cat_1")) {
    points_focal <- count_occurrences(behaviors, focal_winner_beh[[cat]])
    points_partner <- count_occurrences(behaviors, partner_winner_beh[[cat]])

    if (points_focal > points_partner) return("focal")
    if (points_partner > points_focal) return("partner")
  }

  # If still a draw, evaluate retreat behaviors
  points_focal <- count_occurrences(behaviors, focal_loser_beh)
  points_partner <- count_occurrences(behaviors, partner_loser_beh)

  if (points_focal < points_partner) return("focal")
  if (points_partner < points_focal) return("partner")

  return("draw")
}

# Apply the decide_winner function to each row
df_fosc <- df_fosc %>%
  mutate(win_ID = sapply(splitbehaviour, decide_winner))

# investigate the draws
View(df_fosc %>% filter(win_ID == "draw"))
# Clean the dataframe and assing te anmes of winners and losers
df_fosc <- df_fosc %>% filter(win_ID != "draw") %>%
  mutate(
  winner = ifelse(win_ID == "focal", IDIndividual1, IDIndividual2),
  loser = ifelse(win_ID != "focal", IDIndividual1, IDIndividual2),
) %>% select(Date, Time, Group, winner, loser)




#### CREATING *ADLIB* ELO FILE ####
## Load your data using the "Creating ago file"
dd <- read.csv("/Users/mariagranell/Repositories/elo-sociality/data/FinalAgonistic.csv", header=TRUE, na.strings=c(""," ","NA")) %>%
  dplyr::select(-X)

# for the darting of 2023 I will take data for the ranking and the sociality vectors from 2022-07-01 until 2023-02-01.
# for more information check the README of combination darting

d <- dd %>% filter(Date >= "2022-07-01" & Date <= "2023-02-01")

## Assumptions Elo-rating package:
# File is ordered by date
# There are no empty cells
# IDs occur more than once
# Loser ID is different from winner ID

# Order by date
d <- dplyr::arrange(d, Date)

# Omit NA's
d <- na.omit(d)

# set everything to lower case
d$AggressorBehaviour <- tolower(d$AggressorBehaviour)
d$VictimBehaviour <- tolower(d$VictimBehaviour)

# unite the data from focal files
d <- d %>% rbind(fo_2behav %>%
                        rename(Aggressor = IDIndividual1, AggressorBehaviour = BehaviourIndiv1,
                               Victim = IDIndividual2, VictimBehaviour = BehaviourIndv2) %>%
                        dplyr::select(-Remarks))


#### Winner/loser ####
## Written by Stephanie Mercier ##

# In order to add the social rank of initiators & targets and thus get their social rank differences, 
# we first need to create the "fight.data" from data which include all agonistic interactions in which 
# there is a clear winner (defined as the individual being the most aggressive, i.e. who used the most 
# intense aggressive behaviour: 
# 1. approach -> approach.data, 
# 2. aggressive -> st,at,vo,ag+tp/dp, 
# 3. chase -> ch & 
# 4. physical contact -> bi.gb.hi.fi) 
# and a clear loser (defined as the individual showing the most submissive behaviours and/or 
# ending the conflict by moving away from the opponent -> rt,av,fl,le,re,ja,cr). 

fight.data <- d

### Write a function to decide for each obs who is the winner (decide.win) based on categories of behaviours specific to vic or agg
# Victim = individual ending up the conflict being the loser, thus considered as the most submissive one
ret_beh <- c('fl', 'rt', 'av', 'ja', 'cr', 'ss', 'gu',
                        'le','sc') # these behaviour were added by MR on 20-05-2024

# Aggressor = from the least (cat_1) to the most aggressive behaviours (cat_3), the animal performing the most intense aggressive behaviour is the winner
agg_cat <- list(cat_3=c('bi', 'gb', 'hi', 'fi', 'hh', 'so'),
                cat_2=c('ch', 'st', 'tp','at'), cat_1=c('ac', 'dp', 'su', 'fh', 'sf', 'bd',
                        'ap','wb', 'hb')) # these behaviour were added by MR on 20-05-2024


# write the function which defines winner/loser by looking at which individual ends the conflict and/or 
# which one performs the most aggressive behaviours
# I modified the function so, when there is a passive form is not considered an aggressive behaviour
decide.win <- function(beh_x, beh_y) {
  x <- 0
  y <- 0

  # Count retreat behaviors
  for (beh_ in ret_beh) {
    # Match all occurrences of the behavior
    x_matches <- gregexpr(beh_, beh_x)[[1]] # agressor
    y_matches <- gregexpr(beh_, beh_y)[[1]] # victim

    # Filter out matches that are preceded by 'b', i.e. remove the passive actions, "being ..."
    x_filtered <- x_matches[x_matches != -1 & !sapply(x_matches, function(pos) substr(beh_x, pos-1, pos-1) == "b")]
    y_filtered <- y_matches[y_matches != -1 & !sapply(y_matches, function(pos) substr(beh_y, pos-1, pos-1) == "b")]

    x <- x + length(x_filtered)
    y <- y + length(y_filtered)
  }

  # Determine initial winner based on retreat behaviors
  if (x < y) return(1)
  if (y < x) return(2)

  # Count aggressive behaviors
  for (cat_ in agg_cat) {
    x <- 0
    y <- 0

    for (beh_ in cat_) {
      # Match all occurrences of the behavior
      x_matches <- gregexpr(beh_, beh_x)[[1]]
      y_matches <- gregexpr(beh_, beh_y)[[1]]

      # Filter out matches that are preceded by 'b', i.e. remove the passive actions, "being ..."
      x_filtered <- x_matches[x_matches != -1 & !sapply(x_matches, function(pos) substr(beh_x, pos-1, pos-1) == "b")]
      y_filtered <- y_matches[y_matches != -1 & !sapply(y_matches, function(pos) substr(beh_y, pos-1, pos-1) == "b")]

      x <- x + length(x_filtered)
      y <- y + length(y_filtered)
    }

    if (x > y) return(1)
    if (y > x) return(2)
  }

  return(0)
}


# add win/lose to fight.data
fight.data$win_lose <- mapply(FUN = decide.win, 
                              fight.data$AggressorBehaviour,
                              fight.data$VictimBehaviour)

# write the function which allows to calculate for each sequence of obs between two individuals, 
# how many aggressive behaviours of the different severity each of them performed 
# (starting with the most intense one, i.e., involving physical contact), and as soon as one of the individuals
# has a greater number of aggressive behaviours performed, we declare this individual as the winner 
# (ignoring the rest of the sequence, but making sure that this one is not the individual also ending up the conflict by moving away...)

compute.wins <- function(l.data, ind_x, ind_y){
  
  fight.vec <- l.data$win_lose
  
  return(c(sum(c(fight.vec[l.data$Aggressor==ind_x]==1,
                 fight.vec[l.data$Victim==ind_x]==2)),
           sum(c(fight.vec[l.data$Aggressor==ind_y]==1,
                 fight.vec[l.data$Victim==ind_y]==2))))  
}

# Rename lol don't know why but you save your data
seq.data <- fight.data

# define winner & loser using win_lose as 1 means that Aggressor is the most aggressive ind so the winner, 
# whereas 2 means that Victim is the most aggressive ind and thus the winner!

seq.data$winner <- NA
seq.data$BehaviourW <- NA
seq.data$loser <- NA
seq.data$BehaviourL <- NA

i=1
for(i in seq_len(nrow(seq.data))){
  if (seq.data$win_lose[i]=="1") seq.data$winner[i] <- as.character(seq.data$Aggressor[i])
  if (seq.data$win_lose[i]=="1") seq.data$BehaviourW[i] <- as.character(seq.data$AggressorBehaviour[i])
  if (seq.data$win_lose[i]=="1") seq.data$loser[i] <- as.character(seq.data$Victim[i])
  if (seq.data$win_lose[i]=="1") seq.data$BehaviourL[i] <- as.character(seq.data$VictimBehaviour[i])
  if (seq.data$win_lose[i]=="2") seq.data$winner[i] <- as.character(seq.data$Victim[i])
  if (seq.data$win_lose[i]=="2") seq.data$BehaviourW[i] <- as.character(seq.data$VictimBehaviour[i])
  if (seq.data$win_lose[i]=="2") seq.data$loser[i] <- as.character(seq.data$Aggressor[i])
  if (seq.data$win_lose[i]=="2") seq.data$BehaviourL[i] <- as.character(seq.data$AggressorBehaviour[i])
}

# write undecided interaction -> all data where win_lose=0 as it corresponds to interactions with no clear winner/loser
seq.data$Draw <- FALSE # when decided interaction in which clear winner/loser is known 
NotClear <- which(seq.data$win_lose=="0")
seq.data[NotClear,"Draw"] <- TRUE
table(seq.data$Draw) 

rownames(seq.data) <- NULL
#seq.data <- seq.data[,-c(12,13)]


# add intensity: mild, chase, severe
seq.data$intensity <- NA
mild <- which(grepl("ap|ac|ag|at|dp|tp|su|wb|vo|gu|sc|ap0|ap2|ap10|hb|ig",seq.data$BehaviourW)=="TRUE")
seq.data[mild,14] <- "mild"
chase <- which(grepl("ch|st|tp|at",seq.data$BehaviourW)=="TRUE")
seq.data[chase,14] <- "chase" 
severe <- which(grepl("bi|gb|hi|fi|hh|so",seq.data$BehaviourW)=="TRUE")
seq.data[severe,14] <- "severe"

table(seq.data$intensity)
1761+815+540 # 3116

# Exclude the interactions that had no clear winners
X <- which(is.na(seq.data$intensity))
seq.data <- seq.data[-X,]

rownames(seq.data) <- NULL


seq.data$winner <- as.factor(seq.data$winner)
nlevels(seq.data$winner)
seq.data$loser <- as.factor(seq.data$loser)
nlevels(seq.data$loser)

d <- seq.data
d$Date <- as.Date(d$Date)
str(d)

# Correct mistakes
d$winner <- as.character(d$winner)
d$loser <- as.character(d$loser)

# merge with the focal and scan data
d <- d %>% select(Date, Time, Group, winner, loser) %>%
  rbind(df_fosc)

# change group names to short names
d <- change_group_names(d,"Group")

## Link life history to individuals #### # modified to use dplyr

# Import data
LHdata <- read.csv2("/Users/mariagranell/Repositories/elo-sociality/data/darting2023/factchecked_LH_171123.csv", header = T, stringsAsFactors = F, na.strings = c('NA', 'Not yet')) %>%
    # add info that I am missing
  mutate(Sex = case_when(
    AnimalCode == "Kom" ~ "M",
    AnimalCode == "Ted" ~ "M",
    AnimalCode == "Zeu" ~ "M",
    AnimalCode == "Enge" ~ "F",
    AnimalCode == "Gese" ~ "F", TRUE ~ Sex),
         DOB_estimate = case_when( # so they are all adults
    AnimalCode == "Kom" ~ "2010-01-01",
    AnimalCode == "Ted" ~ "2010-01-01",
    AnimalCode == "Zeu" ~ "2010-01-01",
    AnimalCode == "Enge" ~ "2010-01-01",
    AnimalCode == "Gese" ~ "2010-01-01", TRUE ~ DOB_estimate),
         StartDate_mb = case_when( # so they are included in the study
    AnimalCode == "Kom" ~ "2022-01-31",
    AnimalCode == "Ted" ~ "2022-01-31",
    AnimalCode == "Zeu" ~ "2022-01-31",
    AnimalCode == "Enge" ~ "2022-01-31",
    AnimalCode == "Gese" ~ "2022-01-31", TRUE ~ StartDate_mb),
         EndDate_mb = case_when( # so they are included in the study
    AnimalCode == "Kom" ~ "2023-01-31",
    AnimalCode == "Ted" ~ "2023-01-31",
    AnimalCode == "Zeu" ~ "2023-01-31",
    AnimalCode == "Enge" ~ "2023-01-31",
    AnimalCode == "Gese" ~ "2023-01-31", TRUE ~ EndDate_mb),
         Group_mb = case_when(
    AnimalCode == "Kom" ~ "BD",
    AnimalCode == "Ted" ~ "LT",
    AnimalCode == "Zeu" ~ "IF",
    AnimalCode == "Enge" ~ "BD",
    AnimalCode == "Gese" ~ "BD", TRUE ~ Group_mb)) %>%
  # remove individuals that were not present in the study period
  filter(EndDate_mb >= "2022-07-01" & StartDate_mb <= "2023-01-31") %>%
  filter(AnimalCode != "Apa" | Tenure_type != "MigrationGroup1") %>%
  distinct(.)


LHdata <- change_group_names(LHdata,"Group_mb")

# Firstly I am going to make sure that all the OtherID names used in the dataframe are updated with the AnimalCode,
# for both the collumns winner and loser
d <- d %>%
  left_join(LHdata %>% select(OtherID, AnimalCode), by = c("winner" = "OtherID")) %>%
  mutate(winner = coalesce(AnimalCode, winner)) %>%
  select(-AnimalCode) %>%
  left_join(LHdata %>% select(OtherID, AnimalCode), by = c("loser" = "OtherID")) %>%
  mutate(loser = coalesce(AnimalCode, loser)) %>%
  select(-AnimalCode) %>%
  # correct Duckies entry
  mutate(winner = ifelse(winner == "Duckie", "Duc", winner),
         loser = ifelse(loser == "Duckie", "Duc", loser))

# lh for the winner:
lh_winner <-LHdata %>%
  dplyr::select(AnimalCode, Sex, DOB_estimate, FirstDate, StartDate_mb, Group_mb) %>%
  rename(WinnerSex = Sex, DOBAgg = DOB_estimate, FRAgg = FirstDate, DIAgg = StartDate_mb, GpAgg = Group_mb)

# lh for the loser:
lh_loser <-LHdata %>%
  dplyr::select(AnimalCode, Sex, DOB_estimate, FirstDate, StartDate_mb, Group_mb) %>%
  rename(LoserSex = Sex, DOBVic = DOB_estimate, FRVic = FirstDate, DIVic = StartDate_mb, GpVic = Group_mb)

join_d2 <- d %>%
  mutate(Date = as.character(Date)) %>%
  # merge the winner
  left_join(., lh_winner, by =c("winner" = "AnimalCode", "Group" = "GpAgg")) %>%
  left_join(., lh_loser, by =c("loser" = "AnimalCode",  "Group" = "GpVic")) %>%
  rowwise() %>%
  mutate(WinnerAge = add_age(DOBAgg, Date, "Years"),
         LoserAge = add_age(DOBVic, Date, "Years"))

df <- join_d2 %>% select(Date, Time, Group, winner, loser, WinnerSex, WinnerAge, LoserSex, LoserAge)

# Adfdf AgeClass column to the dfataframe pdf
df$AgeClassWinner <- get_age_class(df$WinnerSex, df$WinnerAge)
df$AgeClassLoser <- get_age_class(df$LoserSex, df$LoserAge)

# Investigate NA ####

# NewMale110622 is Haw
df <-df %>% mutate_all(~ ifelse(. == "NewMale110622", "Haw", .))

# winner and loser, only Ves, Yam and Yub are missing, I decide to take the loses. If they are from CR or IF or juveniles
# since we will delte them later
a <- df %>%
   filter(is.na(LoserAge)) %>%
  distinct(loser)

df <- na.omit(df)



# Keep only adult females
df_F <- df %>% filter(AgeClassLoser == "AF" & AgeClassWinner == "AF")
df_M <- df %>% filter(AgeClassWinner %in% c("AM", "SM") & AgeClassLoser %in% c("AM", "SM"))

## From now on you perform the hierarchy on Males or females
## select data baundaries for df -----------------
# Calculate dominance hierarchy for the first darting (2023)
# starting date: "2022-07-01", ending date "2023-01-31"

# check which groups have enough observations for the data range
df_M %>% group_by(Group) %>% summarize(n = n())
# only Ak, Bd, Kb and Nh

# Divide into groups
BD <- subset(df_M,df_M$Group == ("BD"))
NH <- subset(df_M,df_M$Group == ("NH"))
KB <- subset(df_M,df_M$Group == ("KB"))
AK <- subset(df_M,df_M$Group == ("AK"))


# check which groups have enough data


### Define a function to extract the elo ratiing information. The funtion requires:
# elo_seq = data from performing an eloranking hierarchy
# name_range = a list of names that would describe at which standpoints are you extracting the elo rating
# start_date = a date from which where you want to extract the elo. extract_elo will compute all interactions from the
# starting date of the data used in the elo.seq() until the selected date.
process_date_range <- function(elo_seq, name_range, selected_date) {
  Gp_data <- extract_elo(elo_seq, selected_date, standardize = TRUE)
  Gp_data <- data.frame(AnimalCode = names(Gp_data), Rank = as.numeric(Gp_data)) %>%
    mutate(n_males = nrow(.), range = name_range)
  return(Gp_data)
}

#### Baie Dankie  ####

# Darted individuals in BD
# F: Miel, Ouli
# M: Xin, Nak, Win, Dix, Tot, Xia, Bob

# To incorporate presence data
# First read the presence matrix
BDpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceBD2020-2023.csv", header=TRUE) %>%
  dplyr::select(-X, -V1) %>% # I get these two weird columns called "X" and "V1" that I delete
  mutate(Date = ymd(Date))

# Check if data look good
str(BDpres)
head(BDpres)
tail(BDpres)

BDpres[is.na(BDpres)] <- 0

# Check whether data looks good
seqcheck(winner=BD$winner, loser=BD$loser, Date=BD$Date, draw = NULL, presence=BDpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

BDamELO <- elo.seq(winner = BD$winner, loser=BD$loser, Date=BD$Date, presence = BDpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

eloplot(BDamELO)
summary(BDamELO)

# starting date: "2022-07-01", ending date "2023-01-31"
# Define the starting dates and corresponding names
date_ranges <- list(
  c("jun", "2022-07-04"), #adjusted for BD
  c("jul", "2022-08-01"),
  c("aug", "2022-09-01"),
  c("sep", "2022-10-01"),
  c("oct", "2022-11-01"),
  c("nov", "2022-12-01"),
  c("dec", "2023-01-01"),
  c("jan", "2023-01-30") # we includde january as a month for the hair to grow
)

# Initialize an empty data frame
BDrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  BD_data <- process_date_range(BDamELO, range_name[], start_date[])
  BDrank <- rbind(BDrank, BD_data)
}
BDrank <- BDrank %>% mutate(Group = "BD")

#### Ankhase ####
# To incorporate presence data
# First read the presence matrix

AKpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceAK2020-2023.csv", header=TRUE) %>%
  dplyr::select(-X) %>% # I get these two weird columns called "X" and "V1" that I delete
  mutate(Date = ymd(Date))

# Check if data look good

str(AKpres)
head(AKpres)
tail(AKpres)

# Check whether data looks good
seqcheck(winner=AK$winner, loser=AK$loser, Date=AK$Date, draw = NULL, presence=AKpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

AKamELO <- elo.seq(winner = AK$winner, loser=AK$loser, Date=AK$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(AKamELO)
eloplot(AKamELO)

# Define the starting dates and corresponding names
date_ranges <- list(
  c("jun", "2022-07-07"), #adjusted for AK
  c("jul", "2022-08-01"),
  c("aug", "2022-09-01"),
  c("sep", "2022-10-01"),
  c("oct", "2022-11-01"),
  c("nov", "2022-12-01"),
  c("dec", "2023-01-01"),
  c("jan", "2023-01-31") # we includde january as a month for the hair to grow
)

# Initialize an empty data frame
AKrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  AK_data <- process_date_range(AKamELO, range_name, start_date)
  AKrank <- rbind(AKrank, AK_data)
}
AKrank <- AKrank %>% mutate(Group = "AK")

#### Noha ####

# To incorporate presence data
# First read the presence matrix

NHpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceNH2020-2023.csv", header=TRUE) %>%
  dplyr::select(-X) %>% # I get these two weird columns called "X" and "V1" that I delete
  mutate(Date = ymd(Date))

# Check whether data looks good
seqcheck(winner=NH$winner, loser=NH$loser, Date=NH$Date, draw = NULL, presence=NHpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

NHamELO <- elo.seq(winner = NH$winner, loser=NH$loser, Date=NH$Date, presence = NHpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(NHamELO)
eloplot(NHamELO)

# Define the starting dates and corresponding names
date_ranges <- list(
  c("jun", "2022-07-06"), #adjusted for NH
  c("jul", "2022-08-01"),
  c("aug", "2022-09-01"),
  c("sep", "2022-10-01"),
  c("oct", "2022-11-01"),
  c("nov", "2022-12-01"),
  c("dec", "2023-01-01"),
  c("jan", "2023-01-28") # we includde january as a month for the hair to grow
)

# Initialize an empty data frame
NHrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  NH_data <- process_date_range(NHamELO, range_name, start_date)
  NHrank <- rbind(NHrank, NH_data)
}
NHrank <- NHrank%>% mutate(Group = "NH")

#### Kubu ####

# To incorporate presence data
# First read the presence matrix

KBpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceKB2020-2023.csv", header=TRUE) %>%
  dplyr::select(-X) %>% # I get these two weird columns called "X" and "V1" that I delete
  mutate(Date = ymd(Date))

# Check if data look good

str(KBpres)
head(KBpres)
tail(KBpres)

# Check whether data looks good
seqcheck(winner=KB$winner, loser=KB$loser, Date=KB$Date, draw = NULL, presence=KBpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

KBamELO <- elo.seq(winner = KB$winner, loser=KB$loser, Date=KB$Date, presence = KBpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(KBamELO)
eloplot(KBamELO)

# Define the starting dates and corresponding names
date_ranges <- list(
  c("jun", "2022-07-01"), #adjusted for KB
  c("jul", "2022-08-01"),
  c("aug", "2022-09-01"),
  c("sep", "2022-10-01"),
  c("oct", "2022-11-01"),
  c("nov", "2022-12-01"),
  c("dec", "2023-01-01"),
  c("jan", "2023-01-30") # we includde january as a month for the hair to grow
)

# Initialize an empty data frame
KBrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  KB_data <- process_date_range(KBamELO, range_name, start_date)
  KBrank <- rbind(KBrank, KB_data)
}

KBrank <- KBrank %>% mutate(Group = "KB")


#### CREATE A TABLE WITH THE INDIVIDUALS OF INTEREST #############
elo_dmales <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/2023darting/merged_hormone_data_2023.csv") %>%
  dplyr::select(AnimalCode, Group, Sex, Age, Date_darting) %>%
  unique(.)

# CHANGE FOR EACH SEX, MALES OR FEMALES
# merge all the rank data
GP_rank <- BDrank %>%
  rbind(AKrank, NHrank, KBrank)

# How I calculated the variation in rank per idividual:
# First I summed all the real ranks of each month.                                    real_ranks = sum(Rank),
# Then I calculated for how many months the male was in the group                     n_months = n(),
# calculated the expected rank if that male would have not changed in the hierarchy
# that is taking the rank from october and multipliying it for the number of months   expected_rank = ifelse(any(range == "oct"), n() * Rank[range == "oct"], 0)
# lastly, calculated the variation in rank for that indv.                             variation_rank = real_ranks - expected_rank
# then i removed the previous months and kept the last month calculated per male since is the intresting one

# TODO all months from oct-6months (Lukas)

# calculate rank variation. And keep average rank of last months before june.
GP_rank2 <- GP_rank %>%
  group_by(AnimalCode, Group) %>%
  mutate(
    variation_rank = sum(Rank, na.rm = T) - ifelse(any(range == "jun"), n() * Rank[range == "jun"], 0)
  ) %>%
    mutate(range_numeric = case_when(
      range == "jun" ~ 1,
      range == "jul" ~ 2,
      range == "aug" ~ 3,
      range == "sep" ~ 4,
      range == "oct" ~ 5,
      range == "nov" ~ 6,
      range == "dic" ~ 7,
      range == "jan" ~ 8)) %>%
  arrange(AnimalCode, Group, desc(range_numeric))  %>%
  slice(1) %>% select(!range_numeric)

# Change for Males or Females
#write.csv(GP_rank2, "/Users/mariagranell/Repositories/elo-sociality/elo/darting2023/Rank_Males_jun2022-jan2023.csv", row.names = F)

# Combine males and females hierarchies ---------
f <- read.csv("/Users/mariagranell/Repositories/elo-sociality/elo/darting2023/Rank_Females_jun2022-jan2023.csv")
m <- read.csv("/Users/mariagranell/Repositories/elo-sociality/elo/darting2023/Rank_Males_jun2022-jan2023.csv")
f$Sex <- "female"
m$Sex <- "male"
combined <- f %>% rbind(.,m)
write.csv(combined, "/Users/mariagranell/Repositories/elo-sociality/elo/darting2023/Rank_jun2022-jan2023.csv", row.names = F)
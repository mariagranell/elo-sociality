# Script to calculate the dominance hierarchy
# to do this scrip you need a life history file and
# agoinistic data

library(EloRating)
library(dplyr)
source("/Users/mariagranell/Repositories/data/functions.R")

## Load your data using the "Creating ago file"
d <- read.csv("/Users/mariagranell/Repositories/elo-sociality/data/FinalAgonistic.csv", header=TRUE, na.strings=c(""," ","NA"))

## Assumptions Elo-rating package:
# File is ordered by date
# There are no empty cells
# IDs occur more than once
# Loser ID is different from winner ID

# Remove first column called "X"
d <- d[,c(2:8)]

# Order by date
d <- dplyr::arrange(d, Date)

# Omit NA's
d <- na.omit(d)

# set everything to lower case
d$AggressorBehaviour <- tolower(d$AggressorBehaviour)
d$VictimBehaviour <- tolower(d$VictimBehaviour)


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
ret_beh <- c('fl', 'rt', 'av', 'ja', 'cr', 'ss', 'gu')

# Aggressor = from the least (cat_1) to the most aggressive behaviours (cat_3), the animal performing the most intense aggressive behaviour is the winner
agg_cat <- list(cat_3=c('bi', 'gb', 'hi', 'fi', 'hh', 'so'), 
                cat_2='ch', cat_1=c('ac', 'at', 'dp', 'tp', 'st', 'su', 'fh', 'sf', 'hb', 'bd'))


# write the function which defines winner/loser by looking at which individual ends the conflict and/or 
# which one performs the most aggressive behaviours
decide.win <- function(beh_x, beh_y){
  x <- 0
  y <- 0
  
  for(beh_ in ret_beh){
    x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
    y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
  }
  if(x < y) return(1)
  if(y < x) return(2)
  
  for(cat_ in agg_cat){
    x <- 0
    y <- 0
    
    for(beh_ in cat_){
      x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
      y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
    }
    
    if(x > y) return(1)
    if(y > x) return(2)
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
for(i in 1:nrow(seq.data)){
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
mild <- which(grepl("ap|ac|ag|at|dp|tp|st|vo|gu|sc|ap0|ap2|ap10|hb",seq.data$BehaviourW)=="TRUE")
seq.data[mild,14] <- "mild"
chase <- which(grepl("ch",seq.data$BehaviourW)=="TRUE")
seq.data[chase,14] <- "chase" 
severe <- which(grepl("bi|gb|hi|fi|hh|so",seq.data$BehaviourW)=="TRUE")
seq.data[severe,14] <- "severe"

table(seq.data$intensity)
#1542+11149+2452 # 15143
289+1279+387 # 1955
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
d$winner[d$winner == "DodgyKnee"] <- "Dodg"
d$loser[d$loser == "DodgyKnee"] <- "Dodg"


## Link life history to individuals ####
# Import data
LHdata <- read.csv("/Users/mariagranell/Repositories/elo-sociality/IVP_Lifehistory_260523.csv", header = T, stringsAsFactors = F, na.strings = c('NA', 'Not yet'))

# Format dates using the following output = YYYY-mm-dd
LHdata$DOB <- as.Date(format(as.POSIXct(LHdata$DOB, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$FirstRecorded <- as.Date(format(as.POSIXct(LHdata$FirstRecorded, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration1 <- as.Date(format(as.POSIXct(LHdata$DateImmigration1,format = "%d/%m/%Y"), "%Y-%m-%d"))

LHage <- LHdata[,c(3,5,7,8,16)]
colnames(LHage)[1] <- "IDIndividual1"
LHage <- subset(LHage, !LHage$IDIndividual1%in% NA)

# Aggressor first
LHAgg <- LHage
colnames(LHAgg)[1] <- "winner"
join_d <- left_join(d, LHAgg, by = "winner", multiple = "all")
colnames(join_d)[15:18] <- c("WinnerSex", "DOBAgg", "FRAgg", "DIAgg")
join_d <- join_d %>%
  rowwise() %>%
  mutate(WinnerAge = calculate_age(DOBAgg, FRAgg, DIAgg, Date))

# Now victim
LHVic <- LHage
colnames(LHVic)[1] <- "loser"
join_d2 <- left_join(join_d, LHVic, by = "loser", multiple = "all", relationship = "many-to-many")
colnames(join_d2)[20:23] <- c("LoserSex", "DOBVic", "FRVic", "DIVic")
join_d2 <- join_d2 %>%
  rowwise() %>%
  mutate(LoserAge = calculate_age(DOBVic, FRVic, DIVic, Date))

# Select columns of interest
df <- join_d2[, c(1:3,9:12,15,19,20,24)]

# Adfdf AgeClass column to the dfataframe pdf
df$AgeClassWinner <- get_age_class(df$WinnerSex, df$WinnerAge)
df$AgeClassLoser <- get_age_class(df$LoserSex, df$LoserAge)
# correct errors wavy ears, he is not a BB
df[df$winner == "WavyEars","AgeClassWinner"] <- "SM"
df[df$loser == "WavyEars","AgeClassLoser"] <- "SM"


# Omit NA's ####
df$AgeClassLoser[df$AgeClassLoser == ""] <- NA
df$AgeClassWinner[df$AgeClassWinner == ""] <- NA
df$Group[df$Group == ""] <- NA

df <- na.omit(df)

# Keep only adfult andf subadfult males
df <- subset(df, df$AgeClassLoser%in% "AF")
df <- subset(df, df$AgeClassWinner%in% "AF")

## select dfata baundfaries for df -----------------
# Calculate dfominance hierarchy for the first dfarting.
# between October 2021 andf June 2022 is the dfata that i want
# so I will select 3 months before october andf 3 months after
df <- df %>% filter(as.character(Date) > "2021-07-01" & as.character(Date) < "2022-06-01")

# check which groups have enough observations for the dfata range
df %>% group_by(Group) %>% summarize(n = n())

# Dividfe into groups
BD <- subset(df,df$Group == ("Baie Dankie"))
NH <- subset(df,df$Group == ("Noha"))
KB <- subset(df,df$Group == ("Kubu"))
AK <- subset(df,df$Group == ("Ankhase"))
LT <- subset(df,df$Group == ("Lemon Tree"))
CR <- subset(df,df$Group == ("Crossing"))
IF <- subset(df,df$Group == ("IFamily"))

# check which groups have enough data


### Define a function to extract the elo ratiing information. The funtion requires:
# elo_seq = data from performing an eloranking hierarchy
# name_range = a list of names that would describe at which standpoints are you extracting the elo rating
# start_date = a date from which where you want to extract the elo. extract_elo will compute all interactions from the
# starting date of the data used in the elo.seq() until the selected date.
process_date_range <- function(elo_seq, name_range, selected_date) {
  Gp_data <- extract_elo(elo_seq, selected_date, standardize = TRUE)
  Gp_data <- data.frame(AnimalID = names(Gp_data), Rank = as.numeric(Gp_data)) %>%
    mutate(n_males = nrow(.), range = name_range)
  return(Gp_data)
}


#### Baie Dankie  ####

# Darted individuals in BD
# F: Pann, Miel, Asis
# M: Sey, Pom, Hee, Dok, Umb, Flu, Nak, Kom, Xia, War

# To incorporate presence data
# First read the presence matrix

BDpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceBD2020-2023.csv", header=TRUE)
BDpres$Date <- as.Date(format(as.POSIXct(BDpres$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))
#BDpres$Date  <- as.numeric(format(as.POSIXct(BDpres$Date, format = "%d/%m/%Y"), "%d/%m/%Y"))
colnames(BDpres)[1] <- "Delete" # I get these two weird columns called "X" and "V1" that I delete
colnames(BDpres)[3] <- "Delete"
BDpres <- BDpres[,!grepl("Delete",names(BDpres))]

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

# Define the starting dates and corresponding names
date_ranges <- list(
  c("oct", "2021-11-01"),
  c("nov", "2021-12-01"),
  c("dec", "2022-01-01"),
  c("jan", "2022-02-01"),
  c("feb", "2022-03-01"),
  c("mar", "2022-04-01"),
  c("apr", "2022-05-01"),
  c("may", "2022-05-12") # last date before june
)

BD[nrow(BD),1]

# Initialize an empty data frame
BDrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  BD_data <- process_date_range(BDamELO, range_name, start_date)
  BDrank <- rbind(BDrank, BD_data)
}
BDrank <- BDrank %>% mutate(Group = "BD")

#### Ankhase ####
# To incorporate presence data
# First read the presence matrix

AKpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceAK2020-2023.csv", header=TRUE)
AKpres$Date  <- as.Date(as.character(AKpres$Date))
colnames(AKpres)[1] <- "Delete"
AKpres <- AKpres[,!grepl("Delete",names(AKpres))]

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
  c("oct", "2021-11-01"),
  c("nov", "2021-12-01"),
  c("dec", "2022-01-01"),
  c("jan", "2022-02-01"),
  c("feb", "2022-03-01"),
  c("mar", "2022-04-01"),
  c("apr", "2022-05-01"),
  c("may", "2022-05-16")# last date before june
)

AK[nrow(AK),"Date"]

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

NHpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceNH2020-2023.csv", header=TRUE)
NHpres$Date <- as.Date(format(as.POSIXct(NHpres$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))
#NHpres$Date  <- as.Date(as.character(NHpres$Date))
colnames(NHpres)[1] <- "Delete"
NHpres <- NHpres[,!grepl("Delete",names(NHpres))]

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
  c("oct", "2021-11-01"),
  c("nov", "2021-12-01"),
  c("dec", "2022-01-01"),
  c("jan", "2022-02-01"),
  c("feb", "2022-03-01"),
  c("mar", "2022-04-01"),
  c("apr", "2022-05-01"),
  c("may", "2022-05-17") # last date before june
)

NH[nrow(NH),1]

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

KBpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceKB2020-2023.csv", header=TRUE)
KBpres$Date  <- as.Date(as.character(KBpres$Date))
colnames(KBpres)[1] <- "Delete"
KBpres <- KBpres[,!grepl("Delete",names(KBpres))]

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
  c("oct", "2021-11-01"),
  c("nov", "2021-12-01"),
  c("dec", "2022-01-01"),
  c("jan", "2022-02-01"),
  c("feb", "2022-03-01"),
  c("mar", "2022-04-01"),
  c("apr", "2022-05-01"),
  c("may", "2022-05-04") # last day
)

KB[nrow(KB),1]

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

#### Lemon Tree ####

# To incorporate presence data
# First read the presence matrix

LTpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceLT2020-2023.csv", header=TRUE)
LTpres$Date  <- as.Date(as.character(LTpres$Date))
colnames(LTpres)[1] <- "Delete"
LTpres <- LTpres[,!grepl("Delete",names(LTpres))]

# Check if data look good
str(LTpres)
head(LTpres)
tail(LTpres)

# Check whether data looks good
seqcheck(winner=LT$winner, loser=LT$loser, Date=LT$Date, draw = NULL, presence=LTpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

LTamELO <- elo.seq(winner = LT$winner, loser=LT$loser, Date=LT$Date, presence = LTpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(LTamELO)
eloplot(LTamELO)

# Define the starting dates and corresponding names
date_ranges <- list(
  c("oct", "2021-11-01"),
  c("nov", "2021-12-01"),
  c("dec", "2022-01-01"),
  c("jan", "2022-02-01"),
  c("feb", "2022-03-01"),
  c("mar", "2022-04-01") # last day
)

LT[nrow(LT),1]

# Initialize an empty data frame
LTrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  LT_data <- process_date_range(LTamELO, range_name, start_date)
  LTrank <- rbind(LTrank, LT_data)
}

LTrank <- LTrank %>% mutate(Group = "LT")

#### Crossing ####

# To incorporate presence data
# First read the presence matrix

CRpres <- read.csv("/Users/mariagranell/Repositories/data/presence/presenceCR2020-2023.csv", header=TRUE)
CRpres$Date  <- as.Date(as.character(CRpres$Date))
colnames(CRpres)[1] <- "Delete"
CRpres <- CRpres[,!grepl("Delete",names(CRpres))]

# Check if data look good
str(CRpres)
head(CRpres)
tail(CRpres)

# Check whether data looks good
seqcheck(winner=CR$winner, loser=CR$loser, Date=CR$Date, draw = NULL, presence=CRpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

CRamELO <- elo.seq(winner = CR$winner, loser=CR$loser, Date=CR$Date, presence = CRpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(CRamELO)
eloplot(CRamELO)

# Define the starting dates and corresponding names
date_ranges <- list(
  c("oct", "2021-11-01"),
  c("nov", "2021-12-01"),
  c("dec", "2022-01-01"),
  c("jan", "2022-02-01"),
  c("feb", "2022-03-01"),
  c("mar", "2022-04-01"),
  c("apr", "2022-05-01"),
  c("may", "2022-05-03") # last day
)

CR[nrow(CR),1]

# Initialize an empty data frame
CRrank <- data.frame()

# Iterate through date ranges and process data
for (range_info in date_ranges) {
  range_name <- range_info[1]
  start_date <- range_info[2]

  CR_data <- process_date_range(CRamELO, range_name, start_date)
  CRrank <- rbind(CRrank, CR_data)
}

CRrank <- CRrank %>% mutate(Group = "CR")


#### CREATE A TABLE WITH THE MALES OF INTEREST #############
elo_dmales <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/data/merged_hormone_data_2022.csv")
elo_dmales <- elo_dmales[, 9:14] %>% unique(.)

# merge all the rank data
GP_rank <- BDrank %>%
  rbind(AKrank, NHrank, KBrank, LTrank, CRrank)

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
  group_by(AnimalID, Group) %>%
  mutate(
    variation_rank = (sum(Rank) - ifelse(any(range == "oct"), n() * Rank[range == "oct"], 0))
  ) %>%
    mutate(range_numeric = case_when(
      range == "oct" ~ 1,
      range == "nov" ~ 2,
      range == "dec" ~ 3,
      range == "jan" ~ 4,
      range == "feb" ~ 5,
      range == "mar" ~ 6,
      range == "apr" ~ 7,
      range == "may" ~ 8),
    AnimalID = case_when(AnimalID == "WavyEars" ~ "Wavy_ears", TRUE ~ AnimalID)) %>%
  arrange(AnimalID, Group, desc(range_numeric))  %>%
  slice(1) %>% select(!range_numeric)

write.csv(GP_rank2, "/Users/mariagranell/Repositories/elo-sociality/Rank_Males_oct2021-june2022.csv", row.names = F)

# Combine males and females hierarchies ---------
f <- read.csv("/Users/mariagranell/Repositories/elo-sociality/Rank_Females_oct2021-june2022.csv")
m <- read.csv("/Users/mariagranell/Repositories/elo-sociality/Rank_Males_oct2021-june2022.csv")
f$Sex <- "female"
m$Sex <- "male"
combined <- f %>% rbind(.,m)
write.csv(combined, "/Users/mariagranell/Repositories/elo-sociality/Rank_oct2021-june2022.csv", row.names = F)
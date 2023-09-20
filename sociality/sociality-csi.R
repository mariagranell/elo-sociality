# ---------------
# Title: Sociality CSI
# Date: 18 Sep 2023
# Author: mgranellruiz
# Disclaimer: This script was created follwing two scrips created by Josefien Tankink
# Goal: Calculate centrality of individuals in the social network. That is NOT about DSI or particular friendships.
# ultimately find a single number (or two) that represents the social integration of an individual in a group.
# ---------------

# library ---------------------
library(lubridate) #
library(hms) #
library(dplyr) #
library(ggplot2)
library(lme4)
library(ggstatsplot)
library(fitdistrplus)
library(gamlss)
library(ggside)
library(ggpubr)
library(stringr) #
library(gridExtra)
library(ggtext)
library(tidyr) #
library(patchwork)
source('/Users/mariagranell/Repositories/data/functions.R')
library(tibble)

# path ------------------------
setwd('/Users/mariagranell/Repositories/elo-sociality/sociality')

# social data ------------------------
social <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/Social_10.2021-05.2022.csv')

#### CREATING *AD-LIB* CSI FILE ####

s <- social %>%
  mutate(Date = as.character(mdy(Date))) %>%
  filter(
    # Select the study period of the first darting
    Date > "2021-10-01" & Date < "2022-06-01"
  ) %>%
  pivot_longer(cols = starts_with("Remarks"), names_to = "RemarksColumn") %>%
  filter(
    !grepl("bge|bgs|nge|encounter|bgr|wgc", tolower(value)), # bge
    !grepl("bge|bgs|nge|encounter|bgr|wgc|bgd", tolower(Context)), # bge in context
    !grepl("experiment|xp|box|touchscreen|pattern", tolower(value)), # experiments
    !grepl("baby", tolower(value)), # remove the actions that are towards the babies

    # remove erors in the behaviours that were entered
    !grepl("grass|place", BehaviourIndividual1),
    !grepl("griru|pll|bagr", BehaviourIndividual2)
  ) %>%
  pivot_wider(names_from = "RemarksColumn", values_from = "value") %>%
  mutate(Obs.nr = row_number(), # create an observation nr
         BehaviourIndividual1 = tolower(BehaviourIndividual1),
         BehaviourIndividual2 = tolower(BehaviourIndividual2))

# data cleaning -------
# the unk male is kom, Ratel or naal
{s[grepl("kom", s$RemarksA) & grepl("Unknown", s$Individual1), "Individual1"] <- "Kom"
s[grepl("kom", s$RemarksA) & grepl("Unknown", s$Individual2), "Individual2"] <- "Kom"
s[grepl("Ratel", s$RemarksA) & grepl("Unknown", s$Individual1), "Individual1"] <- "Ratel"
s[grepl("naal", s$RemarksA) & grepl("Unknown", s$Individual1), "Individual1"] <- "Naal"}

# Investigate the remarks and remove the ones that are uncertain
#a <- as.data.frame(unique(s$RemarksE)) The ones that are left are ok

# merging all the behaviours -----------
{
s12 <- s %>% select("Group","Individual1","BehaviourIndividual1","Individual2","BehaviourIndividual2", "Date", "Time", "Obs.nr") %>%
  mutate(IntOrder = "Interaction1-2")
s34 <- s %>% mutate(IntOrder = "Interaction3-4",
         Individual1 = Individual3,
         Individual2 = Individual4,
         BehaviourIndividual1 = BehaviourIndividual3,
         BehaviourIndividual2 = BehaviourIndividual4
  ) %>% select("Group","Individual1","BehaviourIndividual1","Individual2","BehaviourIndividual2", "Date", "Time", "Obs.nr","IntOrder")
s56 <- s %>% mutate(IntOrder = "Interaction5-6",
         Individual1 = Individual5,
         Individual2 = Individual6,
         BehaviourIndividual1 = BehaviourIndividual5,
         BehaviourIndividual2 = BehaviourIndividual6
  ) %>% select("Group","Individual1","BehaviourIndividual1","Individual2","BehaviourIndividual2", "Date", "Time", "Obs.nr","IntOrder")

s <- s12 %>% rbind(s34,s56)
rm(s12,s34,s56)}

# Split behaviours into rows -------
s <- split_behaviours(s, c("BehaviourIndividual1", "BehaviourIndividual2"), ' ')
s <- split_behaviours(s, c("BehaviourIndividual1", "BehaviourIndividual2"), '.')

s <- s%>% filter (
  grepl("gr|sg|sw|em|pl", BehaviourIndividual1), # bge
  grepl("gr|sg|sw|em|pl", BehaviourIndividual2)
)

# Create actors and recievers ----
# select only the active behaviours
active_beh <- c('gr', 'sg', 'sw', 'em', 'pl')

# determine actors ID
for(i in seq_along(s$BehaviourIndividual1)) {
  if(s$BehaviourIndividual1[i] %in% active_beh) {
    s$Actor[i] <- s$Individual1[i]
    s$Receiver[i] <- s$Individual2[i]
  } else {
    s$Actor[i] <- s$Individual2[i]
    s$Receiver[i] <- s$Individual1[i]
  }
}

# determine actor behaviour
for(i in seq_along(s$BehaviourIndividual1)) {
  if(s$Individual1[i] == s$Actor[i]) {
    s$BehaviourActor[i] <- s$BehaviourIndividual1[i]
  } else {
    s$BehaviourActor[i] <- s$BehaviourIndividual2[i]
  }
}
rm(i,obs.nr)
s <- s %>% distinct()

#unique(s$BehaviourActor)

#### CREATING *SCANS* CSI FILE ####

# scans data -------------------
scans <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/Scan_10.2021-05.2022.csv')

# Select the study period of the first darting
scans <- scans %>%
  mutate(Date = as.character(mdy(Date))) %>%
  filter(Date > "2021-10-01" & Date < "2022-06-01",
         !is.na(Time),
         !str_detect(Individual, "Unk|Baby")) %>%
  mutate(Group = case_when( # there were some errors in the names
      Group == "Noah" ~ "Noha",
      Group == "Lemon Trmp.kept i.p" ~ "Lemon Tree",
      Group == "Baie Dallies" ~ "Baie Dankie",
      TRUE ~ Group  # Keep other values unchanged
    )
  )

# just make sc (usable version of sc) readable from the shitty pendragon
{
sc <- scans[,c(5:7,14:79)] %>% # remove all the shitty collumns you don´t care
  select(-matches("IB|IN|IC|Intaka|InBetweeners", ignore.case = FALSE)) %>%
  select(-matches("IF|Ifamily", ignore.case = FALSE)) # we were not collecting data on IF on that period

# First subset
ak <- sc %>% filter(Group == 'Ankhase') %>%
  select(-matches("BD|NH|KB|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("AK_", "", .), matches("^AK", ignore.case = FALSE))%>%
  rename(AdultsIn2m = JuvenilesIn2mAK21, JuvsIn2m = JuvenilesIn2mAK2)

bd <- sc %>% filter(Group == 'Baie Dankie') %>%
  select(-matches("AK|NH|KB|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("BD_|BD1|BD2", "", .), matches("BD", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

nh <- sc %>% filter(Group == 'Noha') %>%
  select(-matches("AK|BD|KB|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("NH_|NH1|NH2", "", .), matches("NH", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

kb <- sc %>% filter(Group == 'Kubu') %>%
  select(-matches("AK|BD|NH|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("KB_|KB1|KB2", "", .), matches("KB", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

lt <- sc %>% filter(Group == 'Lemon Tree') %>%
  select(-matches("AK|BD|KB|NH|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("LT_|LT1|LT2", "", .), matches("LT", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

cr <- sc %>% filter(Group == 'Crossing') %>%
  select(-matches("AK|BD|KB|LT|NH", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = AdultsIn2mCR11) %>%
  rename_with(~gsub("CR_|CR1|CR11", "", .), matches("CR", ignore.case = FALSE))

sc <- ak %>% rbind(bd,nh,lt,kb,cr)
rm(ak,bd,nh,lt,kb,cr)
}

# create obs number
obs.nr <- 1
for(i in seq_along(sc$Date)){
  if(i == 1) {
    sc$Obs.nr[i] <- 1
  } else {
    time1 <- as.numeric(as_hms(sc$Time[i]))
    time0 <- as.numeric(as_hms(sc$Time[i - 1]))
    timeinterval <- time1 - time0
    if(sc$Group[i] != sc$Group[i - 1] | (timeinterval > 600) | sc$Date[i] != sc$Date[i - 1]) {
      obs.nr <- obs.nr + 1
    }
    sc$Obs.nr[i] <- obs.nr
  }
}
rm(time0, time1, timeinterval,obs.nr,i)

# separate observed individuals
cols <- c("AdultsIn1m", "JuvsIn1m", "AdultsIn2m", "JuvsIn2m", "AdultsIn5m","JuvsIn5m")
sc <- sc %>% split_behaviours(cols, ";")%>% distinct()

# make it in long format ------
cols <- c("AdultsIn1m", "JuvsIn1m", "AdultsIn2m", "JuvsIn2m", "AdultsIn5m","JuvsIn5m", "NearestNeighbour", "NearestJuvenileNeighbour")
ss <- sc %>% pivot_longer(cols, values_to = "neighbour", names_to = "distance") %>%
  distinct()%>% filter(!is.na(neighbour), !str_detect( neighbour, "Unk|Baby|adult")) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(Individual, neighbour)), collapse = '-'))


# Remove duplicates
# If two individuals are scanned within 10 mins of each other both and both have the same network
# is important to one of the observations beacuse they are not independent
result <- ss %>%
  group_by(Obs.nr, dyad) %>%
  filter(!(n() > 1 & any(!str_detect(distance, "Near"))) | !str_detect(distance, "Near")) %>%
  ungroup()

#### check social bonds package ########
#install.packages("remotes")
#remotes::install_github("gobbios/socialindices")
?socialindices2::CSI()

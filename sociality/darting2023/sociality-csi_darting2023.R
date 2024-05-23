# ---------------
# Title: Sociality CSI
# Date: 21 May 2024
# Author: mgranellruiz
# Disclaimer: This script was created follwing two scrips created by Josefien Tankink
# Goal: Calculate centrality of IDIndividuals in the social network. That is NOT about DSI or particular friendships.
# ultimately find a single number (or two) that represents the social integration of an IDIndividual in a group.
# for the darting of 2023 I will take data for the ranking and the sociality vectors from 2022-07-01 until 2023-02-01.
# for more information check the README of combination darting
# ---------------

# library ---------------------
library(lubridate)
library(hms)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(ggplot2)
library(patchwork)
source('/Users/mariagranell/Repositories/data/functions.R')


# path ------------------------
setwd('/Users/mariagranell/Repositories/elo-sociality/sociality/darting2023')

# social data ------------------------
social <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/darting2023/combinedAffiliative_2022-06_2023-12.csv')

#### CREATING *AD-LIB* CSI FILE ####

ss <- social %>%
  mutate(Date = as.character(ymd(Date))) %>%
  filter(
    # Select the study period of the second darting
    Date > "2022-07-01" & Date < "2023-02-01"
  ) %>%
  filter(
    # remove affiliative interactions in odd context
    # during experiments or presentations, during bge and when the afffiliative is towards the baby
    !grepl("BGE|Experiment|Babies", Context),
    !grepl("corn|swab|presentation|experiment|macaroni|bge|box|novel|tolerance", tolower(OtherContext)),

    # remove erors in the behaviours that were entered
    #!grepl("grass|place", BehaviourIndiv1),
    #!grepl("griru|pll|bagr", BehaviourIndiv2)
  ) %>%
  mutate(Obs.nr = row_number(), # create an observation nr
         BehaviourIndiv1 = tolower(BehaviourIndiv1), # make sure all behaviours are in lower case
         BehaviourIndiv2 = tolower(BehaviourIndiv2))

# data cleaning -------
# the unk male is Xin, Xai or Apa
{ss[grepl("Xin", ss$OtherContext) & grepl("UnkAM", ss$IDIndividual2), "IDIndividual2"] <- "Xin"
ss[grepl("UnkAM is Xai", ss$Remarks) & grepl("UnkAm", ss$IDIndividual1), "IDIndividual1"] <- "Xai"
ss[grepl("unkAM is apa", ss$Remarks) & grepl("UnkAM", ss$IDIndividual1), "IDIndividual1"] <- "Apa"}

# Investigate the remarks and remove the ones that are uncertain
#a <- as.data.frame(unique(s$RemarksE)) The ones that are left are ok

# merging all the behaviours -----------

{s12 <- ss %>% dplyr::select("Group","IDIndividual1","BehaviourIndiv1","IDIndividual2","BehaviourIndiv2", "Date", "Time", "Obs.nr") %>%
  mutate(IntOrder = "Interaction1-2")
s34 <- ss %>% dplyr::select(-IDIndividual1, -IDIndividual2, -BehaviourIndiv1, -BehaviourIndiv2) %>%
  mutate(IntOrder = "Interaction3-4",
         IDIndividual1 = IDIndividual3,
         IDIndividual2 = IDIndividual4,
         BehaviourIndiv1 = BehaviourIndiv3,
         BehaviourIndiv2 = BehaviourIndiv4
  ) %>% dplyr::select("Group","IDIndividual1","BehaviourIndiv1","IDIndividual2","BehaviourIndiv2", "Date", "Time", "Obs.nr","IntOrder")
s56 <- ss %>% dplyr::select(-IDIndividual1, -IDIndividual2, -BehaviourIndiv1, -BehaviourIndiv2) %>%
  mutate(IntOrder = "Interaction5-6",
         IDIndividual1 = IDIndividual5,
         IDIndividual2 = IDIndividual6,
         BehaviourIndiv1 = BehaviourIndiv5,
         BehaviourIndiv2 = BehaviourIndiv6
  ) %>% dplyr::select("Group","IDIndividual1","BehaviourIndiv1","IDIndividual2","BehaviourIndiv2", "Date", "Time", "Obs.nr","IntOrder")

ss <- s12 %>% rbind(s34,s56)
rm(s12,s34,s56)}

# Remove rows in where both behaviours are empty
ss <- ss %>%
  mutate(across(-Obs.nr, ~ na_if(., ""))) %>% # make all empty entries NA
  filter(!(is.na(BehaviourIndiv2) & is.na(BehaviourIndiv1)), # if there is no behaviour for any of them
         !is.na(IDIndividual1), !is.na(IDIndividual2)) # if one of them is not entered

# Split behaviours into rows ------- warnings are fine
ss <- split_behaviours(ss, c("BehaviourIndiv1", "BehaviourIndiv2"), ' ')
ss <- split_behaviours(ss, c("BehaviourIndiv1", "BehaviourIndiv2"), '.')

# select behaviours to calculate the sociality
ss <- ss %>%
  # make all behaviours into lower case
  mutate(BehaviourIndiv1 = tolower(BehaviourIndiv1),
         BehaviourIndiv2 = tolower(BehaviourIndiv2)) %>%
  # make easy behaviour corrections when one of the behaviours is empty
  mutate(
    BehaviourIndiv1 = ifelse(is.na(BehaviourIndiv1) & BehaviourIndiv2 == "bgr", "gr", BehaviourIndiv1),
    BehaviourIndiv1 = ifelse(is.na(BehaviourIndiv1) & BehaviourIndiv2 == "sg", "sg", BehaviourIndiv1),
    BehaviourIndiv2 = ifelse(is.na(BehaviourIndiv2) & BehaviourIndiv1 == "gr", "bgr", BehaviourIndiv2),
    BehaviourIndiv2 = ifelse(is.na(BehaviourIndiv2) & BehaviourIndiv1 == "bgr", "gr", BehaviourIndiv2),
    BehaviourIndiv2 = ifelse(is.na(BehaviourIndiv2) & BehaviourIndiv1 == "pl", "pl", BehaviourIndiv2),
    BehaviourIndiv2 = ifelse(is.na(BehaviourIndiv2) & BehaviourIndiv1 == "sg", "sg", BehaviourIndiv2),
  ) %>%
  filter (grepl("gr|sg|sw|em|pl", BehaviourIndiv1) | grepl("gr|sg|sw|em|pl", BehaviourIndiv2))


# Create actors and recievers ----
# because of the behaviours that we selected sometimes there is no directionality
# for instance pl or sg. Since we are going to use undirected networks to caluclate sociality
# it dosen´t matter much, that is as long as they interacted we counted. Regardless of the direction.

# select only the affiliative active behaviours
active_beh <- c('gr', 'sg', 'sw', 'em', 'pl')

ssa <- ss
# determine actors ID.
for(i in seq_along(ss$BehaviourIndiv1)) {
  if(ss$BehaviourIndiv1[i] %in% active_beh) {
    ss$Actor[i] <- ss$IDIndividual1[i]
    ss$Receiver[i] <- ss$IDIndividual2[i]
  } else {
    ss$Actor[i] <- ss$IDIndividual2[i]
    ss$Receiver[i] <- ss$IDIndividual1[i]
  }
}

# determine actor behaviour
for(i in seq_along(ss$BehaviourIndiv1)) {
  if(ss$IDIndividual1[i] == ss$Actor[i]) {
    ss$BehaviourActor[i] <- ss$BehaviourIndiv1[i]
  } else {
    ss$BehaviourActor[i] <- ss$BehaviourIndiv2[i]
  }
}
rm(i)
ss <- ss %>% distinct() %>%
  dplyr::select(-matches("BehaviourIndiv|IDIndividual", ignore.case = FALSE)) %>%
  # remove interactions in where babies are involved, unk individuals are ok as long as they are not actor, see later
  filter(!grepl("BB|Unk", Actor),
         !grepl("BB", Receiver)) %>%
  # remove interactions that are not in the affiliative behaviours
  filter(BehaviourActor %in% active_beh)

unique(ssa$BehaviourActor)

#### CREATING *SCANS* CSI FILE ####

# scans data -------------------
scans <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/darting2023/combinedScan_2022-06_2023-5.csv')

# Select the study period of the first darting
scans <- scans %>%
  mutate(Date = as.character(ymd(Date))) %>%
  filter(
    # Select the study period of the second darting
    Date > "2022-07-01" & Date < "2023-02-01")

# integrate the remarks
{
  # change the IDIndividual1
scans[grepl("UnkJM i s Aaa", scans$Remarks), "IDIndividual1"] <- "Aaa"
scans[grepl("unkAM is Xin", scans$Remarks), "IDIndividual1"] <- "Xin"
scans[grepl("unkAM is kno", scans$Remarks), "IDIndividual1"] <- "Kno"
scans[grepl("unkAM is Cairo", scans$Remarks), "IDIndividual1"] <- "Cai"
scans[grepl("Oke", scans$Remarks), "IDIndividual1"] <- "Oke"
scans[grepl("Hav", scans$Remarks), "IDIndividual1"] <- "Hav"
scans[grepl("apa", scans$Remarks), "IDIndividual1"] <- "Apa"
scans[grepl("Aaa", scans$Remarks), "IDIndividual1"] <- "Aaa"
  # change the juvenile
scans[grepl("Aaa is juv|AAA juv", scans$Remarks), "NNJuvenile"] <- "Aaa"
  scans[grepl("aal nearest jv", scans$Remarks), "NNJuvenile"] <- "Aal"
  scans[grepl("Aar is juv|Aar juv|uv is Aar", scans$Remarks), "NNJuvenile"] <- "Aar"
  scans[grepl("AAT is juv|AAT juv|juv is aat", scans$Remarks), "NNJuvenile"] <- "Aat"
  scans[grepl("cay is nearest juv", scans$Remarks), "NNJuvenile"] <- "Cay"
  scans[grepl("dalt is nearest jv", scans$Remarks), "NNJuvenile"] <- "Aaa"
  scans[grepl("gil nearest juv", scans$Remarks), "NNJuvenile"] <- "Gil"
  scans[grepl("Gree juv", scans$Remarks), "NNJuvenile"] <- "Gree"
  scans[grepl("Gri juv", scans$Remarks), "NNJuvenile"] <- "Gri"
  scans[grepl("juv is Yelo|Juv is yelo|uv is Yelo", scans$Remarks), "NNJuvenile"] <- "Yelo"
  scans[grepl("juv Roc", scans$Remarks), "NNJuvenile"] <- "Roc"
  scans[grepl("juv uls", scans$Remarks), "NNJuvenile"] <- "Uls"
  scans[grepl("Juv Xop", scans$Remarks), "NNJuvenile"] <- "Xop"
  # there are more but I give up, I am not that instrested in the juveniles
  # change the parners
  scans[grepl("Yuko is partner", scans$Remarks), "IDPartners"] <- "Yuko"
  scans[grepl("Xinp is partner", scans$Remarks), "IDPartners"] <- "Xinp"
  scans[grepl("Uls partner", scans$Remarks), "IDPartners"] <- "Uls"
  scans[grepl("Tev is partner", scans$Remarks), "IDPartners"] <- "Tev"
  scans[grepl("Part partner", scans$Remarks), "IDPartners"] <- "Prat"
  scans[grepl("Gris is partner", scans$Remarks), "IDPartners"] <- "Gris"
  scans[grepl("Gri partner", scans$Remarks), "IDPartners"] <- "Gri"
  scans[grepl("Gale partner", scans$Remarks), "IDPartners"] <- "Gale"
  # change UnkAM and UnkAF
  scans[grepl("UnkAM-Oke", scans$Remarks), "IndArmLength"] <- "Oke"
  scans[grepl("UnkAM-Oke", scans$Remarks), "NNAdult"] <- "Oke"
  scans[grepl("UnkAM is NHNewMale0622", scans$Remarks), "NNAdult"] <- "NHNewMale0622"
  scans[grepl("UnkAF is Xinp", scans$Remarks), "NNAdult"] <- "Xinp"
  scans[grepl("UnkAF is Beir", scans$Remarks), "NNAdult"] <- "Beir"
  scans[grepl("unkAM is Xin", scans$Remarks), "NNAdult"] <- "Xin"
  scans[grepl("unkAM is Tedd", scans$Remarks), "NNAdult"] <- "Ted"
  scans[grepl("unkAM is Tedd", scans$Remarks), "IndArmLength"] <- "Oke"
  scans[grepl("unkAM is Ryan", scans$Remarks), "NNAdult"] <- "Rya"
}

# remove Interobs obs and select collumns
sc <- scans %>% filter(InterObs == "No") %>%
  dplyr::select(Date, Time, Group, IDIndividual1, Behaviour, BehaviourType,
                IDPartners,NNAdult,DistanceNNA,NNJuvenile,DistanceNNJ,
                NbNeighbours10m,IndArmLength,Ind2m,Ind5m)

# remove duplicates, i.e. if Yuko has been named as IDPartners remove from NNAdult, NNJuveline, IndArmLenght, Ind2m, Ind5m
replace_patterns_row <- function(df) {
  for (i in seq_len(nrow(df))) {
    if (!is.na(df$IDPartners[i])) {
      pattern <- df$IDPartners[i]
      df$NNAdult[i] <- sub(pattern, "", df$NNAdult[i])
      df$NNJuvenile[i] <- sub(pattern, "", df$NNJuvenile[i])
      df$IndArmLength[i] <- sub(pattern, "", df$IndArmLength[i])
      df$Ind2m[i] <- sub(pattern, "", df$Ind2m[i])
      df$Ind5m[i] <- sub(pattern, "", df$Ind5m[i])
    }

    if (!is.na(df$NNAdult[i])) {
      pattern <- df$NNAdult[i]
      df$IndArmLength[i] <- sub(pattern, "", df$IndArmLength[i])
      df$Ind2m[i] <- sub(pattern, "", df$Ind2m[i])
      df$Ind5m[i] <- sub(pattern, "", df$Ind5m[i])
    }

    if (!is.na(df$NNJuvenile[i])) {
      pattern <- df$NNJuvenile[i]
      df$IndArmLength[i] <- sub(pattern, "", df$IndArmLength[i])
      df$Ind2m[i] <- sub(pattern, "", df$Ind2m[i])
      df$Ind5m[i] <- sub(pattern, "", df$Ind5m[i])
    }
  }
  return(df)
}
scans <- replace_patterns_row(scans)

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

# separate observed IDIndividuals
cols <- c("IndArmLength", "Ind2m", "Ind5m")
sc <- sc %>% split_behaviours(cols, ";")%>% distinct() # ignore warning

# make it in long format ------
cols <- c("NNAdult","NNJuvenile","IndArmLength", "Ind2m", "Ind5m")
sc <- sc %>% pivot_longer(cols, values_to = "neighbour", names_to = "distance") %>%
  distinct()%>% mutate(neighbour = na_if(neighbour, "")) %>% # make empty entries NA
  filter(!is.na(neighbour), !str_detect( neighbour, "BB")) %>% # remove BB (babies)
  rowwise() %>%
  mutate(dyad = paste(sort(c(IDIndividual1, neighbour)), collapse = '-')) # create a dyad collumn
rm(cols)

# Remove duplicates
# If two IDIndividuals are scanned within 10 mins of each other both and both have the same network
# is important to one of the observations beacuse they are not independent
sca <- sc %>%
  group_by(Obs.nr, dyad) %>%
  filter(!(n() > 1 & any(!str_detect(distance, "NN"))) | !str_detect(distance, "NN")) %>%
  ungroup()

#### CREATING *FOCAL* CSI FILE ####
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
    Behaviour == "Affiliative"
    ) %>%
  # select the collumns of interest for affilitavie
  dplyr::select(Date, Time, Group,IDIndividual1, BehaviourFocal, IDIndividual2,
                BehaviourFocal2, IDIndividual3,
                BehaviourFocal3, IDIndividual4,
                Remarks)

# assume remarks
# ignore making a proper obs number
fo$Obs.nr <- 1

# combine behaviours into long format
{fo12 <- fo %>% dplyr::select(Date, Time, Group,IDIndividual1, BehaviourFocal, IDIndividual2, Obs.nr)
fo13 <- fo %>% dplyr::select(Date, Time, Group,IDIndividual1, BehaviourFocal2, IDIndividual3, Obs.nr) %>%
  rename(BehaviourFocal = BehaviourFocal2, IDIndividual2 = IDIndividual3)
fo14 <- fo %>% dplyr::select(Date, Time, Group,IDIndividual1, BehaviourFocal3, IDIndividual4, Obs.nr) %>%
  rename(BehaviourFocal = BehaviourFocal3, IDIndividual2 = IDIndividual4)

fo <- rbind(fo12,fo13,fo14)
rm(fo12,fo13,fo14)}

# Clean the file
fo <- fo %>% mutate(BehaviourFocal = tolower(BehaviourFocal)) %>%
  filter(!is.na(BehaviourFocal), # if there is no behaviour not intrested
         !str_detect(IDIndividual2, regex("BB")))

# separate observed IDIndividuals
fo <- fo %>% # ignore warnings
  split_behaviours("BehaviourFocal", ".") %>%
  distinct()

# Create actors and recievers ----
# because of the behaviours that we selected sometimes there is no directionality
# for instance pl or sg. Since we are going to use undirected networks to caluclate sociality
# it dosen´t matter much, that is as long as they interacted we counted. Regardless of the direction.

active_beh <- c('gr', 'sg', 'sw', 'em', 'pl') # affiliative active behaviours
passive_beh <- c('bgr', 'bsg', 'bsw', 'bem') # passive affiliative behaviours

# determine actors ID.
fo <- fo %>%
  mutate(
    Actor = case_when(
      BehaviourFocal %in% active_beh ~ IDIndividual1,
      BehaviourFocal %in% passive_beh ~ IDIndividual2,
      TRUE ~ NA_character_
    ),
    Receiver = case_when(
      BehaviourFocal %in% active_beh ~ IDIndividual2,
      BehaviourFocal %in% passive_beh ~ IDIndividual1,
      TRUE ~ NA_character_
    ),
    BehaviourActor = ifelse(BehaviourFocal %in% c(active_beh, passive_beh), BehaviourFocal, NA_character_)
  )

# remove the passive behaviours
fo <- fo %>%
  mutate(BehaviourFocal = str_replace_all(BehaviourFocal, "b", ""),
         Source = "focal") %>%
  dplyr::select(-IDIndividual1, -IDIndividual2, -BehaviourFocal) %>%
  rename(Behaviour = BehaviourActor) %>%
  na.omit()

#### COMBINE THE ADLIB, SCANS AND FOCAL FILES ####

# changing scans data to have Date, Time, Group, Actor, Receiver, BehaviourActor
sca <- sc %>% mutate(Source = "scan") %>%
  dplyr::select(Group, Date, Time, Obs.nr, IDIndividual1, neighbour, distance, Source) %>%
  rename(Actor = IDIndividual1,
         Receiver = neighbour,
         Behaviour = distance
  )

# merge adlib (ss), scans (sca) and focal (fo) data
ssc <- ss %>% mutate(Source = "adlib") %>%
  dplyr::select(Group, Date, Time, Obs.nr, Actor, Receiver, BehaviourActor, Source) %>%
  rename(Behaviour = BehaviourActor) %>%
  rbind(., sca, fo) %>%
  mutate(Focal = Actor) %>%
  # do some cleaning, remove unk and NA in the focal
  filter(!str_detect(Focal, regex("Unk"))) %>%
  na.omit()
rm(sca)

## MERGE WITH LH TO CHANGE THE NAME OF MALES ####

# Create a lookup table with lh
# I selected only AnimalName and OtherID and concatenated to end up with only 1 collumn of weird names called focal
lookup <- read.csv2("/Users/mariagranell/Repositories/elo-sociality/data/darting2023/factchecked_LH_171123.csv") %>%
  dplyr::select(AnimalCode, AnimalName) %>% distinct() %>% rename(OtherID = AnimalName) %>%
  rbind(.,read.csv2("/Users/mariagranell/Repositories/elo-sociality/data/darting2023/factchecked_LH_171123.csv") %>%
  dplyr::select(AnimalCode, OtherID) %>% distinct() )

# Replace misspelled names in ssc$Focal with correct names using the lookup table
ssc <- ssc %>%
    left_join(lookup, by = c("Focal" = "OtherID")) %>%
    mutate(Focal = if_else(is.na(AnimalCode), Focal, AnimalCode),
         Actor = Focal) %>%
    select(-AnimalCode) %>%
    left_join(lookup, by = c("Receiver" = "OtherID")) %>%
    mutate(Receiver = if_else(is.na(AnimalCode), Receiver, AnimalCode)) %>%
    select(-AnimalCode)

#View(ssc%>% anti_join(lookup, by = c("Focal" = "AnimalCode")))


#### SOCIAL BONDS PACKAGE ########
# To create a file for the social bonds package you have:
# A file with behaviour. That contains the collumns: Date, Time, Group, Actor, Receiver, Behaviour
# scans data can be modified in where proximity can be describes as a behaviour. i.e. proox1m, prox5m...

# it dosent include presence data. worrying? not sure
#install.packages("remotes")
#remotes::install_github("gobbios/socialindices")
?socialindices2::CSI()
library(socialindices2)

ssc %>% group_by(Group) %>% summarize(n = n())
# CR and IF don´t have enough data to do CSI

### perform CSI ###
{ simplified_behaviours <- list(
    proximity=c("NNJuvenile", "NNAdult","Ind2m","Ind5m", "sg","bsg","sw", "bsw", "IndArmLength"),
    social=c("gr","em","pl", "bgr","bem"))

# AK --------
csiak <- ssc %>% filter(Group == "Ankhase") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours) %>%
  mutate(group = "AK")

ak <- csiak %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point() +
  labs(title ="AK")

# NH ------------
csinh <- ssc %>% filter(Group == "Noha") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)%>%
  mutate(group = "NH")

nh <- csinh %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(x = zCSI, y =  focal2)) +
  geom_point() +
  labs(title ="NH")

# BD ------------
csibd <- ssc %>% filter(Group == "Baie Dankie") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)%>%
  mutate(group = "BD")

bd <- csibd %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point()+
  labs(title ="BD")

# KB ------------
csikb <- ssc %>% filter(Group == "Kubu") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)%>%
  mutate(group = "KB")

kb <- csikb %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point()+
  labs(title ="KB")

# LT ------------
csilt <- ssc %>% filter(Group == "Lemon Tree") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)%>%
  mutate(group = "LT")

lt <- csilt %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point()+
  labs(title ="LT")


nh + bd + ak +
kb + lt
}
rm(nh,ak,bd,lt,kb)

# merge the CSI.
csiall <- csiak %>% rbind(csibd,csikb,csilt,csinh)
rm(csiak,csibd,csikb,csilt,csinh)

#write.csv(csiall, '/Users/mariagranell/Repositories/elo-sociality/sociality/darting2023/CSI_jun2022-jan2023.csv', row.names = F)

############ FIRST ################

ssc2 <- change_group_names(ssc,c("Group"))
# make the calculations of sociality vectors a function -------------
calculate_igraph_vectors <- function (data_matrix){
vervet.edges <- graph_from_adjacency_matrix(as.matrix(data_matrix), mode="undirected", weighted=TRUE)

# Degree
vervet.degree <- degree(vervet.edges)
degree_df <- data.frame(Name = names(vervet.degree), Degree = as.numeric(vervet.degree))
# Weighted degree, or strength
vervet.strength <- strength(vervet.edges)
strenght_df <- data.frame(Name = names(vervet.strength), Strength = as.numeric(vervet.strength))
# Weighted closeness
vervet.closeness <- closeness(vervet.edges)
closeness_df <- data.frame(Name = names(vervet.closeness), Closeness = as.numeric(vervet.closeness))
# Weighted betweenness
vervet.betweenness <- betweenness(vervet.edges)
betweenness_df <- data.frame(Name = names(vervet.betweenness), Betweenness = as.numeric(vervet.betweenness))
# Eigenvector centrality
vervet.eigencent <- eigen_centrality(vervet.edges)
vervet.eigencent <- vervet.eigencent$vector
eigencent_df <- data.frame(Name = names(vervet.eigencent), Eigencent = as.numeric(vervet.eigencent))

  coefficients <- degree_df %>% left_join(strenght_df, by = "Name") %>%
    left_join(closeness_df, by = "Name") %>%
    left_join(betweenness_df, by = "Name") %>%
    left_join(eigencent_df, by = "Name")

  return(coefficients)
}

# put your data in a matrix ---------------
# 1st decide on a group (make data_matrix for "AK") and then calculate the igraph for that group
decide_gp <- "LT"
{
# change the group name and the bind_rows
data_matrix1 <- ssc2 %>% filter(Group == decide_gp) %>%
  mutate(count = 1) %>%
  dplyr::select(Actor, Receiver) %>%
  group_by(Actor, Receiver) %>%
  summarize(n = n()) %>%
  reshape2::dcast(., Actor ~ Receiver, value.var = "n", fill = 0) %>%
  column_to_rownames(., var ="Actor")

# make the
cols <- colnames(data_matrix1)
rows <- rownames(data_matrix1)
# Names in list1 but not in list2
missing_in_list <- setdiff(cols, rows)

data_matrix1 <- ssc2 %>% filter(Group == decide_gp) %>%
  mutate(count = 1) %>%
  dplyr::select(Actor, Receiver) %>%
  group_by(Actor, Receiver) %>%
  summarize(n = n()) %>%
  bind_rows(data.frame(Actor = missing_in_list, Receiver = missing_in_list, n = 0)) %>% # make the matrix square
  #bind_rows(data.frame(Actor = missing_in_list, Receiver = "Ratel", n = 0)) %>% # add "Ratel" if you´re in LT
  reshape2::dcast(., Actor ~ Receiver, value.var = "n", fill = 0) %>%
  column_to_rownames(., var ="Actor")

} # prepare the data in a square matrix

#AK <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "AK")
#NH <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "NH")
#BD <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "BD")
#KB <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "KB")
#LT <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "LT") # make a change when doing data_matrix1
#CR <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "CR")
rm(cols,rows,data_matrix1,missing_in_list,decide_gp)

# merge the igraph. Not including CR the data is unrelaiable
igraph_combined <- AK %>% rbind(NH,BD,KB,LT,CR)
rm(AK,NH,BD,KB,LT,CR)
# combine with CSI
sociality_vectors <- igraph_combined %>%
  left_join(., csiall, by = c("Name" = "focal", "Group" = "group")) %>%
  rename(AnimalCode = Name) %>%
  dplyr::select(AnimalCode, Group, CSI, zCSI, Degree,
                Strength, Closeness,Betweenness, Eigencent)
#write.csv(sociality_vectors, '/Users/mariagranell/Repositories/elo-sociality/sociality/sociality_vectors_oct2021-june2020.csv', row.names = F)



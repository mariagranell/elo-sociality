# ---------------
# Title: Sociality CSI
# Date: 18 Sep 2023
# Updated: 13 Feb 2024
# Author: mgranellruiz
# Disclaimer: This script was created follwing two scrips created by Josefien Tankink
# Goal: Calculate centrality of individuals in the social network. That is NOT about DSI or particular friendships.
# ultimately find a single number (or two) that represents the social integration of an individual in a group.
# ---------------

# library ---------------------
library(lubridate)
library(hms)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
library(tibble)
library(forcats)
library(hms)

# path ------------------------
setwd('/Users/mariagranell/Repositories/elo-sociality/sociality')

# social data ------------------------
{social1 <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/Social_10.2021-05.2022.csv') %>%
  mutate(Date = as.character(mdy(Date)), Data = "Affiliative") %>% add_season("Date") %>%
  change_group_names("Group") %>%
  dplyr::select(
    Data,
    Date,
    Time,
    Group,
    Context, Season,
    IDIndividual1 = Individual1, IDIndividual2 = Individual2,
    BehaviourIndiv1 = BehaviourIndividual1, BehaviourIndiv2 = BehaviourIndividual2,
    #Interaction = IndEnding,
    IDIndividual3 = Individual3, IDIndividual4 = Individual4,
    BehaviourIndiv3 = BehaviourIndividual3, BehaviourIndiv4 = BehaviourIndividual4,
    IDIndividual5 = Individual5, IDIndividual6 = Individual6,
    BehaviourIndiv5 = BehaviourIndividual5, BehaviourIndiv6 = BehaviourIndividual6,
    Remarks = RemarksA, Remarks1 =RemarksB, Remarks2 = RemarksC,Remarks3 = RemarksD,
    DataInfo = Def,
    DeviceID = RecordId
  ) %>% mutate(OtherContext = NA )
social2 <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/CleanFiles/affiliative_cybertracker.csv")
social <- rbind(social1, social2)}

# check data with plot
social %>% mutate(Date = ymd(Date) ) %>%add_season("Date") %>% mutate(Data = "ot") %>%
  #filter(Group == "KB") %>%
  plot_weekly_summary("Data", "Date")

# data cleaning -------
# the unk male is kom, Ratel or naal
{social[grepl("kom", social$RemarksA) & grepl("Unknown", social$IDIndividual1), "IDIndividual1"] <- "Kom"
social[grepl("kom", social$RemarksA) & grepl("Unknown", social$IDIndividual2), "IDIndividual2"] <- "Kom"
social[grepl("Ratel", social$RemarksA) & grepl("Unknown", social$IDIndividual1), "IDIndividual1"] <- "Ratel"
social[grepl("naal", social$RemarksA) & grepl("Unknown", social$IDIndividual1), "IDIndividual1"] <- "Naal"}

#### CREATING *AD-LIB* CSI FILE ####
ss <- social %>%
  mutate(Date = as.character(ymd(Date))) %>%
  filter(
    # Select the study period of the first darting
    #Date > "2021-10-01" & Date < "2022-06-01"
  ) %>%
  unite("Remarks", starts_with("Remarks"), sep = ";", na.rm = TRUE, remove = TRUE) %>%
  filter(
    !grepl("bge|bgs|nge|encounter|bgr|wgc", tolower(Remarks)), # bge
    !grepl("bge|bgs|nge|encounter|bgr|wgc|bgd", tolower(Context)), # bge in context
    !grepl("experiment|xp|box|touchscreen|pattern", tolower(Remarks)), # experiments
    !grepl("baby|bb", tolower(Remarks)), # remove the actions that are towards the babies

    # remove erors in the behaviours that were entered
    !grepl("grass|place", BehaviourIndiv1),
    !grepl("griru|pll|bagr", BehaviourIndiv2)
  ) %>%
  mutate(Obs.nr = row_number(), # create an observation nr
         BehaviourIndiv1 = tolower(BehaviourIndiv1),
         BehaviourIndiv2 = tolower(BehaviourIndiv2)) %>%
  change_group_names("Group")


# merging all the behaviours -----------
{
s12 <- ss %>% dplyr::select("Group","IDIndividual1","BehaviourIndiv1","IDIndividual2","BehaviourIndiv2", "Date", "Time", "Obs.nr") %>%
  mutate(IntOrder = "Interaction1-2")
s34 <- ss %>% mutate(IntOrder = "Interaction3-4",
         IDIndividual1 = IDIndividual3,
         IDIndividual2 = IDIndividual4,
         BehaviourIndiv1 = BehaviourIndiv3,
         BehaviourIndiv2 = BehaviourIndiv4
  ) %>% dplyr::select("Group","IDIndividual1","BehaviourIndiv1","IDIndividual2","BehaviourIndiv2", "Date", "Time", "Obs.nr","IntOrder")
s56 <- ss %>% mutate(IntOrder = "Interaction5-6",
         IDIndividual1 = IDIndividual5,
         IDIndividual2 = IDIndividual6,
         BehaviourIndiv1 = BehaviourIndiv5,
         BehaviourIndiv2 = BehaviourIndiv6
  ) %>% dplyr::select("Group","IDIndividual1","BehaviourIndiv1","IDIndividual2","BehaviourIndiv2", "Date", "Time", "Obs.nr","IntOrder")

ss <- s12 %>% rbind(s34,s56) %>%
  filter(!is.na(IDIndividual1), !is.na(IDIndividual2))
rm(s12,s34,s56)}

# Split behaviours into rows -------
# ignore warning
ss <- split_behaviours(ss, c("BehaviourIndiv1", "BehaviourIndiv2"), ' ')
ss <- split_behaviours(ss, c("BehaviourIndiv1", "BehaviourIndiv2"), '.')

ss <- ss%>% filter (
  grepl("gr|sg|sw|em|pl", BehaviourIndiv1), # bge
  grepl("gr|sg|sw|em|pl", BehaviourIndiv2)
)

# Create actors and recievers ----
# select only the active behaviours
active_beh <- c('gr', 'sg', 'sw', 'em', 'pl')

# determine actors ID
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
rm(i, active_beh)
ss <- ss %>% distinct() %>%
  dplyr::select(-matches("BehaviourIndiv|IDIndividual", ignore.case = FALSE)) %>%
  filter(!grepl("baby|unknown", tolower(Actor)),
         !grepl("baby|unknown", tolower(Receiver))) %>%
  integrate_otherid(Actor, Receiver)

#unique(ss$BehaviourActor)

#### CREATING *SCANS* CSI FILE ####

# scans data -------------------
scans <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/Scan_10.2021-05.2022.csv') %>%
  rename(IDIndividual = Individual)

# Select the study period of the first darting
scans <- scans %>%
  mutate(Date = as.character(mdy(Date))) %>%
  filter(#Date > "2021-10-01" & Date < "2022-06-01",
         !is.na(Time),
         !str_detect(IDIndividual, "Unk|Baby")) %>%
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
  dplyr::select(-matches("IB|IN|IC|Intaka|InBetweeners", ignore.case = FALSE)) %>%
  dplyr::select(-matches("IF|Ifamily", ignore.case = FALSE)) # we were not collecting data on IF on that period

# First subset
ak <- sc %>% filter(Group == 'Ankhase') %>%
  dplyr::select(-matches("BD|NH|KB|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("AK_", "", .), matches("^AK", ignore.case = FALSE))%>%
  rename(AdultsIn2m = JuvenilesIn2mAK21, JuvsIn2m = JuvenilesIn2mAK2)

bd <- sc %>% filter(Group == 'Baie Dankie') %>%
  dplyr::select(-matches("AK|NH|KB|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("BD_|BD1|BD2", "", .), matches("BD", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

nh <- sc %>% filter(Group == 'Noha') %>%
  dplyr::select(-matches("AK|BD|KB|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("NH_|NH1|NH2", "", .), matches("NH", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

kb <- sc %>% filter(Group == 'Kubu') %>%
  dplyr::select(-matches("AK|BD|NH|LT|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("KB_|KB1|KB2", "", .), matches("KB", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

lt <- sc %>% filter(Group == 'Lemon Tree') %>%
  dplyr::select(-matches("AK|BD|KB|NH|CR", ignore.case = FALSE)) %>%
  rename_with(~gsub("LT_|LT1|LT2", "", .), matches("LT", ignore.case = FALSE)) %>%
  rename(JuvsIn2m = JuvenilesIn2m)

cr <- sc %>% filter(Group == 'Crossing') %>%
  dplyr::select(-matches("AK|BD|KB|LT|NH", ignore.case = FALSE)) %>%
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
sc <- sc %>% pivot_longer(all_of(cols), values_to = "neighbour", names_to = "distance") %>%
  distinct()%>% filter(!is.na(neighbour), !str_detect( neighbour, "Unk|Baby|adult")) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(IDIndividual, neighbour)), collapse = '-'))
rm(cols)

# Remove duplicates
# If two individuals are scanned within 10 mins of each other both and both have the same network
# is important to one of the observations beacuse they are not independent
sc <- sc %>%
  group_by(Obs.nr, dyad) %>%
  filter(!(n() > 1 & any(!str_detect(distance, "Near"))) | !str_detect(distance, "Near")) %>%
  ungroup()

#### SOCIAL BONDS PACKAGE ########
# To create a file for the social bonds package you have:
# A file with behaviour. That contains the collumns: Date, Actor, Reciever and Behaviour
# scans data can be modified in where proximity can be describes as a behaviour. i.e. proox1m, prox5m...

# it dosent include presence data. worryiing? not sure
#install.packages("remotes")
#remotes::install_github("gobbios/socialindices")
?socialindices2::CSI()
library(socialindices2)

#### COMBINE THE ADLIB AND SCANS FILES ####

# changing scans data to have Date, Actor, Reciever and Behaviour
sca <- sc %>% mutate(Source = "scan") %>%
  dplyr::select(Group, Date, Time, Obs.nr, IDIndividual, neighbour, distance, Source) %>%
  rename(Actor = IDIndividual,
         Receiver = neighbour,
         Behaviour = distance
  ) %>%
  change_group_names("Group") %>%
  integrate_otherid(Actor, Receiver)

# merge both
{ssc <- ss %>% mutate(Source = "adlib") %>%
  dplyr::select(Group, Date, Time, Obs.nr, Actor, Receiver, BehaviourActor, Source) %>%
  rename(Behaviour = BehaviourActor) %>%
  mutate(Group = ifelse(Group == "Crossing,", "Crossing", Group)) %>%
  rbind(., sca) %>%
  mutate(Focal = Actor) %>%
  filter(Behaviour != "bagr")

rm(sca)}
table(ssc$Group)

ssc%>% group_by(Group) %>% summarize(n = n())
range(ss$Date)
max(ssc$Date)
# TODO I REMOVED THE SCANS DATA.
write.csv(ss, "/Users/mariagranell/Repositories/elo-sociality/sociality/data/Social_data_20211002_20240907.csv", row.names = F)


### perform CSI ###
{
  simplified_behaviours <- list(proximity=c("NearestJuvenileNeighbour", "NearestNeighbour","AdultsIn1m","JuvsIn1m", "sg","sw"
                              ,"AdultsIn2m","JuvsIn2m"
                              ,"AdultsIn5m","JuvsIn5m"),
                              social=c("gr","em","pl", "bgr","bem")
  )

# AK --------
csiak <- ssc %>% filter(Group == "AK") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours) #, ot.source = ot_social)

ak <- csiak %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point() +
  labs(title ="AK")

# NH ------------
csinh <- ssc %>% filter(Group == "NH") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)

nh <- csinh %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(x = zCSI, y =  focal2)) +
  geom_point() +
  labs(title ="NH")

# BD ------------
csibd <- ssc %>% filter(Group == "BD") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)

bd <- csibd %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point()+
  labs(title ="BD")

# KB ------------
csikb <- ssc %>% filter(Group == "KB") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)

kb <- csikb %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point()+
  labs(title ="KB")

# LT ------------
csilt <- ssc %>% filter(Group == "LT") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)

  lt <- csilt %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point()+
  labs(title ="LT")

# CR ------------
csicr <- ssc %>% filter(Group == "CR") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)

cr <- csicr %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point() +
  labs(title ="CR")

nh + bd + ak +
kb + lt + cr

  rm(nh,bd,ak,cr,lt,kb, simplified_behaviours)
}

# merge the CSI.
csiall <- csiak %>% rbind(csibd,csikb,csilt,csinh, csicr)
rm(csiak,csibd,csicr,csikb,csilt,csinh)

#write.csv(csiall, '/Users/mariagranell/Repositories/elo-sociality/sociality/CSI_oct2021-june2022.csv', row.names = F)



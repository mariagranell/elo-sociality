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
library(forcats)
library(tidyr) #
library(patchwork)
source('/Users/mariagranell/Repositories/data/functions.R')
library(tibble)

# path ------------------------
setwd('/Users/mariagranell/Repositories/elo-sociality/sociality')

# social data ------------------------
social <- read.csv('/Users/mariagranell/Repositories/elo-sociality/data/Social_10.2021-05.2022.csv')

#### CREATING *AD-LIB* CSI FILE ####

ss <- social %>%
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
{ss[grepl("kom", ss$RemarksA) & grepl("Unknown", ss$Individual1), "Individual1"] <- "Kom"
ss[grepl("kom", ss$RemarksA) & grepl("Unknown", ss$Individual2), "Individual2"] <- "Kom"
ss[grepl("Ratel", ss$RemarksA) & grepl("Unknown", ss$Individual1), "Individual1"] <- "Ratel"
ss[grepl("naal", ss$RemarksA) & grepl("Unknown", ss$Individual1), "Individual1"] <- "Naal"}

# Investigate the remarks and remove the ones that are uncertain
#a <- as.data.frame(unique(s$RemarksE)) The ones that are left are ok

# merging all the behaviours -----------
{
s12 <- ss %>% dplyr::select("Group","Individual1","BehaviourIndividual1","Individual2","BehaviourIndividual2", "Date", "Time", "Obs.nr") %>%
  mutate(IntOrder = "Interaction1-2")
s34 <- ss %>% mutate(IntOrder = "Interaction3-4",
         Individual1 = Individual3,
         Individual2 = Individual4,
         BehaviourIndividual1 = BehaviourIndividual3,
         BehaviourIndividual2 = BehaviourIndividual4
  ) %>% dplyr::select("Group","Individual1","BehaviourIndividual1","Individual2","BehaviourIndividual2", "Date", "Time", "Obs.nr","IntOrder")
s56 <- ss %>% mutate(IntOrder = "Interaction5-6",
         Individual1 = Individual5,
         Individual2 = Individual6,
         BehaviourIndividual1 = BehaviourIndividual5,
         BehaviourIndividual2 = BehaviourIndividual6
  ) %>% dplyr::select("Group","Individual1","BehaviourIndividual1","Individual2","BehaviourIndividual2", "Date", "Time", "Obs.nr","IntOrder")

ss <- s12 %>% rbind(s34,s56)
rm(s12,s34,s56)}

# Split behaviours into rows -------
ss <- split_behaviours(ss, c("BehaviourIndividual1", "BehaviourIndividual2"), ' ')
ss <- split_behaviours(ss, c("BehaviourIndividual1", "BehaviourIndividual2"), '.')

ss <- ss%>% filter (
  grepl("gr|sg|sw|em|pl", BehaviourIndividual1), # bge
  grepl("gr|sg|sw|em|pl", BehaviourIndividual2)
)

# Create actors and recievers ----
# select only the active behaviours
active_beh <- c('gr', 'sg', 'sw', 'em', 'pl')

# determine actors ID
for(i in seq_along(ss$BehaviourIndividual1)) {
  if(ss$BehaviourIndividual1[i] %in% active_beh) {
    ss$Actor[i] <- ss$Individual1[i]
    ss$Receiver[i] <- ss$Individual2[i]
  } else {
    ss$Actor[i] <- ss$Individual2[i]
    ss$Receiver[i] <- ss$Individual1[i]
  }
}

# determine actor behaviour
for(i in seq_along(ss$BehaviourIndividual1)) {
  if(ss$Individual1[i] == ss$Actor[i]) {
    ss$BehaviourActor[i] <- ss$BehaviourIndividual1[i]
  } else {
    ss$BehaviourActor[i] <- ss$BehaviourIndividual2[i]
  }
}
rm(i, active_beh)
ss <- ss %>% distinct() %>%
  dplyr::select(-matches("BehaviourIndividual|Individual", ignore.case = FALSE)) %>%
  filter(!grepl("baby|unknown", tolower(Actor)),
         !grepl("baby|unknown", tolower(Receiver)))

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
sc <- sc %>% pivot_longer(cols, values_to = "neighbour", names_to = "distance") %>%
  distinct()%>% filter(!is.na(neighbour), !str_detect( neighbour, "Unk|Baby|adult")) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(Individual, neighbour)), collapse = '-'))
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
  dplyr::select(Group, Date, Time, Obs.nr, Individual, neighbour, distance, Source) %>%
  rename(Actor = Individual,
         Receiver = neighbour,
         Behaviour = distance
  )

# merge both
{ssc <- ss %>% mutate(Source = "adlib") %>%
  dplyr::select(Group, Date, Time, Obs.nr, Actor, Receiver, BehaviourActor, Source) %>%
  rename(Behaviour = BehaviourActor) %>%
  mutate(Group = ifelse(Group == "Crossing,", "Crossing", Group)) %>%
  rbind(., sca) %>%
  mutate(Focal = Actor) %>%
  filter(Behaviour != "bagr")
rm(sca)}

ssc%>% group_by(Group) %>% summarize(n = n())

### perform CSI ###
{
  simplified_behaviours <- list(proximity=c("NearestJuvenileNeighbour", "NearestNeighbour","AdultsIn1m","JuvsIn1m", "sg","sw"
                              ,"AdultsIn2m","JuvsIn2m"
                              ,"AdultsIn5m","JuvsIn5m"),
                              social=c("gr","em","pl", "bgr","bem")
  )

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

# CR ------------
csicr <- ssc %>% filter(Group == "Crossing") %>%
  CSI(., duration.NA.treatm = "count", behaviours = simplified_behaviours)%>%
  mutate(group = "CR")

cr <- csicr %>%
  mutate(focal2 = fct_reorder(focal, CSI)) %>%
  ggplot(., aes(y = focal2, x =  zCSI)) +
  geom_point() +
  labs(title ="CR")

nh + bd + ak +
kb + lt + cr

  rm(nh,bd,ak,cr,lt,kb, simplified_behaviours)
}

# merge the CSI. Not including CR the data is unrelaiable
csiall <- csiak %>% rbind(csibd,csikb,csilt,csinh)
rm(csiak,csibd,csicr,csikb,csilt,csinh)

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
igraph_combined <- AK %>% rbind(NH,BD,KB,LT)
rm(AK,NH,BD,KB,LT,CR)
# combine with CSI
sociality_vectors <- igraph_combined %>%
  left_join(., csiall, by = c("Name" = "focal", "Group" = "group")) %>%
  rename(AnimalCode = Name) %>%
  dplyr::select(AnimalCode, Group, CSI, zCSI, Degree,
                Strength, Closeness,Betweenness, Eigencent)

write.csv(sociality_vectors, '/Users/mariagranell/Repositories/elo-sociality/sociality/sociality_vectors_oct2021-june2020.csv', row.names = F)


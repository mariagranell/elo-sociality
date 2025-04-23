# ---------------
# Title: create sex csi
# Date: 21 april 2025
# Author: mgranellruiz
# Goal: the goal is to combine all the sex data types to create one massive one to use for calculate the mounts_coming12 and mounts_last12
# ---------------

# library ---------------------
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')

# orginal script made by Josie
#https://mail.google.com/mail/u/0/#search/josefien/FMfcgzQZTpwsZCbSwnPLFPHMBmGgWGKh

# path ------------------------
setwd("/Users/mariagranell/Repositories/elo-sociality/sexual")

# data ------------------------
{
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
females_names <- lh %>% filter(Sex == "F") %>% pull(AnimalCode)
males_names <- lh %>% filter(Sex == "M") %>% pull(AnimalCode)
pd <- read.csv("/Users/mariagranell/Repositories/data/Oct2021-May2022/Sexual_10.2021-05.2022.csv") %>%
  dplyr::select(Date, Group, Initiator, InitiatorBehaviour, Receptor, ReceptorBehaviour) %>%
  rename(date = Date,
         group = Group,
         idindividual1 = Initiator,
         behaviourindiv1 = InitiatorBehaviour,
         idindividual2 = Receptor,
         behaviourindiv2 = ReceptorBehaviour) %>%
  mutate(complete = NA,
         date = as.Date(date, format = "%m/%d/%Y"))
pd2 <- read.csv("/Users/mariagranell/Repositories/data/acess_data/OutputData/sexual_access.csv")%>%
  dplyr::select(Date, Group, IDIndividual1, BehaviourIndiv1, IDIndividual2, BehaviourIndiv2, Complete) %>%
  rename_with(tolower)

pd3 <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/CleanFiles/sexual_cybertracker.csv") %>%
  dplyr::select(Date, Group, IDIndividual1, BehaviourIndiv1, IDIndividual2, BehaviourIndiv2, Complete) %>%
  rename_with(tolower)

focal <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/OriginalFiles/Focal_cybertracker.csv") %>%
   rename_with(tolower) %>%
  filter(behaviour == "Sexual",
         #interobs == "No"
  ) %>%
  dplyr::select(date, group, idindividual1, behaviourfocal, idindividual2, complete) %>%
  rename(behaviourindiv1 = behaviourfocal) %>%
  mutate(behaviourindiv2 = NA,
         date = as.Date(date))
}

rbind(pd, pd2, pd3, focal) %>% distinct() %>%
  add_season(.,"date") %>%
  plot_weekly_summary(., "data", "date")

sex_combination <- rbind(pd, pd2, focal) %>% distinct() %>%
  mutate(date = ymd(date)) %>%
  mutate(
    across(behaviourindiv1, tolower),
    complete = case_when(
      complete %in% c("No", "Unknown", "Yes", "Refused") ~ complete,
      grepl("mu|mo", behaviourindiv1) ~ "Unknown",
      grepl("ahg|av|rf|amu|ja|le|st|hi|amo", behaviourindiv1) ~ "Refused",
      TRUE ~ complete
    )
  ) %>%
  filter(complete %in% c("Yes", "No", "Unknown", "Interrupted")) %>%               # Keep only rows where mount was accepted
  filter(idindividual1 != "",
         idindividual2 != "",
         !is.na(idindividual2)) %>%                        # Remove empty IDs
  integrate_otherid(idindividual1, idindividual2) %>%
  mutate(FemaleID = case_when(
    idindividual2 %in% females_names ~ idindividual2,
    idindividual1 %in% females_names ~ idindividual1,
    TRUE ~ NA),
    MaleID = case_when(
      idindividual1 %in% males_names ~ idindividual1,
      idindividual2 %in% males_names ~ idindividual2,
      TRUE ~ NA_character_)
  ) %>%
  distinct()


sex <- change_group_names(sex_combination, "group") %>%
  filter(group %in% groups,
         !is.na(MaleID),
         !is.na(FemaleID)) %>%
  dplyr::select(date, MaleID, FemaleID, group) %>%
  correct_pru_que_mess("MaleID", "date", "group")

write.csv(sex, "/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/Sex_csi_combined_date.csv", row.names = F)

#### Creating presence matrix ####

# Select packages
library(dplyr) # to remove duplicates
library(writexl) # to save as excel file
library(reshape2)
library(data.table)

# Import data
LHdata <- read.csv("/Users/mariagranell/Repositories/elo-sociality/IVP_Lifehistory_260523.csv", header = T, stringsAsFactors = F, na.strings = c('NA', 'Not yet'))

Date <- seq(as.Date("2020-01-01"), as.Date(Sys.Date()), by="days")
dfDate <- as.data.frame(Date)

# Look at data
summary(LHdata)
str(LHdata)

# Select data
LHdata <- LHdata[,c(3,5,7,8,10:26)]
dfDate$Date <- as.Date(format(as.POSIXct(dfDate$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
str(LHdata)
str(dfDate)

# Create "LastSeen" column to add current date if ind still present in a group
LHdata$LastSeen <- NA

for(i in 1:nrow(LHdata)) {
  ifelse(nchar(LHdata$PresentGp[i]) == 2, 
         LHdata$LastSeen[i] <- format(Sys.Date(), "%d.%m.%Y"), 
         LHdata$LastSeen[i] <- NA)
}

# Check that it worked
which(nchar(LHdata$PresentGp) == 2)
head(LHdata[c(which(nchar(LHdata$PresentGp) == 2)),]) # OK
head(LHdata)

# Format dates using the following output = YYYY-mm-dd
LHdata$DOB <- as.Date(format(as.POSIXct(LHdata$DOB, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$FirstRecorded <- as.Date(format(as.POSIXct(LHdata$FirstRecorded, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DepartureNatalGp <- as.Date(format(as.POSIXct(LHdata$DepartureNatalGp,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration1 <- as.Date(format(as.POSIXct(LHdata$DateImmigration1,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen1 <- as.Date(format(as.POSIXct(LHdata$LastSeen1, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration2 <- as.Date(format(as.POSIXct(LHdata$DateImmigration2,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen2 <- as.Date(format(as.POSIXct(LHdata$LastSeen2, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration3 <- as.Date(format(as.POSIXct(LHdata$DateImmigration3,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen3 <- as.Date(format(as.POSIXct(LHdata$LastSeen3, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration4 <- as.Date(format(as.POSIXct(LHdata$DateImmigration4,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen4 <- as.Date(format(as.POSIXct(LHdata$LastSeen4, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen <- as.Date(format(as.POSIXct(LHdata$LastSeen, format = "%d/%m/%Y"), "%Y-%m-%d"))
#LHdata$ImmigrationGp1 <- as.factor(LHdata$ImmigrationGp1)
#LHdata$ImmigrationGp2 <- as.factor(LHdata$ImmigrationGp2)
#LHdata$ImmigrationGp3 <- as.factor(LHdata$ImmigrationGp3)
#LHdata$ImmigrationGp4 <- as.factor(LHdata$ImmigrationGp4)
str(LHdata)
#View(LHdata)

# Create a vector with our groups
Gp <- c("AK","BD","CR","KB","LT","NH","RC","RL", "IF")
Gp <- as.factor(Gp)
#levels(Gp)
#nlevels(Gp)

# create an empty list where results will be stored for each group
LH <- list()

# create loops that will separate the data by groups 
# (using all the different columns as one ind can be in a group at different time of their life: 
# PresentGp,BirthGp,ImmigrationGp1, 2, 3 & 4)

for(i in 1:nlevels(Gp)) {
  
  for(i in 1:nrow(LHdata)) {
    
    PG <- which(LHdata$PresentGp == Gp[i])
    BG <- which(LHdata$BirthGp == Gp[i])
    IG1 <- which(LHdata$ImmigrationGp1 == Gp[i])
    IG2 <- which(LHdata$ImmigrationGp2 == Gp[i])
    IG3 <- which(LHdata$ImmigrationGp3 == Gp[i])
    IG4 <- which(LHdata$ImmigrationGp4 == Gp[i])
    
    output <- rbind(LHdata[PG,],LHdata[BG,],LHdata[IG1,],LHdata[IG2,],LHdata[IG3,],LHdata[IG4,])
    
    LH[[i]] <- output 
    
  }
}

# Extract data for the different groups, removing duplicates & sorting it out by alphabetical order 
# for individuals and resetting row numbers

# create empty list + data frames where results will be stored for each group
LHGp <- data.frame()
listLHGp <- list()

for(i in 1:nlevels(Gp)) {
  
  LHGp <- distinct(as.data.frame(LH[i]))
  LHGp <- LHGp[order(LHGp$Code),]
  rownames(LHGp) <- NULL
  listLHGp[[i]] <- LHGp
  
}


# extract data for each gp
LHAK <- as.data.frame(listLHGp[1]) # 117 ind total present at least once in AK
LHBD <- as.data.frame(listLHGp[2]) # 206 ind total present at least once in BD
LHCR <- as.data.frame(listLHGp[3]) # 132 ind total present at least once in CR
LHKB <- as.data.frame(listLHGp[4]) # 72 ind total present at least once in KB
LHLT <- as.data.frame(listLHGp[5]) # 135 ind total present at least once in LT
LHNH <- as.data.frame(listLHGp[6]) # 161 ind total present at least once in NH
LHRC <- as.data.frame(listLHGp[7]) # 33 ind total present at least once in RC
LHRL <- as.data.frame(listLHGp[8]) # 2 ind total present at least once in RL
LHIF <- as.data.frame(listLHGp[9]) # 6 ind total present at aleast once in IF


#### ANKHASE ####

# add StartAK & EndAK + check structure
LHAK$StartAK <- NA
LHAK$StartAK <- as.Date(LHAK$StartAK, format = "%d.%m.%Y")
LHAK$EndAK <- NA
LHAK$EndAK <- as.Date(LHAK$EndAK, format = "%d.%m.%Y")
str(LHAK)


# extract start date for all the AK individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)
i = 1

for(i in 1:nrow(LHAK)) {
  
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK") {
    LHAK$StartAK[i] <- LHAK$DOB[i]} 
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK" & is.na(LHAK$DOB[i])) {
    LHAK$StartAK[i] <- LHAK$FirstRecorded[i]} 
  if (LHAK$ImmigrationGp2[i] == "AK" & !is.na(LHAK$DateImmigration2[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration2[i]} 
  if (LHAK$ImmigrationGp3[i] == "AK" & !is.na(LHAK$DateImmigration3[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration3[i]} 
  if (LHAK$ImmigrationGp4[i] == "AK" & !is.na(LHAK$DateImmigration4[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration4[i]} 
  if (LHAK$ImmigrationGp1[i] == "AK" & !is.na(LHAK$DateImmigration1[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration1[i]} 
}



# extract end date for all the AK ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHAK)) {
  
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$DepartureNatalGp[i]} 
  if (!is.na(LHAK$ImmigrationGp1[i]) & (LHAK$ImmigrationGp1[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen1[i]}
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK" & is.na(LHAK$DepartureNatalGp[i])) {
    LHAK$EndAK[i] <- LHAK$LastSeen1[i]} 
  if (!is.na(LHAK$ImmigrationGp2[i]) & (LHAK$ImmigrationGp2[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen2[i]} 
  if (!is.na(LHAK$ImmigrationGp3[i]) & (LHAK$ImmigrationGp3[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen3[i]} 
  if (!is.na(LHAK$ImmigrationGp4[i]) & (LHAK$ImmigrationGp4[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen4[i]}
  if (!is.na(LHAK$PresentGp[i]) & LHAK$PresentGp[i] == "AK" & is.na(LHAK$EndAK[i]))  {LHAK$EndAK[i] <- LHAK$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
AK <- LHAK[,c(1,2,3,4,6,23,24)]
AK[,c(8:21)] <- NA
colnames(AK) <- c("Individidual","Sex","DOB","FirstRecorded","BirthGp","StartAK", "EndAK","TotalDurationAK","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Y2018","Y2019","Y2020","Y2021", "Y2022")
head(AK)

## Create AK matrix ####
# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
AK$EndAK[is.na(AK$EndAK)] <- as.Date(Sys.Date())

# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = AK$Individidual,
  Start = AK$StartAK,
  End = AK$EndAK
)


# Create the matrix
AKpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

View(AKpresence)

write.csv(AKpresence,"C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/AK2016-2022.csv")

#### BAIE DANKIE ####

# add StartBD & EndBD + check structure
LHBD$StartBD <- NA
LHBD$StartBD <- as.Date(LHBD$StartBD, format = "%d.%m.%Y")
LHBD$EndBD <- NA
LHBD$EndBD <- as.Date(LHBD$EndBD, format = "%d.%m.%Y")
str(LHBD)

# extract start date for all the BD individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHBD)) {
  
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD") {
    LHBD$StartBD[i] <- LHBD$DOB[i]} 
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD" & is.na(LHBD$DOB[i])) {
    LHBD$StartBD[i] <- LHBD$FirstRecorded[i]} 
  if (LHBD$ImmigrationGp1[i] == "BD" & !is.na(LHBD$DateImmigration1[i])) {
    print(i)
    LHBD$StartBD[i] <- LHBD$DateImmigration1[i]} 
  if (LHBD$ImmigrationGp2[i] == "BD" & !is.na(LHBD$DateImmigration2[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration2[i]} 
  if (LHBD$ImmigrationGp3[i] == "BD" & !is.na(LHBD$DateImmigration3[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration3[i]} 
  if (LHBD$ImmigrationGp4[i] == "BD" & !is.na(LHBD$DateImmigration4[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration4[i]} 
  
}

# extract end date for all the BD ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHBD)) {
  
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$DepartureNatalGp[i]} 
  if (!is.na(LHBD$ImmigrationGp1[i]) & (LHBD$ImmigrationGp1[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen1[i]}
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD" & is.na(LHBD$DepartureNatalGp[i])) {
    LHBD$EndBD[i] <- LHBD$LastSeen1[i]} 
  if (!is.na(LHBD$ImmigrationGp2[i]) & (LHBD$ImmigrationGp2[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen2[i]} 
  if (!is.na(LHBD$ImmigrationGp3[i]) & (LHBD$ImmigrationGp3[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen3[i]} 
  if (!is.na(LHBD$ImmigrationGp4[i]) & (LHBD$ImmigrationGp4[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen4[i]}
  if (!is.na(LHBD$PresentGp[i]) & LHBD$PresentGp[i] == "BD" & is.na(LHBD$EndBD[i]))  {LHBD$EndBD[i] <- LHBD$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
BD <- LHBD[,c(1,2,3,4,6,23,24)]
BD[,c(8:21)] <- NA
colnames(BD) <- c("Individidual","Sex","DOB","FirstRecorded","BirthGp","StartBD", "EndBD","TotalDurationBD","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Y2018","Y2019","Y2020","Y2021", "Y2022")
head(BD)
View(BD)

## Create BD matrix ####

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
BD$EndBD[is.na(BD$EndBD)] <- as.Date(Sys.Date())

# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = BD$Individidual,
  Start = BD$StartBD,
  End = BD$EndBD
)

# Create a matrix
BDpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

View(BDpresence)

# Save output
write.csv(BDpresence, "C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/BD2016-2022.csv")

#### CROSSING ####

# add StartCR & EndCR + check structure
LHCR$StartCR <- NA
LHCR$StartCR <- as.Date(LHCR$StartCR, format = "%d.%m.%Y")
LHCR$EndCR <- NA
LHCR$EndCR <- as.Date(LHCR$EndCR, format = "%d.%m.%Y")
str(LHCR)

# extract start date for all the CR individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHCR)) {
  
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR") {
    LHCR$StartCR[i] <- LHCR$DOB[i]} 
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR" & is.na(LHCR$DOB[i])) {
    LHCR$StartCR[i] <- LHCR$FirstRecorded[i]} 
  if (LHCR$ImmigrationGp1[i] == "CR" & !is.na(LHCR$DateImmigration1[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration1[i]} 
  if (LHCR$ImmigrationGp2[i] == "CR" & !is.na(LHCR$DateImmigration2[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration2[i]} 
  if (LHCR$ImmigrationGp3[i] == "CR" & !is.na(LHCR$DateImmigration3[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration3[i]} 
  if (LHCR$ImmigrationGp4[i] == "CR" & !is.na(LHCR$DateImmigration4[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration4[i]} 
  
}

# extract end date for all the CR ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHCR)) {
  
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$DepartureNatalGp[i]} 
  if (!is.na(LHCR$ImmigrationGp1[i]) & (LHCR$ImmigrationGp1[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen1[i]}
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR" & is.na(LHCR$DepartureNatalGp[i])) {
    LHCR$EndCR[i] <- LHCR$LastSeen1[i]} 
  if (!is.na(LHCR$ImmigrationGp2[i]) & (LHCR$ImmigrationGp2[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen2[i]} 
  if (!is.na(LHCR$ImmigrationGp3[i]) & (LHCR$ImmigrationGp3[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen3[i]} 
  if (!is.na(LHCR$ImmigrationGp4[i]) & (LHCR$ImmigrationGp4[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen4[i]}
  if (!is.na(LHCR$PresentGp[i]) & LHCR$PresentGp[i] == "CR" & is.na(LHCR$EndCR[i]))  {LHCR$EndCR[i] <- LHCR$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
CR <- LHCR[,c(1,2,3,4,6,23,24)]
CR[,c(8:21)] <- NA
colnames(CR) <- c("Individidual","Sex","DOB","FirstRecorded","BirthGp","StartCR", "EndCR","TotalDurationCR","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Y2018","Y2019","Y2020","Y2021", "Y2022")
head(CR)

## Create CR matrix ####

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
CR$EndCR[is.na(CR$EndCR)] <- as.Date(Sys.Date())

# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = CR$Individidual,
  Start = CR$StartCR,
  End = CR$EndCR
)
View(df)

# Create a matrix
CRpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

View(CRpresence)

# Save output
write.csv(CRpresence,"/Users/mariagranell/Repositories/data/presence/presenceCR2020-2023.csv")

#### KUBU ####

# add StartKB & EndKB + check structure
LHKB$StartKB <- NA
LHKB$StartKB <- as.Date(LHKB$StartKB, format = "%d.%m.%Y")
LHKB$EndKB <- NA
LHKB$EndKB <- as.Date(LHKB$EndKB, format = "%d.%m.%Y")
str(LHKB)

# extract start date for all the KB individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHKB)) {
  
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB") {
    LHKB$StartKB[i] <- LHKB$DOB[i]} 
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB" & is.na(LHKB$DOB[i])) {
    LHKB$StartKB[i] <- LHKB$FirstRecorded[i]} 
  if (LHKB$ImmigrationGp1[i] == "KB" & !is.na(LHKB$DateImmigration1[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration1[i]} 
  if (LHKB$ImmigrationGp2[i] == "KB" & !is.na(LHKB$DateImmigration2[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration2[i]} 
  if (LHKB$ImmigrationGp3[i] == "KB" & !is.na(LHKB$DateImmigration3[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration3[i]} 
  if (LHKB$ImmigrationGp4[i] == "KB" & !is.na(LHKB$DateImmigration4[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration4[i]} 
  
}

# extract end date for all the KB ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHKB)) {
  
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$DepartureNatalGp[i]} 
  if (!is.na(LHKB$ImmigrationGp1[i]) & (LHKB$ImmigrationGp1[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen1[i]}
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB" & is.na(LHKB$DepartureNatalGp[i])) {
    LHKB$EndKB[i] <- LHKB$LastSeen1[i]} 
  if (!is.na(LHKB$ImmigrationGp2[i]) & (LHKB$ImmigrationGp2[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen2[i]} 
  if (!is.na(LHKB$ImmigrationGp3[i]) & (LHKB$ImmigrationGp3[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen3[i]} 
  if (!is.na(LHKB$ImmigrationGp4[i]) & (LHKB$ImmigrationGp4[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen4[i]}
  if (!is.na(LHKB$PresentGp[i]) & LHKB$PresentGp[i] == "KB" & is.na(LHKB$EndKB[i]))  {LHKB$EndKB[i] <- LHKB$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
KB <- LHKB[,c(1,2,3,4,6,23,24)]
KB[,c(8:21)] <- NA
colnames(KB) <- c("Individidual","Sex","DOB","FirstRecorded","BirthGp","StartKB", "EndKB","TotalDurationKB","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Y2018","Y2019","Y2020","Y2021", "Y2022")
head(KB)

## Create KB matrix ####

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
KB$EndKB[is.na(KB$EndKB)] <- as.Date(Sys.Date())

# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = KB$Individidual,
  Start = KB$StartKB,
  End = KB$EndKB
)
View(df)

# Create a matrix
KBpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

View(KBpresence)

# Save output
write.csv(KBpresence,"C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/KB2016-2022.csv")

#### LEMON TREE ####

# add StartLT & EndLT + check structure
LHLT$StartLT <- NA
LHLT$StartLT <- as.Date(LHLT$StartLT, format = "%d.%m.%Y")
LHLT$EndLT <- NA
LHLT$EndLT <- as.Date(LHLT$EndLT, format = "%d.%m.%Y")
str(LHLT)

# extract start date for all the LT individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHLT)) {
  
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT") {
    LHLT$StartLT[i] <- LHLT$DOB[i]} 
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT" & is.na(LHLT$DOB[i])) {
    LHLT$StartLT[i] <- LHLT$FirstRecorded[i]} 
  if (LHLT$ImmigrationGp1[i] == "LT" & !is.na(LHLT$DateImmigration1[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration1[i]} 
  if (LHLT$ImmigrationGp2[i] == "LT" & !is.na(LHLT$DateImmigration2[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration2[i]} 
  if (LHLT$ImmigrationGp3[i] == "LT" & !is.na(LHLT$DateImmigration3[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration3[i]} 
  if (LHLT$ImmigrationGp4[i] == "LT" & !is.na(LHLT$DateImmigration4[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration4[i]} 
  
}

# extract end date for all the LT ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHLT)) {
  
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$DepartureNatalGp[i]} 
  if (!is.na(LHLT$ImmigrationGp1[i]) & (LHLT$ImmigrationGp1[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen1[i]}
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT" & is.na(LHLT$DepartureNatalGp[i])) {
    LHLT$EndLT[i] <- LHLT$LastSeen1[i]} 
  if (!is.na(LHLT$ImmigrationGp2[i]) & (LHLT$ImmigrationGp2[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen2[i]} 
  if (!is.na(LHLT$ImmigrationGp3[i]) & (LHLT$ImmigrationGp3[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen3[i]} 
  if (!is.na(LHLT$ImmigrationGp4[i]) & (LHLT$ImmigrationGp4[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen4[i]}
  if (!is.na(LHLT$PresentGp[i]) & LHLT$PresentGp[i] == "LT" & is.na(LHLT$EndLT[i]))  {LHLT$EndLT[i] <- LHLT$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
LT <- LHLT[,c(1,2,3,4,6,23,24)]
LT[,c(8:21)] <- NA
colnames(LT) <- c("Individidual","Sex","DOB","FirstRecorded","BirthGp","StartLT", "EndLT","TotalDurationLT","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Y2018","Y2019","Y2020","Y2021", "Y2022")
head(LT)

## Create LT matrix ####

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
LT$EndLT[is.na(LT$EndLT)] <- as.Date(Sys.Date())

# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = LT$Individidual,
  Start = LT$StartLT,
  End = LT$EndLT
)
View(df)

# Create a matrix
LTpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

View(LTpresence)

# Save output
write.csv(LTpresence,"C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/LT2016-2022.csv")

#### NOHA ####

# add StartNH & EndNH + check structure
LHNH$StartNH <- NA
LHNH$StartNH <- as.Date(LHNH$StartNH, format = "%d.%m.%Y")
LHNH$EndNH <- NA
LHNH$EndNH <- as.Date(LHNH$EndNH, format = "%d.%m.%Y")
str(LHNH)

# extract start date for all the NH individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHNH)) {
  
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH") {
    LHNH$StartNH[i] <- LHNH$DOB[i]} 
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH" & is.na(LHNH$DOB[i])) {
    LHNH$StartNH[i] <- LHNH$FirstRecorded[i]} 
  if (LHNH$ImmigrationGp1[i] == "NH" & !is.na(LHNH$DateImmigration1[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration1[i]} 
  if (LHNH$ImmigrationGp2[i] == "NH" & !is.na(LHNH$DateImmigration2[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration2[i]} 
  if (LHNH$ImmigrationGp3[i] == "NH" & !is.na(LHNH$DateImmigration3[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration3[i]} 
  if (LHNH$ImmigrationGp4[i] == "NH" & !is.na(LHNH$DateImmigration4[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration4[i]} 
  
}

# extract end date for all the NH ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHNH)) {
  
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$DepartureNatalGp[i]} 
  if (!is.na(LHNH$ImmigrationGp1[i]) & (LHNH$ImmigrationGp1[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen1[i]}
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH" & is.na(LHNH$DepartureNatalGp[i])) {
    LHNH$EndNH[i] <- LHNH$LastSeen1[i]} 
  if (!is.na(LHNH$ImmigrationGp2[i]) & (LHNH$ImmigrationGp2[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen2[i]} 
  if (!is.na(LHNH$ImmigrationGp3[i]) & (LHNH$ImmigrationGp3[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen3[i]} 
  if (!is.na(LHNH$ImmigrationGp4[i]) & (LHNH$ImmigrationGp4[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen4[i]}
  if (!is.na(LHNH$PresentGp[i]) & LHNH$PresentGp[i] == "NH" & is.na(LHNH$EndNH[i]))  {LHNH$EndNH[i] <- LHNH$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
NH <- LHNH[,c(1,2,3,4,6,23,24)]
NH[,c(8:21)] <- NA
colnames(NH) <- c("Individidual","Sex","DOB","FirstRecorded","BirthGp","StartNH", "EndNH","TotalDurationNH","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Y2018","Y2019","Y2020","Y2021", "Y2022")
head(NH)

## Create NH matrix ####

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
NH$EndNH[is.na(NH$EndNH)] <- as.Date(Sys.Date())

# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = NH$Individidual,
  Start = NH$StartNH,
  End = NH$EndNH
)
#View(df)

# Create a matrix
NHpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

View(NHpresence)

# Save output
write.csv(NHpresence,"C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/NH2016-2022.csv")

# save presence of all groups

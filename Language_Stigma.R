# Loading required packages
library(car)
library(dplyr)
library(tidyr)
library(reshape2)
library(lme4)
library(psych)
library(Hmisc)
library(sjPlot)


# Importing RPP data
rppdata<- read.csv('/Users/kiley/Documents/Honors Thesis/Data/JobData_RPP_imputation.csv')
# Importing MTurk data
mturkdata <- read.csv('/Users/kiley/Documents/Honors Thesis/Data/JobData_MTurk.csv')

###########################################
# DATA CLEANING
###########################################

# Renaming columns
rppdata <- dplyr::rename(rppdata, Duration = Duration..in.seconds.,
                  heardGoal = Heard.about.goal,
                  noticeLanguage = Notice.language,
                  studyGoal = Goal.of.study,
                  contactLevel = Level.of.contact, Accept = Q3, 
                  Danger1 = Q16_1, Danger2 = Q16_2, Danger3 = Q16_3, 
                  Danger4 = Q16_4, Danger5 = Q16_5, Danger6 = Q16_6,
                  Danger7 = Q16_7, Danger8 = Q16_8, Danger9 = Q16_9)

mturkdata <- dplyr::rename(mturkdata, 
                         Danger1 = Q16_1, Danger2 = Q16_2, Danger3 = Q16_3, 
                         Danger4 = Q16_4, Danger5 = Q16_5, Danger6 = Q16_6,
                         Danger7 = Q16_7, Danger8 = Q16_8, Danger9 = Q16_9)


# Removing participants (failed attention checks, geometric patterns, etc.)
nrow(rppdata) # 168 participants
rppdata <- subset(rppdata, Finished == 1)
nrow(rppdata) # 2 participants removed due to incompleteness
rppdata <- subset(rppdata, 
                  id != 1566 & id != 9512 & id != 10238 &
                     id != 10142 & id != 10463 & id != 65352 &
                     id != 2890 & id != 8222 & id != 8214 &
                     id != 9409 & id != 6915 & id != 3623 &
                     id != 1236 & id != 7762 & id != 10500 & 
                     id != 9826 & id != 65352 & id != 8379 & 
                     id != 10194 & id != 10248 & id !=3412 &
                     id != 10500 & id != 2241 & id != 6024 & 
                     id !=405 & id !=9845 & id !=8730 & 
                     id != 10269 & id != 9886 & id !=2780 &
                     id != 7623 & id != 8214)
nrow(rppdata) # 138 participants

# REMOVING 5 PARTICIPANTS THAT NOTICED LANGUAGE AND CHECKING TO SEE IF IT INFLUENCES MODEL RESULTS
## Uncomment to rerun models and view differences
#rppdata <- subset(rppdata, 
#                  id != 5618 & id != 3888 & id != 8242 &
#                          id != 2247 & id != 8860 & id != 10792)


nrow(mturkdata) # 110 participants

mturkdata <- subset(mturkdata, Finished == 1)
mturkdata <- subset(mturkdata, 
                    MTurk.ID != 48184 & MTurk.ID != 31624 & 
                       MTurk.ID != 53785 &  MTurk.ID != 18478 &
                       MTurk.ID != 35524 & MTurk.ID != 27375 & MTurk.ID != 12922 &
                       MTurk.ID != 21174 & 
                       MTurk.ID != 72347 & MTurk.ID != 65894 &
                       MTurk.ID != 76930 & MTurk.ID != 68551 & 
                      MTurk.ID != 55126)
nrow(mturkdata) # 5 participants removed due to incompleteness

# Histograms of duration on questionnaire
hist(rppdata$Duration, breaks=100)
time1 <- rppdata$Duration
time1[time1>1800] <- NA
hist(time1, breaks=10)
t.test(time1, conf.level=0.95)  
sd(time1, na.rm=T) 

hist(mturkdata$Duration, breaks=100)
time2 <- mturkdata$Duration
time2[time2>1800] <- NA
hist(time2, breaks=10)
t.test(time2, conf.level=0.95) 
sd(time2, na.rm=T) 

#Removing fast responders
nrow(rppdata) # 151 participants
rppdata <- subset(rppdata, Duration > 300)
nrow(rppdata) # 144 participants
# 7 participants removed due to duration

nrow(mturkdata) # 106 participants
mturkdata <- subset(mturkdata, Duration > 300)
nrow(mturkdata) # 96 participants
# 10 participants removed due to duration

# MAJOR
levels(rppdata$Major) <- c("Prefer not to say", "Applied Math", "Biology",
                           "Business Adminstration", "Biology", 
                           "Engineering", "Chemistry", 
                           "Cognitive Science", "Cognitive Science",
                           "Cognitive Science", "Cognitive Science/Asian Studies",
                           "Cognitive Science/Psychology", "Cognitive Science",
                           "Computer Science", "Computer Science/Public Health",
                           "Computer Science/Public Health", 
                           "Computer Science/Applied Math", "Computer Science",
                           "Data Science", "Cognitive Science/Data Science",
                           "Economics", "Economics", "Psychology/Economics", 
                           "Cognitive Science/Economics", "Computer Science",
                           "Environmental Economics & Policy", "Ethnic Studies",
                           "Global Studies", "Biology", 
                           "Biology", "Psychology", "Public Health",
                           "Psychology/Legal Studies", "Biology",
                           "Business Adminstration", "Biology", "Biology",
                           "Biology/Business Adminstration", "Psychology/Biology", 
                           "Biology", "Biology", "Engineering", 
                           "Media Studies", "Biology", "Biology", "Biology",
                           "Biology/Public Health",  "Biology", "Biology",
                           "Nutritional Science", "Nutritional Science", 
                           "Nutritional Science", "Philosophy", 
                           "Political Science", "Psychology", "Psychology",
                           "Psychology", "Psychology", "Psychology", 
                           "Psychology/Business Adminstration", "Psychology",
                           "Psychology/Art", "Psychology/Data Science", 
                           "Psychology/Media Studies", "Psychology/Engineering",
                           "Psychology/Spanish", "Psychology/Sociology", 
                           "Psychology/Legal Studies", "Public Health", 
                           "Public Health", "Public Health", "Psychology/Rhetoric",
                           "Social Welfare", "Society & Environment",
                           "Undeclared", "Undeclared", "Undeclared", "Psychology")
summary(rppdata$Major)

# RELEVELING VARIABLES FOR RPP DATA
levels(rppdata$studyGoal) <- c("Yes", "No")
rppdataNo<- subset(rppdata, 
                     id == 10124 | id == 10330 | id == 4285 |
                     id == 10413 | id == 10837 | id == 10620 | 
                     id == 6816 | id == 10312 | id == 10412 | 
                     id == 10258)

rppdataNo$studyGoal <- 0

rppdataYes <- subset(rppdata, 
       id != 10124 & id != 10330 &id != 4285 &
       id != 10413 & id != 10837 &id != 10620 & 
       id != 6816 & id != 10312 & id != 10412 & 
       id != 10258)
nrow(rppdataYes)
rppdataYes$studyGoal <- 1

rppdata <-rbind(rppdataNo, rppdataYes)
View(rppdata)
nrow(rppdata) #131

# RELEVELING VARIABLES FOR MTURK DATA
levels(mturkdata$studyGoal) <- c("Yes", "No")
mturkdataNo<- subset(mturkdata, 
                   MTurk.ID  == 56931 | MTurk.ID  == 46168 | MTurk.ID  == 68336 | 
                   MTurk.ID  == 51337 | MTurk.ID  == 33598 | MTurk.ID  == 92122 |
                   MTurk.ID  == 43296 | MTurk.ID  == 86389 | MTurk.ID  == 96721 |
                   MTurk.ID  == 28458 | MTurk.ID  == 16938 | MTurk.ID  == 62128 |
                   MTurk.ID  == 73122 | MTurk.ID  == 85627 | MTurk.ID  == 16405 |
                   MTurk.ID  == 19426 | MTurk.ID  == 44984 | MTurk.ID  == 90507 |
                   MTurk.ID  == 17845 | MTurk.ID  == 51840 | MTurk.ID  == 84778 |
                   MTurk.ID  == 72011 | MTurk.ID  == 92607 | MTurk.ID  == 34812 |
                   MTurk.ID  == 37192 | MTurk.ID  == 69767 | MTurk.ID  == 45698 |
                   MTurk.ID  == 60224 | MTurk.ID  == 22524 | MTurk.ID  == 81519 |
                   MTurk.ID  == 72956 | MTurk.ID  == 22778 | MTurk.ID  == 94828 |
                   MTurk.ID  == 47384 | MTurk.ID  == 32245 | MTurk.ID  == 71172 |
                   MTurk.ID  == 56222 | MTurk.ID  == 89563 | MTurk.ID  == 25755 |
                   MTurk.ID  == 98123)
nrow(mturkdataNo) # 40
mturkdataNo$studyGoal <- 0

mturkdataYes<- subset(mturkdata, 
                     MTurk.ID  != 56931 & MTurk.ID != 46168 & MTurk.ID != 68336 & 
                             MTurk.ID  != 51337 & MTurk.ID != 33598 & MTurk.ID != 92122 &
                             MTurk.ID  != 43296 & MTurk.ID != 86389 & MTurk.ID != 96721 &
                             MTurk.ID  != 28458 & MTurk.ID != 16938 & MTurk.ID != 62128 &
                             MTurk.ID  != 73122 & MTurk.ID != 85627 & MTurk.ID != 16405 &
                             MTurk.ID  != 19426 & MTurk.ID != 44984 & MTurk.ID != 90507 &
                             MTurk.ID  != 17845 & MTurk.ID != 51840 & MTurk.ID != 84778 &
                             MTurk.ID  != 72011 & MTurk.ID != 92607 & MTurk.ID != 34812 &
                             MTurk.ID  != 37192 & MTurk.ID != 69767 & MTurk.ID != 45698 &
                             MTurk.ID  != 60224 & MTurk.ID != 22524 & MTurk.ID != 81519 &
                             MTurk.ID  != 72956 & MTurk.ID != 22778 & MTurk.ID != 94828 &
                             MTurk.ID  != 47384 & MTurk.ID != 32245 & MTurk.ID != 71172 &
                             MTurk.ID  != 56222 & MTurk.ID != 89563 & MTurk.ID != 25755 &
                             MTurk.ID  != 98123)
nrow(mturkdataYes) # 66
mturkdataYes$studyGoal <- 1

mturkdata <-rbind(mturkdataNo, mturkdataYes)
nrow(mturkdata) # 81 total
View(mturkdata)
###########################################
# DEMOGRAPHIC STATS
###########################################

# ETHNICITY
## RPP data
rppdata$Hispanic.Latino <- as.factor(rppdata$Hispanic.Latino)
levels(rppdata$Hispanic.Latino) <- c("Yes", "No") 
summary(rppdata$Hispanic.Latino)

## mturk data
mturkdata$Hispanic.Latino <- as.factor(mturkdata$Hispanic.Latino)
levels(mturkdata$Hispanic.Latino) <- c("Yes", "No") 
summary(mturkdata$Hispanic.Latino)

# AGE
## RPP data
rppdata$Age <- as.factor(rppdata$Age)
levels(rppdata$Age) <- c("18", "19", "20", "21", "22", "23 and up")
summary(rppdata$Age)
nrow(rppdata)

## MTurk data
mturkdata$Age <- as.factor(mturkdata$Age)
levels(mturkdata$Age) <- c("20", "21", "22", "23 and up")
summary(mturkdata$Age)
nrow(mturkdata)
# GENDER
## RPP data 
rppdata$Gender <- as.factor(rppdata$Gender)
levels(rppdata$Gender) <- c("Male", "Female", "Non-binary")
summary(rppdata$Gender)

## Mturk data
mturkdata$Gender <- as.factor(mturkdata$Gender)
levels(mturkdata$Gender) <- c("Male", "Female", "Non-binary")
summary(mturkdata$Gender)

# RACE
levels(rppdata$Race) <-c("Prefer not to say", "White", "White/Asian",
                         "White/Asian/Other", "White/Other",
                         "Black", "Asian",
                         "Other", "Prefer not to say")
summary(rppdata$Race)
levels(mturkdata$Race) <- c("Prefer not to say", "White", "White/Black/Other",
                            "White/Native American", "Black", "Asian", 
                            "Other", "Prefer not to say")

summary(mturkdata$Race)

# PERCEIVED SOCIAL STATUS
mean(mturkdata$SES)
sd(mturkdata$SES)
mean(rppdata$SES)
sd(rppdata$SES)

###########################################
# COMBINING DATASETS
###########################################

mturkdata$Dataset<- array(0,nrow(mturkdata))
rppdata$Dataset<- array(0,nrow(rppdata))

mturkdata$Dataset[mturkdata$Dataset==0] <- "MTurk"
rppdata$Dataset[rppdata$Dataset==0] <- "RPP"

mturkdata$Major<- array(0,nrow(mturkdata))
mturkdata$Major[mturkdata$major==0] <- NA

rppdata$MTurk.ID <- array(0,nrow(rppdata))
rppdata$MTurk.ID[rppdata$MTurk.ID==0]<- NA
rppdata$Progress <- array(0,nrow(rppdata))
rppdata$Progress[rppdata$MTurk.ID==0]<- NA

jobdata <- rbind(mturkdata, rppdata)

# Assigning each row participant ID
jobdata$ID <- seq(1, nrow(jobdata))

nrow(jobdata) # 240 participants

###########################################
# CHI-SQUARE SIGNIFICANCE TESTS
###########################################

## Gender
gender <- table(jobdata$Dataset, jobdata$Gender)
chisq.test(jobdata$Dataset, jobdata$Gender, simulate.p.value = T)
fisher.test(gender)

## Race
race <- table(jobdata$Race, jobdata$Dataset)
fisher.test(race)
race <- as.data.frame(race)

race$Var2[race$Var2 == "Prefer not to say"] <- NA
race <- na.omit(race)
race <- as.table(race)
chisq.test(jobdata$Dataset, jobdata$Race, simulate.p.value = T)

## Ethnicity 
ethnicity <- table(jobdata$Dataset, jobdata$Hispanic.Latino)
chisq.test(jobdata$Dataset, jobdata$Hispanic.Latino)

# SES 
t.test(mturkdata$SES, rppdata$SES)
###########################################
# CREATING LANGUAGE CONDITION
###########################################
jobdata$condition <- array(0,nrow(jobdata))
jobdata$condition <- as.character(jobdata$condition)

jobdata[,"condition"]<- with(jobdata, 
                             ifelse(is.na(PFD1_1) & is.na(PFD2_1) & is.na(PFD3_1) &
                                    is.na(NBD1_1) & is.na(NBD2_1) & is.na(NBD3_1),
                                    "identity", jobdata[,"condition"]))
jobdata[,"condition"] <- with(jobdata, 
                              ifelse(is.na(NBD1_1) & is.na(NBD2_1) & is.na(NBD3_1)&
                                     is.na(IFD1_1) & is.na(IFD2_1) & is.na(IFD3_1), 
                                     "person", jobdata[,"condition"]))

jobdata[,"condition"]<- with(jobdata, 
                             ifelse(is.na(IFD1_1) & is.na(IFD2_1) & is.na(IFD3_1) &
                                    is.na(PFD1_1) & is.na(PFD2_1) & is.na(PFD3_1),
                                    "noun", jobdata[,"condition"]))
                               
###########################################
# SCORING ECS VARIABLES
###########################################
# INTELLIGENT 
Dintelligent <- cbind(jobdata$PFD1_1, jobdata$PFD2_1, jobdata$PFD3_1,
                     jobdata$IFD1_1, jobdata$IFD2_1, jobdata$IFD3_1,
                     jobdata$NBD1_1, jobdata$NBD2_1, jobdata$NBD3_1)
jobdata$intelligent.D <- rowMeans(Dintelligent, na.rm=T)
                     
Sintelligent <- cbind(jobdata$PFS1_1, jobdata$PFS2_1, jobdata$PFS3_1,
                      jobdata$IFS1_1, jobdata$IFS2_1, jobdata$IFS3_1,
                      jobdata$NBS1_1, jobdata$NBS2_1, jobdata$NBS3_1)
jobdata$intelligent.S <- rowMeans(Sintelligent, na.rm=T)

Eintelligent <- cbind(jobdata$PFE1_1, jobdata$PFE2_1, jobdata$PFE3_1,
                      jobdata$IFE1_1, jobdata$IFE2_1, jobdata$IFE3_1,
                      jobdata$NBE1_1, jobdata$NBE2_1, jobdata$NBE3_1)
jobdata$intelligent.E <- rowMeans(Eintelligent, na.rm=T)

# CONSISTENT
Dconsistent <- cbind(jobdata$PFD1_2, jobdata$PFD2_2, jobdata$PFD3_2,
                      jobdata$IFD1_2, jobdata$IFD2_2, jobdata$IFD3_2,
                      jobdata$NBD1_2, jobdata$NBD2_2, jobdata$NBD3_2)
jobdata$consistent.D <- rowMeans(Dconsistent, na.rm=T)

Sconsistent <- cbind(jobdata$PFS1_2, jobdata$PFS2_2, jobdata$PFS3_2,
                      jobdata$IFS1_2, jobdata$IFS2_2, jobdata$IFS3_2,
                      jobdata$NBS1_2, jobdata$NBS2_2, jobdata$NBS3_2)
jobdata$consistent.S <- rowMeans(Sconsistent, na.rm=T)

Econsistent <- cbind(jobdata$PFE1_2, jobdata$PFE2_2, jobdata$PFE3_2,
                      jobdata$IFE1_2, jobdata$IFE2_2, jobdata$IFE3_2,
                      jobdata$NBE1_2, jobdata$NBE2_2, jobdata$NBE3_2)
jobdata$consistent.E <- rowMeans(Econsistent, na.rm=T)

# DEPENDABLE 
Ddependable <- cbind(jobdata$PFD1_3, jobdata$PFD2_3, jobdata$PFD3_3,
                      jobdata$IFD1_3, jobdata$IFD2_3, jobdata$IFD3_3,
                      jobdata$NBD1_3, jobdata$NBD2_3, jobdata$NBD3_3)
jobdata$dependable.D <- rowMeans(Ddependable, na.rm=T)

Sdependable <- cbind(jobdata$PFS1_3, jobdata$PFS2_3, jobdata$PFS3_3,
                      jobdata$IFS1_3, jobdata$IFS2_3, jobdata$IFS3_3,
                      jobdata$NBS1_3, jobdata$NBS2_3, jobdata$NBS3_3)
jobdata$dependable.S <- rowMeans(Sdependable, na.rm=T)

Edependable <- cbind(jobdata$PFE1_3, jobdata$PFE2_3, jobdata$PFE3_3,
                      jobdata$IFE1_3, jobdata$IFE2_3, jobdata$IFE3_3,
                      jobdata$NBE1_3, jobdata$NBE2_3, jobdata$NBE3_3)
jobdata$dependable.E <- rowMeans(Edependable, na.rm=T)

# CONFIDENT
Dconfident <- cbind(jobdata$PFD1_4, jobdata$PFD2_4, jobdata$PFD3_4,
                      jobdata$IFD1_4, jobdata$IFD2_4, jobdata$IFD3_4,
                      jobdata$NBD1_4, jobdata$NBD2_4, jobdata$NBD3_4)
jobdata$confident.D <- rowMeans(Dconfident, na.rm=T)

Sconfident <- cbind(jobdata$PFS1_4, jobdata$PFS2_4, jobdata$PFS3_4,
                      jobdata$IFS1_4, jobdata$IFS2_4, jobdata$IFS3_4,
                      jobdata$NBS1_4, jobdata$NBS2_4, jobdata$NBS3_4)
jobdata$confident.S <- rowMeans(Sconfident, na.rm=T)

Econfident <- cbind(jobdata$PFE1_4, jobdata$PFE2_4, jobdata$PFE3_4,
                      jobdata$IFE1_4, jobdata$IFE2_4, jobdata$IFE3_4,
                      jobdata$NBE1_4, jobdata$NBE2_4, jobdata$NBE3_4)
jobdata$confident.E<- rowMeans(Econfident, na.rm=T)

# RESPONSIBLE
Dresponsible <- cbind(jobdata$PFD1_5, jobdata$PFD2_5, jobdata$PFD3_5,
                      jobdata$IFD1_5, jobdata$IFD2_5, jobdata$IFD3_5,
                      jobdata$NBD1_5, jobdata$NBD2_5, jobdata$NBD3_5)
jobdata$responsible.D <- rowMeans(Dresponsible, na.rm=T)

Sresponsible <- cbind(jobdata$PFS1_5, jobdata$PFS2_5, jobdata$PFS3_5,
                      jobdata$IFS1_5, jobdata$IFS2_5, jobdata$IFS3_5,
                      jobdata$NBS1_5, jobdata$NBS2_5, jobdata$NBS3_5)
jobdata$responsible.S <- rowMeans(Sresponsible, na.rm=T)

Eresponsible <- cbind(jobdata$PFE1_5, jobdata$PFE2_5, jobdata$PFE3_5,
                      jobdata$IFE1_5, jobdata$IFE2_5, jobdata$IFE3_5,
                      jobdata$NBE1_5, jobdata$NBE2_5, jobdata$NBE3_5)
jobdata$responsible.E <- rowMeans(Eresponsible, na.rm=T)

# EFFECTIVE 
Deffective <- cbind(jobdata$PFD1_6, jobdata$PFD2_6, jobdata$PFD3_6,
                      jobdata$IFD1_6, jobdata$IFD2_6, jobdata$IFD3_6,
                      jobdata$NBD1_6, jobdata$NBD2_6, jobdata$NBD3_6)
jobdata$effective.D <- rowMeans(Deffective, na.rm=T)

Seffective <- cbind(jobdata$PFS1_6, jobdata$PFS2_6, jobdata$PFS3_6,
                      jobdata$IFS1_6, jobdata$IFS2_6, jobdata$IFS3_6,
                      jobdata$NBS1_6, jobdata$NBS2_6, jobdata$NBS3_6)
jobdata$effective.S <- rowMeans(Seffective, na.rm=T)

Eeffective<- cbind(jobdata$PFE1_6, jobdata$PFE2_6, jobdata$PFE3_6,
                      jobdata$IFE1_6, jobdata$IFE2_6, jobdata$IFE3_6,
                      jobdata$NBE1_6, jobdata$NBE2_6, jobdata$NBE3_6)
jobdata$effective.E <- rowMeans(Eintelligent, na.rm=T)

# STABLE 
Dstable <- cbind(jobdata$PFD1_7, jobdata$PFD2_7, jobdata$PFD3_7,
                      jobdata$IFD1_7, jobdata$IFD2_7, jobdata$IFD3_7,
                      jobdata$NBD1_7, jobdata$NBD2_7, jobdata$NBD3_7)
jobdata$stable.D <- rowMeans(Dstable, na.rm=T)

Sstable <- cbind(jobdata$PFS1_7, jobdata$PFS2_7, jobdata$PFS3_7,
                      jobdata$IFS1_7, jobdata$IFS2_7, jobdata$IFS3_7,
                      jobdata$NBS1_7, jobdata$NBS2_7, jobdata$NBS3_7)
jobdata$stable.S<- rowMeans(Sstable, na.rm=T)

Estable <- cbind(jobdata$PFE1_7, jobdata$PFE2_7, jobdata$PFE3_7,
                      jobdata$IFE1_7, jobdata$IFE2_7, jobdata$IFE3_7,
                      jobdata$NBE1_7, jobdata$NBE2_7, jobdata$NBE3_7)
jobdata$stable.E <- rowMeans(Estable, na.rm=T)

# COOPERATIVE
Dcooperative<- cbind(jobdata$PFD1_8, jobdata$PFD2_8, jobdata$PFD3_8,
                      jobdata$IFD1_8, jobdata$IFD2_8, jobdata$IFD3_8,
                      jobdata$NBD1_8, jobdata$NBD2_8, jobdata$NBD3_8)
jobdata$cooperative.D <- rowMeans(Dcooperative, na.rm=T)

Scooperative <- cbind(jobdata$PFS1_8, jobdata$PFS2_8, jobdata$PFS3_8,
                      jobdata$IFS1_8, jobdata$IFS2_8, jobdata$IFS3_8,
                      jobdata$NBS1_8, jobdata$NBS2_8, jobdata$NBS3_8)
jobdata$cooperative.S <- rowMeans(Scooperative, na.rm=T)

Ecooperative <- cbind(jobdata$PFE1_8, jobdata$PFE2_8, jobdata$PFE3_8,
                      jobdata$IFE1_8, jobdata$IFE2_8, jobdata$IFE3_8,
                      jobdata$NBE1_8, jobdata$NBE2_8, jobdata$NBE3_8)
jobdata$cooperative.E <- rowMeans(Ecooperative, na.rm=T)

# TRUSTWORTHY 
Dtrustworthy <- cbind(jobdata$PFD1_9, jobdata$PFD2_9, jobdata$PFD3_9,
                      jobdata$IFD1_9, jobdata$IFD2_9, jobdata$IFD3_9,
                      jobdata$NBD1_9, jobdata$NBD2_9, jobdata$NBD3_9)
jobdata$trustworthy.D <- rowMeans(Dtrustworthy, na.rm=T)

Strustworthy <- cbind(jobdata$PFS1_9, jobdata$PFS2_9, jobdata$PFS3_9,
                      jobdata$IFS1_9, jobdata$IFS2_9, jobdata$IFS3_9,
                      jobdata$NBS1_9, jobdata$NBS2_9, jobdata$NBS3_9)
jobdata$trustworthy.S <- rowMeans(Strustworthy, na.rm=T)

Etrustworthy <- cbind(jobdata$PFE1_9, jobdata$PFE2_9, jobdata$PFE3_9,
                      jobdata$IFE1_9, jobdata$IFE2_9, jobdata$IFE3_9,
                      jobdata$NBE1_9, jobdata$NBE2_9, jobdata$NBE3_9)
jobdata$trustworthy.E <- rowMeans(Etrustworthy, na.rm=T)

# SKILLFUL 
Dskillful <- cbind(jobdata$PFD1_10, jobdata$PFD2_10, jobdata$PFD3_10,
                      jobdata$IFD1_10, jobdata$IFD2_10, jobdata$IFD3_10,
                      jobdata$NBD1_10, jobdata$NBD2_10, jobdata$NBD3_10)
jobdata$skillful.D <- rowMeans(Dskillful, na.rm=T)

Sskillful <- cbind(jobdata$PFS1_10, jobdata$PFS2_10, jobdata$PFS3_10,
                      jobdata$IFS1_10, jobdata$IFS2_10, jobdata$IFS3_10,
                      jobdata$NBS1_10, jobdata$NBS2_10, jobdata$NBS3_10)
jobdata$skillful.S <- rowMeans(Sskillful, na.rm=T)

Eskillful <- cbind(jobdata$PFE1_10, jobdata$PFE2_10, jobdata$PFE3_10,
                      jobdata$IFE1_10, jobdata$IFE2_10, jobdata$IFE3_10,
                      jobdata$NBE1_10, jobdata$NBE2_10, jobdata$NBE3_10)
jobdata$skillful.E <- rowMeans(Eskillful, na.rm=T)

# POWERFUL 
Dpowerful <- cbind(jobdata$PFD1_11, jobdata$PFD2_11, jobdata$PFD3_11,
                    jobdata$IFD1_11, jobdata$IFD2_11, jobdata$IFD3_11,
                    jobdata$NBD1_11, jobdata$NBD2_11, jobdata$NBD3_11)
jobdata$powerful.D <- rowMeans(Dpowerful, na.rm=T)

Spowerful <- cbind(jobdata$PFS1_11, jobdata$PFS2_11, jobdata$PFS3_11,
                   jobdata$IFS1_11, jobdata$IFS2_11, jobdata$IFS3_11,
                   jobdata$NBS1_11, jobdata$NBS2_11, jobdata$NBS3_11)
jobdata$powerful.S <- rowMeans(Spowerful, na.rm=T)

Epowerful <- cbind(jobdata$PFE1_11, jobdata$PFE2_11, jobdata$PFE3_11,
                   jobdata$IFE1_11, jobdata$IFE2_11, jobdata$IFE3_11,
                   jobdata$NBE1_11, jobdata$NBE2_11, jobdata$NBE3_11)
jobdata$powerful.E <- rowMeans(Epowerful, na.rm=T)

# STRONG 
Dstrong<- cbind(jobdata$PFD1_12, jobdata$PFD2_12, jobdata$PFD3_12,
                   jobdata$IFD1_12, jobdata$IFD2_12, jobdata$IFD3_12,
                   jobdata$NBD1_12, jobdata$NBD2_12, jobdata$NBD3_12)
jobdata$strong.D <- rowMeans(Dstrong, na.rm=T)

Sstrong <- cbind(jobdata$PFS1_12, jobdata$PFS2_12, jobdata$PFS3_12,
                   jobdata$IFS1_12, jobdata$IFS2_12, jobdata$IFS3_12,
                   jobdata$NBS1_12, jobdata$NBS2_12, jobdata$NBS3_12)
jobdata$strong.S <- rowMeans(Sstrong, na.rm=T)

Estrong <- cbind(jobdata$PFE1_12, jobdata$PFE2_12, jobdata$PFE3_12,
                   jobdata$IFE1_12, jobdata$IFE2_12, jobdata$IFE3_12,
                   jobdata$NBE1_12, jobdata$NBE2_12, jobdata$NBE3_12)
jobdata$strong.E <- rowMeans(Estrong, na.rm=T)

# CAPABLE
Dcapable <- cbind(jobdata$PFD1_13, jobdata$PFD2_13, jobdata$PFD3_13,
                   jobdata$IFD1_13, jobdata$IFD2_13, jobdata$IFD3_13,
                   jobdata$NBD1_13, jobdata$NBD2_13, jobdata$NBD3_13)
jobdata$capable.D <- rowMeans(Dcapable, na.rm=T)

Scapable <- cbind(jobdata$PFS1_13, jobdata$PFS2_13, jobdata$PFS3_13,
                   jobdata$IFS1_13, jobdata$IFS2_13, jobdata$IFS3_13,
                   jobdata$NBS1_13, jobdata$NBS2_13, jobdata$NBS3_13)
jobdata$capable.S <- rowMeans(Scapable, na.rm=T)

Ecapable <- cbind(jobdata$PFE1_13, jobdata$PFE2_13, jobdata$PFE3_13,
                   jobdata$IFE1_13, jobdata$IFE2_13, jobdata$IFE3_13,
                   jobdata$NBE1_13, jobdata$NBE2_13, jobdata$NBE3_13)
jobdata$capable.E <- rowMeans(Ecapable, na.rm=T)

# AGGRESSIVE
Daggressive <- cbind(jobdata$PFD1_14, jobdata$PFD2_14, jobdata$PFD3_14,
                   jobdata$IFD1_14, jobdata$IFD2_14, jobdata$IFD3_14,
                   jobdata$NBD1_14, jobdata$NBD2_14, jobdata$NBD3_14)
jobdata$aggressive.D <- rowMeans(Daggressive, na.rm=T)

Saggressive <- cbind(jobdata$PFS1_14, jobdata$PFS2_14, jobdata$PFS3_14,
                   jobdata$IFS1_14, jobdata$IFS2_14, jobdata$IFS3_14,
                   jobdata$NBS1_14, jobdata$NBS2_14, jobdata$NBS3_14)
jobdata$aggressive.S <- rowMeans(Saggressive, na.rm=T)

Eaggressive<- cbind(jobdata$PFE1_14, jobdata$PFE2_14, jobdata$PFE3_14,
                   jobdata$IFE1_14, jobdata$IFE2_14, jobdata$IFE3_14,
                   jobdata$NBE1_14, jobdata$NBE2_14, jobdata$NBE3_104)
jobdata$aggressive.E <- rowMeans(Eaggressive, na.rm=T)

# BOLD 
Dbold <- cbind(jobdata$PFD1_15, jobdata$PFD2_15, jobdata$PFD3_15,
                   jobdata$IFD1_15, jobdata$IFD2_15, jobdata$IFD3_15,
                   jobdata$NBD1_15, jobdata$NBD2_15, jobdata$NBD3_15)
jobdata$bold.D <- rowMeans(Dbold, na.rm=T)

Sbold <- cbind(jobdata$PFS1_15, jobdata$PFS2_15, jobdata$PFS3_15,
                   jobdata$IFS1_15, jobdata$IFS2_15, jobdata$IFS3_15,
                   jobdata$NBS1_15, jobdata$NBS2_15, jobdata$NBS3_15)
jobdata$bold.S <- rowMeans(Sbold, na.rm=T)

Ebold <- cbind(jobdata$PFE1_15, jobdata$PFE2_15, jobdata$PFE3_15,
                   jobdata$IFE1_15, jobdata$IFE2_15, jobdata$IFE3_15,
                   jobdata$NBE1_15, jobdata$NBE2_15, jobdata$NBE3_15)
jobdata$bold.E <- rowMeans(Ebold, na.rm=T)

# SELF-RELIANT
Dselfreliant <- cbind(jobdata$PFD1_16, jobdata$PFD2_16, jobdata$PFD3_16,
                   jobdata$IFD1_16, jobdata$IFD2_16, jobdata$IFD3_16,
                   jobdata$NBD1_16, jobdata$NBD2_16, jobdata$NBD3_16)
jobdata$selfreliant.D <- rowMeans(Dselfreliant, na.rm=T)

Sselfreliant <- cbind(jobdata$PFS1_16, jobdata$PFS2_16, jobdata$PFS3_16,
                   jobdata$IFS1_16, jobdata$IFS2_16, jobdata$IFS3_16,
                   jobdata$NBS1_16, jobdata$NBS2_16, jobdata$NBS3_16)
jobdata$selfreliant.S <- rowMeans(Sselfreliant, na.rm=T)

Eselfreliant <- cbind(jobdata$PFE1_16, jobdata$PFE2_16, jobdata$PFE3_16,
                   jobdata$IFE1_16, jobdata$IFE2_16, jobdata$IFE3_16,
                   jobdata$NBE1_16, jobdata$NBE2_16, jobdata$NBE3_16)
jobdata$selfreliant.E <- rowMeans(Eselfreliant, na.rm=T)

# FORCEFUL 
Dforceful<- cbind(jobdata$PFD1_17, jobdata$PFD2_17, jobdata$PFD3_17,
                   jobdata$IFD1_17, jobdata$IFD2_17, jobdata$IFD3_17,
                   jobdata$NBD1_17, jobdata$NBD2_17, jobdata$NBD3_17)
jobdata$forceful.D <- rowMeans(Dforceful, na.rm=T)

Sforceful <- cbind(jobdata$PFS1_17, jobdata$PFS2_17, jobdata$PFS3_17,
                   jobdata$IFS1_17, jobdata$IFS2_17, jobdata$IFS3_17,
                   jobdata$NBS1_17, jobdata$NBS2_17, jobdata$NBS3_17)
jobdata$forceful.S <- rowMeans(Sforceful, na.rm=T)

Eforceful <- cbind(jobdata$PFE1_17, jobdata$PFE2_17, jobdata$PFE3_17,
                   jobdata$IFE1_17, jobdata$IFE2_17, jobdata$IFE3_17,
                   jobdata$NBE1_17, jobdata$NBE2_17, jobdata$NBE3_17)
jobdata$forceful.E <- rowMeans(Eforceful, na.rm=T)

# DYNAMIC
Ddynamic<- cbind(jobdata$PFD1_18, jobdata$PFD2_18, jobdata$PFD3_18,
                   jobdata$IFD1_18, jobdata$IFD2_18, jobdata$IFD3_18,
                   jobdata$NBD1_18, jobdata$NBD2_18, jobdata$NBD3_18)
jobdata$dynamic.D <- rowMeans(Ddynamic, na.rm=T)

Sdynamic <- cbind(jobdata$PFS1_18, jobdata$PFS2_18, jobdata$PFS3_18,
                   jobdata$IFS1_18, jobdata$IFS2_18, jobdata$IFS3_18,
                   jobdata$NBS1_18, jobdata$NBS2_18, jobdata$NBS3_18)
jobdata$dynamic.S <- rowMeans(Sdynamic, na.rm=T)

Edynamic <- cbind(jobdata$PFE1_18, jobdata$PFE2_18, jobdata$PFE3_18,
                   jobdata$IFE1_18, jobdata$IFE2_18, jobdata$IFE3_18,
                   jobdata$NBE1_18, jobdata$NBE2_18, jobdata$NBE3_18)
jobdata$dynamic.E <- rowMeans(Edynamic, na.rm=T)


# DECISIVE 
Ddecisive<- cbind(jobdata$PFD1_19, jobdata$PFD2_19, jobdata$PFD3_19,
                   jobdata$IFD1_19, jobdata$IFD2_19, jobdata$IFD3_19,
                   jobdata$NBD1_19, jobdata$NBD2_19, jobdata$NBD3_19)
jobdata$decisive.D <- rowMeans(Ddecisive, na.rm=T)

Sdecisive <- cbind(jobdata$PFS1_19, jobdata$PFS2_19, jobdata$PFS3_19,
                   jobdata$IFS1_19, jobdata$IFS2_19, jobdata$IFS3_19,
                   jobdata$NBS1_19, jobdata$NBS2_19, jobdata$NBS3_19)
jobdata$decisive.S <- rowMeans(Sdecisive, na.rm=T)

Edecisive <- cbind(jobdata$PFE1_19, jobdata$PFE2_19, jobdata$PFE3_19,
                   jobdata$IFE1_19, jobdata$IFE2_19, jobdata$IFE3_19,
                   jobdata$NBE1_19, jobdata$NBE2_19, jobdata$NBE3_19)
jobdata$decisive.E <- rowMeans(Edecisive, na.rm=T)

# BUSINESSLIKE
Dbusiness <- cbind(jobdata$PFD1_20, jobdata$PFD2_20, jobdata$PFD3_20,
                   jobdata$IFD1_20, jobdata$IFD2_20, jobdata$IFD3_20,
                   jobdata$NBD1_20, jobdata$NBD2_20, jobdata$NBD3_20)
jobdata$business.D <- rowMeans(Dbusiness, na.rm=T)

Sbusiness <- cbind(jobdata$PFS1_20, jobdata$PFS2_20, jobdata$PFS3_20,
                   jobdata$IFS1_20, jobdata$IFS2_20, jobdata$IFS3_20,
                   jobdata$NBS1_20, jobdata$NBS2_20, jobdata$NBS3_20)
jobdata$business.S <- rowMeans(Sbusiness, na.rm=T)

Ebusiness <- cbind(jobdata$PFE1_20, jobdata$PFE2_20, jobdata$PFE3_20,
                   jobdata$IFE1_20, jobdata$IFE2_20, jobdata$IFE3_20,
                   jobdata$NBE1_20, jobdata$NBE2_20, jobdata$NBE3_20)
jobdata$business.E <- rowMeans(Ebusiness, na.rm=T)

# EFFICIENT
Defficient<- cbind(jobdata$PFD1_21, jobdata$PFD2_21, jobdata$PFD3_21,
                   jobdata$IFD1_21, jobdata$IFD2_21, jobdata$IFD3_21,
                   jobdata$NBD1_21, jobdata$NBD2_21, jobdata$NBD3_21)
jobdata$efficient.D <- rowMeans(Defficient, na.rm=T)

Sefficient <- cbind(jobdata$PFS1_21, jobdata$PFS2_21, jobdata$PFS3_21,
                   jobdata$IFS1_21, jobdata$IFS2_21, jobdata$IFS3_21,
                   jobdata$NBS1_21, jobdata$NBS2_21, jobdata$NBS3_21)
jobdata$efficient.S <- rowMeans(Sefficient, na.rm=T)

Eefficient <- cbind(jobdata$PFE1_21, jobdata$PFE2_21, jobdata$PFE3_21,
                   jobdata$IFE1_21, jobdata$IFE2_21, jobdata$IFE3_21,
                   jobdata$NBE1_21, jobdata$NBE2_21, jobdata$NBE3_21)
jobdata$efficient.E <- rowMeans(Eefficient, na.rm=T)

# EXPERT
Dexpert <- cbind(jobdata$PFD1_22, jobdata$PFD2_22, jobdata$PFD3_22,
                   jobdata$IFD1_22, jobdata$IFD2_22, jobdata$IFD3_22,
                   jobdata$NBD1_22, jobdata$NBD2_22, jobdata$NBD3_22)
jobdata$expert.D <- rowMeans(Dexpert, na.rm=T)

Sexpert <- cbind(jobdata$PFS1_22, jobdata$PFS2_22, jobdata$PFS3_22,
                   jobdata$IFS1_22, jobdata$IFS2_22, jobdata$IFS3_22,
                   jobdata$NBS1_22, jobdata$NBS2_22, jobdata$NBS3_22)
jobdata$expert.S <- rowMeans(Sexpert, na.rm=T)

Eexpert <- cbind(jobdata$PFE1_22, jobdata$PFE2_22, jobdata$PFE3_22,
                   jobdata$IFE1_22, jobdata$IFE2_22, jobdata$IFE3_22,
                   jobdata$NBE1_22, jobdata$NBE2_22, jobdata$NBE3_22)
jobdata$expert.E <- rowMeans(Eexpert, na.rm=T)

# COMPETENT
Dcompetent <- cbind(jobdata$PFD1_23, jobdata$PFD2_23, jobdata$PFD3_23,
                   jobdata$IFD1_23, jobdata$IFD2_23, jobdata$IFD3_23,
                   jobdata$NBD1_23, jobdata$NBD2_23, jobdata$NBD3_23)
jobdata$competent.D <- rowMeans(Dcompetent, na.rm=T)

Scompetent <- cbind(jobdata$PFS1_23, jobdata$PFS2_23, jobdata$PFS3_23,
                   jobdata$IFS1_23, jobdata$IFS2_23, jobdata$IFS3_23,
                   jobdata$NBS1_23, jobdata$NBS2_23, jobdata$NBS3_23)
jobdata$competent.S <- rowMeans(Scompetent, na.rm=T)

Ecompetent <- cbind(jobdata$PFE1_23, jobdata$PFE2_23, jobdata$PFE3_23,
                   jobdata$IFE1_23, jobdata$IFE2_23, jobdata$IFE3_23,
                   jobdata$NBE1_23, jobdata$NBE2_23, jobdata$NBE3_23)
jobdata$competent.E <- rowMeans(Ecompetent, na.rm=T)

# EXPERIENCED
Dexperienced <- cbind(jobdata$PFD1_24, jobdata$PFD2_24, jobdata$PFD3_24,
                   jobdata$IFD1_24, jobdata$IFD2_24, jobdata$IFD3_24,
                   jobdata$NBD1_24, jobdata$NBD2_24, jobdata$NBD3_24)
jobdata$experienced.D <- rowMeans(Dexperienced, na.rm=T)

Sexperienced <- cbind(jobdata$PFS1_24, jobdata$PFS2_24, jobdata$PFS3_24,
                   jobdata$IFS1_24, jobdata$IFS2_24, jobdata$IFS3_24,
                   jobdata$NBS1_24, jobdata$NBS2_24, jobdata$NBS3_24)
jobdata$experienced.S <- rowMeans(Sexperienced, na.rm=T)

Eexperienced <- cbind(jobdata$PFE1_24, jobdata$PFE2_24, jobdata$PFE3_24,
                   jobdata$IFE1_24, jobdata$IFE2_24, jobdata$IFE3_24,
                   jobdata$NBE1_24, jobdata$NBE2_24, jobdata$NBE3_24)
jobdata$experienced.E <- rowMeans(Eexperienced, na.rm=T)

# PROFESSIONAL
Dprofessional <- cbind(jobdata$PFD1_25, jobdata$PFD2_25, jobdata$PFD3_25,
                   jobdata$IFD1_25, jobdata$IFD2_25, jobdata$IFD3_25,
                   jobdata$NBD1_25, jobdata$NBD2_25, jobdata$NBD3_25)
jobdata$professional.D <- rowMeans(Dprofessional, na.rm=T)

Sprofessional <- cbind(jobdata$PFS1_25, jobdata$PFS2_25, jobdata$PFS3_25,
                   jobdata$IFS1_25, jobdata$IFS2_25, jobdata$IFS3_25,
                   jobdata$NBS1_25, jobdata$NBS2_25, jobdata$NBS3_25)
jobdata$professional.S <- rowMeans(Sprofessional, na.rm=T)

Eprofessional <- cbind(jobdata$PFE1_25, jobdata$PFE2_25, jobdata$PFE3_25,
                   jobdata$IFE1_25, jobdata$IFE2_25, jobdata$IFE3_25,
                   jobdata$NBE1_25, jobdata$NBE2_25, jobdata$NBE3_25)
jobdata$professional.E <- rowMeans(Eprofessional, na.rm=T)

# SUCCESSFUL
Dsuccessful<- cbind(jobdata$PFD1_26, jobdata$PFD2_26, jobdata$PFD3_26,
                   jobdata$IFD1_26, jobdata$IFD2_26, jobdata$IFD3_26,
                   jobdata$NBD1_26, jobdata$NBD2_26, jobdata$NBD3_26)
jobdata$successful.D <- rowMeans(Dsuccessful, na.rm=T)

Ssuccessful <- cbind(jobdata$PFS1_26, jobdata$PFS2_26, jobdata$PFS3_26,
                   jobdata$IFS1_26, jobdata$IFS2_26, jobdata$IFS3_26,
                   jobdata$NBS1_26, jobdata$NBS2_26, jobdata$NBS3_26)
jobdata$successful.S <- rowMeans(Ssuccessful, na.rm=T)

Esuccessful <- cbind(jobdata$PFE1_26, jobdata$PFE2_26, jobdata$PFE3_26,
                   jobdata$IFE1_26, jobdata$IFE2_26, jobdata$IFE3_26,
                   jobdata$NBE1_26, jobdata$NBE2_26, jobdata$NBE3_26)
jobdata$successful.E <- rowMeans(Esuccessful, na.rm=T)


jobdata <- select(jobdata, -c("Progress", "Finished", "id", 
                              "MTurk.ID", "PFD1_1":"NBE3_27"))


###########################################
# PERCEIVED DANGEROUSNESS
###########################################
neg.dangerous <- cbind(jobdata$Danger2, jobdata$Danger6)
pos.dangerous <- cbind(jobdata$Danger1, jobdata$Danger3,
                       jobdata$Danger4, jobdata$Danger5, 
                       jobdata$Danger7, jobdata$Danger8)
neg.dangerousR <- 9-neg.dangerous

dangerous.all <- cbind(neg.dangerousR, pos.dangerous)
jobdata$danger <- array(0, nrow(jobdata))
jobdata$danger <- rowMeans(dangerous.all)

psych::alpha(dangerous.all) # alpha = 0.86

###########################################
# LEVEL OF CONTACT
###########################################
jobdata$contactLevel<- as.character(jobdata$contactLevel)
jobdata$contactLevel<- sapply(strsplit(jobdata$contactLevel, "[ ,]+"), 
                              function(i) last((i)))
jobdata$contactLevel <- as.numeric(jobdata$contactLevel)
jobdata$contactLevel[is.na(jobdata$contactLevel)] <- 0

###########################################
# IMPUTING MISSING COMPETENCE VALUES
###########################################
View(jobdata)

library(mice)
init = mice(jobdata, maxit=0) 
meth = init$method
predM = init$predictorMatrix
colnames(jobdata)
meth[c("Accept","Age", "Gender", "Race",
       "Hispanic.Latino","SES", "Danger1", "Danger2","Danger3",
       "Danger4", "Danger5", "Danger6", "Danger7", "Danger8",
       "Danger9", "contactLevel", "studyGoal", "heardGoal", 
       "noticeLanguage", "Major", "ID",  "condition", "intelligent.D", 
       "intelligent.S", "intelligent.E", "consistent.D", "consistent.S",
       "consistent.E", "dependable.D", "dependable.S","dependable.E",
       "confident.D", "confident.S", "confident.E", "responsible.D",
       "responsible.S","responsible.E","effective.D", "effective.S",
       "effective.E", "stable.D", "stable.S", "stable.E","cooperative.D",
       "cooperative.S", "cooperative.E", "trustworthy.D", "trustworthy.S", 
       "trustworthy.E", "skillful.D", "skillful.S", "skillful.E", 
       "powerful.D", "powerful.S", "powerful.E","strong.D","strong.S", 
       "strong.E", "capable.D", "capable.S", "capable.E", "aggressive.D", 
       "aggressive.S", "aggressive.E", "bold.D", "bold.S", "bold.E", 
       "selfreliant.D", "selfreliant.S", "selfreliant.E", 
       "forceful.D", "forceful.S", "forceful.E", "dynamic.D",
       "dynamic.S", "dynamic.E", "decisive.D",  "decisive.S", 
       "decisive.E", "business.D","business.S", "business.E", 
       "efficient.D","efficient.S","efficient.E","expert.D","expert.S", 
       "expert.E", "experienced.D", "experienced.S", "experienced.E", 
       "professional.D", "professional.S", "professional.E", "successful.D",
       "successful.S", "successful.E", "danger")]=""     
meth[c("competent.D", "competent.E", "competent.S")]="pmm"

set.seed(103)
imputed = mice(jobdata, method=meth, 
               predictorMatrix=predM, m=5)
jobdata <- complete(imputed)

###########################################
# RESHAPING DATA (LONG FORMAT)
###########################################
long <- pivot_longer(jobdata, 
                     cols = intelligent.D:successful.E,
                     names_sep = "\\.",
                     names_to = c("measure", "disorder"),
                     values_to = c("value"))

long <- pivot_wider(long,
                      names_from = "measure", 
                      values_from = "value")

###########################################
# ECS (PERSONALITY, POWER, COMPETENCE, PROFESSIONALISM)
###########################################
# PERSONALITY 
long$personality <-cbind(long$intelligent, long$consistent, 
                            long$dependable, long$responsible, 
                            long$stable, long$cooperative, 
                            long$trustworthy)
long$personalityMeans <- rowMeans(long$personality, na.rm=T)

psych::alpha(long$personality) # Raw alpha = 0.89

# POWER
long$power <- cbind(long$powerful, long$strong, long$bold, 
                       long$selfreliant,  
                       long$dynamic, long$decisive, long$forceful, long$aggressive) 
long$powerMeans <- rowMeans(long$power, na.rm=T)
psych::alpha(long$power) # Raw alpha = 0.86

# COMPETENCE
long$competence <- cbind(long$intelligent, long$confident, 
                            long$skillful, long$capable, 
                            long$efficient, long$business)
long$competenceMeans <- rowMeans(long$competence, na.rm=T)
psych::alpha(long$competence) # Raw alpha = 0.89


# PROFESSIONALISM
long$professionalism <- cbind(long$expert, long$experienced, 
                              long$professional, long$successful) 
long$professionalMeans <- rowMeans(long$professionalism, na.rm=T)
psych::alpha(long$professionalism) # Raw alpha = 0.85



# COMBINED MEANS
all.means <- cbind(long$personalityMeans, long$powerMeans, 
                   long$competenceMeans, long$professionalMeans)
long$allMeans <- rowMeans(all.means, na.rm=T)
psych::alpha(all.means) # Raw alpha is 0.91

corrStudyGoal<- cbind(long$studyGoal, long$allMeans)
rcorr(corrStudyGoal, type = c("spearman"))

noDat <- subset(long, studyGoal == 0)
mean(noDat$allMeans) #4.327

yesDat <- subset(long, studyGoal == 1)
mean(yesDat$allMeans) #4.302

studyGoalMod <- lmer(allMeans ~ studyGoal + (1|ID), REML = F, data = long)
summary(studyGoalMod)

corrmatrix <- cbind(long$allMeans, long$powerMeans,
                    long$competenceMeans, long$professionalMeans,
                    long$personalityMeans, long$SES,
                    long$contactLevel, long$danger)
rcorr(corrmatrix, type = c("pearson"))

corrmatrix_ECS <- cbind(long$allMeans,long$SES,
                    long$contactLevel, long$danger)
rcorr(corrmatrix_ECS, type = c("pearson"))

###########################################
# DESCRIPTIVE STATS
###########################################
# Subsetting by different language/disorder conditions
## Noun-based
noun <- subset(long, condition == 'noun') 

nounD <- subset(noun, disorder == 'D')
nounS <- subset(noun, disorder == 'S')

nounE <- subset(noun, disorder == 'E')

# Identity-first
identity <- subset(long, condition == 'identity')

identityD <- subset(identity, disorder == 'D')
identityS <- subset(identity, disorder == 'S')
identityE <- subset(identity, disorder == 'E')

## Person-first
person <- subset(long, condition == 'person')

personD <- subset(person, disorder == 'D')
personS <- subset(person, disorder == 'S')
personE <- subset(person, disorder == 'E')

# EMPLOYABILITY
## Person-first
# Depression
mean(personD$allMeans, na.rm=T)
sd(personD$allMeans, na.rm=T)
# Schizophrenia
mean(personS$allMeans, na.rm=T)
sd(personS$allMeans, na.rm=T)
# Epilepsy
mean(personE$allMeans, na.rm=T)
sd(personE$allMeans, na.rm=T)

## Identity-first
# Depression
mean(identityD$allMeans, na.rm=T)
sd(identityD$allMeans, na.rm=T)
# Schizophrenia
mean(identityS$allMeans, na.rm=T)
sd(identityS$allMeans, na.rm=T)
# Epilepsy
mean(identityE$allMeans, na.rm=T)
sd(identityE$allMeans, na.rm=T)

## Noun-based 
# Depression
mean(nounD$allMeans, na.rm=T)
sd(nounD$allMeans, na.rm=T)
# Schizophrenia
mean(nounS$allMeans, na.rm=T)
sd(nounS$allMeans, na.rm=T)
# Epilepsy
mean(nounE$allMeans, na.rm=T)
sd(nounE$allMeans, na.rm=T)

# POWER
## Person-first
# Depression
mean(personD$powerMeans, na.rm=T)
sd(personD$powerMeans, na.rm=T)
# Schizophrenia
mean(personS$powerMeans, na.rm=T)
sd(personS$powerMeans, na.rm=T)
# Epilepsy
mean(personE$powerMeans, na.rm=T)
sd(personE$powerMeans, na.rm=T)

## Identity-first
# Depression
mean(identityD$powerMeans, na.rm=T)
sd(identityD$powerMeans, na.rm=T)
# Schizophrenia
mean(identityS$powerMeans, na.rm=T)
sd(identityS$powerMeans, na.rm=T)
# Epilepsy
mean(identityE$powerMeans, na.rm=T)
sd(identityE$powerMeans, na.rm=T)

## Noun-based 
# Depression
mean(nounD$powerMeans, na.rm=T)
sd(nounD$powerMeans, na.rm=T)
# Schizophrenia
mean(nounS$powerMeans, na.rm=T)
sd(nounS$powerMeans, na.rm=T)
# Epilepsy
mean(nounE$powerMeans, na.rm=T)
sd(nounE$powerMeans, na.rm=T)

# COMPETENCE
## Person-first
# Depression
mean(personD$competenceMeans, na.rm=T)
sd(personD$competenceMeans, na.rm=T)
# Schizophrenia
mean(personS$competenceMeans, na.rm=T)
sd(personS$competenceMeans, na.rm=T)
# Epilepsy
mean(personE$competenceMeans, na.rm=T)
sd(personE$competenceMeans, na.rm=T)

## Identity-first
# Depression
mean(identityD$competenceMeans, na.rm=T)
sd(identityD$competenceMeans, na.rm=T)
# Schizophrenia
mean(identityS$competenceMeans, na.rm=T)
sd(identityS$competenceMeans, na.rm=T)
# Epilepsy
mean(identityE$competenceMeans, na.rm=T)
sd(identityE$competenceMeans, na.rm=T)

## Noun-based 
# Depression
mean(nounD$competenceMeans, na.rm=T)
sd(nounD$competenceMeans, na.rm=T)
# Schizophrenia
mean(nounS$competenceMeans, na.rm=T)
sd(nounS$competenceMeans, na.rm=T)
# Epilepsy
mean(nounE$competenceMeans, na.rm=T)
sd(nounE$competenceMeans, na.rm=T)

# PROFESSIONALISM
## Person-first
# Depression
mean(personD$professionalMeans, na.rm=T)
sd(personD$professionalMeans, na.rm=T)
# Schizophrenia
mean(personS$professionalMeans, na.rm=T)
sd(personS$professionalMeans, na.rm=T)
# Epilepsy
mean(personE$professionalMeans, na.rm=T)
sd(personE$professionalMeans, na.rm=T)

## Identity-first
# Depression
mean(identityD$professionalMeans, na.rm=T)
sd(identityD$professionalMeans, na.rm=T)
# Schizophrenia
mean(identityS$professionalMeans, na.rm=T)
sd(identityS$professionalMeans, na.rm=T)
# Epilepsy
mean(identityE$professionalMeans, na.rm=T)
sd(identityE$professionalMeans, na.rm=T)

## Noun-based 
# Depression
mean(nounD$professionalMeans, na.rm=T)
sd(nounD$professionalMeans, na.rm=T)
# Schizophrenia
mean(nounS$professionalMeans, na.rm=T)
sd(nounS$professionalMeans, na.rm=T)
# Epilepsy
mean(nounE$professionalMeans, na.rm=T)
sd(nounE$professionalMeans, na.rm=T)

# PERSONALITY
## Person-first
# Depression
mean(personD$personalityMeans, na.rm=T)
sd(personD$personalityMeans, na.rm=T)
# Schizophrenia
mean(personS$personalityMeans, na.rm=T)
sd(personS$personalityMeans, na.rm=T)
# Epilepsy
mean(personE$personalityMeans, na.rm=T)
sd(personE$personalityMeans, na.rm=T)

## Identity-first
# Depression
mean(identityD$personalityMeans, na.rm=T)
sd(identityD$personalityMeans, na.rm=T)
# Schizophrenia
mean(identityS$personalityMeans, na.rm=T)
sd(identityS$personalityMeans, na.rm=T)
# Epilepsy
mean(identityE$personalityMeans, na.rm=T)
sd(identityE$personalityMeans, na.rm=T)

## Noun-based 
# Depression
mean(nounD$personalityMeans, na.rm=T)
sd(nounD$personalityMeans, na.rm=T)
# Schizophrenia
mean(nounS$personalityMeans, na.rm=T)
sd(nounS$personalityMeans, na.rm=T)
# Epilepsy
mean(nounE$personalityMeans, na.rm=T)
sd(nounE$Means, na.rm=T)

# Subsetting by disorder conditions 
depression <- subset(long, disorder == "D")
schizophrenia <- subset(long, disorder == "S")
epilepsy <- subset(long, disorder == "E")

## Employability
mean(depression$allMeans, na.rm=T)
sd(depression$allMeans, na.rm=T)

mean(schizophrenia$allMeans, na.rm=T)
sd(schizophrenia$allMeans, na.rm=T)

mean(epilepsy$allMeans, na.rm=T)
sd(epilepsy$allMeans, na.rm=T)

## Power
mean(depression$powerMeans, na.rm=T)
sd(depression$powerMeans, na.rm=T)

mean(schizophrenia$powerMeans, na.rm=T)
sd(schizophrenia$powerMeans, na.rm=T)

mean(epilepsy$powerMeans, na.rm=T)
sd(epilepsy$powerMeans, na.rm=T)

## Competence
mean(depression$competenceMeans, na.rm=T)
sd(depression$competenceMeans, na.rm=T)

mean(schizophrenia$competenceMeans, na.rm=T)
sd(schizophrenia$competenceMeans, na.rm=T)

mean(epilepsy$competenceMeans, na.rm=T)
sd(epilepsy$competenceMeans, na.rm=T)

## Professionalism
mean(depression$professionalMeans, na.rm=T)
sd(depression$competenceMeans, na.rm=T)

mean(schizophrenia$professionalMeans, na.rm=T)
sd(schizophrenia$professionalMeans, na.rm=T)

mean(epilepsy$professionalMeans, na.rm=T)
sd(epilepsy$professionalMeans, na.rm=T)

## Personality
mean(depression$personalityMeans, na.rm=T)
sd(depression$personalityMeans, na.rm=T)

mean(schizophrenia$personalityMeans, na.rm=T)
sd(schizophrenia$personalityMeans, na.rm=T)

mean(epilepsy$personalityMeans, na.rm=T)
sd(epilepsy$personalityMeans, na.rm=T)


###########################################
# MODEL 1
###########################################
# Releveling condition variable 
levels(long$condition)
long$condition <- as.factor(long$condition)
long$condition <- relevel(long$condition, "person")

# MODEL 1
# Releveling disorder variable
levels(long$disorder)
long$disorder <- as.factor(long$disorder)
long$disorder <- relevel(long$disorder, "E")
View(long)
library(lme4)
library(lmerTest)
employMod1 <- lmer(allMeans ~ disorder*condition*scale(contactLevel)*scale(SES)*scale(danger) + (1|ID), REML = F, data = long)
summary(employMod1)


long$disorder
is.factor(long$disorder)
levels(long$disorder) <- c("Epilepsy", "Depression", "Schizophrenia")

long$condition
levels(long$condition) <-c("Person-first", "Identity-first", "Noun-based")
library(ggplot2)
bp1 <- ggplot(long, aes(x=condition, y=allMeans, fill=disorder)) + 
   geom_boxplot() + labs(x="Language Condition", y = "Employability", fill="Disorder")
bp1 + scale_fill_brewer(palette="Blues") + theme_minimal()


###########################################
# MODEL 2
###########################################
# Subsetting by disorder
midata <- subset(long, disorder != "Epilepsy")


# Releveling disorder variable
is.factor(midata$disorder)
midata$disorder <- relevel(midata$disorder, "D")
employMod2 <- lmer(allMeans ~ disorder*condition*scale(contactLevel)*scale(SES)*scale(danger) + (1|ID), REML = F, data = midata)

modelSummary<- summary(employMod2)
modelSummary$residuals

###########################################
# INTERACTION EFFECTS
###########################################

# INTERACTION EFFECT #1
## EMPLOYABILITY ~ DANGEROUSNESS X  LANGUAGE

interact_plot(employMod2, danger, modx = condition,
              x.label = "Perceived dangerousness",
              y.label = "Employability")

# INTERACTION EFFECT #2
## EMPLOYABILITY ~ LEVEL OF CONTACT X SES

interact_plot(employMod2, contactLevel, modx = SES,
              x.label = "Level of contact",
              y.label = "Employability")

# INTERACTION EFFECT #3
## EMPLOYABILITY ~ LEVEL OF CONTACT X LANGUAGE

interact_plot(employMod2, contactLevel, modx = condition,
              x.label = "Level of contact",
              y.label = "Employability")













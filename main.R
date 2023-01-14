library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

df <- read_excel("DEMENTED_ClinDia2.xlsx")

mean.age = mean(df$age.at.visit)
sd.age = sd(df$age.at.visit)

print(paste("MEAN AGE: ", mean.age))
print(paste("SD AGE: ", sd.age))

df$DementedLetter <- rep(0, nrow(df))

num.CN <- 0
num.MCI <- 0
num.DMN <- 0

for(i in 1:nrow(df)){
  if(df$DEMENTED[i] == 3){
    df$DementedLetter[i] <- "CN"
    num.CN <- num.CN + 1
  }
  if(df$DEMENTED[i] == 0){
    df$DementedLetter[i] <- "MCI"
    num.MCI <- num.MCI + 1
  }
  if(df$DEMENTED[i] == 1){
    df$DementedLetter[i] <- "DMN"
    num.DMN <- num.DMN + 1
  }
}

print(paste(num.CN, " are CN"))
print(paste(num.MCI, " are MCI"))
print(paste(num.DMN, " are DMN"))

ggplot(df, aes(x=DementedLetter, y=age.at.visit, fill=DementedLetter)) +
  geom_boxplot(aes(color = DementedLetter)) +
  geom_jitter(aes(color = DementedLetter), width = 0.5, size = 0.5, alpha = 0.8) +
  labs(title = "Title idk") +
  theme(plot.title = element_text(hjust = 0.5))
  
num.patients <- 1
cur.id <- df$OASISID[1]

for(i in 1:nrow(df)){
  if(df$OASISID[i] != cur.id){
    cur.id = df$OASISID[i]
    num.patients <- num.patients + 1
  }
}

print(paste("There are ", num.patients, " patients"))

#data frame to compare
CN <- c("CN", "MCI", "DMN")
MCI <- c("MCI", "DMN", "")
DMN <- c("DMN", "", "")

valid <- data.frame(CN, MCI, DMN)
#-----------------------------------
#sorting
cur.id <- df$OASISID[1]
lo <- 1

for(i in 1:nrow(df)){
  if(df$OASISID[i] != cur.id){
    hi <- i - 1
    df[lo:hi, ] <- df[lo:hi,][order(df[lo:hi,"age.at.visit"]),]
    cur.id = df$OASISID[i]
    lo = i
  }
}

#---------------------------------
#fixing progression

cur.id <- df$OASISID[1]
greatest.demented.letter <- df$DementedLetter[1]

for(i in 1:nrow(df)){
  id <- df$OASISID[i]
  if(id != cur.id){
    greatest.demented.letter <- df$DementedLetter[i]
    cur.id <- id
  }
  if(df$DementedLetter[i] %in% get(greatest.demented.letter, valid)) { 
    greatest.demented.letter <- df$DementedLetter[i]
  }
  else{
    df$DementedLetter[i] <- greatest.demented.letter
  }
}
view(df)
#------------------------------------------
#dataframe that calculates duration of each stage

duration <- data.frame(matrix(ncol = 5, nrow = num.patients))
colnames(duration) <- c('OASISID', 'age.at.visit', 'CN', 'MCI', 'DMN')

cur.id <- df$OASISID[1]
cur.letter <- df$DementedLetter[1]
original.age <- df$age.at.visit[1]

cur.row <- 1

duration$OASISID[cur.row] <- cur.id
duration$age.at.visit[cur.row] <- original.age

for(i in 1:nrow(df)){
  id <- df$OASISID[i]
  age <- df$age.at.visit[i]
  letter <- df$DementedLetter[i]
  years <- df$years_to_Visit[i]
  if(letter != cur.letter | id != cur.id){
    if(cur.letter == 'CN'){
      duration$CN[cur.row] <- round(df$age.at.visit[i - 1] - original.age + 1, 0)
    }
    if(cur.letter == 'MCI'){
      duration$MCI[cur.row] <- round(df$age.at.visit[i - 1] - original.age + 1, 0)
    }
    if(cur.letter == 'DMN'){
      duration$DMN[cur.row] <- round(df$age.at.visit[i - 1] - original.age + 1, 0)
    }
    original.age <- age
    cur.letter <- letter
  }
  if(id != cur.id){
    cur.row <- cur.row + 1
    cur.id <- id
    duration$OASISID[cur.row] <- cur.id
    duration$age.at.visit[cur.row] <- age
  }
}

view(duration)
#---------------------------------------------
#final summary
summary <- data.frame(matrix(ncol = 7, nrow = 5))
colnames(summary) <- c('progression', 'patients', 'percentage', 'average duration', 'duration sd', 'average age', 'age sd')
summary$progression[1] <- 'CN-DMN'
summary$progression[2] <- 'CN-MCI'
summary$progression[3] <- 'MCI-DMN'
summary$progression[4] <- 'CN-MCI-DMN'
summary$progression[5] <- 'Total'

for(i in 1:nrow(duration)){
  if(!is.na(duration$CN[i]) & is.na(duration$MCI[i]) & !is.na(duration$DMN[i])){
    if(is.na(summary$patients[1])){
      summary$patients[1] <- 0
    }
    summary$patients[1] <- summary$patients[1] + 1
  }
  if(!is.na(duration$CN[i]) & !is.na(duration$MCI[i]) & is.na(duration$DMN[i])){
    if(is.na(summary$patients[2])){
      summary$patients[2] <- 0
    }
    summary$patients[2] <- summary$patients[2] + 1
  }
  if(is.na(duration$CN[i]) & !is.na(duration$MCI[i]) & !is.na(duration$DMN[i])){
    if(is.na(summary$patients[3])){
      summary$patients[3] <- 0
    }
    summary$patients[3] <- summary$patients[3] + 1
  }
  if(!is.na(duration$CN[i]) & !is.na(duration$MCI[i]) & !is.na(duration$DMN[i])){
    if(is.na(summary$patients[4])){
      summary$patients[4] <- 0
    }
    summary$patients[4] <- summary$patients[4] + 1
  }
}
summary$patients[5] <- summary$patients[1] + summary$patients[2] + summary$patients[3] + summary$patients[4]
summary$percentage[1] <- round(summary$patients[1]/summary$patients[5] * 100, 1)
summary$percentage[2] <- round(summary$patients[2]/summary$patients[5] * 100, 1)
summary$percentage[3] <- round(summary$patients[3]/summary$patients[5] * 100, 1)
summary$percentage[4] <- round(summary$patients[4]/summary$patients[5] * 100, 1)
view(summary)



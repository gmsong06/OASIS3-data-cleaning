library(tidyverse)
library(readxl)
df <- read_excel("patient7.xlsx")

# beginningDay = df[1, 3]
# curDemented <- df[1, 5]
# for (i in 1:nrow(df)) {
#   row <- df[i, ]
#   print(row)
#   days <- row[3]
#   demented <- row[5]
# 
#   if(demented != curDemented){
#       print(paste("DAYS SINCE CHANGE IN CONDITION: ", days - beginningDay))
#       beginningDay = days
#       curDemented = demented
#   }
# 
#   print("NEXT")
# }

greatestDemented <- df[1, 7]
greatestDementedLetter <- df[1, 6]
for(i in 1:nrow(df)){
  if(greatestDemented <= df[i, 7]) {
    greatestDemented <- df[i, 7]
    greatestDementedLetter <- df[i, 6]
  }
  else{
    df[i, 7] <- greatestDemented
    df[i, 6] <- greatestDementedLetter
  }
}

print(df)

#  Spring Board   DPLYR Exercise - Refine excel 
#install.packages("random")
library(dplyr)
library(tidyr)
library(random)


# load the refine data into dataframe 
titanic <-  read.csv(file="C:\\Users\\Dhathri\\Desktop\\R\\titanic_original.csv", header=TRUE, sep=",")

names(titanic)

# set the blank/empty strings in titanic$embarked to S 

titanic$embarked[titanic$embarked ==  ""] <- "S"
titanic$embarked[titanic$embarked ==  "  "] <- "S"
titanic$embarked

# set the missing age to mean of all ages 

titanic$age[is.na(titanic$age) ] <- mean(titanic$age,na.rm = TRUE)

titanic$age

#  SET THE VALUE OF LIFEBOAT TO NA 

titanic$boat[titanic$boat == ""]  <- 'NA'
titanic$boat

#CABIN 



titanic$has_cabin_number[titanic$cabin == ""]   <-  0
titanic$has_cabin_number[titanic$cabin != "" ]   <-  1

titanic$has_cabin_number


write.csv(titanic, file = "C:\\Users\\Dhathri\\Desktop\\R\\titanic_clean.csv")
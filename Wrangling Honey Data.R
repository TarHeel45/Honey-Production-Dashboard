# This script is meant to clean the additional data that will be appended to the original dataset 

# Load Libraries -------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(plyr)
library(formattable)
detach(package::plyr)

# Load Data ----------------------------------------------------------------------------
# There are four datasets that provide the same information about the honey bees. This information is identical in nature to the existent datasets 

honeyraw2013 <- read_lines("/Users/John/Documents/Honey Production Dash Shiny/Honey Data/Hone-03-20-2015/hony_p02_t002.csv")
honeyraw2013 <- data.frame(honeyraw2013[-1:-9])

honeyraw2014 <- read_lines("/Users/John/Documents/Honey Production Dash Shiny/Honey Data/Hone-03-22-2016/hony_p02_t002.csv")
honeyraw2014 <- data.frame(honeyraw2014[-1:-9])

honeyraw2015 <- read_lines("/Users/John/Documents/Honey Production Dash Shiny/Honey Data/Hone-03-22-2017/hony_p03_t002.csv")
honeyraw2015 <- data.frame(honeyraw2015[-1:-9])

honeyraw2016 <- read_lines("/Users/John/Documents/Honey Production Dash Shiny/Honey Data/Hone-03-14-2018/hony_p02_t019.csv")
honeyraw2016 <- data.frame(honeyraw2016[-1:-9])

#Clean data from 2013 -------------------------------------------------------------------
View(honeyraw2013)
honeyraw2013 <- honeyraw2013[1:46,] #select only data of states, other states, and US
honeyraw2013 <- honeyraw2013[-c(11,22,33,43,45)] #remove empty rows 
honeyraw2013 <- as.data.frame(honeyraw2013) #convert to dataframe
honeyraw2013$honeyraw2013 <- as.character(honeyraw2013$honeyraw2013) #convert rows to character strings

honeyClean13 <- separate(honeyraw2013, 1, paste0("X",1:9), sep = ",") #separate rows by comma, create generic column names 
honeyClean13 <- honeyClean13 %>%
  setNames(c("X1", "X2", "state", "numcol", "yieldpercol","totalprod",
             "stocks","priceperlb","prodvalue")) %>%
  select(-X1, -X2)

honeyClean13$state <- gsub("\"", "", honeyClean13$state) #clean state names
honeyClean13$state[40:41] <- c("Other States", "United States") 
honeyClean13$numcol <- as.integer(honeyClean13$numcol)*1000 #convert to total actual units
honeyClean13$yieldpercol <- as.integer(honeyClean13$yieldpercol) 
honeyClean13$totalprod <- as.integer(honeyClean13$totalprod)*1000 
honeyClean13$stocks <- as.integer(honeyClean13$stocks)*1000
honeyClean13$priceperlb <- as.numeric(honeyClean13$priceperlb)/100 #dollars and cents
honeyClean13$prodvalue <- as.numeric(honeyClean13$prodvalue)*1000
honeyClean13$year <- 2013 #add year column 


#Clean data from 2014 for input into cleaning function --------------------------------

View(honeyraw2014) #to get indexes of rows that need deleting/editing 
honeyraw2014 <- honeyraw2014[1:47,] 
honeyraw2014 <- honeyraw2014[-c(11,22,33,44,46)]
honeyraw2014 <- as.data.frame(honeyraw2014)
honeyraw2014$honeyraw2014 <- as.character(honeyraw2014$honeyraw2014)




#Generalize cleaning process into function ---------------------------------------------
# see comments on 2013 data for how this function works 
CleanHoneyData <- function(df, year) {
  dfClean <- separate(df, 1, paste0("X",1:9), sep=",") 
  dfClean <- dfClean %>%
    setNames(c("X1", "X2", "state", "numcol", "yieldpercol","totalprod",
               "stocks","priceperlb","prodvalue")) %>%
    select(-X1, -X2)
  USIndex <- nrow(df)
  OSIndex <- nrow(df) - 1
  
  dfClean$state <- gsub("\"", "", dfClean$state)
  dfClean$state[OSIndex:USIndex] <- c("Other States", "United States")
  dfClean$numcol <- as.integer(dfClean$numcol)*1000
  dfClean$yieldpercol <- as.integer(dfClean$yieldpercol)
  dfClean$totalprod <- as.integer(dfClean$totalprod)*1000
  dfClean$stocks <- as.integer(dfClean$stocks)*1000
  dfClean$priceperlb <- as.numeric(dfClean$priceperlb)/100
  dfClean$prodvalue <- as.numeric(dfClean$prodvalue)*1000
  dfClean$year <- year
  return(dfClean)
}

honeyClean14 <- CleanHoneyData(honeyraw2014, 2014)


# Clean data from 2015 for input into cleaning function ---------------------------------

View(honeyraw2015) #to get indexes of rows that need deleting/editing 
honeyraw2015 <- honeyraw2015[1:47,]
honeyraw2015 <- honeyraw2015[-c(11,22,33,44,46)]
honeyraw2015 <- as.data.frame(honeyraw2015)
honeyraw2015$honeyraw2015 <- as.character(honeyraw2015$honeyraw2015)

# Enter 2015 data into cleaning function

honeyClean15 <- CleanHoneyData(honeyraw2015, 2015)


# Clean data from 2016 for input into cleaning function ---------------------------------

View(honeyraw2016)
honeyraw2016 <- honeyraw2016[1:47,]
honeyraw2016 <- honeyraw2016[-c(11,22,33,44,46)]
honeyraw2016 <- as.data.frame(honeyraw2016)
honeyraw2016$honeyraw2016 <- as.character(honeyraw2016$honeyraw2016)

# Enter 2016 data into cleaning function

honeyClean16 <- CleanHoneyData(honeyraw2016, 2016)
honeyClean16


# Extract only state data from cleaned data in order to append easily to already cleaned data
stateHoneyClean13 <- honeyClean13[1:39,]
stateHoneyClean14 <- honeyClean14[1:40,]
stateHoneyClean15 <- honeyClean15[1:40,]
stateHoneyClean16 <- honeyClean16[1:40,]


honeyClean9812 <- read_csv(file = "./Data/honeyproduction.csv") #read in Kaggle dataset

stateMatch <- data.frame(state.abb, state.name) #create dataframe of state abbreviations and full names
colnames(stateMatch) <- c("state", "stateFull") #change names

honeyClean9812$state <- as.factor(honeyClean9812$state) #change from character to factor

honeyClean9812 <- honeyClean9812 %>%
  left_join(stateMatch, by = "state") #match each entry's abbreviation with full state name


honeyClean9812 <- honeyClean9812[,c(9,2:8)] #reorder columns 
colnames(honeyClean9812)[1] <- c("state") #match state column name to allow for binding

honeyClean <- rbind(honeyClean9812, stateHoneyClean13, stateHoneyClean14, stateHoneyClean15, stateHoneyClean16) #append new data to Kaggle datset


write.csv(honeyClean, file = "HoneyProduction9816.csv") #write new, added data to csv
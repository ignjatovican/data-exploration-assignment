#Anamaria Ignjatovic 
#Data Exploration Assignment 

#The College Scorecard was released at the start of September 2015. Among colleges that predominantly grant bachelor’s degrees,
#did the release of the Scorecard shift student interest to high-earnings colleges relative to low-earnings ones 
#(as proxied by Google searches for keywords associated with those colleges)?


#Data Cleaning

#Import Packages
library(dplyr)
library(tidyverse)
library(jtools)
library(vtable)
library(purrr)
library(ggplot2)

#Load Data
UT <- read.csv("trends_up_to_UT.csv")
UM <- read.csv("trends_up_to_UM.csv")
UPhoenix <- read.csv("trends_up_to_UPhoenix.csv")
UTMB <- read.csv("trends_up_to_UTMB.csv")
Yorktowne <- read.csv("trends_up_to_Yorktowne.csv")
inter1 <- read.csv("trends_up_to_inter_1.csv")
inter2 <- read.csv("trends_up_to_inter_2.csv")
inter3 <- read.csv("trends_up_to_inter_3.csv")
inter4 <- read.csv("trends_up_to_inter_4.csv")
inter5 <- read.csv("trends_up_to_inter_5.csv")
inter6 <- read.csv("trends_up_to_inter_6.csv")
Finish <- read.csv("trends_up_to_finish.csv")

Cohorts <- read.csv("Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv") %>%
  select(OPEID, UNITID, md_earn_wne_p10.REPORTED.EARNINGS, PREDDEG) %>% filter(PREDDEG == 3) %>%
  rename_with(tolower)

Id_Name_Link <- read.csv("Lab3_Rawdata/id_name_link.csv")


#Combine Data Sets
Trends <- list.files(path = "Lab3_Rawdata", pattern = "trends_up_to_", full.names = TRUE) %>% 
  map(read_csv) %>%
  bind_rows
str(Trends)

#Merge school earnings to google searches
head(Id_Name_Link); head(Cohorts); head(Trends)
Alldat <- Trends %>%
  left_join(y = Id_Name_Link, 
            by = 'schname') %>%
  left_join(y = Cohorts,
            by = c('opeid', 'unitid'))

#Are keyword and keynum 1:1
Alldat %>% 
  select(keyword, keynum) %>%
  unique #no
#keyword for schools have various keynum, though individual to each school


#Make earnings numeric
str(Alldat)
Alldat <- Alldat %>% mutate(earnings = as.numeric(md_earn_wne_p10.reported.earnings))


#Split monthorweek into start + end columns
Alldat <- Alldat %>% separate(
  col = monthorweek,
  into = c('startdate', 'enddate'),
  sep = ' - ',
  remove = TRUE,
  extra = "warn",
  fill = "warn"
)

#Format as dates
Alldat <- Alldat %>% mutate(startdate = as.Date(x = startdate, format = "%F")) %>% mutate(enddate = as.Date(x = enddate, format = "%F"))


#Standardize the Google Trends Data
Alldat <- Alldat %>%
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index,na.rm = TRUE))/sd(index, na.rm = TRUE)) 
  
#Drop NA values
Alldat <- Alldat %>% drop_na()

#Look at distribution of reported earnings
ggplot(data = Alldat, mapping = aes(x = earnings )) +
  geom_histogram()

#Convert clean data to .csv to use in analysis
write.csv(Alldat, "AlldatCleanData.csv")




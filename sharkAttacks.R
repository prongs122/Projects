#An Analysis of Shark Attacks Data for the period 1543 - 2016.

setwd("~/Documents/Python")

library(dplyr)
library(ggplot2)
library(readr)

sharkAttacks <- read.csv("attacks.csv",stringsAsFactors = T)
head(sharkAttacks)
summary(sharkAttacks)

#Variables need to be assigned correct atomic vector types.
#The Date variabe contains some anomalous entries (e.g. the year 2068!). The Year variable seems to be mostly accurate apart from very early years, however,
#and will likely be more important for this analysis. Variables deemed unimportant will also be removed (these are: Case Number,
#Date, Name, Time, Investigator and Injury).

sharkAttacks$Date <- as.Date(sharkAttacks$Date,"%d-%b-%y")
class(sharkAttacks$Date)
sharkAttacks$Age <- as.numeric(sharkAttacks$Age)
reasons <- sharkAttacks%>%select(Activity)%>%group_by(Activity)%>%summarise(Total = n())

sharkAttacksTidy <- sharkAttacks%>%select(3:8,10,11,13,15)
-------------------------------------------------------------------------------------------------------------------------------------
#The summary statistics reveal some interesting information. 

## The USA has experienced the highest number of shark attacks (2116), followed by Australia (1279).
## Over the period examined, average attacks per year are 4.5 and 2.7, respectively.

## In the USA, Florida is the state which experiences the highest number of shark attacks (990). In Australia, it is New South
## Wales (468) and Queensland (300). Hawaii (282) and California (276) are also attack-prone states.

## The most attack-prone location is New Smyrna Beach, Volusia County. Daytona Beach and Ponce Inlet experience attacks on
## a smaller scale.

## The activity most greatly associated with shark attacks is surfing (904) followed closely by swimming (819).

## The vast majority of attack subjects have been male (4829). Female (585).

## There have been 1539 fatalities in total from shark attacks. Between 1543 and 2016, there have been 1481. This gives an
## average of 3.1 deaths per year globally.

## The species of shark with the most attacks is the White Shark (157). There is, however, a lot of missing data for this
## variable (2961 NAs).
-------------------------------------------------------------------------------------------------------------------------------------

#A number of interesting questions can be put forward:
  
  #1 What is the age distribution of attack subjects?
  #2 In which year were attacks highest? Is this related to better data recording?
  #3 Where are the shark attack "hotspots"?
  #4 What is the probability of suffering a fatal shark attack in one of the "hotspots"?
  #5 Has shark attack incidence been increasing as Homo sapien population increases?
  

##(1)##  

#A summary of the Age variable suggests that there are some data quality issues with this variable. A maximum age of 152 is recorded.
#When looking at the age distribution, then, we should remove observations where the age is greater than say, 100. The rest of
#the data associated with these observations may or may not be accurate. We can investigate later.

hundredOrless <- sharkAttacksTidy%>%filter(Age <= 100)
summary(hundredOrless$Age)

#From the histogram, the distribution isn't particularly normal. It has a positive skew which inflates the mean.
#75% of the subjects are 63 years of age or below.
  
ggplot(na.omit(hundredOrless),aes(x=Age)) +
  geom_histogram(fill="lightblue2",colour="black") 
  
#The qqnorm plot and Shapiro-Wilk test confirm that the distribution is not normal.
 
shapiro.test(hundredOrless$Age)
qqnorm(hundredOrless$Age);qqline(hundredOrless$Age)


##(2)##(5)##

summary(sharkAttacksTidy$Year)

#Remove observations for which the year is before 1543 and get total attacks for each year.

sharkAttacksTidy%>%select(Year)%>%filter(Year >= 1543)%>%group_by(Year)%>%summarise(Total = n())%>%arrange(desc(Total))

#To answer our question, 2015 saw the highest number of shark attacks. We can create a new dataframe to look at any
#trend in attacks over time. 

#This code gives us the dataframe to work with.

trend_df <- sharkAttacksTidy%>%
            select(Year)%>%
            filter(Year >= 1543)%>%
            group_by(Year)%>%
            summarise(Total = n())


ggplot(data = trend_df,aes(x = Year,y = Total)) +
  geom_point(alpha = 1/2) +
  scale_x_continuous(breaks = seq(1500,2020,50)) +
  scale_y_continuous(breaks = seq(0,150,20)) +
  ylab("No. of shark attacks") 



#The shape of the data appears to be exponential. There are likely to be multiple factors causing this pattern. 
#Better data recording/reporting and an exponential increase in human population are likely to heavily influence the number of recorded attacks.
#This plot looks alarming but it shouldn't be an impetus for a shark culling mission. 

#Maybe a more logical examinantion with regards to data accuracy would be to examine data from 1950 onwards when recording may 
#have been carried out more effectively. The trend is less "shocking" then as we see 1960 had about as many attacks as 2016.

ggplot(data = trend_df,aes(x = Year,y = Total)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1500,2020,10)) +
  scale_y_continuous(breaks = seq(0,150,20)) +
  ylab("No. of shark attacks") +
  coord_cartesian(xlim = c(1950,2016))


#How many fatalities occurred in each year? First, filter the tidy dataframe to include just entries for Fatal of Y and N. We also
#filter for years equal to or above 1543. Then, convert Fatal to character atomic type. We need to hack about to get the 
#variable in order. Then convert to factor.


fatal_df <- sharkAttacksTidy%>%filter(Fatal%in%c("Y","N"),Year >= 1543) 

fatal_df$Fatal <- as.character(fatal_df$Fatal)
fatal_df$Fatal <- as.factor(fatal_df$Fatal)

#Create a dataframe so we can see fatal and non-fatal counts by year.

fatal_df2 <- fatal_df%>%select(Year,Fatal)%>%group_by(Year,Fatal)%>%summarise(Total = n())


fatal_df2%>%filter(Fatal == "Y")


--------------------------------

year <- c(2016,2015,2014,2013,2012,2011,2010)  
attacks <- c(124,211,22,11,44,55,66)  
fatalY <- c(0,2,3,0,0,1,1)
fatalN <- c(1,2,2,1,0,5,1)

df <- data.frame(year,attacks,fatalY,fatalN)



--------------------------------
## Total fatalities vs non-fatalities.

fatal_df$Fatal <- as.integer(fatal_df$Fatal)


fatal_df%>%
  select(Year,Fatal)%>%
  filter(Year >= 1543)%>%
  group_by(Year,Fatal)

ggplot(fatal_df,aes(x=Year,y=Total)) +
  geom_point(alpha=1/2) +
  ylab("Total fatalities") +
  scale_x_continuous(breaks = seq(1500,2020,50))
  

fatal_df%>%group_by(Year)%>%summarise(Fatals = )







##3##

#Convert to numeric and back to get 2 - level factor.

classification_df <- sharkAttacksTidy%>%filter(Fatal%in%c("Y","N"))
classification_df$Fatal <- as.character(classification_df$Fatal)
classification_df$Fatal <- as.factor(classification_df$Fatal)



##4##

#For the top 6 shark attack locations, what percentage of attacks are fatal?

locations <- classification_df%>%
              select(Location)%>%
              group_by(Location)%>%
              summarise(Total = n())%>%
              arrange(desc(Total))

head(na.omit(locations))

top6fatals <- classification_df%>%
              select(Location,Fatal)%>%
              filter(Fatal == "Y")%>%
              group_by(Location)%>%
              summarise(Total = n())%>%
              arrange(desc(Total))

head(na.omit(top6fatals))

#The top 6 locations for attacks are not the top 6 locations where fatal attacks have occurred.


#Get fatality values for the top 6:

hotSpots <- classification_df%>%
            select(Location,Fatal)%>%
            filter(Location%in%c("New Smyrna Beach, Volusia County","Daytona Beach, Volusia County","Myrtle Beach, Horry County",
                       "Ponce Inlet, Volusia County","Melbourne Beach, Brevard County","Isle of Palms, Charleston County"))%>%
            group_by(Location)%>%
            summarise(Total = n())

#There have been no fatalities in the top 6 shark attack locations.
  
#Attacks by country. Get total attacks for each country and write to .csv file for external ISO3 modification.

library(rworldmap)
glimpse(countryExData)

mapAttacks <- sharkAttacks%>%select(Country)%>%group_by(Country)%>%summarise(Total = n())%>%arrange(desc(Total))
write.csv(file = "mapAttacks.csv",mapAttacks)

#Once the dataframe is ready, we can plot the values onto a world map using bubbles to represent the attacks.
#Create a spatial polygon dataframe.

sPDF <- joinCountryData2Map(mapAttacks,joinCode = "NAME",nameJoinColumn = "Country")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(sPDF,nameColumnToPlot = "Total")
mapBubbles(dF = sPDF,nameZSize = "Total",oceanCol = "lightblue2",landCol = "wheat",addLegend = T,legendTitle = "No. of Attacks",
           maxZVal = 2116,nameZColour = adjustcolor("firebrick4"),legendPos = "bottomleft")


             











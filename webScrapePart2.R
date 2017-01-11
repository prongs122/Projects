## PART 1 (A) ##


#Scrape the presidential election polling data from Real Clear Politics. Load up required packages and read in the URL.
#Required packages.

library(XML)
library(RCurl)
library(dplyr)
library(tidyr)

rcp <- getURL("http://www.realclearpolitics.com/epolls/2016/president/us/general_election_trump_vs_clinton_vs_johnson_vs_stein-5952.html")

#Read in HTML tables from the 4-Way race webpage.

tbl <- readHTMLTable(rcp,stringsAsFactors=F)
glimpse(tbl)

#There are 4 tables (Table 3 is NULL). We will focus on Table 4.

tbl1 <- tbl[[1]]
tbl2 <- tbl[[2]]
tbl4 <- tbl[[4]]

#There are 28 unique pollsters in this dataset (27 once the ageRCP Average is removed).

tbl4%>%
  select(Poll)%>%
  n_distinct()

#Take the 11 most recent pollsters. Remove the RCP average row, duplicate pollsters and any unnecessary columns from the Table 4 dataframe. To replicate the RCP time series graph,
#we need just the date and polling percentages for each canditate. The pollster is included for reference.

#Note that as the RCP polling table is dynamic, this code will filter different entries each time it is used. Duplicates may then be present.

tidy_df <- tbl4%>%
  filter(Poll != "RCP Average")%>%
  filter(row_number()%in%c(2:7,11,12,14,15,16))%>%
  select(1,2,5,6,7,8)

#Next, we need to split the date variable into two new variables: "Start" and "End", and convert them to class date. In addition, 
#the values for the candidates need to be converted to numeric vector type.

tidy_df <- separate(tidy_df,Date,into=c("Start","Finish"),sep="-",extra="merge")
tidy_df$Finish <- as.Date(tidy_df$Finish,"%m/%d")
tidy_df$Start <- as.Date(tidy_df$Start,"%m/%d")

tidy_df$`Clinton (D)` <- as.numeric(tidy_df$`Clinton (D)`)
tidy_df$`Trump (R)` <- as.numeric(tidy_df$`Trump (R)`)
tidy_df$`Johnson (L)` <- as.numeric(tidy_df$`Johnson (L)`)
tidy_df$`Stein (G)` <- as.numeric(tidy_df$`Stein (G)`)

#Write the script to a .text file.

write.table(tidy_df,file="tidy_df.txt",quote=FALSE,sep=",",row.names = FALSE)

#Write the script to a .csv file.

write.csv(tidy_df,file = "tidy_df.csv",row.names=F)

# PART 1 (B) ##

#To take the averages, we need to use the full dataset in Table 4. Split the dates and convert to date type as required.
#Convert character strings to numeric vectors and add extra variable, "Accuracy", which is the inverse of MoE. Also, remove the
#RCP Average row and variables Sample and Spread.

tbl4 <-tbl4 <- separate(tbl4,Date,into=c("Start","Finish"),sep="-",extra="merge")
tbl4$Finish <- as.Date(tbl4$Finish,"%m/%d")
tbl4$Start <- as.Date(tbl4$Start,"%m/%d")

tbl4$`Clinton (D)` <- as.numeric(tbl4$`Clinton (D)`)
tbl4$`Trump (R)` <- as.numeric(tbl4$`Trump (R)`)
tbl4$`Johnson (L)` <- as.numeric(tbl4$`Johnson (L)`)
tbl4$`Stein (G)` <- as.numeric(tbl4$`Stein (G)`)
tbl4$MoE <- as.numeric(tbl4$MoE)

#Tidied version of polling dataset ready for analysis.

tbl4 <- tbl4%>%mutate(Accuracy=1/MoE)%>%filter(Poll!="RCP Average")%>%select(-4,-10)

#Save to a .csv file in case RCP website clears data after Tuesday's POTUS election.

write.csv(tbl4,file="tidyRCP.csv")



## PART 2 ##


#As a backup, the tidy RCP dataset created in PART 1 (B) has been saved to the working directory below along with 
#an RCP .csv file provided by Adrian O Connor which contains daily averages based on recent polls.

setwd("~/Documents/Portfolio")

library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(dplyr)

#Using Adrian's dataset.

pollsDF <- read.csv("RCP.csv") 

names(pollsDF) <- c("Date","Clinton(D)","Trump(R)","Johnson(L)","Stein(G)")
pollsDF$Date <- as.Date(pollsDF$Date,"%d/%m/%Y")

#Some reshaping of the dataframe is required to build a line plot containing all canditates.

pollsDF_long <- melt(pollsDF,id="Date")  # convert to long format

#Reproduce RCP time series graph.

ggplot(data=pollsDF_long,aes(x=Date, y=value, colour=variable)) + 
  geom_line(size=1) +
  theme_stata() +
  scale_colour_manual(values = c("blue","red","yellow","green")) +
  scale_y_continuous(breaks=seq(0,45,5)) +
  ylab("") +
  xlab("") +
  guides(colour=guide_legend(NULL)) +
  ggtitle("Trump vs. Clinton vs. Johnson vs. Stein") +
  theme(plot.title = element_text(size = 15, face = "bold"))


#Using my tidied dataset from PART 1 (B). I use the end date of polls as the date variable and remove some variables unused in
#the line graph. Some reshaping is then required before plotting the graph. This graph contains absolute values for the end
#dates of polls. It captures every bit of variation in the data. The lines can be smoothed using polynomial functions or by
#averaging as per Adrian's dataset.

tbl4_LinePlot <- tbl4%>%select(3,5:8)
tbl4_LinePlot<- melt(tbl4_LinePlot,id="Finish")  # convert to long format


ggplot(tbl4_LinePlot, aes(x = Finish, y = value, colour = variable)) + 
  geom_smooth(fill = NA, span = 0.15) +
  theme_stata() +
  scale_colour_manual(values = c("blue", "red", "yellow", "green")) +
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  ylab("") +
  xlab("") +
  guides(colour = guide_legend(NULL)) +
  ggtitle("Trump vs. Clinton vs. Johnson vs. Stein") +
  theme(plot.title = element_text(size = 15, face = "bold"))


#Extra visualisations just for fun!

p1 <-ggplot(tbl4,aes(x=Finish,y=tbl4$`Trump (R)`,size=Accuracy)) +
  geom_point(na.rm=T,alpha=1/2,colour="red") +
  xlab("") +
  ylab("Percentage Votes") +
  ggtitle("% Votes for Trump and Poll Accuracy")

p2 <-ggplot(tbl4,aes(x=Finish,y=tbl4$`Clinton (D)`,size=Accuracy)) +
  geom_point(na.rm=T,alpha=1/2,colour="blue") +
  xlab("") +
  ylab("Percentage Votes") +
  ggtitle("% Votes for Clinton and Poll Accuracy")

p3 <- ggplot(tbl4,aes(x=Finish,y=tbl4$`Johnson (L)`,size=Accuracy)) +
  geom_point(na.rm=T,alpha=1/2,colour="yellow") +
  xlab("") +
  ylab("Percentage Votes") +
  ggtitle("% Votes for Johnson and Poll Accuracy")

p4 <- ggplot(tbl4,aes(x=Finish,y=tbl4$`Stein (G)`,size=Accuracy)) +
  geom_point(na.rm=T,alpha=1/2,colour="green") +
  xlab("") +
  ylab("Percentage Votes") +
  ggtitle("% Votes for Stein and Poll Accuracy")


grid.arrange(p2,p1,p3,p4)

#In general, it appears that poll accuracy increased over the course of the presidential election race. Positive trends are seen in the values for both 
#Trump and Clinton while no real patterns emerge from the Stein and Johnson data. Values for Johnson are unimodal where popularity peaked 
#around September but then decreased significantly. Stein's popularity peaked between June and July but has been decreasing steadily since.

#Which poll is the most accurate? Note that the MoE is not given for some polls so their accuracy cannot be calculated.
 

rank <- tbl4%>%group_by(Poll)%>%
  select(Accuracy)%>%
  filter(!is.na(Accuracy))%>%
  summarise(mean=mean(Accuracy),total=length(Accuracy))%>%
  arrange(desc(mean))

head(rank)

#From this table, we see that the NBC News/SMNBC News poll has the highest accuracy. The mean accuracy for
#polls with fewer observations such as Pew Research and Guardian SUSA is less reliable.
  



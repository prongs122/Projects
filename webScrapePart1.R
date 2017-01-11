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

tbl4%>%select(Poll)%>%n_distinct()

#Take the 11 most recent pollsters. Remove the RCP average row, duplicate pollsters and any unnecessary columns from the Table 4 dataframe. To replicate the RCP time series graph,
#we need just the date and polling percentages for each canditate. The pollster is included for reference.

#Note that as the RCP polling table is dynamic, this code will filter different entries each time it is used. Duplicates may then be present.

tidy_df <- tbl4%>%filter(Poll!="RCP Average")%>%filter(row_number()%in%c(1:7,11,12,14,15))%>%select(1,2,5,6,7,8)

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







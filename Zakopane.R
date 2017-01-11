# Data has been gathered from the Zakopane ski jump location in Poland for the period 2010-2016.  

setwd("~/Documents/Ski_Jumping")

library(dplyr)
library(ggplot2)
library(gridExtra)

# Read in the dataset for Zakopane (2010-2016).
 
Zakopane_DF <- read.csv("Zakopane_DF.csv")

# How many ski jumpers are in the dataset? 145.

Zakopane_DF %>% 
  summarise(n_distinct(Name))

# Carry out some exploratory data analysis.

# Interesting questions:

#1. What is the mean jump length at Zakopane?
#2. What is the median jump length at Zakopane?
#3. What is the standard deviation, range, IQR etc. of all jumps?
#4. What is the distribution of a sample containing just podium jumps?
#4. Which jumpers do well at this location?
#5. What are the observational probabilities of the top jumpers winning the Zakopane event in 2017?
#6. What is the longest jump at Zakopane between 2010-2016?
#7. What is the shortest jump at Zakopane between 2010-2016?

summary(Zakopane_DF)

# Extract all jumps observed at Zakopane (2010-2016) and get summary statistics. The all_jumps vector represents the population of jumps (2010-2016).

all_jumps <- c(Zakopane_DF$Jump.1, Zakopane_DF$Jump.2)
summary(all_jumps)

# NAs indicate instances where a jumper did not qualify for the second round or where it was a one-jump competition.

mean(all_jumps, na.rm = T)
median(all_jumps, na.rm = T)
sd(all_jumps, na.rm = T)
range(all_jumps, na.rm = T)
IQR(all_jumps, na.rm = T)

# Maximum and minimum jump details (2010-2016).

Zakopane_DF %>%
  filter(Jump.1 == 79 | Jump.2 == 79)

Zakopane_DF %>%
  filter(Jump.1 == 140 | Jump.2 == 140)

# The mean jump length is 120.5 m.
# The median jump length is 120.5 m. 
# The standard deviation from the mean jump length is 7.7 m.
# The range is 79 m to 140 m (61.0 m).
# The IQR is 10.1 m.
# The longest jump is 140.0 m (PREVC Peter (2016)).
# The shortest jump is 79.0 m (INGVALDSEN Ole Marius (2011)).


# Extract all podium jumps (1-3) as a numeric vector all_podium_jumps. This is a sample of the population all_jumps.

podium_jumps <- subset(Zakopane_DF, Rank <= 3)
all_podium_jumps <- c(podium_jumps$Jump.1, podium_jumps$Jump.2)
summary(all_podium_jumps)

# The mean jump for podium finishes is 130.6 m.
# The median jump is 131.0 m.
# The maximum jump is 140.0 m.
# The minimum jump is 120.0 m.
# The range is 120  m to 140 m (20 m).
# The standard deviation from the mean is 4.2 m.


# Histograms of the distributions of the population and the sample.

df1 <- data.frame(Podium_Jumps = all_podium_jumps)
df2 <- data.frame(All_Jumps = all_jumps)

hist1 <- ggplot(df2, aes(x = All_Jumps)) +
          geom_histogram(fill = "lightblue2", colour = "black", binwidth = 8, alpha = 0.5) +
          ylab("No. of Jumps") +
          xlab("Jump Length / m") +
          ggtitle("All Ski Jumps at Zakopane (2010-2016)") 

hist2 <- ggplot(df1, aes(x = Podium_Jumps)) +
          geom_histogram(fill = "gold", colour = "black", binwidth = 5, alpha = 0.5) +
          ylab("No. of Jumps") +
          xlab("Jump Length / m") +
          ggtitle("Podium Ski Jumps at Zakopane (2010-2016)") 
  
grid.arrange(hist1, hist2, ncol = 2)

# Density curves of the population and sample of ski jumps. Note that the histograms use frequency density (frequency / interval) here rather 
# than total count of jumps.

f1 <- ggplot(df2, aes(x = All_Jumps)) + 
  geom_histogram(aes(y = ..density..), binwidth = 8, colour = "black", fill = "lightblue2", alpha = 0.5) +
  geom_density(alpha = 0.2) +
  geom_vline(aes(xintercept = 120.5), colour = "red", linetype = "dashed", size = 1.5) +
  xlab("Ski Jump Length / m") +
  ylab("Frequency Density") +
  ggtitle("Frquency Density Distribution of all Jumps at Zakopane (2010-2016)") +
  annotate("text", x = 85, y = 0.045, label = "Dashed red line\n is the median (120.5 m)")


f2 <- ggplot(df1, aes(x = Podium_Jumps)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black", fill = "gold", alpha = 0.5) +
  geom_density(alpha = 0.2) +
  geom_vline(aes(xintercept = 130.6), colour = "red", linetype = "dashed", size = 1.5) +
  xlab("Ski Jump Length / m") +
  ylab("Frequency Density") +
  ggtitle("Frquency Density Distribution of Podium Jumps at Zakopane (2010-2016)") +
  annotate("text", x = 120, y = 0.1, label = "Dashed red line\n is the mean (130.6 m)")

grid.arrange(f1, f2, ncol = 2)



# The distribution of all jumps is negatively skewed from normaility. This is confirmed by the Shapiro-Wilk test (p-value = 9.854 ^ -10). The
# distribution of the podium jumps is normal (confirmed by the p-value of 0.7749). We can, therefore, assume a Gaussian distribution for podium jumps  
# while we should approach the all jumps population with caution.

shapiro.test(all_jumps)
shapiro.test(all_podium_jumps)


# There are typically 50 ranked positions at the end of each competition. The top three get to the podium. If we treat the population all_jumps
# as a normal distribution (which it has been determined that it is not), the top three jump threshold would 
# then be the (1 - (3/50) = 0.94) 94th percentile. Therefore, it is reasonable to suggest that the top 6% of jumps will be the podium jumps.
# We can calculate the 94th percentile using the quantile() function applied to the all jumps population.

percentile_94 <- quantile(all_jumps, 0.94, na.rm = T)
print(unname(percentile_94))

# The top 6 % of jumps at Zakopane are equal to or greater than 131.5 m. Does this reasoning translate to real world podium places? It does. The mean jump
# length for podium jumps is 130.6 m. We could set this as a target for jumpers in order to have a high probability of a podium finish but a mean jump
# target for both Jump 1 and Jump 2 might be a better approach. This is covered later in the analysis.


# Which jumpers do well at Zakopane in terms of podium finishes?

top_jumpers <- podium_jumps %>% 
  select(Rank, Name) %>%
  group_by(Name) %>%
  summarise(TotalPodiums = n()) %>%
  arrange(desc(TotalPodiums))

# The probability of a podium finish can be calculated for each jumper once the number of competitions participated in is known.
# Observational probability: podiums / competitions participated in. Get appearances by top jumpers. If the jumper's name
# is in the list, he has participated in a competition. This pattern recognition code gives the number of competitions entered.

appearances <- c(nrow(filter(Zakopane_DF, grepl("STOCH", Name))),
nrow(filter(Zakopane_DF, grepl("BARDAL", Name))),
nrow(filter(Zakopane_DF, grepl("FREITAG", Name))),
nrow(filter(Zakopane_DF, grepl("AMMANN", Name))),
nrow(filter(Zakopane_DF, grepl("FREUND", Name))),
nrow(filter(Zakopane_DF, grepl("HILDE", Name))),
nrow(filter(Zakopane_DF, grepl("KOFLER", Name))),
nrow(filter(Zakopane_DF, grepl("KRAFT", Name))),
nrow(filter(Zakopane_DF, grepl("MORGENSTERN", Name))),
nrow(filter(Zakopane_DF, grepl("PREVC Peter", Name))),
nrow(filter(Zakopane_DF, grepl("SCHLIERENZAUER", Name))),
nrow(filter(Zakopane_DF, grepl("HAYBOECK", Name))),
nrow(filter(Zakopane_DF, grepl("JACOBSEN", Name))),
nrow(filter(Zakopane_DF, grepl("MALYSZ", Name))),
nrow(filter(Zakopane_DF, grepl("UHRMANN", Name)))
)

top_jumpers <- cbind(top_jumpers, appearances)

top_jumpers <- top_jumpers %>%
  mutate(Probability = TotalPodiums / appearances) %>%
  arrange(desc(appearances))

# Let's take the three jumpers with the highest observational probabilities and analyse them separately.

stoch <- Zakopane_DF %>%
  filter(grepl("STOCH", Name))

kraft <- Zakopane_DF %>%
  filter(grepl("KRAFT", Name))

freitag <- Zakopane_DF %>%
  filter(grepl("FREITAG", Name))

# Create the Top3 dataframe.

TOP3_DF <- rbind(stoch, freitag, kraft)

# We now need summary statistics for each top 3 jumper. 

TOP3_DF_summary <- TOP3_DF %>%
  group_by(Name) %>%
  summarise(Appearances = n()) %>%
  arrange(desc(Appearances))


# Create numerous summary statistic vectors for addition to Top3 summary dataframe.

podiums <- c(4, 3, 2)
probs <- c(0.4, 0.5, 0.5)
stoch_jumps <- c(stoch$Jump.1, stoch$Jump.2)
kraft_jumps <- c(kraft$Jump.1, kraft$Jump.2)
freitag_jumps <- c(freitag$Jump.1, freitag$Jump.2)

# A quick function for counting ranks.

rankCounter <- function(df, rank) {
  num <- nrow(df %>%
                filter(Rank == rank))
  print(num)
}

Firsts <- c(rankCounter(stoch, 1),
            rankCounter(freitag, 1),
            rankCounter(kraft, 1))

Seconds <- c(rankCounter(stoch, 2),
            rankCounter(freitag, 2),
            rankCounter(kraft, 2))

Thirds <- c(rankCounter(stoch, 3),
            rankCounter(freitag, 3),
            rankCounter(kraft, 3))


# For podium jumps, what was the mean jump (Jump1 and Jump2 / 2)?

stoch <- stoch %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

freitag <- freitag %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

kraft <- kraft %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

stoch[3, 12] <- 126.5
freitag[3, 12] <- 129.0
kraft[3, 12] <- 119.5

# Take the mean of the mean podium jumps and create a vector.

MeanOfMeanPodiumJumps <- c(129.8, 128.8, 133.1)

# Add everything to the Top 3 summary dataframe, re-order and rename variables to get final product.

TOP3_DF_summary <-  cbind(TOP3_DF_summary, podiums, c(mean(stoch_jumps, na.rm = T), mean(freitag_jumps, na.rm = T), mean(kraft_jumps, na.rm = T)),
                          c(median(stoch_jumps, na.rm = T), median(freitag_jumps, na.rm = T), median(kraft_jumps, na.rm = T)),
                          probs, c(max(stoch_jumps, na.rm = T), max(freitag_jumps, na.rm = T), max(kraft_jumps, na.rm = T)),
                          Firsts, Seconds, Thirds, MeanOfMeanPodiumJumps)

TOP3_DF_summary <- TOP3_DF_summary %>%
  mutate(ProbWin = Firsts / Appearances) %>%
  select(1, 2, 3, 8, 9, 10, 6, 12, 11, 4, 5, 7)

names(TOP3_DF_summary) <- c("Name", "Apps", "Podiums", "First", "Second", "Third", "ProbPodium", "ProbWin", "MeanPodiumJump", "Mean", "Median", "MaxJump")

# Finally, we calculate the mean jump for all podium jumps and then the mean of the means to set a target jump for each round of a TWO-JUMP 
# competition. For a single jump competition, the target is 128.0 m to 131.0 m.

podium_jumps <- podium_jumps %>%
  mutate(MeanJump = (Jump.1 + Jump.2) / 2)

# Fill in means for single jump competition in 2014.

podium_jumps[7, 12] <- 131.5
podium_jumps[8, 12] <- 124.5
podium_jumps[9, 12] <- 129.0

# Calculate the mean of the mean podium jumps. The result is 130.5 m. 

TargetMean <- mean(podium_jumps$MeanJump)
print(TargetMean)


# Conclusions:

# The minimum jump which contributed to a podium place was 120.0 m (M. Uhrmann Jump 1 in 2010). Obviously, a second round jump much higher was
# required to seal the podium (for Uhrmann it was 131.5 m).

# A mean jump of 130.5 m is a good target to ensure a podium place.

# Based on the observational probabilities, Kamil Stoch is most likely to win Zakopane 2017. However, Stefan Kraft has performed extremely well
# in the four competitions he has been a part of. He also has the longest mean jump and mean podium jump of the trio at the location.

# Based on this analysis, I would predict that Stoch or Kraft will win Zakopane 2017. I would strongly predict both to be on the podium while
# Richard Freitag may also secure a podium place but his form has been poor of late. A wildcard option may be Domen Prevc or Peter Prevc.







  
  

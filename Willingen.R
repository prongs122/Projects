# Historical ski-jumping data for Willingen (2010-2016) have been scraped from the FIS website. The event in 2013 was cancelled 
# so there are no data for that year. Note that Severin Freund will not participate in the 2017 competition due to an ACL injury.

library(dplyr)
library(ggplot2)
library(ggExtra)
library(gridExtra)

willingen_DF <- read.csv("willingen_DF.csv")

# How many ski jumpers are in the dataset? 145.

willingen_DF %>% 
  summarise(n_distinct(Name))

# Carry out some exploratory data analysis.

summary(willingen_DF)

# Extract all jumps observed at Willingen (2010-2016) and get summary statistics. The all_jumps vector represents the population of jumps (2010-2016).

all_jumps <- c(willingen_DF$Jump.1, willingen_DF$Jump.2)
summary(all_jumps)

# NAs indicate instances where a jumper did not qualify for the second round or where it was a one-jump competition.

mean(all_jumps, na.rm = T)
median(all_jumps, na.rm = T)
sd(all_jumps, na.rm = T)
range(all_jumps, na.rm = T)
IQR(all_jumps, na.rm = T)

# Maximum and minimum jump details (2010-2016).

willingen_DF %>%
  filter(Jump.1 == 101.5 | Jump.2 == 101.5)

willingen_DF %>%
  filter(Jump.1 == 152.0 | Jump.2 == 152.0)

# The mean jump length for all jumps at Willingen is 131.5 m.
# The median jump length is 131.8 m. 
# The standard deviation from the mean jump length is 8.3 m.
# The range is 101.5 m to 152.0 m (50.5 m).
# The IQR is 11.5 m.
# The longest jump is 152.0 m (TEPES Jurij (2014)).
# The shortest jump is 101.5 m (ZOGRAFSKI Vladimir (2012)).


# Extract all podium jumps (1st-3rd) and create a numeric vector all_podium_jumps. This is a sample of the population all_jumps.

podium_jumps <- subset(willingen_DF, Rank <= 3)
all_podium_jumps <- c(podium_jumps$Jump.1, podium_jumps$Jump.2)
summary(all_podium_jumps)

# The mean jump for all podium jumps is 142.5 m.
# The median jump is 142.5 m.
# The maximum jump is 150.5 m.
# The minimum jump is 132.0 m.
# The range is 132.0  m to 150.5 m (18.5 m).
# The standard deviation from the mean is 4.4 m.
# The IQR is 6.0 m.


# Histograms of the distributions of the population (all jumps) and the sample (podium jumps).

df1 <- data.frame(Podium_Jumps = all_podium_jumps)
df2 <- data.frame(All_Jumps = all_jumps)

hist1 <- ggplot(df2, aes(x = All_Jumps)) +
  geom_histogram(fill = "lightblue2", colour = "black", binwidth = 10, alpha = 0.5) +
  ylab("No. of Jumps") +
  xlab("Jump Length / m") +
  ggtitle("All Ski Jumps at Willingen (2010-2016)") 

hist2 <- ggplot(df1, aes(x = Podium_Jumps)) +
  geom_histogram(fill = "gold", colour = "black", binwidth = 4, alpha = 0.5) +
  ylab("No. of Jumps") +
  xlab("Jump Length / m") +
  ggtitle("Podium Ski Jumps at Willingen (2010-2016)") 

grid.arrange(hist1, hist2, ncol = 2)

# Density curves of the population and sample of ski jumps. Note that the histograms use frequency density (frequency / interval) here rather 
# than total count of jumps.

f1 <- ggplot(df2, aes(x = All_Jumps)) + 
  geom_histogram(aes(y = ..density..), binwidth = 10, colour = "black", fill = "lightblue2", alpha = 0.5) +
  geom_density() +
  geom_vline(aes(xintercept = 131.8), colour = "red", linetype = "dashed", size = 1.5) +
  xlab("Ski Jump Length / m") +
  ylab("Frequency Density") +
  ggtitle("Frequency Density Distribution\nof all Jumps at Willingen (2010-2016)") +
  annotate("text", x = 108, y = 0.04, label = "Dashed red line\n is the median (131.8 m)")


f2 <- ggplot(df1, aes(x = Podium_Jumps)) + 
  geom_histogram(aes(y = ..density..), binwidth = 4, colour = "black", fill = "gold", alpha = 0.5) +
  geom_density() +
  geom_vline(aes(xintercept = 142.5), colour = "red", linetype = "dashed", size = 1.5) +
  xlab("Ski Jump Length / m") +
  ylab("Frequency Density") +
  ggtitle("Frequency Density Distribution\nof Podium Jumps at Willingen (2010-2016)") +
  annotate("text", x = 132.5, y = 0.075, label = "Dashed red\n line is the\n  mean (142.5 m)")

grid.arrange(f1, f2, ncol = 2)

# The distribution of all jumps is negatively skewed from normaility. This is confirmed by the Shapiro-Wilk test (p-value = 0.0001876). The
# distribution of the podium jumps is normal (confirmed by the p-value of 0.358). We can, therefore, assume a Gaussian distribution for podium jumps  
# while we should approach the all jumps population with caution.

shapiro.test(all_jumps)
shapiro.test(all_podium_jumps)


# There are typically 50 ranked positions at the end of each competition. The top three get to the podium. If we treat the population all_jumps
# as a normal distribution (which it has been determined not to be), the top three jump threshold would 
# then be the (1 - (3/50) = 0.94) 94th percentile. Therefore, it is reasonable to suggest that the top 6% of jumps will be the podium jumps.
# We can calculate the 94th percentile using the quantile() function applied to the all jumps population.

percentile_94 <- quantile(all_jumps, 0.94, na.rm = T)
print(unname(percentile_94))

# Based on a Gaussian distribution, the top 6 % of all jumps at Willingen (2020-2016) are equal to or greater than 144.0 m. Does this reasoning translate to real world podium places? It does. The mean jump
# length for podium jumps is 142.5 m. We could set this as a target for jumpers in order to have a high probability of a podium finish.  However, a mean 
# of Jump 1 and Jump 2 might be a better target for each jump. This is covered later in the analysis.


# Which jumpers do well at Willingen in terms of podium finishes?

top_jumpers <- podium_jumps %>% 
  select(Rank, Name) %>%
  group_by(Name) %>%
  summarise(TotalPodiums = n()) %>%
  arrange(desc(TotalPodiums))

# The probability of a podium finish can be calculated for each jumper once the number of competitions participated in is known.
# Observational probability: podiums / competitions participated in. Get appearances by top jumpers. If the jumper's name
# is in the list, he has participated in a competition. This pattern recognition code gives the number of competitions entered.

appearances <- c(nrow(filter(willingen_DF, grepl("FREUND", Name))),
                  nrow(filter(willingen_DF, grepl("PREVC Peter", Name))),
                  nrow(filter(willingen_DF, grepl("STOCH", Name))),
                  nrow(filter(willingen_DF, grepl("AMMANN", Name))),
                  nrow(filter(willingen_DF, grepl("BARDAL", Name))),
                  nrow(filter(willingen_DF, grepl("DAMJAN", Name))),
                  nrow(filter(willingen_DF, grepl("GANG", Name))),
                  nrow(filter(willingen_DF, grepl("ITO", Name))),
                  nrow(filter(willingen_DF, grepl("JACOBSEN", Name))),
                  nrow(filter(willingen_DF, grepl("KOCH", Name))),
                  nrow(filter(willingen_DF, grepl("KOUDELKA", Name))),
                  nrow(filter(willingen_DF, grepl("NEUMAYER", Name))),
                  nrow(filter(willingen_DF, grepl("SCHLIERENZAUER", Name)))
)

top_jumpers <- cbind(top_jumpers, appearances)

top_jumpers <- top_jumpers %>% 
  mutate(Probability = TotalPodiums / appearances) %>%
  arrange(desc(Probability))

# Severin Freund is out of the 2017 competition so we can omit him from our analysis.

top_jumpers <- top_jumpers[-1, ]
row.names(top_jumpers) <- NULL

# Let's take the jumpers with the highest probability of a podium and analyse them separately.

stoch <- willingen_DF %>%
  filter(grepl("STOCH", Name))

prevc <- willingen_DF %>%
  filter(grepl("PREVC Peter", Name))

gangnes <- willingen_DF %>%
  filter(grepl("GANG", Name))

jacobsen <- willingen_DF %>%
  filter(grepl("JACOBSEN", Name))

koch <- willingen_DF %>%
  filter(grepl("KOCH", Name))


# Create a dataframe containing information on the top jumpers at Willingen.

TOP_DF <- rbind(stoch, prevc, gangnes, jacobsen, koch)

# We now need summary statistics for each top jumper. First, we create a new dataframe for the summary information.

TOP_DF_summary <- TOP_DF %>%
  group_by(Name) %>%
  summarise(Appearances = n()) %>%
  arrange(desc(Appearances))


# Create numerous summary statistic vectors for addition to the top summary dataframe.

podiums <- c(3, 3, 1, 1, 1)
probs <- c(0.5, 0.6, 0.5, 0.5, 0.5)
prevc_jumps <- c(prevc$Jump.1, prevc$Jump.2)
stoch_jumps <- c(stoch$Jump.1, stoch$Jump.2)
gangnes_jumps <- c(gangnes$Jump.1, gangnes$Jump.2)
jacobsen_jumps <- c(jacobsen$Jump.1, jacobsen$Jump.2)
koch_jumps <- c(koch$Jump.1, koch$Jump.2)


# A quick function for counting ranks.

rankCounter <- function(df, rank) {
  num <- nrow(df %>%
                filter(Rank == rank))
  print(num)
}

Firsts <- c(rankCounter(prevc, 1),
            rankCounter(stoch, 1),
            rankCounter(gangnes, 1),
            rankCounter(jacobsen, 1),
            rankCounter(koch, 1))

Seconds <- c(rankCounter(prevc, 2),
             rankCounter(stoch, 2),
             rankCounter(gangnes, 2),
             rankCounter(jacobsen, 2),
             rankCounter(koch, 2))


Thirds <- c(rankCounter(prevc, 3),
            rankCounter(stoch, 3),
            rankCounter(gangnes, 3),
            rankCounter(jacobsen, 3),
            rankCounter(koch, 3))



# For podium jumps, what was the mean jump over the two rounds (Jump1 and Jump2 / 2)?

stoch <- stoch %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

prevc <- prevc %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

gangnes <- gangnes %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

jacobsen <- jacobsen %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

koch <- koch %>%
  mutate(MeanPodiumJump = (Jump.1 + Jump.2) / 2)

# Filter for just podium jumps.

stoch <- stoch[3:5, ]
prevc <- prevc[4:6, ]
gangnes <- gangnes[2, ]
jacobsen <- jacobsen[1, ]
koch <- koch[1, ]

# Take the mean of the mean podium jumps and create a vector.

MeanOfMeanPodiumJumps <- c(142.4, 144.5, 140.3, 138.8, 141)

# Now add everything to the Top jumpers summary dataframe, re-order and rename variables to get the final product.

TOP_DF_summary <-  cbind(TOP_DF_summary, podiums, c(mean(prevc_jumps, na.rm = T), mean(stoch_jumps, na.rm = T), mean(gangnes_jumps, na.rm = T),
                          mean(jacobsen_jumps, na.rm = T), mean(koch_jumps, na.rm = T)),
                          c(median(prevc_jumps, na.rm = T), median(stoch_jumps, na.rm = T), median(gangnes_jumps, na.rm = T),
                          median(jacobsen_jumps, na.rm = T), median(koch_jumps, na.rm = T)),
                          probs, c(max(prevc_jumps, na.rm = T), max(stoch_jumps, na.rm = T), max(gangnes_jumps, na.rm = T), max(jacobsen_jumps, na.rm = T),
                          max(koch_jumps, na.rm = T)),
                          Firsts, Seconds, Thirds, MeanOfMeanPodiumJumps)

TOP_DF_summary <- TOP_DF_summary %>%
  mutate(ProbWin = Firsts / Appearances) %>%
  select(1, 2, 3, 8, 9, 10, 6, 12, 11, 4, 5, 7)

names(TOP_DF_summary) <- c("Name", "Apps", "Podiums", "First", "Second", "Third", "ProbPodium", "ProbWin", "MeanPodiumJump", "Mean", "Median", "MaxJump")

# Finally, we calculate the mean jump for all podium jumps and then the mean of the means to set a target jump for each round of a TWO-JUMP 
# competition.

podium_jumps <- podium_jumps %>%
  mutate(MeanJump = (Jump.1 + Jump.2) / 2)

# Calculate the mean of the mean podium jumps. The result is 142.5 m, the same as the histogram earlier.

TargetMean <- mean(podium_jumps$MeanJump)
print(TargetMean)


# Conclusions:

# The minimum jump which contributed to a podium place was 132.0 m (P. Prevc in 2014). Obviously, the other round jump was much higher in order
# to seal the podium (for P. Prevc it was 145.5 m).

# A mean jump of 142.5 m is a good target to ensure a podium place at Willingen.

# Based on the observational probabilities, Kamil Stoch is most likely to win Willingen 2017 while Peter Prevc is a good contender for a podium
# finish. There are not enough data for D. Prevc but he did not to well here last year. Wildcard options are A. Wellinger, D. Prevc, S. Kraft and R. Freitag.



# Season form.


##### Decision tree predictive model. ####

library(partykit)

output_tree <- ctree(Podium ~ Jump.1 + Jump.2 + Points, data = willingen_DF)
plot(output_tree)

## 90% of the sample size
smp_size <- floor(0.90 * nrow(willingen_DF))

## Set the seed to make your partition reproducible
set.seed(101)
train_ind <- sample(seq_len(nrow(willingen_DF)), size = smp_size)

train <- willingen_DF[train_ind, ]
test <- willingen_DF[-train_ind, ]

# Test the model (CRISP-DM).

output_tree2 <- ctree(Podium ~ Jump.1 + Jump.2 + Points, data = train)
plot(output_tree2, main = "Conditional Classification Tree for Willingen")
print(output_tree2)
predictions <- predict(output_tree2, newdata = test)


confusionMatrix <- table(test$Podium, predictions)
print(confusionMatrix)

# Calculate model accuracy.

# correctly predicted outcomes / total outcomes



# Plot of classification zones as determined by output_tree2. As there are three predictors, we use a 3D plot.

library(scatterplot3d)


scatterplot3d(x = willingen_DF$Jump.1,
              y = willingen_DF$Jump.2,
              z = willingen_DF$Points,
              xlab = "Jump 1",
              ylab = "Jump 2",
              zlab = "Total Points",
              main = "3D Scatterplot of Predictors",
              grid = T,
              color = "red",
              pch = 20)


# A 2D representation if the number of predictors equals two.

p1 <- ggplot(willingen_DF, aes(x = Jump.1, y = Jump.2, colour = Podium)) +
  geom_point(alpha = 0.75) +
  geom_segment(aes(x = 124.0, xend = 124.0, y = 145, yend = 131.0), colour = "black") +
  geom_segment(aes(x = 124.0, xend = 130.0, y = 131.0, yend = 131.0), colour = "black") +
  geom_segment(aes(x = 130.0, xend = 130.0, y = 131.0, yend = 123.0), colour = "black") +
  geom_segment(aes(x = 130.0, xend = 155.0, y = 123.0, yend = 123.0), colour = "black") +
  coord_cartesian(xlim = c(100, 150), ylim = c(100, 140)) +
  xlab("Jump 1 / m") +
  ylab("Jump 2 / m") +
  annotate("text", x = 105, y = 135, label = "Prediction:\nno podium") +
  annotate("text", x = 145, y = 135, label = "Prediction:\npodium") 

ggExtra::ggMarginal(
  p = p1,
  type = 'histogram',
  margins = 'both',
  size = 5,
  col = "black",
  fill = "grey",
  yparams = list(binwidth = 5),
  xparams = list(binwidth = 5)
)



# Additional visualisation for fun!

persp()


# Test output_tree2 on Willingen 2017 data.

willingen2017 <- read.csv("willingen2017.csv")
predictions2017 <- predict(output_tree2, newdata = willingen2017)

# The model does  not predict any podium places as the total points conditions inhibits positive results. We can remove the totl points variable
# from the model and use only jump lengths as predictors.

table(willingen2017$Podium, predictions2017)

look <- cbind(willingen2017, predictions2017, predictions2017_mod)

output_tree_mod <- ctree(Podium ~ Jump.1 + Jump.2, data = train)
plot(output_tree_mod)

# Let's rerun the test.

predictions2017_mod <- predict(output_tree_mod, newdata = willingen2017)
table(willingen2017$Podium, predictions2017_mod)

# Conclusion.

# The model performed poorly here as the second round jums were of lower standard than the first round jumps. Th model classified all 
# athletes as non-podium finishers. We must add this 2017 data to the model and tweak it for future usage.




# A Comparative Analysis of the GymAware, ChronoJump, Pushband and MyJump Devices.

# Objective: To test for differences in mean jump height between instruments using 1-Way ANOVA.
# The GymAware instrument is considered to be the gold standard in sports science, 
# as such, the other instruments will be tested against it.

# There are some assumptions made when using 1-Way ANOVA:
#1 Normally distributed response values
#2 Constant variances across response values
#3 Random sampling

# In any experiment, the researcher must set a maximum significance level (p-value) for which they will reject the
# null hypothesis. This is usually set to 5% (0.05) but can vary in certain experiments such as high
# precision physics experiments, where it can be 0.1% or less (0.001), or in social science experiments where
# it can be as high as 10% (0.1). We should use a p-value of 0.05 for this experiment (unless advised otherwise).

# Next, you should state your null hypothesis and hypothesis (these should be introduced in the abstract
# and introduction sections of your report). H0 is the null, H1 is the alternative hypothesis. See below.

# H0) Sample means are equal.
# H1) Sample means are not equal.

setwd("~/Desktop")
df <- read.csv("Plyometric_Jumps.csv")
df$Subject <- factor(df$Subject , levels = unique(df$Subject)) 

library(reshape2)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(pwr)


# We want to investigate differences in the mean jump height recorded by each device. Therefore, biostatistics
# such as weight, age and height are of less interest/irrelevant. We keep the Subject variable for later analysis.

# Dataset properties. We have 110 observations of 8 variables. There is one factor variable, five numeric
# variables and two integer variables. A power analysis is carried out to determine whether the sample size is adequate 
# to find a significant difference if one exists. 

# No preprocessing is required regarding NAs. Let's carry out some univariate and multivariate
# profiling. Then, we will look at some descriptive statistics.

str(df)
dim(df)

# Univariate profiling of response values. The first assumption of 1-Way ANOVA is normally distributed response
# values for each treatment (jump device). Deviations from normality will not affect the F-statistic greatly
# provided sample sizes are equal (which they are at 110).

df2 <- melt(df[ , c(1 , 5:8)])

ggplot(df2, aes(x = value)) +
  geom_histogram(colour = "black", fill = "lavender", bins = 10) +
  facet_wrap(~ variable) +
  theme_few() +
  xlab("Jump Height / cm")

# Formal normality testing.

lapply(df[ , 5:8], shapiro.test)
lapply(df[ , 5:8], qqnorm)

# We reject that the samples come from normally distributed populations. However, as mentioned, the F-statistic
# is robust to deviations from normality. We should use the median as a comparitive descriptive measure but
# the ANOVA can proceed using the sample means. Another assumption is constant variance. One look at a 
# boxplot and we can see all of the skewed distributions and the presence of outliers in the GymAware data.
# The skewness of each distribution is light and variances look similar across all devices except the GymAware.

ggplot(df2, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lavender") +
  xlab(" ") +
  ylab("Jump Height / cm") +
  theme_few()

# Obtain descriptive statistics

summary(df)
lapply(df[ , 5:8] , sd)

# All devices were highly correlated using Pearson's Product Correlation Coefficient.

cor(df[ , 5:8])

# Variances are not all constant but the F-statistic is also robust to this. The Gymaware instrument 
# recorded some outlier values which increase the range of the data. These are valid observations, however, 
# and should not be discarded. The data are randomly sampled so the third assumption is met. 

# Perform the 1-Way ANOVA:

model <- lm(value ~ variable, data = df2)
print(anova(model))

# The results of the 1-Way ANOVA show that there is a statistically significant difference between at least
# one group of measurements and the others at the p < 0.05 level of significance. The p-value calculated
# is very small indeed (< 0.001).

# Power test:

# We must calculate the effect size (in ANOVA table this is: variable SS / (variable SS + Residuals SS )).
effect_size <- 4773.5 / (4773.5 + 24588.2)

# Use the effect size and other information to obtain a power statistic at the set p-value level (0.05).
pwr.anova.test(f = 0.16, k = 4, n = 110, sig.level = 0.05)

# We calculate a power of 0.81 (81%). The probability of finding a difference if one actually exists is 81%. The sample size is large enough.

# Get effect size of each treatment (treatment mean - grand mean).

grand_mean <- (mean(df$ChronoJump) + mean(df$Gymaware) + mean(df$Pushband) + mean(df$My.Jump)) / 4

CJ_effect <- mean(df$ChronoJump) - grand_mean
GA_effect <- mean(df$Gymaware) - grand_mean
PB_effect <- mean(df$Pushband) - grand_mean
MJ_effect <- mean(df$My.Jump) - grand_mean

# The effect of Gymaware and Pushband is positive while Chronojump and MyJump is negative. 

# The null hypothesis can be rejected based on the ANOVA results. At least one treatment group mean is 
# is significantly different from the others. 

# Is the general pattern in measurement differences repeated in each subject'sindividual jump data? The pattern is:
# GymAware > Pushband > MyJump > ChronoJump.
# The boxplots by subject show an agreement across most of the test subjects.

ggplot(df2, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lavender") +
  facet_wrap(~ Subject) +
  theme_few() +
  xlab(" ") +
  ylab("Jump Height / cm")

# We can employ t-tests to identify differences between individual samples. Identify where the differences 
# lie using a series of t-tests (Welch version).

t.test(df$Gymaware, df$ChronoJump) # Significant difference
t.test(df$Gymaware, df$Pushband) # No significant difference
t.test(df$Gymaware, df$My.Jump) # Significant difference
t.test(df$ChronoJump, df$My.Jump) # No significant difference


# Run 1-Way ANOVA across all test subjects to test each one statistically.

levs <- levels(df$Subject)
sub_list <- vector("list" , length = length(levs))
names(sub_list) <- levs

for(l in levs) {
  for(i in sub_list) {
    sub_list[[l]] <- df2 %>%
      filter(grepl(l , Subject))
  }
}

models <- lapply(sub_list, function(l) { lm(l[[3]] ~ l[[2]] , data = l) } )
anova_tabs <- lapply(models , function(m) { anova(m) })
print(anova_tabs)

# The sample means are not equal across all test subjects. We can be confident that compiling all observations
# together provides a robust result.

# Scatterplots of correlations between devices.

ggplot(df, aes(x = Gymaware, y = Pushband)) +
  geom_point() +
  annotate("text", x = 10, y = 35, label = "r = 0.87") +
  stat_smooth(method = "lm", formula = y ~ x, fill = NA, colour = "black", size = 0.5) +
  theme_few() +
  xlab("GymAware / cm") +
  ylab("Pushband / cm")

ggplot(df, aes(x = My.Jump, y = ChronoJump)) +
  geom_point() +
  annotate("text", x = 10, y = 30, label = "r = 0.98") +
  stat_smooth(method = "lm", formula = y ~ x, fill = NA, colour = "black", size = 0.5) +
  theme_few() +
  xlab("MyJump / cm") +
  ylab("ChronoJump / cm")


# CONCLUSION

# A comparison of mean and median values between devices indicates that both the ChronoJump and MyJump
# devices record lower average jump heights than both the GymAware and Pushband devices. 

# A 1-Way ANOVA confirmed that at least one sample differed from the others at the p < 0.05 significance
# level. A power analysis statistic of 81% ensured that the sample size was adequate to detect a difference if one 
# existed.

# Individual t-tests showed that there were significant differences in mean jump height between 
# GymAware and ChronoJump and also GymAware and MyJump. There was no difference between GymAware and
# Pushband nor between ChronoJump and MyJump.

# This same pattern holds true when looking at individual test subject's jump distributions.

# A strong correlation exists (0.87) between the Pushband and GymAware devices. There is also a strong
# correlation between the ChronoJump and MyJump devices (0.98).

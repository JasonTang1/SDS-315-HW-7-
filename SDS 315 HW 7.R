library(tidyverse)
library(MatchIt)
library(mosaic)

arm <- read_csv("armfold.csv")
turnout <- read_csv("turnout.csv")

arm_adjusted <- arm %>%
  group_by(Sex) %>%
  summarize(people_count = length(W.Hnd), left_on_top = sum(LonR_fold))


#A
#based on arm_adjusted, there are 111 females and 106 males 
male_prop = 50/106
#sample proportion of males who folded left on top is 50/106 or 50 over 106 males 0.472
female_prop = 47/111
#sample proportion of females who folded left on top is 47/111 or 47 over 111 females 0.423

#B
#difference in proportion between male - female
diff_in_prop = male_prop-female_prop
#(50/106) - (47/111) 0.048

#C

#differences in proportion SE
Standard_Error = sqrt((male_prop*(1-male_prop)/106) + (female_prop*(1-female_prop)/111))

confidence_int_lower = diff_in_prop-1.96*Standard_Error
confidence_int_upper = diff_in_prop+1.96*Standard_Error

x <- c(50, 47) # Number of successes in each group
n <- c(106, 111) # Total number of observations in each group

result <- prop.test(x, n,conf.level = 0.95,correct = FALSE)
result$conf.int

#D
#If we were to repeat this sampling process many times, 95% of the resulting confidence intervals would contain the true difference in proportions between males and females who fold their left arm on top, which we estimate to be between -0.0834 and 0.180.
 
#E 
# The standard error represents the variability of the difference in sample proportions if we were to repeatedly sample from the population. It measures the standard deviation of the sampling distribution, and how much we expect the observed difference in proportions to fluctuate.

#F
# In this context, the sampling distribution refers to the distribution of the difference in sample proportion of males and females who fold their left arm on top, across many hypothetical random samples of the same sample size. Since we resample from the same 106 males and 111 females, which hand on top will vary from sample to sample, and thus cause the sample proportions to vary as well. The populations of male and females themselves stays the same.

#G
#The Central Limit Theorem (CLT) justifies using a normal distribution to approximate the sampling distribution of the difference in sample proportions. The CLT states that if we take N independent samples from some wider population, with n being sufficientily large, the distribution of the sample proportion will approach a normal distribution even if the population distribution is not normal.

#H
# I would say that there might be evidence in a sex difference in arm folding, although we can't confidentially conclude that it is statistically significant as 0 is in the confidence interval.

#I
# Yes, if we repeated this experiment many times with different random samples of university students, the confidence intervals would vary across samples. This is because the random samples differ in who folds their left arm on top, leading to different sample proportions and confidence intervals. However, if we collected all the confidence intervals, the distribution of these intervals would form a normal distribution, centered around the true population difference in proportions.


turnout1 <- turnout %>%
  group_by(GOTV_call) %>%
  summarize(voted_or_not = sum(voted1998), number_of_people = length(GOTV_call))

#A proportion who received a GOTV call and voted: 
GOTV_voted = 160/247
no_call_voted = 4701/10582

x2 <- c(160, 4701) # Number of successes in each group
n2 <- c(247, 10582) # Total number of observations in each group

result2 <- prop.test(x2, n2,conf.level = 0.95,correct = FALSE)
result2$conf.int

#B

ggplot(turnout, aes(x = factor(GOTV_call), y = AGE)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "GOTV Call", y = "Age", title = "Distribution of Age by GOTV Call Status") +
  theme_minimal()


# Association between confounders and GOTV call
table(turnout$voted1996, turnout$GOTV_call)  
table(turnout$MAJORPTY, turnout$GOTV_call)   # Party affiliation by call


# Chi-square tests for categorical variables
chisq.test(table(turnout$voted1996, turnout$GOTV_call))  
chisq.test(table(turnout$MAJORPTY, turnout$GOTV_call))  

t.test(turnout$AGE ~ turnout$GOTV_call)      # Age differences

diffmeans_MAJORPTY <- diffmean(MAJORPTY ~ GOTV_call, data = turnout)
set.seed(123)
boot_match <- do(1000) * diffmean(MAJORPTY ~ GOTV_call, data = resample(turnout))
confint(boot_match)

diffmeans_voted1996 <- diffmean(voted1996 ~ GOTV_call, data = turnout)
set.seed(123)  
boot_match <- do(1000) * diffmean(voted1996 ~ GOTV_call, data = resample(turnout))
confint(boot_match)


#C

call_match = matchit(GOTV_call ~ AGE + factor(voted1996) + factor(MAJORPTY), data = turnout, ratio=5)
summary(call_match)

call_matched = match.data(call_match)



#checking if confounders still exist
t.test(call_matched$AGE ~ call_matched$GOTV_call)      # Age differences

diffmeans_MAJORPTY2 <- diffmean(MAJORPTY ~ GOTV_call, data = call_matched)
set.seed(123) 
boot_match3 <- do(1000) * diffmean(MAJORPTY ~ GOTV_call, data = resample(call_matched))
confint(boot_match3)

diffmeans_voted1996_2 <- diffmean(voted1996 ~ GOTV_call, data = turnout)
set.seed(123) 
boot_match4 <- do(1000) * diffmean(voted1996 ~ GOTV_call, data = resample(call_matched))
confint(boot_match4)




#Large sample confidence interval for difference in 2 proportions
call_matched1 <- call_matched %>%
  group_by(GOTV_call) %>%
  summarize(voted_or_not = sum(voted1998), number_of_people = length(GOTV_call))

x3 <- c(160, 703) # Number of successes in each group
n3 <- c(247, 1235) # Total number of observations in each group

result3 <- prop.test(x3, n3,conf.level = 0.95,correct = FALSE)
result3$conf.int

#I conclude that the overall effect of the GOTV call on the likelihood of voting in the 1998 election is still statistically significant as the confidence interval of 0.013 to 0.144 does not contain 0, meaning on average, according to our sample population, a greater proportion of people will vote because of the GOTV call.
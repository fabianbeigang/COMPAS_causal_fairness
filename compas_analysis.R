## SETTING UP THE PROJECT

# Install libraries
library(ggplot2)
library(skimr)
library(stats)
library(dplyr)
library(Matching)
library(cobalt)
library(rbounds) 

# Mode function for categorical variables
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


## DATA SET PREPARATION

# Import dataset
raw_data <- read.csv("compas-scores-two-years.csv")

# Turn into data frame
df <- data.frame(raw_data)

# Delete rows according to COMPAS
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')

# Remove all categories from Ethnic Code except "Caucasian" and "African-American"
df <- df[which(df$race=="Caucasian" | df$race=="African-American"),]

# Add binary variable "high_risk" for high risk of simple recidivism (i.e. decile_score greater than 7)
df <- transform(df, high_risk= ifelse(decile_score>7, 1, 0))

# Add binary variable "high_risk_2" for high risk of simple recidivism (i.e. decile_score greater than 6)
df <- transform(df, high_risk_2= ifelse(decile_score>6, 1, 0))

# Add binary variable "high_risk_3" for high risk of simple recidivism (i.e. decile_score greater than 5)
df <- transform(df, high_risk_3= ifelse(decile_score>5, 1, 0))


## EXPLORATORY ANALYSIS RISK OF RECIDIVISM

# Summarize the data set
summary(df)
summary(df$race)

# Look at the structure
str(df)

# Skim
skim(df)

# Number of recidivists
sum(df$is_recid)

# Table recidivism/high risk by ethnicity
xtabs(~ two_year_recid + race, data=df)
xtabs(~ high_risk_3 + race, data=df)

# Estimated outcome (risk>5) by ethnicity
ggplot(df, aes(x = race, fill = as.factor(abs(high_risk_3-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Ethnicity") + 
  ylab("Total count") +
  labs(fill = "Predicted Recidivism Risk") + 
  scale_fill_discrete(labels = c("High risk", "Low risk"))
 

# Actual outcome (is_recid) by ethnicity
ggplot(df, aes(x = race, fill = as.factor(abs(two_year_recid-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Ethnicity") + 
  ylab("Total count") +
  labs(fill = "Reoffended") + 
  scale_fill_discrete(labels = c("Yes", "No"))

# Estimated outcome (score_text) by sex
ggplot(df, aes(x = sex, fill = as.factor(abs(high_risk_3-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Sex") + 
  ylab("Total count") +
  labs(fill = "Predicted Recidivism Risk") + 
  scale_fill_discrete(labels = c("High risk", "Low risk"))

# Actual outcome (is_recid) by sex
ggplot(df, aes(x = sex, fill = as.factor(abs(two_year_recid-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Sex") + 
  ylab("Total count") +
  labs(fill = "Reoffended") + 
  scale_fill_discrete(labels = c("Yes", "No"))

# Estimated outcome by charge degree
ggplot(df, aes(x = c_charge_degree, fill = as.factor(abs(high_risk_3-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Charge degree (Misdemeanor/Felony)") + 
  ylab("Total count") +
  labs(fill = "Predicted Recidivism Risk") + 
  scale_fill_discrete(labels = c("High risk", "Low risk"))

# Actual outcome (is_recid) by charge degree
ggplot(df, aes(x = c_charge_degree, fill = as.factor(abs(two_year_recid-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Charge degree (Misdemeanor/Felony)") + 
  ylab("Total count") +
  labs(fill = "Reoffended") + 
  scale_fill_discrete(labels = c("Yes", "No"))

# High risk (<5) by age category
ggplot(df, aes(x = age_cat, fill = as.factor(abs(high_risk_3-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Age Category") + 
  ylab("Total count") +
  labs(fill = "Predicted Recidivism Risk") + 
  scale_fill_discrete(labels = c("High risk", "Low risk"))


# Actual outcome (is_recid) by age category
ggplot(df, aes(x = age_cat, fill = as.factor(abs(two_year_recid-1)))) +
  geom_bar(width = 0.5) + 
  xlab("Age category") + 
  ylab("Total count") +
  labs(fill = "Reoffended") + 
  scale_fill_discrete(labels = c("Yes", "No"))

# High risk (<5) by ethnicity, while contorlling for age
ggplot(df, aes(x = race, fill = as.factor(abs(high_risk_3-1)))) +
  geom_bar(width = 0.5) + 
  facet_wrap(~age_cat) +
  xlab("Ethnicity") + 
  ylab("Total count") +
  labs(fill = "Predicted Recidivism Risk") + 
  scale_fill_discrete(labels = c("High risk", "Low risk"))

# Actual outcome (is_recid) by ethnicity, while contorlling for age
ggplot(df, aes(x = race, fill = as.factor(abs(two_year_recid-1)))) +
  geom_bar(width = 0.5) + 
  facet_wrap(~age_cat) +
  xlab("Ethnicity") + 
  ylab("Total count") +
  labs(fill = "Reoffended") + 
  scale_fill_discrete(labels = c("Yes", "No"))

# High risk (<5) by ethnicity, while contorlling for charge degree (felony/misdemeanor)
ggplot(df, aes(x = race, fill = as.factor(abs(high_risk_3-1)))) +
  geom_bar(width = 0.5) + 
  facet_wrap(~c_charge_degree) +
  xlab("Ethnicity") + 
  ylab("Total count") +
  labs(fill = "Predicted Recidivism Risk") + 
  scale_fill_discrete(labels = c("High risk", "Low risk"))

# Actual outcome (is_recid) by ethnicity, while contorlling for charge degree (felony/misdemeanor)
ggplot(df, aes(x = race, fill = as.factor(abs(two_year_recid-1)))) +
  geom_bar(width = 0.5) + 
  facet_wrap(~c_charge_degree) +
  xlab("Ethnicity") + 
  ylab("Total count") +
  labs(fill = "Reoffended") + 
  scale_fill_discrete(labels = c("Yes", "No"))

# T-test for high risk by ethnicity 
t_risk <- t.test(high_risk_3 ~ race, var.equal = TRUE, data = df)
t_risk
t_risk$estimate[1]-t_risk$estimate[2]

# T-test for recidivism by ethnicity 
t_recid <- t.test(is_recid ~ race, var.equal = TRUE, data = df)
t_recid
t_recid$estimate[1]-t_recid$estimate[2]

## Regression adjusted difference in means
# Regression adjusted difference in relative frequencies for being high risk (African-American vs. Caucasian)
logit_risk_reg <- glm(high_risk_3 ~ race + age + sex + priors_count + c_charge_degree, data=df, family=binomial(link="logit"))

# Print coefficients
print(logit_risk_reg)

# Print relative frequency for being high risk (African-American vs. Caucasian) for average person
age_avg <- round(mean(df$age))
sex_avg <- calculate_mode(df$sex)
priors_count_avg <- round(mean(df$priors_count))
c_charge_degree_avg <- calculate_mode(df$c_charge_degree)

# Probability for high risk as average African-American
prob_aa_reg <- predict(logit_risk_reg, 
                   newdata = data.frame(
                     race = "African-American", 
                     age = age_avg, 
                     sex = sex_avg,
                     priors_count = priors_count_avg,
                     juv_misd_count = juv_misd_count_avg,
                     c_charge_degree = c_charge_degree_avg
                     ), type = "response")

# Probability for high risk as average Caucasian
prob_ca_reg <- predict(logit_risk_reg, 
                       newdata = data.frame(
                         race = "Caucasian", 
                         age = age_avg, 
                         sex = sex_avg,
                         priors_count = priors_count_avg,
                         juv_misd_count = juv_misd_count_avg,
                         c_charge_degree = c_charge_degree_avg
                       ), type = "response")

# Difference by ethnicity in estimation for average person
print(prob_aa_reg-prob_ca_reg)

# Regression adjusted difference in relative frequencies for recidivating (African-American vs. Caucasian)
logit_recid_reg <- glm(two_year_recid ~ race + age + sex + priors_count + c_charge_degree, data=df, family=binomial(link="logit"))

# Print coefficients
print(logit_recid_reg)

# Print difference in mean probabilities for recidivating (African-American vs. Caucasian) for average person
prob_aa_recid_reg <- predict(logit_recid_reg, 
                       newdata = data.frame(
                         race = "African-American", 
                         age = age_avg, 
                         sex = sex_avg,
                         priors_count = priors_count_avg,
                         juv_misd_count = juv_misd_count_avg,
                         c_charge_degree = c_charge_degree_avg
                       ), type = "response")
prob_ca_recid_reg <- predict(logit_recid_reg, 
                       newdata = data.frame(
                         race = "Caucasian", 
                         age = age_avg, 
                         sex = sex_avg,
                         priors_count = priors_count_avg,
                         juv_misd_count = juv_misd_count_avg,
                         c_charge_degree = c_charge_degree_avg
                       ), type = "response")

# Difference by ethnicity in the actual outcome for average person
print(prob_aa_recid_reg-prob_ca_recid_reg)

## Constructing the matched sample: For each Caucasian individual find the Mahalanobis-closest African-American 

# Adding a logical vector that indicates treatment
df$treatment <- df$race == "African-American"

# Transforming sex and c_charge_degree into binary variable and adding to data frame
df$sex_bin <- ifelse(df$sex == "Male", 1, 0)
df$charge_bin <- ifelse(df$c_charge_degree == "M", 1, 0)

# Subset of covariates age, priors, sex, charge degree
X <- df %>%
  dplyr::select(age, priors_count, sex_bin, charge_bin)
  
# Mahalanobis matching 1:1
matchmaha1 <- Match(Tr = df$treatment, X = X, estimand = "ATT", M = 1, ties = TRUE, replace = TRUE, Weight = 2)

# Check covariate balance
bal_tab11 <- bal.tab(matchmaha1, treat = df$treatment, covs = X)
bal_tab11

# Graphical representation of balance
love.plot(matchmaha1, threshold = .05, binary = 'std', treat = df$treatment,
covs = X)

# Plotting the individual covariates
bal.plot(matchmaha1, var.name = "age", treat = df$treatment, covs = X,
         which = 'both')

bal.plot(matchmaha1, var.name = "priors_count", treat = df$treatment, covs = X,
         which = 'both')

bal.plot(matchmaha1, var.name = "charge_bin", treat = df$treatment, covs = X,
         which = 'both')

bal.plot(matchmaha1, var.name = "sex_bin", treat = df$treatment, covs = X,
         which = 'both')

# Mahalanobis matching 1:2
matchmaha2 <- Match(Tr = df$treatment, X = X, estimand = "ATT", M = 2, ties = TRUE, replace = TRUE, Weight = 2)

# Check covariate balance
bal_tab21 <- bal.tab(matchmaha2, treat = df$treatment, covs = X)
bal_tab21

# Graphical representation of balance
love.plot(matchmaha2, threshold = .05, binary = 'std', treat = df$treatment,
          covs = X)

# Plotting the individual covariates
bal.plot(matchmaha2, var.name = "age", treat = df$treatment, covs = X,
         which = 'both')

bal.plot(matchmaha2, var.name = "priors_count", treat = df$treatment, covs = X,
         which = 'both')

bal.plot(matchmaha2, var.name = "charge_bin", treat = df$treatment, covs = X,
         which = 'both')

bal.plot(matchmaha2, var.name = "sex_bin", treat = df$treatment, covs = X,
         which = 'both')

## ESTIMATING AVERAGE TREATMENT EFFECT
# Average treatment effect for high risk: ethnicity is statistically significant (p = 0.0000037), hence has a causal effect on outcome
matchmaha1risk <- Match(Y = df$high_risk_3, 
                       Tr = df$treatment, 
                       X = X, estimand = "ATE", 
                       M = 1, ties = TRUE, 
                       replace = TRUE, 
                       Weight = 2, 
                       BiasAdjust = TRUE)

summary(matchmaha1risk)

# Average treatment effect for actual recidivism: ethnicity is not statistically significant (p = 0.18)
matchmaha1recid <- Match(Y = df$two_year_recid, 
                       Tr = df$treatment, 
                       X = X, estimand = "ATE", 
                       M = 1, ties = TRUE, 
                       replace = TRUE, 
                       Weight = 2, 
                       BiasAdjust = TRUE)

summary(matchmaha1recid)

## Sensitivity analysis 
# Analysis for risk score: gamma of up to 2 in increments of 0.1
match_risk_psens <- psens(matchmaha1risk, Gamma = 2, GammaInc = .1)
match_risk_psens

match_risk_hlsens <- hlsens(matchmaha1risk, Gamma = 2, GammaInc = .1)
match_risk_hlsens

# Analysis for actual recidivism: gamma of up to 2 in increments of 0.1
match_recid_psens <- psens(matchmaha1recid, Gamma = 2, GammaInc = .1)
match_recid_psens

match_recid_hlsens <- hlsens(matchmaha1recid, Gamma = 2, GammaInc = .1)
match_recid_hlsens

## Summary plot
summary_df <- data.frame(variable = rep('Treatment', 2),
                               coefficient = c(matchmaha1risk$est,
                                               matchmaha1recid$est),
                               se = c(matchmaha1risk$se,
                                      matchmaha1recid$se),
                               modname = c('Estimated Risk (Matched sample)',
                                           'Actual Recidivism (Matched sample)'),
                               indicator = 1:2)
summary_df

interval <- -qnorm((1 - 0.95) / 2)

plot <- ggplot(summary_df) +
  geom_hline(yintercept = 0, color = gray(.5), lty = 2) +
  
  geom_pointrange(aes(x = modname, y = coefficient,
                      ymin = coefficient - se * interval, ymax = coefficient + se * interval),
                  lwd = .5, shape = 21, fill = 'white') +
  xlab('Dependent variable') + ylab('Treatment Effect Estimate') +
  labs(caption = paste('Note: Circles are difference-in-proportion estimates; lines ',
                       'are 95% confidence intervals.',
                       sep = '')) + theme(plot.caption = element_text(hjust = 0)) + coord_flip()
plot


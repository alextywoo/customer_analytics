rm(list=ls())   # Clean environment
xbox_live <- read.csv('xbox_live.csv')

library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(skimr)

representative_summary <-  xbox_live %>%
  filter(representative == 1) %>%
  select(africa, asia, consldays, 
         crackle, creditaa, ctrlrs, custcare,
         disneyp, europe, gchange, hulu, 
         mchange, mcycle, mdrops, mmins, 
         months, namerica, netflix, numgames, 
         oceania, over18, refurb, retired, 
         rural, rv, spotify, 
         telemundo, truck, youtube) %>%
  summarise_all(list(temp_mean = mean, temp_sd = sd, temp_min = min, temp_max = max)) %>%
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_temp_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, min, max)

representative_summary


#question 1
orig_prob <- 0.0872
reweighted_prob <- 0.5
xbox_live$sweight <- ifelse(xbox_live$churn == 1,
                            orig_prob/reweighted_prob,
                            (1-orig_prob)/(1-reweighted_prob))

logit_churn1 <- glm(churn ~ africa + asia + consldays
                   + crackle + creditaa + ctrlrs +custcare
                   + disneyp + europe + gchange + hulu
                   + mchange + mcycle + mdrops + mmins
                   + months + namerica + netflix + numgames
                   + oceania + over18 + refurb + retired
                   + rural + rv + spotify
                   + telemundo + truck + youtube,
                   family = binomial(link = 'logit'),
                   weight = sweight, data = subset(xbox_live, xbox_live$training == 1))




library(sandwich)
library(lmtest)
coeftest(logit_churn1, vcov = vcovHC(logit_churn1, type = 'HC1'))

exp(logit_churn1$coef)


logit_churn_variablenames <- variable.names(logit_churn1)
logit_churn_pvalues <- coeftest(logit_churn1, vcov = vcovHC(logit_churn1, type="HC1"))[,4]
logit_churn_oddsratios <- exp(logit_churn1$coef)

xbox_live$churnpred1 <- predict(logit_churn1, xbox_live, type = 'response')


#validation to see if there is any over fitting
library('pROC')
Validation_set <- subset(xbox_live, training == 0)
roc_logit_vald <- roc(Validation_set$churn, Validation_set$churnpred1)
roc_logit_training <- roc(subset(xbox_live$churn, xbox_live$training ==1),
                          subset(xbox_live$churnpred1, xbox_live$training ==1))
auc(roc_logit_vald)
auc(roc_logit_training)

#chart prediction over representative data
xbox_live_rep <- subset(xbox_live, representative ==1)
xbox_live_rep$churn_prob_30 <- 31 - ntile(xbox_live_rep$churnpred1, 30)

ggplot(xbox_live_rep) +
  stat_summary(aes(x = churn_prob_30, y = churnpred1, color = 'predicted'), fun = 'mean', geom = 'line') +
  stat_summary(aes(x = churn_prob_30, y = churn, color = 'actual'), fun = 'mean', geom = 'line') +
  xlab('churn probability quantile') + 
  ylab('churn probability')


#create a table for important variables

# Create a summary table where each row is an explanatory variable
variable_summary1 <- tibble(
  variable = logit_churn_variablenames,
  odds_ratio = logit_churn_oddsratios,
  p_value = logit_churn_pvalues
  
)

View(variable_summary1)

# Drop the first row (for the intercept)
variable_summary2 <- slice(variable_summary1, 2:nrow(variable_summary1))

# Arrange the variables alphabetically
variable_summary2 <- variable_summary2 %>% arrange(variable)

# Include the standard deviations (from the representative sample)
variable_summary2$std_dev <- representative_summary$sd

# Denote dummy vs. non-dummy variables
variable_summary2$dummy <- 
  ifelse(variable_summary2$variable == "africa", 1,
         ifelse(variable_summary2$variable == "asia", 1,
                ifelse(variable_summary2$variable == "crackle", 1,                
                       ifelse(variable_summary2$variable == "creditaa", 1,  
                              ifelse(variable_summary2$variable == "disneyp", 1,  
                                     ifelse(variable_summary2$variable == "europe", 1,
                                            ifelse(variable_summary2$variable == "hulu", 1,
                                                   ifelse(variable_summary2$variable == "mcycle", 1,
                                                          ifelse(variable_summary2$variable == "namerica", 1,
                                                                 ifelse(variable_summary2$variable == "netflix", 1,
                                                                        ifelse(variable_summary2$variable == "oceania", 1,
                                                                               ifelse(variable_summary2$variable == "over18", 1,
                                                                                      ifelse(variable_summary2$variable == "refurb", 1,
                                                                                             ifelse(variable_summary2$variable == "retired", 1,
                                                                                                    ifelse(variable_summary2$variable == "rural", 1,
                                                                                                           ifelse(variable_summary2$variable == "rv", 1,
                                                                                                                  ifelse(variable_summary2$variable == "spotify", 1,
                                                                                                                         ifelse(variable_summary2$variable == "telemundo", 1,
                                                                                                                                ifelse(variable_summary2$variable == "truck", 1,
                                                                                                                                       ifelse(variable_summary2$variable == "youtube", 1,0 ))))))))))))))))))))

# Calculate the OR ^ (2 * SD): 
## only for significant & non-dummy variables
## missing for non-significant or for dummy variables
variable_summary2$OR_2_SD <- 
  ifelse(variable_summary2$dummy == 0 & variable_summary2$p_value < 0.05,
         variable_summary2$odds_ratio ^ (2 * variable_summary2$std_dev),
         NA)

# Calculate the importance, and create missing values if the variable is non-significant
variable_summary2$X_orig <- ifelse(variable_summary2$dummy == 1,
                                   variable_summary2$odds_ratio,
                                   variable_summary2$OR_2_SD)

variable_summary2$X <- ifelse(variable_summary2$p_value < 0.05,
                              variable_summary2$X_orig,
                              NA)

variable_summary2$importance <- ifelse(variable_summary2$X > 1,
                                       variable_summary2$X,
                                       1 / variable_summary2$X)

View(variable_summary2)


# Get rid of the "X" and "X_orig" variables and sort the remaining variables by alphabetical order
variable_summary3 <- variable_summary2 %>%
  select(variable, odds_ratio, p_value, std_dev, dummy, OR_2_SD, importance) %>%
  arrange(variable)

View(variable_summary3)


# Keep the significant variables and sort the remaining variables by importance
variable_summary4 <- variable_summary3 %>%
  filter(p_value < 0.05) %>%
  arrange(desc(importance))

View(variable_summary4)


# Keep only the variable name and importance; add a new column for whether the effect is positive or negative
variable_summary5 <- variable_summary4 %>%
  mutate(effect = ifelse(odds_ratio > 1, "+", "-")) %>%
  select(variable, importance, effect)

View(variable_summary5)


#Test action effect

#action 1 on console days
#Targeting rule: people who have not purchase new consoles over two years (>=730 days)

xbox_live$consldays_orig <- xbox_live$consldays

xbox_live$consldays <- xbox_live$consldays_orig * 0.8

xbox_live$churnpred2 <- predict.glm(logit_churn1, xbox_live, type = "response")

xbox_live %>%
  filter(representative == 1) %>%
  filter(consldays_orig >= 730) %>%
  select(churnpred1, churnpred2) %>%
  summary()

#action 2 on mchange - increase by 60 minutes   

xbox_live$consldays <- xbox_live$consldays_orig

xbox_live$mchange_orig <- xbox_live$mchange

xbox_live$mchange <- xbox_live$mchange_orig + 60

xbox_live$churnpred3 <- predict.glm(logit_churn1, xbox_live, type = 'response')

xbox_live %>%
  filter(representative ==1 ) %>%
  select(churnpred1, churnpred3) %>%
  summary()






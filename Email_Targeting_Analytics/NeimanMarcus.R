rm(list=ls())   #Clean environment
neiman_marcus <- read.csv('neiman_marcus.csv')
install.packages("sandwich")
install.packages("lmtest")
library(dplyr)
neiman_marcus$sweight <- ifelse(neiman_marcus$click_and_purch == 1, 0.0062/0.5, (1-0.0062)/(1-0.5))
neiman_marcus$age <- as.factor(neiman_marcus$age)
neiman_marcus$region <- as.factor(neiman_marcus$region)
library(sandwich)
library(lmtest)
#1
#clothing
logit_womens_clothing <- glm(click_and_purch ~ age + female + income + 
                               education + children + region + numitems_womens_clothing +
                               numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                               numitems_home +
                               age * female + income * children + age * income + income * region,
                             family = binomial(link = 'logit'), 
                             weight = sweight,
                             data = subset(neiman_marcus, training == 1 & message == 'womens_clothing'))

coeftest(logit_womens_clothing, vcov = vcovHC(logit_womens_clothing, type = 'HC1'))
exp(logit_womens_clothing$coefficient)
                             
neiman_marcus$p_womens_clothing <- predict(logit_womens_clothing, neiman_marcus, type = 'response')
                             
#shoes

logit_shoes <- glm(click_and_purch ~ age + female + income + 
                               education + children + region + numitems_womens_clothing +
                               numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                               numitems_home +
                               age * female + income * children + age * income + income * region,
                             family = binomial(link = 'logit'),
                            weight = sweight,
                             data = subset(neiman_marcus, training == 1 & message == 'shoes'))

coeftest(logit_shoes, vcov = vcovHC(logit_shoes, type = 'HC1'))
exp(logit_shoes$coefficient)
neiman_marcus$p_shoes <- predict(logit_shoes, neiman_marcus, type = 'response')

#handbag
logit_handbags <- glm(click_and_purch ~ age + female + income + 
                     education + children + region + numitems_womens_clothing +
                     numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                     numitems_home +
                     age * female + income * children + age * income + income * region,
                   family = binomial(link = 'logit'),
                   weight = sweight,
                   data = subset(neiman_marcus, training == 1 & message == 'handbags'))

coeftest(logit_handbags, vcov = vcovHC(logit_handbags, type = 'HC1'))
exp(logit_handbags$coefficient)
neiman_marcus$p_handbags <- predict(logit_handbags, neiman_marcus, type = 'response')

#jewelry

logit_jewelry <- glm(click_and_purch ~ age + female + income + 
                     education + children + region + numitems_womens_clothing +
                     numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                     numitems_home +
                     age * female + income * children + age * income + income * region,
                   family = binomial(link = 'logit'),
                   weight = sweight,
                   data = subset(neiman_marcus, training == 1 & message == 'jewelry'))

coeftest(logit_jewelry, vcov = vcovHC(logit_jewelry, type = 'HC1'))
exp(logit_jewelry$coefficient)

neiman_marcus$p_jewelry <- predict(logit_jewelry, neiman_marcus, type = 'response')

#beauty
logit_beauty <- glm(click_and_purch ~ age + female + income + 
                       education + children + region + numitems_womens_clothing +
                       numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                       numitems_home +
                       age * female + income * children + age * income + income * region,
                     family = binomial(link = 'logit'),
                    weight = sweight,
                     data = subset(neiman_marcus, training == 1 & message == 'beauty'))

coeftest(logit_beauty, vcov = vcovHC(logit_beauty, type = 'HC1'))
exp(logit_beauty$coefficient)

neiman_marcus$p_beauty <- predict(logit_beauty, neiman_marcus, type = 'response')



#men
logit_men <- glm(click_and_purch ~ age + female + income + 
                      education + children + region + numitems_womens_clothing +
                      numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                      numitems_home +
                      age * female + income * children + age * income + income * region,
                    family = binomial(link = 'logit'),
                 weight = sweight,
                    data = subset(neiman_marcus, training == 1 & message == 'men'))

coeftest(logit_beauty, vcov = vcovHC(logit_beauty, type = 'HC1'))
exp(logit_men$coefficient)

neiman_marcus$p_men <- predict(logit_men, neiman_marcus, type = 'response')



#home
logit_home <- glm(click_and_purch ~ age + female + income + 
                   education + children + region + numitems_womens_clothing +
                   numitems_shoes + numitems_handbags + numitems_jewelry + numitems_beauty + numitems_men +
                   numitems_home +
                   age * female + income * children + age * income + income * region,
                 family = binomial(link = 'logit'),
                 weight = sweight,
                 data = subset(neiman_marcus, training == 1 & message == 'home'))

coeftest(logit_beauty, vcov = vcovHC(logit_beauty, type = 'HC1'))
exp(logit_home$coefficient)

neiman_marcus$p_home <- predict(logit_home, neiman_marcus, type = 'response')





#find the max

neiman_marcus$p_max <- pmax(neiman_marcus$p_beauty, neiman_marcus$p_handbags, neiman_marcus$p_home,
                            neiman_marcus$p_jewelry, neiman_marcus$p_men, neiman_marcus$p_shoes,
                            neiman_marcus$p_womens_clothing)

neiman_marcus$email_offer <- ifelse(neiman_marcus$p_max == neiman_marcus$p_beauty, "beauty",
                         ifelse(neiman_marcus$p_max == neiman_marcus$p_handbags, "handbags",
                                ifelse(neiman_marcus$p_max == neiman_marcus$p_home, "home",
                                       ifelse(neiman_marcus$p_max == neiman_marcus$p_jewelry, "jewelry",
                                              ifelse(neiman_marcus$p_max == neiman_marcus$p_men, "men",
                                                     ifelse(neiman_marcus$p_max == neiman_marcus$p_shoes, "shoes",'womens_clothing'))))))

#Validation to see if auc on validation is way lower than auc training
library('pROC')
Validation_set <- subset(neiman_marcus, training == 0)
roc_logit_vald <- roc(Validation_set$click_and_purch, Validation_set$p_max)
roc_logit_training <- roc(subset(neiman_marcus$click_and_purch, neiman_marcus$training ==1),
                          subset(neiman_marcus$p_max, neiman_marcus$training ==1))
auc(roc_logit_vald)
auc(roc_logit_training)



#2
table(subset(neiman_marcus$email_offer, neiman_marcus$representative == 1))
table(subset(neiman_marcus$email_offer, neiman_marcus$representative == 1))* 
100/nrow(subset(neiman_marcus, neiman_marcus$representative == 1))


#3
neiman_marcus$max_prof_exp <- neiman_marcus$p_max * neiman_marcus$os_total* 0.4

#4 each customer's expected profit would be maximized if purchase rate is maximized, given a fixed amount of
# os_total and 40% profit margin ratio

table(neiman_marcus$email_offer)
table(neiman_marcus$email_offer) * 100/nrow(neiman_marcus)

#5 
avg_prof_max <- mean(subset(neiman_marcus$max_prof_exp,neiman_marcus$representative == 1))

#6
neiman_marcus$exp_beauty <- neiman_marcus$p_beauty * neiman_marcus$os_total * 0.4
neiman_marcus$exp_handbags <- neiman_marcus$p_handbags * neiman_marcus$os_total * 0.4
neiman_marcus$exp_home <- neiman_marcus$p_home * neiman_marcus$os_total * 0.4
neiman_marcus$exp_jewelry <- neiman_marcus$p_jewelry * neiman_marcus$os_total * 0.4
neiman_marcus$exp_men <- neiman_marcus$p_men * neiman_marcus$os_total * 0.4
neiman_marcus$exp_shoes <- neiman_marcus$p_shoes * neiman_marcus$os_total * 0.4
neiman_marcus$exp_womens_clothing <- neiman_marcus$p_womens_clothing * neiman_marcus$os_total * 0.4

avg_prof_beauty <- mean(subset(neiman_marcus$exp_beauty, neiman_marcus$representative == 1))
avg_prof_handbags <- mean(subset(neiman_marcus$exp_handbags, neiman_marcus$representative == 1))
avg_prof_home <- mean(subset(neiman_marcus$exp_home, neiman_marcus$representative == 1))
avg_prof_jewelry <- mean(subset(neiman_marcus$exp_jewelry, neiman_marcus$representative == 1))
avg_prof_men <- mean(subset(neiman_marcus$exp_men, neiman_marcus$representative == 1))
avg_prof_shoes <- mean(subset(neiman_marcus$exp_shoes, neiman_marcus$representative == 1))
avg_prof_womens_clothing <- mean(subset(neiman_marcus$exp_womens_clothing, neiman_marcus$representative == 1))

#7 take an average of 7 probabilities
neiman_marcus$p_random <- (neiman_marcus$p_beauty + neiman_marcus$p_handbags+neiman_marcus$p_home+
                              neiman_marcus$p_jewelry+neiman_marcus$p_men+neiman_marcus$p_shoes+
                              neiman_marcus$p_womens_clothing)/7

neiman_marcus$exp_random <- neiman_marcus$p_random * neiman_marcus$os_total * 0.4
avg_prof_random <- mean(subset(neiman_marcus$exp_random, neiman_marcus$representative == 1))

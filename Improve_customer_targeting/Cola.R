rm(list=ls())   # Clean environment

# Reading data
Cola_test <- read.csv("cocacola.csv", stringsAsFactors = FALSE)
library(dplyr)

mean(Cola_test$buy1)

##neural network

library(neuralnet)

Cola_test$typenum = ifelse(Cola_test$type == 'fastcausual', 1, ifelse(Cola_test$type == 'quickservice', 2, 3))

Cola_v1 <- Cola_test %>% select(zip,speeddown, speedup, last, numords, dollars, sincepurch, oldmodel, 
                                refurb, eightvalve, typenum,
                                income, medhvalue, buy1, training)

Cola_max = apply(Cola_v1, 2, max)
Cola_min = apply(Cola_v1, 2, min)

Cola_scaled <- data.frame(scale(Cola_v1, center = Cola_min, scale = Cola_max - Cola_min))

set.seed(123)

Cola_nn <- neuralnet(buy1 ~ zip + speeddown + speedup + last + numords + dollars + sincepurch + oldmodel + refurb + 
                       eightvalve + income + typenum +
                       medhvalue, subset(Cola_scaled, training == 1), hidden = 3, linear.output = F)

Cola_covariates <- Cola_scaled %>% select(zip, speeddown, speedup, last, numords, dollars, sincepurch, oldmodel, refurb,
                                            eightvalve, income, typenum, medhvalue)

Cola_nn_pred <- neuralnet::compute(Cola_nn, Cola_covariates)

Cola_v1$pred_nn <- Cola_nn_pred$net.result

Cola_test$buy2_nn <- as.vector(Cola_v1$pred_nn)

##logit1
Cola_test$typenum = ifelse(Cola_test$type == 'fastcausual', 1, ifelse(Cola_test$type == 'quickservice', 2, 3))

Cola_logit <- glm(buy1 ~ zip + last + numords + dollars + sincepurch + oldmodel + refurb + 
                    eightvalve + income + typenum, family = binomial(link = 'logit'), 
                  data = subset(Cola_test, training == 1))

summary(Cola_logit)

exp(Cola_logit$coefficients)

Cola_test$buy2_logit <- predict(Cola_logit, Cola_test, type = 'response')

#logit2
Cola_logit2 <- glm(buy1 ~ zip + last + numords + dollars + sincepurch + oldmodel + refurb + 
                    eightvalve + income + typenum + medhvalue, family = binomial(link = 'logit'), 
                  data = subset(Cola_test, training == 1))

summary(Cola_logit2)

exp(Cola_logit2$coefficients)

Cola_test$buy2_logit2 <- predict(Cola_logit2, Cola_test, type = 'response')

#logit3
Cola_logit3 <- glm(buy1 ~ zip + speeddown + speedup + last + numords + dollars + sincepurch + oldmodel + refurb + 
                     eightvalve + income + typenum + medhvalue, family = binomial(link = 'logit'), 
                   data = subset(Cola_test, training == 1))

summary(Cola_logit3)

exp(Cola_logit3$coefficients)

Cola_test$buy2_logit3 <- predict(Cola_logit3, Cola_test, type = 'response')

#logit4
Cola_logit4 <- glm(buy1 ~ zip + speeddown + speedup + last + numords + dollars + sincepurch + oldmodel + refurb + 
                     eightvalve + income + typenum + medhvalue + income*medhvalue, family = binomial(link = 'logit'), 
                   data = subset(Cola_test, training == 1))

summary(Cola_logit4)

exp(Cola_logit4$coefficients)

Cola_test$buy2_logit4 <- predict(Cola_logit4, Cola_test, type = 'response')

#logit5
Cola_logit5 <- glm(buy1 ~ zip + speeddown + speedup + last + numords + dollars + sincepurch + oldmodel + refurb + 
                     eightvalve + income + typenum + medhvalue + income*medhvalue + oldmodel*numords, family = binomial(link = 'logit'), 
                   data = subset(Cola_test, training == 1))

summary(Cola_logit5)

exp(Cola_logit5$coefficients)

Cola_test$buy2_logit5 <- predict(Cola_logit5, Cola_test, type = 'response')



##decision tree

library(rpart)
library(rpart.plot)

Cola_test$refurbed <- ifelse(Cola_test$refurb == 1, "refurbed", "new")
Cola_test$discontinued_model <- ifelse(Cola_test$oldmodel == 1, "Discontinued", "existing")
Cola_test$eightvalved <- ifelse(Cola_test$eightvalve == 1, "eight", "six")
Cola_test$offer1 <- ifelse(Cola_test$buy1 == 1, 'accepted', 'rejected')

Cola_test$state <- as.factor(Cola_test$state)
Cola_test$type <- as.factor(Cola_test$type)

buy2_tree1 <- rpart(offer1 ~ state + zip + speeddown + speedup + last + numords
                    + dollars + sincepurch + discontinued_model + 
                      refurbed + eightvalved + type + income + medhvalue,
                    data = subset(Cola_test, training == 1), method = 'class')

rpart.plot(buy2_tree1, type = 3, clip.right.labs = FALSE)
rpart.rules(buy2_tree1)

##linear regression

Cola_lm <- lm(buy1 ~  last + numords + dollars + sincepurch + oldmodel + refurb + 
                eightvalve + income + type, data = subset(Cola_test, training == 1))

summary(Cola_lm)

##Validation

library(pROC)
Validation_set <- subset(Cola_test, training == 0)

roc_nn <- roc(Validation_set$buy1, Validation_set$buy2_nn)
roc_logit <- roc(Validation_set$buy1, Validation_set$buy2_logit)
roc_logit2 <- roc(Validation_set$buy1, Validation_set$buy2_logit2)
roc_logit3 <- roc(Validation_set$buy1, Validation_set$buy2_logit3)
roc_logit4 <- roc(Validation_set$buy1, Validation_set$buy2_logit4)
roc_logit5 <- roc(Validation_set$buy1, Validation_set$buy2_logit5)

  
auc(roc_nn)
auc(roc_logit)
auc(roc_logit2)
auc(roc_logit3)
auc(roc_logit4)
auc(roc_logit5)

library(ggplot2)
roc.list <- roc(buy1 ~ buy2_nn + buy2_logit + buy2_logit2 + buy2_logit3 + buy2_logit4 + buy2_logit5, data = Validation_set)
ggroc(roc.list, legacy.axes = TRUE) + xlab('False Positive Rate') + ylab('True Positive Rate')


#Visualize logit5's performance


#Decide whom to target
breakeven_rate <- 20/786
Cola_test$wave2_pred <- 0.5*Cola_test$buy2_logit5
Cola_test$target_wave2 <- ifelse(Cola_test$wave2_pred>breakeven_rate & Cola_test$buy1 == 0, 1, 0)

#Project Profit
validation_target <- subset(Cola_test, Cola_test$target_wave2 == 1 & Cola_test$training == 0)
validation_target_num <- nrow(subset(Cola_test, Cola_test$target_wave2 == 1 & Cola_test$training == 0))
res_avg_valid <- mean(validation_target$wave2_pred)
Profit_validation <- (786 * validation_target_num * res_avg_valid - 20 * validation_target_num)
ROI <- Profit_validation / (validation_target_num * 20)
Profit_total <- Profit_validation/15000*50000




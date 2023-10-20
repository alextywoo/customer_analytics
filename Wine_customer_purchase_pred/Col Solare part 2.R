rm(list=ls())   # Clean environment
setwd("~/OneDrive - UW/MKTG 562/Team assignment 01")
ColSolare <- read.csv('ColSolare.csv')
#1
ColSolare$restaurant <- ifelse(ColSolare$customer_type == 'restaurant', 1, 0)
ColSolarelogit <- glm(buyer ~ last_purch + dollars +
                        restaurant + customer_sqft + cab_franc +
                        cab_sauvignon + malbec + merlot + red_blend + syrah,
                      family = binomial(link='logit'), data = ColSolare)
summary(ColSolarelogit)

ColSolare$purch_prob <- predict.glm(ColSolarelogit,
                                    ColSolare, type = "response")
#2
ColSolare$purch_odds <- ColSolare$purch_prob/(1-ColSolare$purch_prob)
exp(ColSolarelogit$coefficients)

#3
ColSolare$purch_prob_decile <- 11- ntile(ColSolare$purch_prob,10)

#4
library(ggplot2)

ggplot(ColSolare) + stat_summary(aes(x = ColSolare$purch_prob_decile, 
                                 y = ColSolare$buyer), fun = 'mean', geom = 'bar')

#5
library(dplyr)

Response_decile <- ColSolare %>%
  group_by(purch_prob_decile) %>%
  summarise(customer_num = length(customer_id), 
            purchase_num = sum(buyer),
            response_pct = mean(buyer))

#6
ColSolarelogit_mer <- glm(buyer ~ merlot, family = binomial(link = 'logit'), data = ColSolare)
summary(ColSolarelogit_mer)

exp(ColSolarelogit_mer$coefficients)

#7
Response_decile$lift <- Response_decile$response_pct/(sum(Response_decile$purchase_num)/sum(Response_decile$customer_num))
Response_decile$cum_lift <- 
  
#8

#11
response_break_even = 30/(720-160)

#12
target_mailto_list <- subset(ColSolare, purch_prob > response_break_even)
#a
target_mailto_num = 120000*(nrow(subset(ColSolare, purch_prob > response_break_even))/40000)

#b
est_repsonse_pct = mean(target_mailto_list$buyer)
#c
exp_profit = target_mailto_num * est_repsonse_pct * (720-160) - 30*target_mailto_num

#d
exp_roi = exp_profit / (30*target_mailto_num)

#14
ColSolareLR <-  lm(dollars ~ first_purch + malbec +
                      merlot + syrah, data = ColSolare)
summary(ColSolareLR)
rm(list=ls())
setwd('~/OneDrive - UW/MKTG 562/Case assignment_Col Solare part 1')
test_campaign <- read.csv('~/OneDrive - UW/MKTG 562/Case assignment_Col Solare part 1/ColSolare.csv')
num_restaurant <- nrow(subset(test_campaign, customer_type == 'restaurant')) 
num_total <- nrow(test_campaign)
pct_restaurant <- 100*num_restaurant/num_total

library(dplyr)
customer_bystate <- test_campaign %>% count(state, sort = TRUE)
customer_bystate$pct <- customer_bystate$n / sum(customer_bystate$n)

avg_summary <- test_campaign %>% summarise(
              avg_totspd = mean(dollars), 
              avg_cases = mean(cases),
              avg_purchfrq = mean(last_purch))

test_campaign$avg_price <- test_campaign$dollars/test_campaign$cases
test_campaign %>% summarise(min_prc = min(avg_price), 
                            max_prc = max(avg_price),
                            avg_prc = mean(avg_price))

unit_sold_type <- test_campaign %>% summarise(franc_totl = sum(cab_franc),
                                              sauvignon_totl = sum(cab_sauvignon),
                                              malbec_totl = sum(malbec),
                                              merlot_totl = sum(merlot),
                                              blend_totl = sum(red_blend),
                                              syrah_totl = sum(syrah))
max(unit_sold_type)
min(unit_sold_type)

library(ggplot2)


red_blend_customer <- subset(test_campaign, test_campaign$red_blend > 0)

redblend_rest <- length(subset(red_blend_customer$customer_type, red_blend_customer$customer_type == 'restaurant'))
redblend_bar <- length(subset(red_blend_customer$customer_type, red_blend_customer$customer_type == 'bar'))


red_blend_ratio <- test_campaign %>% 
  summarise(red_blend_R = nrow(subset(test_campaign, test_campaign$red_blend > 0 & customer_type == 'restaurant' )),
            totalR = nrow(subset(test_campaign, customer_type == 'restaurant' )),
            red_blend_B = nrow(subset(test_campaign, test_campaign$red_blend > 0 & customer_type == 'bar' )),
            totalB = nrow(subset(test_campaign, customer_type == 'bar' )))

red_blend_ratio <- test_campaign %>% 
  group_by(customer_type) %>%
  summarise(red_blend = nrow(subset(test_campaign, test_campaign$red_blend > 0)),
            total = nrow(test_campaign))

ifelse(test_campaign$red_blend > 0, test_campaign$red_blend_positive <- 1, test_campaign$red_blend_positive <- 0)
test_campaign$red_blend_positive <- ifelse(test_campaign$red_blend > 0, 1, 0)


test_campaign$red_blend_positive <- test_campaign$red_blend/test_campaign$red_blend
test_campaign$red_blend_positive [is.nan(test_campaign$red_blend_positive)] <- 0

ggplot(test_campaign) + 
  stat_summary(aes(x = customer_type, y = red_blend_positive), fun = 'mean', geom = "bar") +
  ylab("proportion bought 2016 Red Blend")

red_blend_ratio <- test_campaign %>% 
  group_by(customer_type) %>%
  summarise(red_blend_portion = mean(red_blend_positive))
#6
library(ggplot2)

ggplot(test_campaign) + 
  stat_summary(aes(x = customer_type, y = buyer), fun = 'mean', geom = "bar") +
  ylab("proportion bought 2016 Red Blend")

red_blend_ratio <- test_campaign %>% 
  group_by(customer_type) %>%
  summarise(red_blend_portion = mean(buyer))

#7

purcahse_summary <- test_campaign %>%
  group_by(customer_type) %>%
  summarise(total_wine_purchased = sum(cases),
            avg_wine_purchased = mean(cases))

#8

test_campaign$purchase_gap <- test_campaign$first_purch - test_campaign$last_purch
summary(test_campaign$purchase_gap)
min(test_campaign$purchase_gap)
max(test_campaign$purchase_gap)
mean(test_campaign$purchase_gap)

#9
past_customer_2c <- subset(test_campaign, cases >= 2)
nrow(subset(past_customer_2c, buyer == 1)) / nrow(past_customer_2c)

#10
cor(test_campaign$customer_sqft, test_campaign$cases)

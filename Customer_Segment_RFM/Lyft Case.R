#1
rm(list=ls())
test_campaign <- read.csv("Lyft.csv", stringsAsFactors = FALSE)
summary(test_campaign)
offer_used_pct <- mean(test_campaign$used_offer)
offer_used_pct
#2
promo_cust <- subset(test_campaign, test_campaign$used_offer == 1)
mean(promo_cust$ride_fare)
#3
library(ggplot2)
library(dplyr)
test_campaign$rec_quin <- ntile(test_campaign$rec,5)
test_campaign$freq_quin <- 6 - ntile(test_campaign$freq,5)
test_campaign$mon_quin <- 6 - ntile(test_campaign$mon,5)
ggplot(test_campaign) + stat_summary(aes(x = rec_quin, y = used_offer), fun='mean', geom = 'bar')
ggplot(test_campaign) + stat_summary(aes(x = freq_quin, y = used_offer), fun = 'mean', geom = 'bar') + ylab('response rate')
ggplot(test_campaign) + stat_summary(aes(x = mon_quin, y = used_offer), fun = 'mean', geom = 'bar' ) + ylab('response rate')

#4
promo_cust <- subset(test_campaign, test_campaign$used_offer == 1)

ggplot(promo_cust) + stat_summary(aes(x = rec_quin, y = ride_fare), fun = 'mean', geom = 'bar') + ylab('average ride fare')

ggplot(promo_cust) + stat_summary(aes(x = freq_quin, y = ride_fare), fun = 'mean', geom = 'bar') + ylab('average ride fare')

ggplot(promo_cust) + stat_summary(aes(x = mon_quin, y = ride_fare), fun ='mean', geom ='bar') + ylab('average ride fare')

#5
#6
break_even_rate <- 1.14/(0.75*mean(promo_cust$ride_fare)*0.38)
#7
21169259*offer_used_pct*mean(promo_cust$ride_fare)*0.75*0.38 - 1.14*21169259

21169259*9.62/100*19.94*0.38 - 1.14*21169259

-12558307/(21169259*1.14)

#8
test_campaign <- test_campaign %>%
    group_by(rfm_seq) %>%
  mutate(response_rate = mean(used_offer))
test_campaign$sendto_iq <- ifelse(test_campaign$response_rate > break_even_rate, 1, 0)
mean(test_campaign$sendto_iq)
total_targeted_user <- mean(test_campaign$sendto_iq) * 21169259


mean(subset(test_campaign, sendto_iq == 1)$response_rate)
buyer_num <- total_targeted_user * mean(subset(test_campaign, sendto_iq == 1)$response_rate)

profit_est <- 19.94*0.75*0.38*buyer_num - 1.14*total_targeted_user

target_promo_return <- profit_est / (1.14*total_targeted_user)


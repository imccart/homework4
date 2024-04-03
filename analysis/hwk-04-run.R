
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 4 Answers
## Author:        Ian McCarthy
## Date Created:  3/26/2020
## Date Edited:   4/3/2024
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               modelsummary, fixest, kableExtra, broom, rdd, rdrobust, rddensity, cobalt)


# Read data and set workspace for knitr -------------------------------
ma.data <- readRDS('data/output/final_ma_data.rds')


# Create objects for markdown ---------------------------------------------

## Summary, Question 1
f <- function(x) {
  r <- quantile(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))
  names(r) <- c("ymin","lower","middle","upper","ymax")
  r
}

final.plan.plot <- ma.data %>% 
  group_by(fips, year) %>% 
  select(fips, year) %>% summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year),y=plan_count)) + 
  stat_summary(fun.data=f, geom="boxplot") +
  labs(
    x="Year",
    y="Number of Plans"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()


## Summary, Question 2
ratings.years <- ma.data %>% 
  filter(year==2010 | year==2012 | year==2015, !is.na(Star_Rating)) %>%
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar(aes(fill=as.factor(year)), position=position_dodge(preserve="single"), width=0.75) +
  scale_fill_grey() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    fill = "Year"
  ) + theme_bw() +
  scale_y_continuous(labels = comma)


## Summary, Question 3
avg.benchmark <- ma.data %>% group_by(ssa, year) %>%
  ggplot(aes(x=as.factor(year),y=ma_rate, group=1)) + 
  stat_summary(fun="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Benchmark Payments ($)"
  ) + scale_y_continuous(labels=comma, limits=c(600,900)) +
  theme_bw()


## Summary, Question 4
ma.mkt.data <- ma.data %>% group_by(fips, year) %>% 
  summarize(enroll=first(avg_enrolled), medicare=first(avg_eligibles), bench=mean(ma_rate, na.rm=TRUE)) %>%
  mutate(mkt_share=enroll/medicare)

ma.share <- ma.mkt.data %>% 
  ggplot(aes(x=as.factor(year), y=mkt_share, group=1)) +
  stat_summary(fun="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="MA Market Share"
  ) + theme_bw()

share.reg <- lm(mkt_share~bench, data=ma.mkt.data)



## ATE, Question 1
ma.data.clean <- ma.data %>% ungroup() %>%
  filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
          avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
          bid, avg_ffscost, ma_rate, plan_type, partd) %>%
    mutate(mkt_share = avg_enrollment/avg_eligibles,
           HMO=str_detect(plan_type,"HMO"))

ma.rounded <- ma.data.clean %>%
  mutate(rounded_30=ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0),
         rounded_35=ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45=ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0),
         rounded_50=ifelse(raw_rating>=4.75 & raw_rating<5.00 & Star_Rating==5,1,0)) %>%
  group_by(Star_Rating) %>% filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>%
  summarize(count_30=sum(rounded_30), 
            count_35=sum(rounded_35), 
            count_40=sum(rounded_40),
            count_45=sum(rounded_45),
            count_50=sum(rounded_50)) %>%
  mutate(rounded=count_30+count_35+count_40+count_45+count_50) %>%
  select(Star_Rating, rounded)


## ATE, Questions 2-3
star30   <- lm(mkt_share ~ treat + score, 
               data=(ma.data.clean %>% 
                       filter(raw_rating>=(2.75-0.125), 
                              raw_rating<=(2.75+0.125),
                              Star_Rating %in% c(2.5, 3.0)) %>%
                       mutate(treat=(Star_Rating==3.0),
                              score=raw_rating-2.75)))

star35   <- lm(mkt_share ~ treat + score, 
               data=(ma.data.clean %>% 
                       filter(raw_rating>=(3.25-0.125), 
                              raw_rating<=(3.25+0.125),
                              Star_Rating %in% c(3.0, 3.5)) %>%
                       mutate(treat=(Star_Rating==3.5),
                              score=raw_rating-3.25)))


for (h in seq(0.1, 0.15, 0.01)) {
  star30bw   <- lm(mkt_share ~ treat + score, 
                 data=(ma.data.clean %>% 
                         filter(raw_rating>=(2.75-h), 
                                raw_rating<=(2.75+h),
                                Star_Rating %in% c(2.5, 3.0)) %>%
                         mutate(treat=(Star_Rating==3.0),
                                score=raw_rating-2.75)))
  coef.30 <- tidy(star30bw, conf.int=TRUE) %>% mutate(rating=30)
  
  star35bw   <- lm(mkt_share ~ treat + score, 
                 data=(ma.data.clean %>% 
                         filter(raw_rating>=(3.25-h), 
                                raw_rating<=(3.25+h),
                                Star_Rating %in% c(3.0, 3.5)) %>%
                         mutate(treat=(Star_Rating==3.5),
                                score=raw_rating-3.25)))
  
  coef.35 <- tidy(star35bw, conf.int=TRUE) %>% mutate(rating=35)
    
  est.collect <- rbind(coef.30, coef.35) %>%
    mutate(bandwidth=h)

  if (h==0.1) {
    est.final <- est.collect
  }
  else {
    est.final <- rbind(est.final,est.collect)
  }

}

rd.estimates <- est.final %>% filter(term=="treatTRUE") %>%
  ggplot(aes(x=as.factor(bandwidth),y=estimate,shape=as.factor(rating))) +
  geom_hline(aes(yintercept=0),linetype="dashed") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x="Bandwidth",
    shape="Star Rating") +
  geom_point(size=3,position=position_dodge(width=0.5)) +
  theme_bw()


## ATE, Question 4
kd.running30 <- ma.data.clean %>%
  filter( (raw_rating>=2.75-.125 & Star_Rating==2.5) | 
          (raw_rating<=2.75+.125 & Star_Rating==3)) %>%
  ggplot(aes(x=raw_rating)) + 
  geom_density() + 
  geom_vline(xintercept=2.75, linetype='dashed') + 
  labs(
    x="Running Variable",
    y="Number of Plans"
  ) + scale_x_continuous(breaks=seq(2.60,2.90,0.05),limits=c(2.6,2.90)) + theme_bw()

kd.running35 <- ma.data.clean %>%
  filter( (raw_rating>=3.25-.125 & Star_Rating==3) | 
            (raw_rating<=3.25+.125 & Star_Rating==3.5)) %>%
  ggplot(aes(x=raw_rating)) + 
  geom_density() + 
  geom_vline(xintercept=3.25, linetype='dashed') +   
  labs(
    x="Running Variable",
    y="Number of Plans"
  ) + scale_x_continuous(breaks=seq(3.10,3.40,0.05),limits=c(3.1,3.40)) + theme_bw()


#mcrary.data30 <- ma.data.clean %>% 
#  filter( (raw_rating>=2.75-.125 & Star_Rating==2.5) | 
#            (raw_rating<=2.75+.125 & Star_Rating==3) )

#mcrary.data35 <- ma.data.clean %>% 
#  filter( (raw_rating>=3.25-.125 & Star_Rating==3) | 
#          (raw_rating<=3.25+.125 & Star_Rating==3.5) )

#rd.35 <- rddensity(mcrary.data35$raw_rating, c=3.25, h=0.125, kernel="triangular")
#rd35.plot <- rdplotdensity(rd.35, mcrary.data35$raw_rating)$Estplot +
#  labs(x="Running Variable",
#       y="Density") +
#  scale_x_continuous(breaks=seq(3.1,3.5,0.05))


#mcrary.plot30 <- DCdensity(mcrary.data30$raw_rating, cutpoint=2.75, bw=0.125, bin=0.01)
#mcrary.plot35 <- DCdensity(mcrary.data35$raw_rating, cutpoint=3.25, bw=0.125, bin=0.01)


## ATE, Question 5 

lp.vars <- ma.data.clean %>% ungroup() %>%
  filter( (raw_rating>=2.75-.125 & Star_Rating==2.5) |
          (raw_rating<=2.75+.125 & Star_Rating==3) ) %>%
  mutate(rounded=(Star_Rating==3)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)
  
plot.30 <- love.plot(bal.tab(lp.covs,treat=lp.vars$rounded), colors="black", shapes="circle", threshold=0.1) + 
  theme_bw() + theme(legend.position="none")


lp.vars <- ma.data.clean %>% ungroup() %>%
  filter( (raw_rating>=3.25-.125 & Star_Rating==3) |
            (raw_rating<=3.25+.125 & Star_Rating==3.5) ) %>%
  mutate(rounded=(Star_Rating==3.5)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.35 <- love.plot(bal.tab(lp.covs,treat=lp.vars$rounded), colors="black", shapes="circle", threshold=0.1) + 
  theme_bw() + theme(legend.position="none")


rm(list=c("full.ma.data", "ma.data","ma.mkt.data", "ma.data.clean"))
save.image("assignments/Hwk4_workspace.Rdata")

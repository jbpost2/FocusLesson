#Recreate table of values
earnings <- data.frame(
  Age = c(18:64),
  High_School_Diploma = c(rep(18600, 2),
                          rep(22600, 5),
                          rep(29300, 5),
                          rep(31900, 5),
                          rep(36300, 5),
                          rep(37300, 5),
                          rep(40100, 5),
                          rep(41200, 5),
                          rep(41200, 5),
                          rep(40400, 5)),
  Some_College_No_Degree = c(0, 16600,
                             rep(23000, 5),
                             rep(31400, 5),
                             rep(37100, 5),
                             rep(41900, 5),
                             rep(45500, 5),
                             rep(47800, 5),
                             rep(49400, 5),
                             rep(49600, 5),
                             rep(49300, 5)),
  Associate_Degree = c(rep(0, 2),
                       rep(25600, 5),
                       rep(35400, 5),
                       rep(41200, 5),
                       rep(46600, 5),
                       rep(49500, 5),
                       rep(51800, 5),
                       rep(52300, 5),
                       rep(52600, 5),
                       rep(52300, 5)),
  Bachelor_Degree = c(rep(0, 4),
                      rep(35400, 3),
                      rep(46000, 5),
                      rep(55200, 5),
                      rep(65700, 5),
                      rep(70800, 5),
                      rep(74300, 5),
                      rep(75800, 5),
                      rep(73600, 5),
                      rep(70000, 5))
)

library(tidyverse)
long_earnings <- earnings %>% pivot_longer(2:5, names_to = "education", values_to = "earnings")

ggplot(long_earnings,
       aes(x = Age, y = earnings)) +
  geom_line(aes(color = education))


#need a four year weighted average
library(zoo)
rollapply(earnings$High_School_Diploma, 4, mean, fill = "extend")
long_earnings <- long_earnings %>% 
  group_by(education) %>%
  mutate(roll_earnings = rollapply(earnings, 5, mean, fill = "extend"))

ggplot(long_earnings,
       aes(x = Age, y = roll_earnings)) +
  geom_line(aes(color = education))

#ah, but that is just yearly earnings. Need to do cumulative earnings
long_earnings <- long_earnings %>%
  group_by(education) %>%
  mutate(cumulative_earnings = cumsum(earnings))

options(scipen = 999)
ggplot(long_earnings,
       aes(x = Age, y = cumulative_earnings)) +
  geom_line(aes(color = education)) +
  scale_y_continuous(labels = scales::comma)


#lastly, they take out the price of tuition, fees, and books and supplies
#repayment of loans...
#############
#        NOTES: Excludes bachelor’s degree recipients who earn advanced degrees. Assumes students borrow the cost of tuition and fees and books and supplies and pay it off over 10 years after graduation with a 4.45% annual interest rate during and after college. Tuition/loan payments and earnings are discounted at 3%, compounded every year beyond age 18. The 2020-21 price is projected using the 2019-20 price and a 3% annual increase.
#############

interest_rate <- 0.0445
t <- 10

cost_some_college <- 9230
cost_associate <- c(4960, 5070)
cost_bachelor <- c(18840, 19300, 19830, 20420)

#age 18 owe
cost_some_college*exp(interest_rate*0)
#age 19 owe, need to start paying off
cost_some_college*exp(interest_rate*1)


MP = 



cost_some_college*exp(interest_rate*2)
net_earnings









### read in data on loan by major and look at that
setwd("S:\\Documents\\repos")
cohorts_institution <- read_csv("Most-Recent-Cohorts-Institution.csv")

cohorts_field_of_study <- read_csv("Most-Recent-Cohorts-Field-of-Study.csv")

colnames(cohorts_field_of_study)
my_sub <- cohorts_field_of_study %>% 
  select(1:11, starts_with("DEBT_ALL_PP_EVAL")) %>%
  mutate(num_debt_n = if_else(DEBT_ALL_PP_EVAL_N  == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_EVAL_N)),
         num_debt_mean = ifelse(DEBT_ALL_PP_EVAL_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_EVAL_MEAN)),
         num_debt_median = ifelse(DEBT_ALL_PP_EVAL_MDN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_EVAL_MDN))) 

#Ok, I think that is a reasonable set of variables to look at
#Now let's look at the distribution of loan means for institution type
my_sub %>%
  filter(CONTROL == "Public") %>% 
  group_by(CIPCODE) %>%
  summarize(mean = mean(num_debt_mean, na.rm = TRUE))

#2701 - math
#2601 - biology
my_sub %>%
  filter(CONTROL == "Public", CIPCODE == 2601) %>%
  pull(num_debt_mean) %>%
  hist(breaks = 20)


my_sub %>%
  filter(CONTROL %in% c("Public", "Private, nonprofit")) %>%
  pull(num_debt_mean) %>%
  hist()
  
my_sub %>%
  filter(CONTROL %in% c("Public")) %>%
  pull(num_debt_mean) %>%
  hist()

my_sub %>%
  filter(CONTROL %in% c("Private, nonprofit")) %>%
  pull(num_debt_mean) %>%
  hist()

my_tab <- table(my_sub %>% 
                  filter(CREDLEV == 3) %>% 
                  pull(CIPDESC))

largest_programs <- names(my_tab[my_tab>100])
lots_data <- my_sub %>%
  filter(CIPDESC %in% largest_programs) %>% 
  group_by(CIPDESC) %>%
  summarize(not_missing = sum(!is.na(num_debt_n))) %>% 
  filter(not_missing > 30)

#Get schools with the most amount of avaiable data
institutions <- my_sub %>%
  group_by(INSTNM) %>%
  summarize(count = n()) %>%
  filter(count > 50)


my_sub %>%
  filter(CIPDESC %in% lots_data$CIPDESC) %>%
  mutate(mean_med_diff = num_debt_mean- num_debt_median) %>%
  pull(mean_med_diff) %>%
  hist(breaks = 20)


my_sub %>%
  filter(CIPDESC %in% lots_data$CIPDESC) %>%
  pull(num_debt_mean) %>%
  hist(breaks = 20)

my_sub %>%
  filter(CIPDESC == c("Biology, General.")) %>%
  pull(num_debt_median) %>%
  hist(breaks = 20)


my_sub %>%
  filter(CIPDESC == c("Civil Engineering.")) %>%
  pull(num_debt_mean) %>%
  hist(breaks = 10)

my_sub %>%
  filter(CIPDESC == c("Chemistry.")) %>%
  pull(num_debt_mean) %>%
  hist(breaks = 10)

my_sub %>%
  filter(CIPDESC == c("Social Work.")) %>%
  pull(num_debt_median) %>%
  hist(breaks = 10)

my_sub %>%
  filter(CIPDESC == c("History.")) %>%
  pull(num_debt_mean) %>%
  hist(breaks = 10)


#consider two different majors
my_sub %>%
  filter(CIPDESC %in% c("Biology, General.", "Mathematics."), CREDLEV == 3) %>%
  ggplot(aes(x = num_debt_mean, fill = CIPDESC)) +
  geom_histogram()

my_sub %>%
  filter(CIPDESC %in% c("Biology, General.", "Computer Science."), CREDLEV == 3) %>%
  ggplot(aes(x = num_debt_mean, fill = CIPDESC)) +
  geom_histogram()


#within an institution
my_sub %>%
  filter(INSTNM == "North Carolina State University at Raleigh", CREDLEV == 3) %>%
  pull(num_debt_mean) %>%
  hist(breaks = 8)
my_sub %>%
  filter(INSTNM == "North Carolina State University at Raleigh", CREDLEV == 3) %>%
  pull(num_debt_median) %>%
  hist(breaks = 8)



###########################3
my_sub_Pell <- cohorts_field_of_study %>% 
  select(1:11, starts_with("DEBT_PELL_PP_ANY")) %>%
  mutate(num_debt_n = if_else(DEBT_PELL_PP_ANY_N  == "PrivacySuppressed", NA, as.numeric(DEBT_PELL_PP_ANY_N)),
         num_debt_mean = ifelse(DEBT_PELL_PP_ANY_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_PELL_PP_ANY_MEAN)),
         num_debt_median = ifelse(DEBT_PELL_PP_ANY_MDN == "PrivacySuppressed", NA, as.numeric(DEBT_PELL_PP_ANY_MDN)))  %>%
  filter(CONTROL %in% c("Private, nonprofit", "Public"))

hist(my_sub_Pell$num_debt_mean, breaks = 20)

my_sub_Pell[which(my_sub_Pell$num_debt_mean > 60000), "CIPDESC"]


#within an institution
my_sub_Pell %>%
  filter(INSTNM == "North Carolina State University at Raleigh") %>%
  pull(num_debt_mean) %>%
  hist(breaks = 8)





###########################3
my_sub_Pell <- cohorts_field_of_study %>% 
  select(1:11, starts_with("DEBT_ALL_STGP_ANY")) %>%
  mutate(num_debt_n = if_else(DEBT_ALL_STGP_ANY_N  == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_STGP_ANY_N)),
         num_debt_mean = ifelse(DEBT_ALL_STGP_ANY_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_STGP_ANY_MEAN)),
         num_debt_median = ifelse(DEBT_ALL_STGP_ANY_MDN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_STGP_ANY_MDN)))  %>%
  filter(CONTROL %in% c("Private, nonprofit", "Public"))

hist(my_sub_Pell$num_debt_mean, breaks = 20)

my_sub_Pell[which(my_sub_Pell$num_debt_mean > 60000), "CIPDESC"]


#within an institution
my_sub_Pell %>%
  filter(INSTNM == "North Carolina State University at Raleigh") %>%
  pull(num_debt_mean) %>%
  hist(breaks = 8)









##########################
#After meeting thoughts
##########################
# Need to consider mean for each major and approximate sd for noramal curve
# means are fine to get (just use mean given for a particular major)
# sd, try to approximate by looking at the 

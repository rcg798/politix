##############################
##############################
#
# VOTER ANALYSIS
#
##############################
##############################

# load dependencies
library(dplyr)
library(ggplot2)

###############
#
# GET DATA
#
###############

# read in data 
# since r is case sensitive, let's just lowercase participant IDs while we're at it
# so later on our joins don't get messed up
wd <- getwd()
# for each ID, what exp group they got
rand_data <- read.csv(paste0(wd, "/candidate_rand.csv"), stringsAsFactors = F) %>%
  mutate(id=tolower(id))
# for each ID, their age/race/gender/partisan score if available
covar_data <- read.csv(paste0(wd, "/candidate_covariates.csv"), stringsAsFactors = F) %>%
  mutate(id=tolower(id))
# for each ID, their survey response (outcome) and if they voted (turnout) if available
outcome_data <- read.csv(paste0(wd, "/candidate_outcomes.csv"), stringsAsFactors = F) %>%
  mutate(id=tolower(id))

###############
#
# CLEAN DATA
#
###############

# is the randomization data duplicated? yes
# are IDs assigned to more than one treatment (due to buggy data)? no
rand_data %>% summarise(n()) # 280572 raw
rand_data %>% select(id) %>% distinct() %>% summarise(n()) # 260844 unique IDs, so dupes
rand_data %>% distinct() %>% summarise(n()) # 260844 unique rows, so unique ID: treatment

# what about the other dfs, do IDs have multiple conflicting outcomes or covars (due to buggy data)? no
# do we have a covar and outcome data row for every ID in rand data? likely since also 260844!
covar_data %>% select(id) %>% distinct() %>% summarise(n()) # 260844 unique IDs
covar_data %>% distinct() %>% summarise(n()) # 260844 unique rows, so unique ID: covar
outcome_data %>% distinct() %>% summarise(n()) # 260844 unique IDs
outcome_data %>% distinct() %>% summarise(n()) # 260844 unique rows, so unique ID: outcome

# let's dedupe rand data and combine all the data together using left joins 
combined_data <- rand_data %>%
  distinct() %>% # dedupe
  left_join(covar_data, by='id') %>%
  left_join(outcome_data, by='id')

# the age, race, and gender fields from covars and outcomes should agree...but do they? yes, good!
# do they differ in number? just gender
combined_data %>%
  mutate(age_mismatch=ifelse(!is.na(age.x)&!is.na(age.y)&age.x!=age.y, 1, 0),
         gender_mismatch=ifelse(!is.na(gender.x)&!is.na(gender.y)&gender.x!=gender.y, 1, 0),
         race_mismatch=ifelse(!is.na(race.x)&!is.na(race.y)&race.x!=race.y, 1, 0)) %>%
  summarise(num_age_mismatches=sum(age_mismatch),
            num_gender_mismatches=sum(gender_mismatch),
            num_race_mismatches=sum(race_mismatch),
            age_diff=sum(is.na(age.x))-sum(is.na(age.y)),
            gender_diff=sum(is.na(gender.x))-sum(is.na(gender.y)),
            race=sum(is.na(race.x))-sum(is.na(race.y)))

# so let's just coalesce the 2 gender sources into one
# and keep 1 col for each of the 3 duped fields
combined_data <- combined_data %>%
  mutate(ai_gender=ifelse(is.na(gender.x), gender.y, gender.x)) %>%
  rename(age=age.y, gender=ai_gender, race=race.y) %>%
  select(-gender.x, -gender.y, -age.x, -race.x) 
  
# what about the different fields? do we need to lowercase/clean those up too? yes, outcome

# looks ok
combined_data %>%
  select(message_treat) %>%
  distinct()
# looks ok
combined_data %>%
  select(race) %>%
  distinct()
# looks ok
combined_data %>%
  select(gender) %>%
  distinct()
# looks ok
combined_data %>%
  select(turnout2014) %>%
  distinct()
# needs cleaning!!!
combined_data %>%
  select(outcome) %>%
  distinct()

# reduce number of race categories since we don't have enough people in the smaller groups
combined_data$race_clean <- NA
combined_data$race_clean[combined_data$race == "caucasian"] <- "white"
combined_data$race_clean[combined_data$race == "black"] <- "black"
combined_data$race_clean[combined_data$race == "hispanic"] <- "hispanic"
combined_data$race_clean[combined_data$race == "asian" |
                combined_data$race == "middleEastern" |
                combined_data$race == "unknown" |
                combined_data$race == "nativeAmerican"] <- "other"

combined_data <- combined_data %>%
  mutate(age=ifelse(age > 200, NA, age))
hist(combined_data$age)

# also code outcome variable since messy, multiple entry formats for same outcome
combined_data <- combined_data %>%
  mutate(outcome_clean=ifelse(is.na(outcome), NA, 
                              ifelse(outcome=='Lean Democrat'|outcome=='Democrat', TRUE, FALSE)))

# check for balance across assignment 

dist_bins <- combined_data %>%
  ungroup() %>%
  group_by(turnout2014, race_clean, gender) %>%
  summarise(num_total=n())

# quick check of treatment distribution within the turnout-gender-race bins shows it's pretty good
# each of the 3 treatments is assigned to about 33% of the people in each bin 
# this isn't checking for further distribution when continous vars of partisan score and age taken into account
# since we are tight on time
# but you get the idea
# would be good to graph this out
dist_treatments <- combined_data %>%
  ungroup() %>%
  group_by(turnout2014, race_clean, gender, message_treat) %>%
  summarise(num_treatments=n()) %>%
  left_join(dist_bins) %>%
  group_by(turnout2014, race_clean, gender, message_treat) %>%
  summarise(num_treatments,
            num_total,
            perc_in_treatment=num_treatments/num_total) %>%
  mutate(bin=paste(turnout2014, race_clean, gender)) %>%
  arrange(bin)

###############
#
# ANALYZE
#
###############

# before we analyze, do we really have enough outcomes/turnouts to make a model on?
# yeah, should be ok

# outcomes
# 5145 F
# 1825 T
combined_data %>% 
  group_by(outcome_clean) %>%
  summarise(num=n())

# turnout
# 194374 F
# 66470 T
combined_data %>% 
  group_by(turnout2014) %>%
  summarise(num=n())

# ok, so partisan and age are going to be continuous, but the rest are categorical

########
# estimate voter persuasion treatment effects overall
########

# pro health seemed to do better than pro teach
# sig p of 0.01 for pro health compare to control
# for pro teach compared to control, only 0.39
persuade_no_covar <- glm(outcome_clean ~ message_treat, data = combined_data, family = "binomial")
summary(persuade_no_covar)

# now see what it looks like
# controlling for race, age, gender, partisanship, and whether they turned out in 2014
# not surprisingly, partisan score very significant predictor of outcome
# pro health almost sig
# pro teach not
persuade_covar <- glm(outcome_clean ~ message_treat + race_clean + age + gender +
                        partisanscore + turnout2014,
                      data = combined_data, family = "binomial")
summary(persuade_covar)

########
# estimate voter mobilization treatment effects 
########

# both seemed to perform significant diff from control
# looks like both ProHealth and ProTeach increased turnout, but ProTeach by more
turnout_no_covar <- glm(turnout2014 ~ message_treat, data = combined_data,
                        family = "binomial")
summary(turnout_no_covar)

# now see what it looks like
# controlling for race, age, gender, partisanship
# same trends in message
# age, gender, partisan score also impact turnout
turnout_covar <- glm(turnout2014 ~ message_treat + race_clean + age + gender +
                       partisanscore,
                     data = combined_data, family = "binomial")
summary(turnout_covar)

###############
#
# VISUALIZE
#
###############

# outcome

outcomes <- combined_data %>% 
  filter(!is.na(outcome_clean)) %>%
  group_by(message_treat) %>%
  summarise(percent_dem=sum(outcome_clean==TRUE)/n())

ggplot(outcomes, aes(x=message_treat, y=percent_dem)) + 
  geom_bar(stat = "identity", fill="blue") + 
  ylim(0.0, 0.5) +
  xlab("Message treatment") +
  ylab("Percent outcomes in favor of Democratic candidate") +
  ggtitle("Percent outcomes in favor of Democratic candidate by message treatment")

# turnout

treatment <- combined_data %>% 
  group_by(message_treat) %>%
  summarise(percent_turnout=sum(turnout2014==TRUE)/n())

ggplot(treatment, aes(x=message_treat, y=percent_turnout)) + 
  geom_bar(stat = "identity", fill="blue") + 
  ylim(0.0, 0.5) +
  xlab("Message treatment") +
  ylab("Percent turnout") +
  ggtitle("Percent turnout by message treatment")

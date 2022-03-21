
# DHS Calendar


library(tidyverse)
library(haven)
library(sjlabelled)
library(questionr)
library(survey)
library(jtools)

women <- read_dta("C:/Users/KristinBietsch/files/DHSLoop/SNIR8AFL.DTA")

women_clean <- women %>% select(caseid, v001, v002, v003, v005, v007, v008, v011, v017, v018, v019, v021, v022, v023, v101, v102, v106, v190, vcal_1, vcal_2 )

# Step 7.1
# set length of calendar in a local macro
vcal_len = nchar(women_clean$vcal_1[1])


for (i in 1:vcal_len){
  women_clean[paste("vcal1_",i,sep="")]  <- str_sub(women_clean$vcal_1, i, i) 
  women_clean[paste("vcal2_",i,sep="")]  <- str_sub(women_clean$vcal_2, i, i) 
}

women_long <- women_clean %>% gather(Variable, Value, vcal1_1:ncol(women_clean)) %>%
  separate(Variable, c("CalCol", "Month"), sep="_") %>%
  mutate(Month=(as.numeric(as.character(Month)) * -1) + vcal_len + 1) %>%
  spread(CalCol, Value) %>%
  arrange(caseid, Month) %>%
  group_by(caseid) %>%
  mutate(episode_new=case_when(Month==1 ~ 1,
                               Month!=1 & vcal1!=lag(vcal1) ~ 1,
                               TRUE ~ 0),
         ev004 = cumsum(episode_new),
         cmc=v017+Month-1) %>% # create the century month code (CMC) for each month
  filter(Month<=v019) %>% # drop the blank episode after the date of interview
 mutate(n=1)


women_episode <- women_long %>% group_by(caseid, ev004) %>%
  summarise(v001=first(v001),
            v002=first(v002),
            v003=first(v003),
            v005=first(v005),
            v007=first(v007),
            v008=first(v008),
            v011=first(v011),
            v017=first(v017),
            v018=first(v018),
            v019=first(v019),
            v023=first(v023),
            v101=first(v101),
            v102=first(v102),
            v106=first(v106),
            v190=first(v190),
            v021=first(v021),
            v022=first(v022),
            ev900=first(cmc),
            ev901=last(cmc),
            ev901a=sum(n),
            ev902a=last(vcal1),
            ev903a=last(vcal2)) %>%
  mutate(ev902= case_when(ev902a=="0" ~ 0,
                          ev902a=="1" ~ 1,
                          ev902a=="2" ~ 2,
                          ev902a=="3" ~ 3,
                          ev902a=="4" ~ 4,
                          ev902a=="5" ~ 5,
                          ev902a=="6" ~ 6,
                          ev902a=="7" ~ 7,
                          ev902a=="8" ~ 8,
                          ev902a=="9" ~ 9,
                          ev902a=="W" ~ 10,
                          ev902a=="N" ~ 11,
                          ev902a=="A" ~ 12,
                          ev902a=="L" ~ 13,
                          ev902a=="C" ~ 14,
                          ev902a=="F" ~ 15,
                          ev902a=="E" ~ 16,
                          ev902a=="M" ~ 17,
                          ev902a=="S" ~ 18,
                          ev902a=="B" ~ 81,
                          ev902a=="P" ~ 83,
                          ev902a=="T" ~ 82,
                          ev902a=="?" ~ 99,
                          TRUE ~ -1),
         ev903=case_when(ev903a=="1" ~ 1,
                         ev903a=="2" ~ 2,
                         ev903a=="3" ~ 3,
                         ev903a=="4" ~ 4,
                         ev903a=="5" ~ 5,
                         ev903a=="6" ~ 6,
                         ev903a=="7" ~ 7,
                         ev903a=="8" ~ 8,
                         ev903a=="9" ~ 9,
                         ev903a=="C" ~ 10,
                         ev903a=="F" ~ 11,
                         ev903a=="A" ~ 12,
                         ev903a=="D" ~ 13,
                         ev903a=="W" ~ 96,
                         ev903a=="K" ~ 98,
                         ev903a=="?" ~ 99,
                         ev903a!="" & ev903a!=" " ~ -1)) %>%
  ungroup() %>%
  group_by(caseid) %>%
  mutate(ev904=lag(ev902),
         ev904x=lag(ev901a),
         ev904b=lag(ev902, 2),
         ev904xb=lag(ev901a, 2),
         ev905=lead(ev902),
         ev905x=lead(ev901a),
         ev905b=lead(ev902, 2),
         ev905xb=lead(ev901a, 2),
         ev905cmc=lead(ev901),
         ev905bcmc=lead(ev901,2),
         ev904cmc=lag(ev901),
         ev904bcmc=lag(ev901,2),
         ev903_prev=lag(ev903),
         ev903_prev2=lag(ev903,2),
         ev903_future=lead(ev903))

# ev900  "CMC event begins"
# ev901  "CMC event ends"
# ev901a "Duration of event"
# ev902a "Event code (alpha)"
# ev902 "Event code (numeric)"
# ev903a "Discontinuation code (alpha)"
# ev903 "Discontinuation code (numeric)"

# ev903_prev  "Discontinuation code previous"
# ev903_prev2  "Discontinuation code previous 2"
# ev903_future  "Discontinuation code future"
# ev904  "Prior event code"
# ev904x "Duration of prior event"
# ev905  "Next event code"
# ev905x "Duration of next event"
# ev904b  "2nd Prior event code"
# ev904xb "Duration of 2nd prior event"
# ev905b  "2nd Next event code"
# ev905xb "Duration of 2nd next event"

#label def event ///
#  0 "No method used" ///
#  1 "Pill" ///
#  2 "IUD" ///
#  3 "Injectable" ///
#  4 "Diaphragm" ///
#  5 "Condom" ///
#  6 "Female sterilization" ///
#  7 "Male sterilization" ///
#  8 "Periodic abstinence/Rhythm" ///
#  9 "Withdrawal" ///
# 10 "Other traditional methods" ///
#  11 "Norplant" ///
#  12 "Abstinence" ///
#  13 "Lactational amenorrhea method" ///
#  14 "Female condom" ///
#  15 "Foam and Jelly" ///
#  16 "Emergency contraception" ///
#  17 "Other modern method" ///
#  18 "Standard days method" ///
#  81 "Birth" ///
#  82 "Termination" ///
#  83 "Pregnancy" ///
#  99 "Missing" ///
#  -1 "***Unknown code not recoded***" 
#label def reason ///
#  0 "No discontinuation" ///
#  1 "Became pregnant while using" ///
#  2 "Wanted to become pregnant" ///
#  3 "Husband disapproved" ///
#  4 "Side effects" ///
#  5 "Health concerns" ///
#  6 "Access/availability" ///
#  7 "Wanted more effective method" ///
#  8 "Inconvenient to use" ///
#  9 "Infrequent sex/husband away" ///
#  10 "Cost" ///
#  11 "Fatalistic" ///
#  12 "Difficult to get pregnant/menopause" ///
#  13 "Marital dissolution" ///
#  96 "Other" ///
#  98 "Don't know" ///
#  99 "Missing" ///
#  -1 "***Unknown code not recoded***" 

# Time to conception
ttc <- women_episode %>% filter(ev903_prev==2) %>%
  filter(ev902==83 | (ev902==0 & ev905==83) ) %>%
  mutate(timetopreg=case_when(ev902==83 ~ 1,
                              ev902==0 & ev905==83 ~ ev901a + 1),
         weight=v005/100000)

weighted.mean(ttc$timetopreg, ttc$weight)

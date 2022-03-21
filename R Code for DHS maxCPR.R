
library(tidyverse)
library(haven)
library(sjlabelled)
library(questionr)
library(survey)
library(jtools)

women <- read_dta("C:/Users/KristinBietsch/files/DHSLoop/SNIR8AFL.DTA")

# CPR of PPI

women_ppi <- women %>% mutate(cpr=case_when(v313!=0 ~ 1, v313==0 ~ 0),
                                ppi=case_when(v405==1 | v406==1 ~ 1,
                                              TRUE ~ 0),
                                weight=v005/100000) %>%
  select( cpr, ppi, weight) %>%
  filter(ppi==1)

weighted.mean(women_ppi$cpr, women_ppi$weight)

# Population Distribution All Women



women_dist <- women %>% mutate(group=case_when(v525==0 ~ "s_neversex",
                                               v626a==9 ~ "s_infecund",
                                               v525!=0 & v201==0 ~ "s_sex_fbirth",
                                               v201==1 ~ "s_birth1",
                                               v201==2 ~ "s_birth2",
                                               v201==3 ~ "s_birth3",
                                               v201==4 ~ "s_birth4",
                                               v201==5 ~ "s_birth5",
                                               v201==6 ~ "s_birth6",
                                               v201==7 ~ "s_birth7",
                                               v201==8 ~ "s_birth8",
                                               v201>=9 & !is.na(v201) ~ "s_birth9"),
                               weight=v005/100000)   %>%
  select( group, weight)

women_dist_all <- as.data.frame(prop.table(wtd.table(x = women_dist$group, weights = women_dist$weight))) %>%
  spread(Var1, Freq)
################################################################
# Ever married surveys only
evermar <- read_dta("C:/Users/KristinBietsch/files/DHSLoop/PKIR71FL.DTA")
evermar_pr <- read_dta("C:/Users/KristinBietsch/files/DHSLoop/PKPR71FL.DTA")


women_dist_pr <- evermar_pr %>% filter(hv102==1 & hv104==2 & (hv105>=15 & hv105<=49)) %>%
  filter(!is.na(hv115)) %>%
  mutate( weight=hv005/100000) %>%
  select( hv115, weight)


women_dist_0 <- as.data.frame(prop.table(wtd.table(x = as.factor(women_dist_pr$hv115), weights = women_dist_pr$weight))) %>%
  filter(Var1==0) %>%
  mutate(Inflat_Freq=Freq,
         Var1="s_neversex") 


women_dist_0_freq <- women_dist_0$Freq[1]
  
women_dist_1 <- evermar %>% mutate(group=case_when(v525==0 ~ "s_neversex",
                                               v626a==9 ~ "s_infecund",
                                               v525!=0 & v201==0 ~ "s_sex_fbirth",
                                               v201==1 ~ "s_birth1",
                                               v201==2 ~ "s_birth2",
                                               v201==3 ~ "s_birth3",
                                               v201==4 ~ "s_birth4",
                                               v201==5 ~ "s_birth5",
                                               v201==6 ~ "s_birth6",
                                               v201==7 ~ "s_birth7",
                                               v201==8 ~ "s_birth8",
                                               v201>=9 & !is.na(v201) ~ "s_birth9"),
                               weight=v005/100000)   %>%
  select( group, weight)

women_dist_mar <- as.data.frame(prop.table(wtd.table(x = women_dist_1$group, weights = women_dist_1$weight))) %>%
  mutate(Inflat_Freq=Freq*(1-women_dist_0_freq)) %>%
  bind_rows(women_dist_0) %>%
  group_by(Var1) %>% # in case there are never sex in the ever married group
  summarise(Inflat_Freq=sum(Inflat_Freq)) %>%
  spread(Var1, Inflat_Freq)


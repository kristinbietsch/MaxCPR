
library(jsonlite) 
library(data.table)
library(tidyverse)
library(xlsx)
library(stringi)
library(haven)
library(sjlabelled)
library(questionr)


countryiso <- read.csv("C:/Users/KristinBietsch/files/R Code/2022 Training/Country Regions.csv") %>% select(ISONum, Geographic)

surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));

recent_survey <- surveys %>% 
  filter(API_ID!="ET2019DHS") %>% # cant use the 2019 Ethiopia- now infecund
  group_by(Country) %>% mutate(Recent=max(StartYear)) %>% filter(StartYear==Recent) %>% ungroup() %>%
  filter(StartYear>=1990) %>%
  mutate(IRfile=paste( Survey, ".DTA" , sep=""),
         PRfile=paste( PR, ".DTA" , sep="")) %>%
  mutate(EverMarried=case_when(API_ID=="TR2013DHS" | API_ID=="YE2013DHS" ~ 2,
                               TRUE ~ EverMarried))

ID <- recent_survey %>% select(API_ID, IRfile)

results_total <- setNames(data.frame(matrix(ncol = 2,  nrow = 0)),  c("PPI" , "IRfile")) %>% mutate(PPI=as.numeric(PPI), IRfile=as.character(IRfile))
results_single <- setNames(data.frame(matrix(ncol = 2,  nrow = 1)),  c("PPI" , "IRfile")) %>% mutate(PPI=as.numeric(PPI), IRfile=as.character(IRfile))

results_dist <- setNames(data.frame(matrix(ncol = 13,  nrow = 0)),  c( "s_birth1",     "s_birth2",     "s_birth3",     "s_birth4" ,   
                                                                      "s_birth5",     "s_birth6",     "s_birth7",     "s_birth8" ,    "s_birth9",    
                                                                       "s_infecund",   "s_neversex",   "s_sex_fbirth", "IRfile")) %>%
  mutate(s_birth1=as.numeric(s_birth1),
         s_birth2=as.numeric(s_birth2),
         s_birth3=as.numeric(s_birth3),
         s_birth4=as.numeric(s_birth4),
         s_birth5=as.numeric(s_birth5),
         s_birth6=as.numeric(s_birth6),
         s_birth7=as.numeric(s_birth7),
         s_birth8=as.numeric(s_birth8),
         s_birth9=as.numeric(s_birth9),
         s_infecund=as.numeric(s_infecund),
         s_neversex=as.numeric(s_neversex),
         s_sex_fbirth=as.numeric(s_sex_fbirth),
         IRfile=as.character(IRfile))



####################### Ever Married only
recent_survey_em <- recent_survey %>% filter(EverMarried==1)

for (row in 1:nrow(recent_survey_em)) {
  data <- recent_survey_em[row, "IRfile"]
  data_name <- data$IRfile[1]
  
  prdata  <- recent_survey_em[row, "PRfile"]
  prdata_name <- prdata$PRfile[1]
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  women <- read_dta(data_name, col_select = any_of(c(	"v005",	"v313",		"v405",	"v406" , "v525", "v626a", "v201")))
  pr <-  read_dta(prdata_name, col_select = any_of(c(	"hv102",  "hv104", "hv105", "hv115", "hv005")))
  
  
  if (exists("hv102", pr) & exists("hv104", pr) & exists("hv105", pr)  & exists("hv115", pr)  & exists("hv005", pr)  &
      exists("v525", women) & exists("v626a", women) & exists("v201", women) & exists("v005", women) ) {
    
  
  women_dist_pr <- pr %>% filter(hv102==1 & hv104==2 & (hv105>=15 & hv105<=49)) %>%
    filter(!is.na(hv115)) %>%
    mutate( weight=hv005/100000) %>%
    select( hv115, weight)
 
   women_dist_0 <- as.data.frame(prop.table(wtd.table(x = as.factor(women_dist_pr$hv115), weights = women_dist_pr$weight))) %>%
    filter(Var1==0) %>%
    mutate(Inflat_Freq=Freq,
           Var1="s_neversex") 

      women_dist_0_freq <- women_dist_0$Freq[1]
   
  
      women_dist_1 <- women %>% mutate(group=case_when(v525==0 ~ "s_neversex",
                                                         v626a==9 ~ "s_infecund",
                                                          v201==0 ~ "s_sex_fbirth",
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
        spread(Var1, Inflat_Freq) %>%
        mutate(IRfile=data_name)
      
      results_dist <- bind_rows(results_dist , women_dist_mar)
      
  }
  
  

  
  if (exists("v313", women) & exists("v405", women) & exists("v406", women)  ) {
    
    women_ppi <- women %>% mutate(cpr=case_when(v313!=0 ~ 1, v313==0 ~ 0),
                                  ppi=case_when(v405==1 | v406==1 ~ 1,
                                                TRUE ~ 0),
                                  weight=v005/100000) %>%
      select( cpr, ppi, weight) %>%
      filter(ppi==1)
    
    resultppi <- weighted.mean(women_ppi$cpr, women_ppi$weight)
    
    results_single$PPI[1] <-  resultppi

  } 
  results_single$IRfile[1] <-  data_name
  
  results_total <- bind_rows(results_total , results_single)
  
}

######################### surveys with all women interviewed


recent_survey_aw <- recent_survey %>% filter(is.na(EverMarried))

for (row in 1:nrow(recent_survey_aw)) {
  data <- recent_survey_aw[row, "IRfile"]
  data_name <- data$IRfile[1]
  

  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  women <- read_dta(data_name, col_select = any_of(c(	"v005",	"v313",		"v405",	"v406" , "v525", "v626" , "v626a", "v201")))

  
  if (exists("v525", women) & exists("v626a", women) & exists("v201", women) & exists("v005", women) ) {
    
    
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
      spread(Var1, Freq) %>%
      mutate(IRfile=data_name)
    
    
    results_dist <- bind_rows(results_dist , women_dist_all)
    
  } else if(exists("v525", women) & exists("v626", women) & exists("v201", women) & exists("v005", women)) {
    women_dist <- women %>% mutate(group=case_when(v525==0 ~ "s_neversex",
                                                   v626==9 ~ "s_infecund",
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
    
    women_dist_all626 <- as.data.frame(prop.table(wtd.table(x = women_dist$group, weights = women_dist$weight))) %>%
      spread(Var1, Freq) %>%
      mutate(IRfile=data_name)
    
    
    results_dist <- bind_rows(results_dist , women_dist_all626)
  }
  
  
  
  
  if (exists("v313", women) & exists("v405", women) & exists("v406", women)  ) {
    
    women_ppi <- women %>% mutate(cpr=case_when(v313!=0 ~ 1, v313==0 ~ 0),
                                  ppi=case_when(v405==1 | v406==1 ~ 1,
                                                TRUE ~ 0),
                                  weight=v005/100000) %>%
      select( cpr, ppi, weight) %>%
      filter(ppi==1)
    
    resultppi <- weighted.mean(women_ppi$cpr, women_ppi$weight)
    
    results_single$PPI[1] <-  resultppi
    
  } 
  results_single$IRfile[1] <-  data_name
  
  results_total <- bind_rows(results_total , results_single)
  
}

######################### Special surveys with all women interviewed, but v525 not asked


recent_survey_aw2 <- recent_survey %>%  filter(EverMarried==2)

for (row in 1:nrow(recent_survey_aw2)) {
  data <- recent_survey_aw2[row, "IRfile"]
  data_name <- data$IRfile[1]
  
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  women <- read_dta(data_name, col_select = any_of(c(	"v005",	"v313",		"v405",	"v406" , "v502", "v626a", "v201")))
  
  
  if ( exists("v502", women) &  exists("v626a", women) & exists("v201", women) & exists("v005", women) ) {
    
    
    women_dist <- women %>% mutate(group=case_when(v502==0 ~ "s_neversex",
                                                   v626a==9 ~ "s_infecund",
                                                   v201==0 ~ "s_sex_fbirth",
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
      spread(Var1, Freq) %>%
      mutate(IRfile=data_name)
    
    
    results_dist <- bind_rows(results_dist , women_dist_all)
    
  }
  
  
  
  
  if (exists("v313", women) & exists("v405", women) & exists("v406", women)  ) {
    
    women_ppi <- women %>% mutate(cpr=case_when(v313!=0 ~ 1, v313==0 ~ 0),
                                  ppi=case_when(v405==1 | v406==1 ~ 1,
                                                TRUE ~ 0),
                                  weight=v005/100000) %>%
      select( cpr, ppi, weight) %>%
      filter(ppi==1)
    
    resultppi <- weighted.mean(women_ppi$cpr, women_ppi$weight)
    
    results_single$PPI[1] <-  resultppi
    
  } 
  results_single$IRfile[1] <-  data_name
  
  results_total <- bind_rows(results_total , results_single)
  
}

results <- full_join(results_total, results_dist, by="IRfile") %>% left_join(ID, by="IRfile")

write.csv(results, "C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/DHSMicroResults031822.csv", row.names = F, na="")
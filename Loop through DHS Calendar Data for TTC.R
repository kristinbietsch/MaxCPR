
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
  mutate(IRfile=paste( Survey, ".DTA" , sep="")) %>%
  filter(Country!="India")  # was taking so long to run that R was crashing

results_total <- setNames(data.frame(matrix(ncol = 2,  nrow = 0)),  c("ttc" , "IRfile")) %>% mutate(ttc=as.numeric(ttc), IRfile=as.character(IRfile))
results_single <- setNames(data.frame(matrix(ncol = 2,  nrow = 1)),  c("ttc" , "IRfile")) %>% mutate(ttc=as.numeric(ttc), IRfile=as.character(IRfile))

# Row 32 india is breaking

for (row in 1:nrow(recent_survey)) {
  data <- recent_survey[row, "IRfile"]
  data_name <- data$IRfile[1]
  
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  women <- read_dta(data_name, col_select = any_of(c("caseid", "v005", "v017", "v019", "vcal_1", "vcal_2"	)))
  
  if (exists("caseid", women) & exists("v005", women) & exists("v017", women) & exists("v019", women) & exists("vcal_1", women) & exists("vcal_2", women) ) {
    vcal_len = nchar(women$vcal_1[1])
    
    
    for (i in 1:vcal_len){
      women[paste("vcal1_",i,sep="")]  <- str_sub(women$vcal_1, i, i) 
      women[paste("vcal2_",i,sep="")]  <- str_sub(women$vcal_2, i, i) 
    }
    
    women_long <- women %>% gather(Variable, Value, vcal1_1:ncol(women)) %>%
      mutate(CalCol=substr(Variable, 1, 5)   ,
             Month1=substr(Variable, 7, 7),
             Month2=substr(Variable, 8, 8),
             Month=paste(Month1, Month2, sep=""))    %>% 
      select(-Variable, -vcal_1, -vcal_2, -Month1, -Month2) %>%
      mutate(Month=(as.numeric(as.character(Month)) * -1) + vcal_len + 1)
    
    women_long_col1 <- women_long %>% filter(CalCol=="vcal1") %>% rename(vcal1=Value) %>% select(caseid, v005, v017, v019, Month, vcal1)
    women_long_col2 <- women_long %>% filter(CalCol=="vcal2") %>% rename(vcal2=Value) %>% select(caseid, Month, vcal2)
  
    
    women_long_col <- left_join(women_long_col1, women_long_col2, by=c("caseid",  "Month"))    %>%
      arrange(caseid, Month) %>%
      group_by(caseid)  %>%
      mutate(episode_new=case_when(Month==1 ~ 1,
                                   Month!=1 & vcal1!=lag(vcal1) ~ 1,
                                   TRUE ~ 0),
             ev004 = cumsum(episode_new),
             cmc=v017+Month-1) %>% # create the century month code (CMC) for each month
      filter(Month<=v019) %>% # drop the blank episode after the date of interview
      mutate(n=1)
    
    
    women_episode <- women_long_col %>% group_by(caseid, ev004) %>%
      summarise(v005=first(v005),
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

    
    # Time to conception
    ttc <- women_episode %>% filter(ev903_prev==2) %>%
      filter(ev902==83 | (ev902==0 & ev905==83) ) %>%
      mutate(timetopreg=case_when(ev902==83 ~ 1,
                                  ev902==0 & ev905==83 ~ ev901a + 1),
             weight=v005/100000)
    
    resultttc <- weighted.mean(ttc$timetopreg, ttc$weight)
    
    results_single$ttc[1] <-  resultttc
    
  } else {
    results_single$ttc[1] <- NA
  }
  results_single$IRfile[1] <-  data_name
  
  results_total <- bind_rows(results_total , results_single)
  
}

# less than 2.5, change to 6
# over 10, change to 6
# Missing, change to 6

results_total_edited <- results_total %>% mutate(ttc=case_when(ttc<2.5 ~ 6,
                                                               ttc>10 ~ 6,
                                                               is.na(ttc) ~ 6,
                                                               TRUE ~ ttc))

# Add in India 2015 = 5.6

results_single$ttc[1] <-  5.6
results_single$IRfile[1] <-  "IAIR73FL.DTA"

results_total_edited <- bind_rows(results_total_edited , results_single)

# Save
write.csv(results_total_edited, "C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/DHSCalendarMicroResults031822.csv", row.names = F, na="")

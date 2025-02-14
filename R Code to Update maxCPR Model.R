
library(jsonlite) 
library(data.table)
library(tidyverse)
library(xlsx)
library(stringi)



countryiso <- read.csv("C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/Country Regions FR.csv") %>% select(ISONum, Pays, Geographic)

surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));

recent_survey <- surveys %>% filter(API_ID!="ET2019DHS") %>% # cant use the 2019 Ethiopia- no infecund
  group_by(Country) %>% mutate(Recent=max(StartYear)) %>% filter(StartYear==Recent) %>% ungroup() %>%
  filter(StartYear>=1990)
# might have to put a year filter on there too


#################################################################################################
pre2020pop <- read.csv("C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE Pre2020.csv")
post2020pop <- read.csv("C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE Post2020.csv") %>% filter(Year!=2020)

wrapop <- bind_rows(pre2020pop, post2020pop) %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) %>%
  mutate(a15=as.numeric(a15), a20=as.numeric(a20), a25=as.numeric(a25), a30=as.numeric(a30), a35=as.numeric(a35), a40=as.numeric(a40), a45=as.numeric(a45)) %>%
  mutate(WRA=(a15+a20+a25+a30+a35+a40+a45)*1000) %>%
  filter(!is.na(WRA)) %>%
  rename(StartYear=Year) %>%
  select(StartYear, ISONum, WRA)
  


#################################################################################################
# Bring in Microdata results

# Loop through DHS Calendar Data for TTC.R
ttc <- read.csv("C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/DHSCalendarMicroResults021225.csv") %>%
  mutate(Survey=substr(IRfile, 1,8)) %>% select(-IRfile) 

# Loop for DHS MicroData.R
micro <- read.csv("C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/DHSMicroResults021225.csv") 

#################################################################################################
#	Ideal Number of Children, All Women (ideal) PR_IDLC_W_MNA
#	Median Age at First Sex (25-49) (age_fsex) SX_AAFS_W_M2B
#	Median Age at First Birth (25-49) (age_fbirth) FE_AAFB_W_M2B
#	Median Birth Interval (Months) (ave_bi) FE_BINT_C_MED
#	Median Duration of Postpartum Insusceptible (ppi) FE_PPID_W_MDI
#	CPR, All Women (CPR_all) 	FP_CUSA_W_ANY
#	Using to Space, All Women (Spacing_all) FP_NADA_W_MNS
#	Using to Limit, All Women (Limiting_all) FP_NADA_W_MNL
#	CPR, Married Women (CPR_mar) 	FP_CUSM_W_ANY
#	Using to Space, Married Women (Spacing_mar) FP_NADM_W_MNS
#	Using to Limit, Married Women (Limiting_mar) FP_NADM_W_MNL

url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=indicatorIds=FE_FRTR_W_TFR,PR_IDLC_W_MNA,SX_AAFS_W_M2B,FE_AAFB_W_M2B,FE_BINT_C_MED,FE_PPID_W_MDI,FP_CUSA_W_ANY,FP_NADA_W_MNS,FP_NADA_W_MNL,FP_CUSM_W_ANY,FP_NADM_W_MNS,FP_NADM_W_MNL&surveyid=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
data_clean <- dta %>% select(SurveyId, IndicatorId, Value) %>% spread(IndicatorId, Value) %>% rename(API_ID=SurveyId)

data_recent <- left_join(recent_survey, data_clean, by="API_ID") %>% left_join(countryiso, by="ISONum") %>%
  left_join(wrapop, by=c("ISONum", "StartYear"))   %>%
  left_join(ttc, by="Survey") %>% select(-Survey) %>%
  left_join(micro, by="API_ID") %>%
  rename(Survey=API_ID,
         CountryYear=CountryYear,
         Country=Country,
         CountryFR=Pays,
         Region=Geographic,
         Source=FullYear,
         age_fsex=SX_AAFS_W_M2B,
         age_fbirth=FE_AAFB_W_M2B,	
         ave_bi=FE_BINT_C_MED,
         ideal= PR_IDLC_W_MNA,
         ppi=FE_PPID_W_MDI,
         CPR_all=FP_CUSA_W_ANY,
         Spacing_all=FP_NADA_W_MNS,
         Limiting_all=FP_NADA_W_MNL,
         CPR_mar=FP_CUSM_W_ANY,
         Spacing_mar=FP_NADM_W_MNS,
         Limiting_mar=FP_NADM_W_MNL,
         pop=WRA,
         ppfp=PPI) %>%
  mutate(Notes=NA, NotesFR=NA) %>%
  select(Survey,	CountryYear,	Country,	CountryFR,	Region,	Source,	age_fsex,	age_fbirth,	ave_bi,	ideal,	ppi,	ttc,	ppfp,	s_neversex,	s_sex_fbirth,	s_birth1,	s_birth2,	s_birth3,	s_birth4,	s_birth5,	s_birth6,	s_birth7,	s_birth8,	s_birth9,	s_infecund,	CPR_all,	Spacing_all,	Limiting_all,	CPR_mar,	Spacing_mar,	Limiting_mar,	Notes, NotesFR,	pop)
  

# Creating a blank line of data for people to input their own
blank_data <- setNames(data.frame(matrix(ncol = 34,  nrow = 1)),  c("Survey",	"CountryYear",	"Country", "CountryFR",	"Region",	"Source",	"age_fsex",	"age_fbirth",	"ave_bi",	"ideal",	"ppi",	"ttc",	"ppfp",	"s_neversex",	"s_sex_fbirth",	"s_birth1",	"s_birth2",	"s_birth3",	"s_birth4",	"s_birth5",	"s_birth6",	"s_birth7",	"s_birth8",	"s_birth9",	"s_infecund",	"CPR_all",	"Spacing_all",	"Limiting_all",	"CPR_mar",	"Spacing_mar",	"Limiting_mar",	"Notes", "NotesFR",	"pop")) 
blank_data$Country[1] <- "Add your own country data"
blank_data$CountryFR[1] <- "Ajouter vos propres donnees de pays"
blank_data$age_fsex[1] <- 0
blank_data$age_fbirth[1] <- 0
blank_data$ave_bi[1] <- 0
blank_data$ideal[1] <- 4
blank_data$ppi[1] <- 0
blank_data$ttc[1] <- 0
blank_data$ppfp[1] <- 0
blank_data$s_neversex[1] <- 0
blank_data$s_sex_fbirth[1] <- 0
blank_data$s_birth1[1] <- 0
blank_data$s_birth2[1] <- 0
blank_data$s_birth3[1] <- 0
blank_data$s_birth4[1] <- 0
blank_data$s_birth5[1] <- 0
blank_data$s_birth6[1] <- 0
blank_data$s_birth7[1] <- 0
blank_data$s_birth8[1] <- 0
blank_data$s_birth9[1] <- 0
blank_data$s_infecund[1] <- 0



# Cleaning up the data
data_recent_clean <- data_recent %>%
  filter(Survey!="PY1990DHS") %>% # removing survey with lack of data
  filter(Survey!="MA2003DHS") %>% # removing survey with lack of data
  filter(Survey!="VN2002DHS") %>% # removing survey with lack of data
  mutate(age_fsex=case_when(Survey=="EG2014DHS" ~ 20.8, # assuming Age F Sex is Age F Marriage (25-49)
                            Survey=="JO2017DHS" ~ 22.7,
                            Survey=="TR2013DHS" ~ 21,
                            Survey=="YE2013DHS" ~ 18.2,
                            TRUE ~ age_fsex)) %>%
  mutate(ave_bi=case_when(Survey=="MZ2015AIS" ~ 34.8, # using 2011 DHS
                          TRUE ~ ave_bi)) %>%
  mutate(age_fbirth=case_when(Survey=="NM2013DHS" ~ 21.6,
                              Survey=="ZA2016DHS" ~ 21.3,
                              TRUE ~ age_fbirth)) %>%
  mutate(ideal=case_when(Survey=="MZ2015AIS" ~ 4.8, # using 2011 DHS
                         Survey=="MB2005DHS" ~ 1.7, # doesnt ask ideal, using TFR
                          TRUE ~ ideal))  %>%
  mutate(ppi=case_when(Survey=="MZ2015AIS" ~ 15, # using 2011 DHS
                         Survey=="YE2013DHS" ~ 3.5, # just asks amenorrheic, using that 
                         TRUE ~ ppi)) %>%
  mutate(ppfp=case_when(Survey=="MZ2015AIS" ~ 0.072, # using 2011 DHS
                          TRUE ~ ppfp)) %>%
  mutate(s_birth8=case_when(is.na(s_birth8) ~ 0,
                            !is.na(s_birth8) ~ s_birth8)) %>% # will need to add in more parities if more surveys in the future dont have women of high parity
  mutate(s_birth9=case_when(is.na(s_birth9) ~ 0,
                            !is.na(s_birth9) ~ s_birth9)) %>%
  mutate(CPR_all=case_when(Survey=="AF2015DHS" ~ CPR_mar * .679, # multipled married CPR by % married to calculate all women CPR
                           Survey=="BD2017DHS" ~ CPR_mar * .803,
                           Survey=="EG2014DHS" ~ CPR_mar * .697,
                           Survey=="JO2017DHS" ~ CPR_mar * .558,
                           Survey=="PK2017DHS" ~ CPR_mar * .618,
                           TRUE ~ CPR_all)) %>%
  mutate(Spacing_all=case_when(Survey=="AF2015DHS" ~ Spacing_mar * .679, # multipled married CPR by % married to calculate all women CPR
                           Survey=="BD2017DHS" ~ Spacing_mar * .803,
                           Survey=="EG2014DHS" ~ Spacing_mar * .697,
                           Survey=="JO2017DHS" ~ Spacing_mar * .558,
                           Survey=="PK2017DHS" ~ Spacing_mar * .618,
                           TRUE ~ Spacing_all)) %>%
  mutate(Limiting_all=case_when(Survey=="AF2015DHS" ~ Limiting_mar * .679, # multipled married CPR by % married to calculate all women CPR
                           Survey=="BD2017DHS" ~ Limiting_mar * .803,
                           Survey=="EG2014DHS" ~ Limiting_mar * .697,
                           Survey=="JO2017DHS" ~ Limiting_mar * .558,
                           Survey=="PK2017DHS" ~ Limiting_mar * .618,
                           TRUE ~ Limiting_all)) %>%
  mutate(ppfp=100*ppfp) %>%
  mutate(s_neversex=100*s_neversex) %>%
  mutate(s_sex_fbirth=100*s_sex_fbirth) %>%
  mutate(s_birth1=100*s_birth1) %>%
  mutate(s_birth2=100*s_birth2) %>%
  mutate(s_birth3=100*s_birth3) %>%
  mutate(s_birth4=100*s_birth4) %>%
  mutate(s_birth5=100*s_birth5) %>%
  mutate(s_birth6=100*s_birth6) %>%
  mutate(s_birth7=100*s_birth7) %>%
  mutate(s_birth8=100*s_birth8) %>%
  mutate(s_birth9=100*s_birth9) %>%
  mutate(s_infecund=100*s_infecund) %>%
  bind_rows(blank_data)


# Note: Kristin (3/21/22) In next round of updates add in notes in french and english, add names in French
  
write.csv(data_recent_clean, "C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/SpacingLimitingDefault021225.csv", row.names = F, na="")
#write.csv(data_recent_clean, "C:/Users/KristinBietsch/files/Track20/Spacing and Limiting/Code for Updating Default Data/SpacingLimitingDefault120522.csv", row.names = F, na="")




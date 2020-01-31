# Maximum CPR Model- Parity Specific Average Birth Intervals
# Kristin Bietsch, PhD and Emily Sonneveldt, PhD
# Avenir Health

######################################################################
############################# Input Data #############################
######################################################################

# Ideal Number of Children (Round, Range 1-9)
ideal <- 6

# Age at First Birth and First Sex
age_fsex <- 18.7 
age_fbirth <- 20.1

# Post Partum Insusceptibility and Percent of Post Partum Insusceptible Women Using Family Planning
ppi <- 4.4
ppfp <- 15

# Time to Conception
ttc <- 3

# Parity specific birth intervals
bi_12 <- 26.8
bi_23 <- 26.8
bi_34 <- 29.8
bi_45 <- 29.8
bi_56 <- 29.8
bi_67 <- 30.6
bi_78 <- 30.6
bi_89 <- 30.6

## Population Distribution (Total=100)
s_neversex <- 30
s_sex_fbirth <- 5
s_infecund <- 13.3
s_birth1 <- 7.1
s_birth2 <- 7.9
s_birth3 <- 7.5
s_birth4 <- 7
s_birth5 <- 5.9
s_birth6 <- 5.2
s_birth7 <- 4
s_birth8 <- 3
s_birth9 <- 4.1

# Distribution Check
distribution <- s_neversex +  s_sex_fbirth +  s_infecund +  s_birth1 +  s_birth2 +  s_birth3 +  s_birth4 +  s_birth5 +  s_birth6 +  s_birth7 +  s_birth8 +  s_birth9
# Should equal 100
distribution
###############################################################################################
# Model: Parity Specific Birth Intervals
###############################################################################################

# Calculating the total number of miscarriages and months lost to miscarriage per birth interval
totalpreg <- ideal/(1-.1)
totalterm <- totalpreg -ideal 
monthsmisc <- (ttc+3)*totalterm
months_per_BI <- monthsmisc*(1/ideal)

###############################################################################################
# Spacing Calculations

# Percent of Contraceptive Use for Interval between First Sex and First Birth
per_bfb <- ((age_fbirth*12)-(age_fsex*12)-9-ttc-months_per_BI)/((age_fbirth*12)-(age_fsex*12))

# Percent of Contraceptive Use for Interval between First Birth and Last Birth
per_space12 <- (bi_12 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_12
per_space23 <- (bi_23 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_23
per_space34 <- (bi_34 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_34
per_space45 <- (bi_45 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_45
per_space56 <- (bi_56 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_56
per_space67 <- (bi_67 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_67
per_space78 <- (bi_78 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_78
per_space89 <- (bi_89 - (ppi*((100-ppfp)/100))-ttc-9-months_per_BI)/bi_89

# Multiplying the percent use at each birth interval by the percent of women at each birth interval
per_pop_space<- ifelse(ideal==1, 0,
                ifelse(ideal==2, (s_birth1*per_space12),
                ifelse(ideal==3, (s_birth1*per_space12)+ (s_birth2*per_space23),
                ifelse(ideal==4, (s_birth1*per_space12)+ (s_birth2*per_space23)+ (s_birth3*per_space34),
                ifelse(ideal==5, (s_birth1*per_space12)+ (s_birth2*per_space23)+ (s_birth3*per_space34)+ (s_birth4*per_space45),
                ifelse(ideal==6, (s_birth1*per_space12)+ (s_birth2*per_space23)+ (s_birth3*per_space34)+ (s_birth4*per_space45)+ (s_birth5*per_space56),
                ifelse(ideal==7, (s_birth1*per_space12)+ (s_birth2*per_space23)+ (s_birth3*per_space34)+ (s_birth4*per_space45)+ (s_birth5*per_space56)+ (s_birth6*per_space67),
                ifelse(ideal==8, (s_birth1*per_space12)+ (s_birth2*per_space23)+ (s_birth3*per_space34)+ (s_birth4*per_space45)+ (s_birth5*per_space56)+ (s_birth6*per_space67)+ (s_birth7*per_space78),
                ifelse(ideal==9, (s_birth1*per_space12)+ (s_birth2*per_space23)+ (s_birth3*per_space34)+ (s_birth4*per_space45)+ (s_birth5*per_space56)+ (s_birth6*per_space67)+ (s_birth7*per_space78)+ (s_birth8*per_space89), NA)))))))))


cpr_space <- (per_bfb*s_sex_fbirth) + per_pop_space

###############################################################################################
# Limiting Calculations

# Total months in closed birth intervals
total_bi <- ifelse(ideal==1, 0,
            ifelse(ideal==2, bi_12,
            ifelse(ideal==3, bi_12 + bi_23,
            ifelse(ideal==4, bi_12 + bi_23 + bi_34,
            ifelse(ideal==5, bi_12 + bi_23 + bi_34 + bi_45,
            ifelse(ideal==6, bi_12 + bi_23 + bi_34 + bi_45 + bi_56,
            ifelse(ideal==7, bi_12 + bi_23 + bi_34 + bi_45 + bi_56 + bi_67,
            ifelse(ideal==8, bi_12 + bi_23 + bi_34 + bi_45 + bi_56 + bi_67 + bi_78,
            ifelse(ideal==9, bi_12 + bi_23 + bi_34 + bi_45 + bi_56 + bi_67 + bi_78 + bi_89, NA)))))))))

# Age at last births
age_lb <- ((age_fbirth*12) + total_bi)/12

# Use after last birth
ualb <- (50- age_lb)*12-(ppi*((100-ppfp)/100))
per_limit <- ( ualb)/((50- age_lb)*12)

# Population of limiters
pop_limit<- ifelse(ideal==1,  s_birth1+ s_birth2+ s_birth3+ s_birth4+ s_birth5+ s_birth6+ s_birth7+ s_birth8+ s_birth9,
                   ifelse(ideal==2,  s_birth2+ s_birth3+ s_birth4+ s_birth5+ s_birth6+ s_birth7+ s_birth8+ s_birth9,
                          ifelse(ideal==3,  s_birth3+ s_birth4+ s_birth5+ s_birth6+ s_birth7+ s_birth8+ s_birth9,
                                 ifelse(ideal==4, s_birth4+ s_birth5+ s_birth6+ s_birth7+ s_birth8+ s_birth9,
                                        ifelse(ideal==5, s_birth5+ s_birth6+ s_birth7+ s_birth8+ s_birth9,
                                               ifelse(ideal==6, s_birth6+ s_birth7+ s_birth8+ s_birth9,
                                                      ifelse(ideal==7, s_birth7+ s_birth8+ s_birth9,
                                                             ifelse(ideal==8, s_birth8+ s_birth9,
                                                                    ifelse(ideal==9, s_birth9, NA)))))))))


cpr_limit <-  per_limit *  pop_limit

###########################################
cpr_total <- cpr_space + cpr_limit


###########################################
# Display Results
cpr_total
cpr_space
cpr_limit


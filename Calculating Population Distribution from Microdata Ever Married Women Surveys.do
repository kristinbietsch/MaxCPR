/**
Population Distribution, Ever Married Women Surveys
Kristin Bietsch, PhD
Track 20 Project, Avenir Health
March 6, 2019
**/


/**
Notes: For most ever married women surveys, we assume that there is no premarital sexual activity.  
Therefore, the percent of women who are not sexually active is the percent of women who are never married.
To calculate the percent of women in the population who are not sexually active, we use the PR file to calculate the percent of women 15-49 who have never married.
To calculate the other groups, we use the IR file, then deflate the proportions by the the percent of never married women from the PR file.
**/

clear all
set more off
set maxvar 6500

* Load PR File

keep if hv102==1
keep if hv104==2
keep if hv105>=15 & hv105<=49

gen weight=hv005/100000
svyset hv021 [pw=weight], strata(hv022) singleunit(scaled)

svy: tab hv115

**************************************************

clear all
set more off
set maxvar 6500

* Load IR File

gen group=.
replace group=2 if v525!=0 & v201==0
replace group=31 if v201==1
replace group=32 if v201==2
replace group=33 if v201==3
replace group=34 if v201==4
replace group=35 if v201==5
replace group=36 if v201==6
replace group=37 if v201==7
replace group=38 if v201==8
replace group=39 if v201>=9 & v201!=.
replace group=4 if v626a==9
replace group=1 if v525==0

label define group 1 "Group 1" 2 "Group 2" 31 "Group 31" 32 "Group 32" 33 "Group 33" 34 "Group 34" 35 "Group 35" 36 "Group 36" 37 "Group 37" 38 "Group 38" 39 "Group 39" 4 "Group 4"  
label values group group  

gen weight=v005/100000
svyset v021 [pw=weight], strata(v022) singleunit(scaled)

svy: tab group



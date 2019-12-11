/**
Population Distribution, All Women Surveys
Kristin Bietsch, PhD
Track 20 Project, Avenir Health
March 6, 2019
**/

clear all
set more off
set maxvar 6500

* Load IR File
cd "C:\Users\KBietsch\Files\DHSLoop"

/**
ALIR71FL
AOIR71FL
AMIR71FL
BJIR71FL
COIR71FL
DRIR61FL
GAIR60FL
GUIR71FL
MVIR71FL
NMIR61FL
PEIR6IFL

* no sexual activity data 
JOIR71FL
PKIR71FL
TRIR61FL

**/


use "NGIR6AFL.DTA", clear

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

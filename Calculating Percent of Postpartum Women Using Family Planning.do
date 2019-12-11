/**
Percent of Postpartum Women Using Family Planning
Kristin Bietsch, PhD
Track 20 Project, Avenir Health
March 6, 2019
**/

clear all
set more off
set maxvar 6500

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
JOIR71FL
MVIR71FL
NMIR61FL
PKIR71FL
PEIR6IFL
TRIR61FL

**/


use "TRIR61FL.DTA", clear

* Load IR File

gen cpr=1 if v313!=0
replace cpr=0 if v313==0

gen ppi= 1 if v405==1 | v406==1

gen weight=v005/100000
svyset v021 [pw=weight], strata(v022) singleunit(scaled)

svy: mean cpr if ppi==1


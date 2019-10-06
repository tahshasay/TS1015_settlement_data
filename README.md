# TS1015_settlement_stats

#These data and scripts are supplement to Say, T. E., & Degnan, S. M. (2019). Interdependent photo- and chemosensory systems regulate larval settlement in a marine sponge. bioRxiv, 519512. doi:10.1101/519512


#This folder contains the files and scripts used to perform the statistical tests on the larval settlement data 

Script:
"R_Script_settlement_TS1015.sh" - R commands used for the statistical tests on settlement data. 

INPUT_files:
"R_sett_d4.3.15_ind1hpe_cox_light-dark_2018.02.05.txt" - raw settlement data used as input for binomial glm.

"R_sett_d4.3.15_ind1hpe_cox_2018.03.07.txt" - raw settlement data used as input for log rank test comparing ACA to FSW (censored).

"R_sett_d4.3.15_ind1hpe_cox_light-dark_2018.02.05.txt" - raw settlement data used as input for log rank test comparing larvae that were maintained in constant light to larvae that were transferred from constant light into darkness

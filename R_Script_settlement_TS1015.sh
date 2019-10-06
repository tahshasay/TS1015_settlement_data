# These data and scripts are supplement to Say, T. E., & Degnan, S. M. (2019). 
# Interdependent photo- and chemosensory systems regulate larval settlement in a marine sponge. bioRxiv, 519512. doi:10.1101/519512
#
#This script contains the code used to perform the statistical tests on the larval settlement data 

####################################################################################
# Larval settlement statistics
# Code by Tahsha E. Say
#
# Part 1)
# binomial glm comparing larval settlement under three different light regimes including natural day-night cycle (nat), constant light (lgt) and constant dark (drk).  
#
#
####################################################################################

getwd() # get working directory
setwd("/Users/tahshasay/Documents/Binary_Data_from_R/Sett_2015Mar4_20191002")
getwd()

data_wide_calc = read.delim("R_Sett_2015Mar04_20181016_censor_totals_wide_20181018_final_i2.txt", header = T, sep = "\t")
head(data_wide_calc)

# X.1 X    fine broad time censored settled cum_sett
# 1   1 1 control   drk    1       12       0        0
# 2   2 2 control   drk    2        0       0        0
# 3   3 3 control   drk    3        7       0        0
# 4   4 4 control   drk    4        0       0        0
# 5   5 5 control   drk    5        7       0        0
# 6   6 6 control   drk    6        0       0        0
#   total_start total_rm_censor not prop percent i2
# 1          54              54  54    0       0  1
# 2          54              42  42    0       0  2
# 3          54              42  42    0       0  3
# 4          54              35  35    0       0  4
# 5          54              35  35    0       0  5
# 6          54              28  28    0       0  6

data_wide_calc$time <- factor(data_wide_calc$time)
levels(data_wide_calc$time)

levels(data_wide_calc$fine)
data_wide_calc$fine=relevel(data_wide_calc$fine, ref="induced")

levels(data_wide_calc$broad)
data_wide_calc$broad=relevel(data_wide_calc$broad, ref="nat")




# remove sterile (these are outliers when ploting residuals)
# too many 0s
subset<-subset(data_wide_calc, fine!="control") 




head(data_wide_calc)
#   X.1 X    fine broad time censored settled cum_sett total_start
# 1   1 1 control   drk    1       12       0        0          54
# 2   2 2 control   drk    2        0       0        0          54
# 3   3 3 control   drk    3        7       0        0          54
# 4   4 4 control   drk    4        0       0        0          54
# 5   5 5 control   drk    5        7       0        0          54
# 6   6 6 control   drk    6        0       0        0          54
#   total_rm_censor not prop percent i2
# 1              54  54    0       0  1
# 2              42  42    0       0  2
# 3              42  42    0       0  3
# 4              35  35    0       0  4
# 5              35  35    0       0  5
# 6              28  28    0       0  6


head(subset)
#    X.1  X    fine broad time censored settled cum_sett total_start
# 31  31 40 induced   drk    1        0       3        3          54
# 32  32 41 induced   drk    2        0       3        6          54
# 33  33 42 induced   drk    3        0       6       12          54
# 34  34 43 induced   drk    4        0       4       16          54
# 35  35 44 induced   drk    5        0       3       19          54
# 36  36 45 induced   drk    6        0       3       22          54
#    total_rm_censor not       prop   percent i2
# 31              54  51 0.05555556  5.555556 31
# 32              54  48 0.11111111 11.111111 32
# 33              54  42 0.22222222 22.222222 33
# 34              54  38 0.29629630 29.629630 34
# 35              54  35 0.35185185 35.185185 35
# 36              54  32 0.40740741 40.740741 36

#library(lme4)
library(multcomp)

subset$broad=relevel(subset$broad, ref="lgt")

levels(subset$broad)


subset$time <- factor(subset$time)
levels(subset$time)


fit <- glm(cbind(cum_sett, not) ~ broad + time, data=subset, family=binomial)

fit
summary(fit)
# Call:
# glm(formula = cbind(cum_sett, not) ~ broad + time, family = binomial, 
#     data = subset)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.6790  -1.0904  -0.0906   0.9021   2.4272  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -7.1871     0.6523 -11.018  < 2e-16 ***
# broadnat      3.3657     0.2976  11.311  < 2e-16 ***
# broaddrk      3.8211     0.2980  12.822  < 2e-16 ***
# time2         0.7227     0.7209   1.002 0.316115    
# time3         1.4773     0.6610   2.235 0.025419 *  
# time4         1.8079     0.6454   2.801 0.005088 ** 
# time5         2.3045     0.6298   3.659 0.000253 ***
# time6         2.6913     0.6223   4.324 1.53e-05 ***
# time7         3.1007     0.6177   5.019 5.18e-07 ***
# time8         3.6135     0.6156   5.870 4.36e-09 ***
# time9         3.7557     0.6156   6.101 1.06e-09 ***
# time10        3.9342     0.6160   6.387 1.69e-10 ***
# time11        3.9958     0.6168   6.478 9.28e-11 ***
# time12        4.2607     0.6184   6.889 5.60e-12 ***
# time13        4.4192     0.6199   7.129 1.01e-12 ***
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 838.301  on 38  degrees of freedom
# Residual deviance:  80.191  on 24  degrees of freedom
# AIC: 219.67
# 
# Number of Fisher Scoring iterations: 5


#fit <- glmer(cbind(settled, not) ~ broad + fine + time  + (1|i2), data=data_wide_calc, family=binomial)


tests <- glht(fit, linfct=mcp(broad="Tukey"))
plot(tests)
summary(tests)
# 	 Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: glm(formula = cbind(cum_sett, not) ~ broad + time, family = binomial, 
#     data = subset)
# 
# Linear Hypotheses:
#                Estimate Std. Error z value Pr(>|z|)    
# nat - lgt == 0   3.3657     0.2976   11.31  < 1e-05 ***
# drk - lgt == 0   3.8211     0.2980   12.82  < 1e-05 ***
# drk - nat == 0   0.4554     0.1272    3.58 0.000887 ***
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)





fit <- glm(cbind(cum_sett, not) ~ broad + time, data=subset, family=binomial)

fit
summary(fit)
# Call:
# glm(formula = cbind(cum_sett, not) ~ broad + time, family = binomial, 
#     data = subset)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.6790  -1.0904  -0.0906   0.9021   2.4272  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -7.1871     0.6523 -11.018  < 2e-16 ***
# broadnat      3.3657     0.2976  11.311  < 2e-16 ***
# broaddrk      3.8211     0.2980  12.822  < 2e-16 ***
# time2         0.7227     0.7209   1.002 0.316115    
# time3         1.4773     0.6610   2.235 0.025419 *  
# time4         1.8079     0.6454   2.801 0.005088 ** 
# time5         2.3045     0.6298   3.659 0.000253 ***
# time6         2.6913     0.6223   4.324 1.53e-05 ***
# time7         3.1007     0.6177   5.019 5.18e-07 ***
# time8         3.6135     0.6156   5.870 4.36e-09 ***
# time9         3.7557     0.6156   6.101 1.06e-09 ***
# time10        3.9342     0.6160   6.387 1.69e-10 ***
# time11        3.9958     0.6168   6.478 9.28e-11 ***
# time12        4.2607     0.6184   6.889 5.60e-12 ***
# time13        4.4192     0.6199   7.129 1.01e-12 ***
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 838.301  on 38  degrees of freedom
# Residual deviance:  80.191  on 24  degrees of freedom
# AIC: 219.67
# 
# Number of Fisher Scoring iterations: 5



tests <- glht(fit, linfct=mcp(broad="Tukey"))
plot(tests)
summary(tests)

# 	 Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: glm(formula = cbind(cum_sett, not) ~ broad + time, family = binomial, 
#     data = subset)
# 
# Linear Hypotheses:
#                Estimate Std. Error z value Pr(>|z|)    
# nat - lgt == 0   3.3657     0.2976   11.31  < 1e-05 ***
# drk - lgt == 0   3.8211     0.2980   12.82  < 1e-05 ***
# drk - nat == 0   0.4554     0.1272    3.58 0.000889 ***
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)



tests <- glht(fit, linfct=mcp(time="Tukey"))
plot(tests)
summary(tests)


vignette("using-lsmeans")
help("lsmeans", package = "lsmeans")

library(lsmeans)
#summary(fit, type = "response")
#lsmeans(fit, prop~shading | time)

lsmeans(fit, pairwise ~ time | broad, transform="response")


fit <- glm(cbind(cum_sett, not) ~ broad + time + fine, data=data_wide_calc, family=binomial)


####################################################################################
# Larval settlement statistics
# Code by Tahsha E. Say
#
# Part 2)
# log rank test in survival package comparing ACA to FSW
#
####################################################################################

library("survival")
library("survminer")

getwd() # get working directory
setwd("/Users/tahshasay/Documents/Binary_Data_from_R/Sett_2015Mar4_20191002")
getwd()


sett = read.delim("R_sett_d4.3.15_ind1hpe_cox_2018.03.07.txt", header = T, sep = "\t")

head(sett)

# tells you the number of samples in each treatment etc
summary(sett)

#----
# alt
summary(interaction(sett$broad, sett$fine))
summary(interaction(sett$induction, sett$fine))
writeLines(capture.output(summary(interaction(sett$induction, sett$fine))), "R_sett_TS1015_ind.ctrl__cox_lvno.s_2018.03.11_am.me.st.txt")



# SET UP BASELINE TREATMENT (CONSTANT LIGHT)
names(sett)

# order here matters


sett$fine <- factor(sett$fine, levels = c("induced", "control"))
#sett$fine <- relevel(sett$fine, c("am.me.st", "amphi", "sterile"))
##### be aware if you change this to sterile it will not pass assumptions!!!! 2018.03.11
sett$broad <- relevel(sett$broad, "nat")
summary(sett)
# check that after re-leveling your "baseline" treatment appears first in the list
levels(sett$broad)
levels(sett$fine)


# fit the model with without including the fine cue (controls were removed so only one factor remains)
fit.km <- coxph(Surv(time, censor) ~ fine, data = sett)


########################################################
### TESTING ASSUMPTIONS OF THE COX PROPORTIONAL TEST ###
########################################################

# test first assumption p164 in Survival analysis using S.  Expect the data to fall within the 95% confidence interval (ie. between the dashed lines).  Two do not - t7, t8 - expect that these are the two larvae that settled in the constant light treatment (can seem them on the previous survival plot).  Simon suggested removing these - but i dont not feel as if i need to.  


# 2018.02.26
# see also
# http://www.sthda.com/english/wiki/cox-model-assumptions
#
# The function cox.zph() [in the survival package] provides a convenient solution to test the proportional hazards assumption for each covariate included in a Cox refression model fit. For each covariate, the function cox.zph() correlates the corresponding set of scaled Schoenfeld residuals with time, to test for independence between residuals and time. Additionally, it performs a global test for the model as a whole.

#--------
# Example
#            rho chisq     p
#age     -0.0483 0.378 0.538
#sex      0.1265 2.349 0.125
#wt.loss  0.0126 0.024 0.877
#GLOBAL       NA 2.846 0.416

# From the output above, the test is not statistically significant for each of the covariates, and the global test is also not statistically significant. Therefore, we can assume the proportional hazards.


####################################
# The proportional hazard assumption is supported by a non-significant relationship between residuals and time, and refuted by a significant relationship.
####################################


#?cox.zph
cox.zph(fit.km)

writeLines(capture.output(cox.zph(fit.km)), "R_sett_TS1015_ind.ctrl__cox.zph_fine_assumptions_PASS_2018.03.11_am.me.st.txt")



fit.km

writeLines(capture.output(fit.km), "R_sett_TS1015_ind.ctrl__fit.km_fine_2018.03.11.txt")


# MORE ON ASSUMPTIONS?????? 2015
pdf("cox.zph_fitkm_plot_assumptions.pdf")
plot(cox.zph(fit.km))
dev.off()

# get output for use in stats table 
## exp(coef) corresponds to the 
summary(fit.km)

writeLines(capture.output(summary(fit.km)), "R_sett_TS1015_ind.ctrl__fit.km_fine_summary.txt")

##############################################################
# Call:
# coxph(formula = Surv(time, censor) ~ fine, data = sett)
# 
#   n= 324, number of events= 77 
# 
#                   coef  exp(coef)   se(coef)      z Pr(>|z|)
# finecontrol -1.986e+01  2.368e-09  3.132e+03 -0.006    0.995
# 
#             exp(coef) exp(-coef) lower .95 upper .95
# finecontrol 2.368e-09  422361242         0       Inf
# 
# Concordance= 0.697  (se = 0.031 )
# Rsquare= 0.183   (max possible= 0.914 )
# Likelihood ratio test= 65.67  on 1 df,   p=5e-16
# Wald test            = 0  on 1 df,   p=1
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Score (logrank) test = 42.79  on 1 df,   p=6e-11
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##############################################################


# PAIRWISE
#------------------------------------------------------
#  dealing with non events in one treatment group.
#------------------------------------------------------

#BECAUASE below may not work if you have 0 events in the sterile treatment
res.cox <- coxph(Surv(time, censor) ~ fine, data =  sett)
summary(res.cox)
# automatically returns pvalue


# so try below
fit <- survfit(res.cox)
fit 

sdf <- survfit(Surv(time, censor)~fine, data=indsett)
summary(sdf)
#---------------------------------------------------------------------------------------


####################################################################################
# Larval settlement statistics
# Code by Tahsha E. Say
#
# Part 3)
# log rank test in survival package comparing larvae that were maintained in constant light to larvae that were transferred from constant light into darkness
#
####################################################################################

library("survival")
library("survminer")

setwd("/Users/tahshasay/Documents/Binary_Data_from_R/Sett_2015Mar4_20191002")
getwd()


sett = read.delim("R_sett_d4.3.15_ind1hpe_cox_light-dark_2018.02.05.txt", header = T, sep = "\t")
head(transfer)


head(sett)

# tells you the number of samples in each treatment etc
summary(sett)

#----
# alt
summary(interaction(sett$broad, sett$fine))
summary(interaction(sett$induction, sett$fine))
writeLines(capture.output(summary(interaction(sett$induction, sett$fine))), "R_sett_TS1015_ind.ctrl__cox_lvno.s_2018.03.11_am.me.st.txt")

# SET UP BASELINE TREATMENT (CONSTANT LIGHT i.e. lgt)
names(sett)

sett$broad <- relevel(sett$broad, "lgt")
summary(sett)
# check that after re-leveling your "baseline" treatment appears first in the list
levels(sett$broad)
levels(sett$fine)

# fit the model with without including the fine cue (controls were removed so only one factor remains)
fit.km <- coxph(Surv(time, censor) ~ broad, data = sett)


cox.zph(fit.km)
writeLines(capture.output(cox.zph(fit.km)), "R_sett_TS1015_ind.ctrl__cox.zph_fine_assumptions_PASS_2018.03.11_am.me.st.txt")


fit.km

writeLines(capture.output(fit.km), "R_sett_TS1015_ind.ctrl__fit.km_fine_2018.03.11.txt")


pdf("cox.zph_fitkm_plot_assumptions.pdf")
plot(cox.zph(fit.km))
dev.off()

summary(fit.km)

writeLines(capture.output(summary(fit.km)), "R_sett_TS1015_ind.ctrl__fit.km_fine_summary.txt")

##############################################################
# Call:
# coxph(formula = Surv(time, censor) ~ broad, data = sett)
# 
#   n= 41, number of events= 13 
# 
#               coef exp(coef) se(coef)     z Pr(>|z|)   
# broadlgtdrk 2.1942    8.9728   0.7723 2.841  0.00449 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#             exp(coef) exp(-coef) lower .95 upper .95
# broadlgtdrk     8.973     0.1114     1.975     40.76
# 
# Concordance= 0.733  (se = 0.078 )
# Rsquare= 0.251   (max possible= 0.895 )
# Likelihood ratio test= 11.84  on 1 df,   p=6e-04
# Wald test            = 8.07  on 1 df,   p=0.004
# Score (logrank) test = 11.71  on 1 df,   p=6e-04
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Score (logrank) test = 11.71  on 1 df,   p=6e-04
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##############################################################
#---------------------------------------------------------------------------------------



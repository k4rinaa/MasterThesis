####### LIBRARIES #########
library("intsvy")
library("nlme")
library("dplyr")
library("mitools")
library("mitml")
library("mice")
library("tidyverse")
library("corrplot")
library("lme4")
library("WeMix")
library("reshape2")
library("jtools")
library("mixtools")

####### 1. DATA IMPORT #######
pisa2018_all <- pisa.select.merge(folder = file.path(getwd(), "PISA 2018"),
                              school.file = "CY07_MSU_SCH_QQQ.sav",
                              student.file = "CY07_MSU_STU_QQQ.sav",
                              student = c("CNT",
                                          "CNTSCHID",
                                          "STRATUM",
                                          "ST004D01T",
                                          "ESCS",
                                          "UNDREM",
                                          "METASUM",
                                          "JOYREAD",
                                          "SCREADCOMP",
                                          "SCREADDIFF",
                                          "WORKMAST",
                                          "GFOFAIL",
                                          "RESILIENCE",
                                          "MASTGOAL"),
                              school = c("CNTSCHID",
                                         "SC001Q01TA",
                                         "SC013Q01TA",
                                         "SCHSIZE",
                                         "W_SCHGRNRABWT"), 
                              countries = c("LTU", "LVA", "EST"))


# Which area school is located in (town, small town, village, etc.)
pisa2018_all$WHICH_AREA <- pisa2018_all$SC001Q01TA

# Public or private school adjusted
pisa2018_all$PUBLIC <- NA
pisa2018_all$PUBLIC[pisa2018_all$SC013Q01TA==1] <- 0
pisa2018_all$PUBLIC[pisa2018_all$SC013Q01TA==2] <- 1

# Adjusting Gender variable (1=female, 2=male) (converting to binary) - MALE
pisa2018_all$MALE <- NA
pisa2018_all$MALE[pisa2018_all$ST004D01T==1] <- 0
pisa2018_all$MALE[pisa2018_all$ST004D01T==2] <- 1


# Lithuania
pisa2018 <- pisa2018_all[pisa2018_all$CNT=='LTU',]
# Latvia
pisa2018_LVA <- pisa2018_all[pisa2018_all$CNT=='LVA',]
# Estonia
pisa2018_EST <- pisa2018_all[pisa2018_all$CNT=='EST', !grepl("GLCM", names(pisa2018_all))]

####### 2. DATA CLEANUP & PREPARATION #######

# CLUSTER WEIGHT - LTU
# Student sample size in school j
pisa2018 <- pisa2018 %>%
  group_by(CNTSCHID) %>%
  mutate(count = n())

# Sum of total student weights in school j
pisa2018 <- pisa2018 %>%
  group_by(CNTSCHID) %>%
  mutate(w_st_total_j = sum(W_FSTUWT))

# Cluster weight for student i in school j:
cluster_weight <- (pisa2018$count/pisa2018$w_st_total_j)*pisa2018$W_FSTUWT
pisa2018 <- cbind(pisa2018, cluster_weights=cluster_weight)
pisa2018 <- as.data.frame(pisa2018)

#--------------------------------#

# CLUSTER WEIGHT - LVA
# Student sample size in school j
pisa2018_LVA <- pisa2018_LVA %>%
  group_by(CNTSCHID) %>%
  mutate(count = n())

# Sum of total student weights in school j
pisa2018_LVA <- pisa2018_LVA %>%
  group_by(CNTSCHID) %>%
  mutate(w_st_total_j = sum(W_FSTUWT))

# Cluster weight for student i in school j:
cluster_weight <- (pisa2018_LVA$count/pisa2018_LVA$w_st_total_j)*pisa2018_LVA$W_FSTUWT
pisa2018_LVA <- cbind(pisa2018_LVA, cluster_weights=cluster_weight)
pisa2018_LVA <- as.data.frame(pisa2018_LVA)

#--------------------------------#

# CLUSTER WEIGHT - EST
# Student sample size in school j
pisa2018_EST <- pisa2018_EST %>%
  group_by(CNTSCHID) %>%
  mutate(count = n())

# Sum of total student weights in school j
pisa2018_EST <- pisa2018_EST %>%
  group_by(CNTSCHID) %>%
  mutate(w_st_total_j = sum(W_FSTUWT))

# Cluster weight for student i in school j:
cluster_weight <- (pisa2018_EST$count/pisa2018_EST$w_st_total_j)*pisa2018_EST$W_FSTUWT
pisa2018_EST <- cbind(pisa2018_EST, cluster_weights=cluster_weight)
pisa2018_EST <- as.data.frame(pisa2018_EST)

#--------------------------------#
####### 3. EXPLORATORY ANALYSIS ########
## Descriptive statistic
pisa.mean.pv(pvlabel="READ", data=pisa2018_all, by="CNT")
pisa.mean.pv(pvlabel="READ", data=pisa2018_all, by=c("CNT", "MALE"))
pisa.mean.pv(pvlabel="READ", data=pisa2018_all, by=c("CNT", "WHICH_AREA"))
pisa.mean(variable="MALE", data=pisa2018_all, by="CNT")
pisa.mean(variable="UNDREM", data=pisa2018_all, by="CNT")
pisa.mean(variable="METASUM", data=pisa2018_all, by="CNT")
pisa.mean(variable="JOYREAD", data=pisa2018_all, by="CNT")
pisa.mean(variable="SCREADCOMP", data=pisa2018_all, by="CNT")
pisa.mean(variable="SCREADDIFF", data=pisa2018_all, by="CNT")
pisa.mean(variable="WORKMAST", data=pisa2018_all, by="CNT")
pisa.mean(variable="GFOFAIL", data=pisa2018_all, by="CNT")
pisa.mean(variable="RESILIENCE", data=pisa2018_all, by="CNT")
pisa.mean(variable="MASTGOAL", data=pisa2018_all, by="CNT")
pisa.mean(variable="ESCS", data=pisa2018_all, by="CNT")
pisa.mean(variable="WHICH_AREA", data=pisa2018_all, by="CNT")
pisa.mean(variable="PUBLIC", data=pisa2018_all, by="CNT")

# Descriptives by proficiency levels
pisa.ben.pv(pvlabel="READ", data=pisa2018_all, by="CNT")

# Plotting averages in each school

pisa2018$READ_AVG <- rowMeans(pisa2018[,c(98, 99, 100, 101, 102, 103, 104, 105, 106, 107)])
pisa2018_LVA$READ_AVG <- rowMeans(pisa2018_LVA[,c(98, 99, 100, 101, 102, 103, 104, 105, 106, 107)])
pisa2018_EST$READ_AVG <- rowMeans(pisa2018_EST[,c(98, 99, 100, 101, 102, 103, 104, 105, 106, 107)])

pisa2018 <- pisa2018 %>% 
  group_by(CNTSCHID) %>% 
  mutate(avg = mean(READ_AVG))

pisa2018_LVA <- pisa2018_LVA %>% 
  group_by(CNTSCHID) %>% 
  mutate(avg = mean(READ_AVG))

pisa2018_EST<- pisa2018_EST %>% 
  group_by(CNTSCHID) %>% 
  mutate(avg = mean(READ_AVG))

par(mfrow=c(1,3))

plot(sort(unique(pisa2018$avg), decreasing = FALSE), 
     ylab = "Reading literacy", xlab = "School", pch=20, cex=2,
     main = "LITHUANIA", ylim = c(400,750), 
     cex.lab=1.5, cex.axis=1.5)
  abline(h = 475.87, col="red", lwd=3, lty=1)

plot(sort(unique(pisa2018_LVA$avg), decreasing = FALSE),
     ylab = "Reading literacy", xlab = "School", pch=20, cex=2,
     main = "LATVIA", ylim = c(400,750),
     cex.lab=1.5, cex.axis=1.5)
abline(h = 478.7, col="red", lwd=3, lty=1)

plot(sort(unique(pisa2018_EST$avg), decreasing = FALSE),
     ylab = "Reading literacy", xlab = "School", pch=20, cex=2,
     main = "ESTONIA", 
     cex.lab=1.5, cex.axis=1.5)
abline(h = 523.02, col="red", lwd=3, lty=1)

#--------------------------------#

## Missing values analysis

# How many missing values?
sum(is.na(pisa2018))
sum(is.na(pisa2018_LVA))
sum(is.na(pisa2018_EST))

# How many missing values by column?
colSums(is.na(pisa2018))
colSums(is.na(pisa2018_LVA))
colSums(is.na(pisa2018_EST))

# Which columns have missing values?
names(which(colSums(is.na(pisa2018))>0))
names(which(colSums(is.na(pisa2018_LVA))>0))
names(which(colSums(is.na(pisa2018_EST))>0))

## Handling missing values
# Omitting NA values by listwise deletion
pisa2018_2 <- na.omit(pisa2018)
pisa2018_2_LVA <- na.omit(pisa2018_LVA)
pisa2018_2_EST <- na.omit(pisa2018_EST)

#--------------------------------#

pisa2018final <- pisa2018_2
pisa2018final_LVA <- pisa2018_2_LVA
pisa2018final_EST <- pisa2018_2_EST

#--------------------------------#

# Create variable for average school ESCS

pisa2018final <- pisa2018final %>% 
  group_by(CNTSCHID) %>% 
  mutate(SCHOOL_ESCS = mean(ESCS))

pisa2018final_LVA <- pisa2018final_LVA %>% 
  group_by(CNTSCHID) %>% 
  mutate(SCHOOL_ESCS = mean(ESCS))

pisa2018final_EST <- pisa2018final_EST %>% 
  group_by(CNTSCHID) %>% 
  mutate(SCHOOL_ESCS = mean(ESCS))

#--------------------------------#

####### 3. MODELLING PART #######

######## UNCONDITIONAL MODEL #######
####### Lithuania ######
model0_pv1 <- mix(PV1READ~(1|CNTSCHID), 
              weights = c("cluster_weights", "W_SCHGRNRABWT"), 
              data = pisa2018final)
model0_pv2 <- mix(PV2READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv3 <- mix(PV3READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv4 <- mix(PV4READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv5 <- mix(PV5READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv6 <- mix(PV6READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv7 <- mix(PV7READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv8 <- mix(PV8READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv9 <- mix(PV9READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model0_pv10 <- mix(PV10READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)

## COEFFICIENTS
# Averages from all 10 PVs
model0_coef <- mean(model0_pv1$coef, model0_pv2$coef, model0_pv3$coef, model0_pv4$coef, model0_pv5$coef,
                    model0_pv6$coef, model0_pv7$coef, model0_pv8$coef, model0_pv9$coef, model0_pv10$coef)
round(model0_coef, 3)

## SE
M <- 10
model0_se <- mean(model0_pv1$SE, model0_pv2$SE, model0_pv3$SE, model0_pv4$SE, model0_pv5$SE,
                  model0_pv6$SE, model0_pv7$SE, model0_pv8$SE, model0_pv9$SE, model0_pv10$SE)+
  (1+(1/M))*var(c(model0_pv1$coef, model0_pv2$coef, model0_pv3$coef, model0_pv4$coef, model0_pv5$coef,
                  model0_pv6$coef, model0_pv7$coef, model0_pv8$coef, model0_pv9$coef, model0_pv10$coef))


## t-values
model0_t <- model0_coef/model0_se

## p-values
model0_p <- 2 * pnorm( abs(model0_t), lower.tail=FALSE)

## ICC
model0_ICC <- mean(model0_pv1$ICC, model0_pv2$ICC, model0_pv3$ICC, model0_pv4$ICC, model0_pv5$ICC,
                   model0_pv6$ICC, model0_pv7$ICC, model0_pv8$ICC, model0_pv9$ICC, model0_pv10$ICC)

# School variance
mean(model0_pv1$vars[1], model0_pv2$vars[1], model0_pv3$vars[1], model0_pv4$vars[1], model0_pv5$vars[1],
         model0_pv6$vars[1], model0_pv7$vars[1], model0_pv8$vars[1], model0_pv9$vars[1], model0_pv10$vars[1])

# Student level variance
mean(model0_pv1$vars[2], model0_pv2$vars[2], model0_pv3$vars[2], model0_pv4$vars[2], model0_pv5$vars[2],
     model0_pv6$vars[2], model0_pv7$vars[2], model0_pv8$vars[2], model0_pv9$vars[2], model0_pv20$vars[2])

## AIC
model0_loglik <- mean(model0_pv1$lnl, model0_pv2$lnl, model0_pv3$lnl, model0_pv4$lnl, model0_pv5$lnl,
                      model0_pv6$lnl, model0_pv7$lnl, model0_pv8$lnl, model0_pv9$lnl, model0_pv10$lnl)

model0_AIC <- -2 * model0_loglik + 2 * 2

## BIC
model0_BIC <- log(nrow(pisa2018final)) * 2 - 2 * model0_loglik

#--------------------------------#
########## Latvia #######

model0_pv1_LVA <- mix(PV1READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv2_LVA <- mix(PV2READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv3_LVA <- mix(PV3READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv4_LVA <- mix(PV4READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv5_LVA <- mix(PV5READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv6_LVA <- mix(PV6READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv7_LVA <- mix(PV7READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv8_LVA <- mix(PV8READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv9_LVA <- mix(PV9READ~(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model0_pv10_LVA <- mix(PV10READ~(1|CNTSCHID), 
                   weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                   data = pisa2018final_LVA)

## COEFFICIENTS
# Averages from all 10 PVs
model0_coef_LVA <- mean(model0_pv1_LVA$coef, model0_pv2_LVA$coef, model0_pv3_LVA$coef, model0_pv4_LVA$coef, model0_pv5_LVA$coef,
                    model0_pv6_LVA$coef, model0_pv7_LVA$coef, model0_pv8_LVA$coef, model0_pv9_LVA$coef, model0_pv10_LVA$coef)

## SE
M <- 10
model0_se_LVA <- mean(model0_pv1_LVA$SE, model0_pv2_LVA$SE, model0_pv3_LVA$SE, model0_pv4_LVA$SE, model0_pv5_LVA$SE,
                  model0_pv6_LVA$SE, model0_pv7_LVA$SE, model0_pv8_LVA$SE, model0_pv9_LVA$SE, model0_pv10_LVA$SE)
+(1+(1/M))*var(c(model0_pv1_LVA$coef, model0_pv2_LVA$coef, model0_pv3_LVA$coef, model0_pv4_LVA$coef, model0_pv5_LVA$coef,
                 model0_pv6_LVA$coef, model0_pv7_LVA$coef, model0_pv8_LVA$coef, model0_pv9_LVA$coef, model0_pv10_LVA$coef))


## t-values
model0_t_LVA <- model0_coef_LVA/model0_se_LVA

## p-values
model0_p_LVA <- 2 * pnorm( abs(model0_t_LVA), lower.tail=FALSE)

## ICC
model0_ICC_LVA<- mean(model0_pv1_LVA$ICC, model0_pv2_LVA$ICC, model0_pv3_LVA$ICC, model0_pv4_LVA$ICC, model0_pv5_LVA$ICC,
                   model0_pv6_LVA$ICC, model0_pv7_LVA$ICC, model0_pv8_LVA$ICC, model0_pv9_LVA$ICC, model0_pv10_LVA$ICC)

# School variance
mean(model0_pv1_LVA$vars[1], model0_pv2_LVA$vars[1], model0_pv3_LVA$vars[1], model0_pv4_LVA$vars[1], model0_pv5_LVA$vars[1],
     model0_pv6_LVA$vars[1], model0_pv7_LVA$vars[1], model0_pv8_LVA$vars[1], model0_pv9_LVA$vars[1], model0_pv10_LVA$vars[1])

# Student level variance
mean(model0_pv1_LVA$vars[2], model0_pv2_LVA$vars[2], model0_pv3_LVA$vars[2], model0_pv4_LVA$vars[2], model0_pv5_LVA$vars[2],
     model0_pv6_LVA$vars[2], model0_pv7_LVA$vars[2], model0_pv8_LVA$vars[2], model0_pv9_LVA$vars[2], model0_pv10_LVA$vars[2])

## AIC
model0_loglik_LVA <- mean(model0_pv1_LVA$lnl, model0_pv2_LVA$lnl, model0_pv3_LVA$lnl, model0_pv4_LVA$lnl, model0_pv5_LVA$lnl,
                      model0_pv6_LVA$lnl, model0_pv7_LVA$lnl, model0_pv8_LVA$lnl, model0_pv9_LVA$lnl, model0_pv10_LVA$lnl)

model0_AIC_LVA <- -2 * model0_loglik_LVA + 2 * 1

## BIC
model0_BIC_LVA <- log(nrow(pisa2018final_LVA)) * 2 - 2 * model0_loglik_LVA

#--------------------------------#
######## Estonia #######

model0_pv1_EST <- mix(PV1READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv2_EST <- mix(PV2READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv3_EST <- mix(PV3READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv4_EST <- mix(PV4READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv5_EST <- mix(PV5READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv6_EST <- mix(PV6READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv7_EST <- mix(PV7READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv8_EST <- mix(PV8READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv9_EST <- mix(PV9READ~(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model0_pv10_EST <- mix(PV10READ~(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)

## COEFFICIENTS
# Averages from all 10 PVs
model0_coef_EST <- mean(model0_pv1_EST$coef, model0_pv2_EST$coef, model0_pv3_EST$coef, model0_pv4_EST$coef, model0_pv5_EST$coef,
                        model0_pv6_EST$coef, model0_pv7_EST$coef, model0_pv8_EST$coef, model0_pv9_EST$coef, model0_pv10_EST$coef)

## SE
M <- 10
model0_se_EST <- mean(model0_pv1_EST$SE, model0_pv2_EST$SE, model0_pv3_EST$SE, model0_pv4_EST$SE, model0_pv5_EST$SE,
                      model0_pv6_EST$SE, model0_pv7_EST$SE, model0_pv8_EST$SE, model0_pv9_EST$SE, model0_pv10_EST$SE)
+(1+(1/M))*var(c(model0_pv1_EST$coef, model0_pv2_EST$coef, model0_pv3_EST$coef, model0_pv4_EST$coef, model0_pv5_EST$coef,
                 model0_pv6_EST$coef, model0_pv7_EST$coef, model0_pv8_EST$coef, model0_pv9_EST$coef, model0_pv10_EST$coef))


## t-values
model0_t_EST <- model0_coef_EST/model0_se_EST

## p-values
model0_p_EST <- 2 * pnorm( abs(model0_t_EST), lower.tail=FALSE)

## ICC
model0_ICC_EST<- mean(model0_pv1_EST$ICC, model0_pv2_EST$ICC, model0_pv3_EST$ICC, model0_pv4_EST$ICC, model0_pv5_EST$ICC,
                      model0_pv6_EST$ICC, model0_pv7_EST$ICC, model0_pv8_EST$ICC, model0_pv9_EST$ICC, model0_pv10_EST$ICC)


# School variance
mean(model0_pv1_EST$vars[1], model0_pv2_EST$vars[1], model0_pv3_EST$vars[1], model0_pv4_EST$vars[1], model0_pv5_EST$vars[1],
     model0_pv6_EST$vars[1], model0_pv7_EST$vars[1], model0_pv8_EST$vars[1], model0_pv9_EST$vars[1], model0_pv10_EST$vars[1])

# Student level variance
mean(model0_pv1_EST$vars[2], model0_pv2_EST$vars[2], model0_pv3_EST$vars[2], model0_pv4_EST$vars[2], model0_pv5_EST$vars[2],
     model0_pv6_EST$vars[2], model0_pv7_EST$vars[2], model0_pv8_EST$vars[2], model0_pv9_EST$vars[2], model0_pv10_EST$vars[2])



## AIC
model0_loglik_EST <- mean(model0_pv1_EST$lnl, model0_pv2_EST$lnl, model0_pv3_EST$lnl, model0_pv4_EST$lnl, model0_pv5_EST$lnl,
                          model0_pv6_EST$lnl, model0_pv7_EST$lnl, model0_pv8_EST$lnl, model0_pv9_EST$lnl, model0_pv10_EST$lnl)

model0_AIC_EST <- -2 * model0_loglik_EST + 2 * 2

## BIC
model0_BIC_EST <- log(nrow(pisa2018final_EST)) * 2 - 2 * model0_loglik_EST


#--------------------------------#

########## Only student level variables included

########## Lithuania #######
model1_pv1 <- mix(PV1READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv2 <- mix(PV2READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv3 <- mix(PV3READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv4 <- mix(PV4READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv5 <- mix(PV5READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv6 <- mix(PV6READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv7 <- mix(PV7READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv8 <- mix(PV8READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv9 <- mix(PV9READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model1_pv10 <- mix(PV10READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                   weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                   data = pisa2018final)

## COEFFICIENTS
# Averages from all 10 PVs
model1_coef <- rowMeans(cbind(model1_pv1$coef, model1_pv2$coef, model1_pv3$coef, model1_pv4$coef, model1_pv5$coef,
                    model1_pv6$coef, model1_pv7$coef, model1_pv8$coef, model1_pv9$coef, model1_pv10$coef))
as.data.frame(model1_coef)

## SE
M <- 10

se_coef1 <- mean(model1_pv1$SE[1], model1_pv2$SE[1], model1_pv3$SE[1], model1_pv4$SE[1], model1_pv5$SE[1],
     model1_pv6$SE[1], model1_pv7$SE[1], model1_pv8$SE[1], model1_pv9$SE[1], model1_pv10$SE[1])+(1+(1/M))*var(
       c(model1_pv1$coef[1], model1_pv2$coef[1], model1_pv3$coef[1], model1_pv4$coef[1], 
                 model1_pv5$coef[1], model1_pv6$coef[1], model1_pv7$coef[1], model1_pv8$coef[1], 
                 model1_pv9$coef[1], model1_pv10$coef[1]))

se_coef2 <- mean(model1_pv1$SE[2], model1_pv2$SE[2], model1_pv3$SE[2], model1_pv4$SE[2], model1_pv5$SE[2],
                 model1_pv6$SE[2], model1_pv7$SE[2], model1_pv8$SE[2], model1_pv9$SE[2], model1_pv10$SE[2])+(1+(1/M))*var(
                   c(model1_pv1$coef[2], model1_pv2$coef[2], model1_pv3$coef[2], model1_pv4$coef[2], 
                     model1_pv5$coef[2], model1_pv6$coef[2], model1_pv7$coef[2], model1_pv8$coef[2], 
                     model1_pv9$coef[2], model1_pv10$coef[2]))

se_coef3 <- mean(model1_pv1$SE[3], model1_pv2$SE[3], model1_pv3$SE[3], model1_pv4$SE[3], model1_pv5$SE[3],
                 model1_pv6$SE[3], model1_pv7$SE[3], model1_pv8$SE[3], model1_pv9$SE[3], model1_pv10$SE[3])+(1+(1/M))*var(
                   c(model1_pv1$coef[3], model1_pv2$coef[3], model1_pv3$coef[3], model1_pv4$coef[3], 
                     model1_pv5$coef[3], model1_pv6$coef[3], model1_pv7$coef[3], model1_pv8$coef[3], 
                     model1_pv9$coef[3], model1_pv10$coef[3]))

se_coef4 <- mean(model1_pv1$SE[4], model1_pv2$SE[4], model1_pv3$SE[4], model1_pv4$SE[4], model1_pv5$SE[4],
                 model1_pv6$SE[4], model1_pv7$SE[4], model1_pv8$SE[4], model1_pv9$SE[4], model1_pv10$SE[4])+(1+(1/M))*var(
                   c(model1_pv1$coef[4], model1_pv2$coef[4], model1_pv3$coef[4], model1_pv4$coef[4], 
                     model1_pv5$coef[4], model1_pv6$coef[4], model1_pv7$coef[4], model1_pv8$coef[4], 
                     model1_pv9$coef[4], model1_pv10$coef[4]))

se_coef5 <- mean(model1_pv1$SE[5], model1_pv2$SE[5], model1_pv3$SE[5], model1_pv4$SE[5], model1_pv5$SE[5],
                 model1_pv6$SE[5], model1_pv7$SE[5], model1_pv8$SE[5], model1_pv9$SE[5], model1_pv10$SE[5])+(1+(1/M))*var(
                   c(model1_pv1$coef[5], model1_pv2$coef[5], model1_pv3$coef[5], model1_pv4$coef[5], 
                     model1_pv5$coef[5], model1_pv6$coef[5], model1_pv7$coef[5], model1_pv8$coef[5], 
                     model1_pv9$coef[5], model1_pv10$coef[5]))

se_coef6 <- mean(model1_pv1$SE[6], model1_pv2$SE[6], model1_pv3$SE[6], model1_pv4$SE[6], model1_pv5$SE[6],
                 model1_pv6$SE[6], model1_pv7$SE[6], model1_pv8$SE[6], model1_pv9$SE[6], model1_pv10$SE[6])+(1+(1/M))*var(
                   c(model1_pv1$coef[6], model1_pv2$coef[6], model1_pv3$coef[6], model1_pv4$coef[6], 
                     model1_pv5$coef[6], model1_pv6$coef[6], model1_pv7$coef[6], model1_pv8$coef[6], 
                     model1_pv9$coef[6], model1_pv10$coef[6]))

se_coef7 <- mean(model1_pv1$SE[7], model1_pv2$SE[7], model1_pv3$SE[7], model1_pv4$SE[7], model1_pv5$SE[7],
                 model1_pv6$SE[7], model1_pv7$SE[7], model1_pv8$SE[7], model1_pv9$SE[7], model1_pv10$SE[7])+(1+(1/M))*var(
                   c(model1_pv1$coef[7], model1_pv2$coef[7], model1_pv3$coef[7], model1_pv4$coef[7], 
                     model1_pv5$coef[7], model1_pv6$coef[7], model1_pv7$coef[7], model1_pv8$coef[7], 
                     model1_pv9$coef[7], model1_pv10$coef[7]))

se_coef8 <- mean(model1_pv1$SE[8], model1_pv2$SE[8], model1_pv3$SE[8], model1_pv4$SE[8], model1_pv5$SE[8],
                 model1_pv6$SE[8], model1_pv7$SE[8], model1_pv8$SE[8], model1_pv9$SE[8], model1_pv10$SE[8])+(1+(1/M))*var(
                   c(model1_pv1$coef[8], model1_pv2$coef[8], model1_pv3$coef[8], model1_pv4$coef[8], 
                     model1_pv5$coef[8], model1_pv6$coef[8], model1_pv7$coef[8], model1_pv8$coef[8], 
                     model1_pv9$coef[8], model1_pv10$coef[8]))

se_coef9 <- mean(model1_pv1$SE[9], model1_pv2$SE[9], model1_pv3$SE[9], model1_pv4$SE[9], model1_pv5$SE[9],
                 model1_pv6$SE[9], model1_pv7$SE[9], model1_pv8$SE[9], model1_pv9$SE[9], model1_pv10$SE[9])+(1+(1/M))*var(
                   c(model1_pv1$coef[9], model1_pv2$coef[9], model1_pv3$coef[9], model1_pv4$coef[9], 
                     model1_pv5$coef[9], model1_pv6$coef[9], model1_pv7$coef[9], model1_pv8$coef[9], 
                     model1_pv9$coef[9], model1_pv10$coef[9]))

se_coef10 <- mean(model1_pv1$SE[10], model1_pv2$SE[10], model1_pv3$SE[10], model1_pv4$SE[10], model1_pv5$SE[10],
                 model1_pv6$SE[10], model1_pv7$SE[10], model1_pv8$SE[10], model1_pv9$SE[10], model1_pv10$SE[10])+(1+(1/M))*var(
                   c(model1_pv1$coef[10], model1_pv2$coef[10], model1_pv3$coef[10], model1_pv4$coef[10], 
                     model1_pv5$coef[10], model1_pv6$coef[10], model1_pv7$coef[10], model1_pv8$coef[10], 
                     model1_pv9$coef[10], model1_pv10$coef[10]))

se_coef11 <- mean(model1_pv1$SE[11], model1_pv2$SE[11], model1_pv3$SE[11], model1_pv4$SE[11], model1_pv5$SE[11],
                 model1_pv6$SE[11], model1_pv7$SE[11], model1_pv8$SE[11], model1_pv9$SE[11], model1_pv10$SE[11])+(1+(1/M))*var(
                   c(model1_pv1$coef[11], model1_pv2$coef[11], model1_pv3$coef[11], model1_pv4$coef[11], 
                     model1_pv5$coef[11], model1_pv6$coef[11], model1_pv7$coef[11], model1_pv8$coef[11], 
                     model1_pv9$coef[11], model1_pv10$coef[11]))

model1_se <- rbind(Intercept=se_coef1, MALE=se_coef2, UNDREM=se_coef3, METASUM=se_coef4, JOYREAD=se_coef5, 
                   RESILIENCE=se_coef6, SCREADCOMP=se_coef7, SCREADDIFF=se_coef8, GFOFAIL=se_coef9, WORKMAST=se_coef10, MASTGOAL=se_coef11)


## t-values
model1_t <- model1_coef/model1_se

## p-values
model1_p <- round(2 * pnorm( abs(model1_t), lower.tail=FALSE), 3)


## ICC
model1_ICC <- mean(model1_pv1$ICC, model1_pv2$ICC, model1_pv3$ICC, model1_pv4$ICC, model1_pv5$ICC,
                   model1_pv6$ICC, model1_pv7$ICC, model1_pv8$ICC, model1_pv9$ICC, model1_pv10$ICC)


# School variance
mean(model1_pv1$vars[1], model1_pv2$vars[1], model1_pv3$vars[1], model1_pv4$vars[1], model1_pv5$vars[1],
     model1_pv6$vars[1], model1_pv7$vars[1], model1_pv8$vars[1], model1_pv9$vars[1], model1_pv10$vars[1])

# Student level variance
mean(model1_pv1$vars[2], model1_pv2$vars[2], model1_pv3$vars[2], model1_pv4$vars[2], model1_pv5$vars[2],
     model1_pv6$vars[2], model1_pv7$vars[2], model1_pv8$vars[2], model1_pv9$vars[2], model1_pv20$vars[2])

## AIC
model1_loglik <- mean(model1_pv1$lnl, model1_pv2$lnl, model1_pv3$lnl, model1_pv4$lnl, model1_pv5$lnl,
                      model1_pv6$lnl, model1_pv7$lnl, model1_pv8$lnl, model1_pv9$lnl, model1_pv10$lnl)

model1_AIC <- -2 * model1_loglik + 2 * 11

## BIC
model1_BIC <- log(nrow(pisa2018final)) * 11 - 2 * model1_loglik


#--------------------------------#
######## Latvia ######

model1_pv1_lva <- mix(PV1READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv2_lva <- mix(PV2READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv3_lva <- mix(PV3READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv4_lva <- mix(PV4READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv5_lva <- mix(PV5READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv6_lva <- mix(PV6READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv7_lva <- mix(PV7READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv8_lva <- mix(PV8READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv9_lva <- mix(PV9READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_LVA)
model1_pv10_lva <- mix(PV10READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_LVA)

## COEFFICIENTS
# Averages from all 10 PVs
model1_coef_lva <- rowMeans(cbind(model1_pv1_lva$coef, model1_pv2_lva$coef, model1_pv3_lva$coef, model1_pv4_lva$coef, model1_pv5_lva$coef,
                              model1_pv6_lva$coef, model1_pv7_lva$coef, model1_pv8_lva$coef, model1_pv9_lva$coef, model1_pv10_lva$coef))
as.data.frame(model1_coef_lva)
## SE
M <- 10

se_coef1_lva <- mean(model1_pv1_lva$SE[1], model1_pv2_lva$SE[1], model1_pv3_lva$SE[1], model1_pv4_lva$SE[1], model1_pv5_lva$SE[1],
                     model1_pv6_lva$SE[1], model1_pv7_lva$SE[1], model1_pv8_lva$SE[1], model1_pv9_lva$SE[1], model1_pv10_lva$SE[1])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[1], model1_pv2_lva$coef[1], model1_pv3_lva$coef[1], model1_pv4_lva$coef[1], 
                         model1_pv5_lva$coef[1], model1_pv6_lva$coef[1], model1_pv7_lva$coef[1], model1_pv8_lva$coef[1], 
                         model1_pv9_lva$coef[1], model1_pv10_lva$coef[1]))

se_coef2_lva <- mean(model1_pv1_lva$SE[2], model1_pv2_lva$SE[2], model1_pv3_lva$SE[2], model1_pv4_lva$SE[2], model1_pv5_lva$SE[2],
                     model1_pv6_lva$SE[2], model1_pv7_lva$SE[2], model1_pv8_lva$SE[2], model1_pv9_lva$SE[2], model1_pv10_lva$SE[2])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[2], model1_pv2_lva$coef[2], model1_pv3_lva$coef[2], model1_pv4_lva$coef[2], 
                         model1_pv5_lva$coef[2], model1_pv6_lva$coef[2], model1_pv7_lva$coef[2], model1_pv8_lva$coef[2], 
                         model1_pv9_lva$coef[2], model1_pv10_lva$coef[2]))

se_coef3_lva <- mean(model1_pv1_lva$SE[3], model1_pv2_lva$SE[3], model1_pv3_lva$SE[3], model1_pv4_lva$SE[3], model1_pv5_lva$SE[3],
                     model1_pv6_lva$SE[3], model1_pv7_lva$SE[3], model1_pv8_lva$SE[3], model1_pv9_lva$SE[3], model1_pv10_lva$SE[3])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[3], model1_pv2_lva$coef[3], model1_pv3_lva$coef[3], model1_pv4_lva$coef[3], 
                         model1_pv5_lva$coef[3], model1_pv6_lva$coef[3], model1_pv7_lva$coef[3], model1_pv8_lva$coef[3], 
                         model1_pv9_lva$coef[3], model1_pv10_lva$coef[3]))

se_coef4_lva <- mean(model1_pv1_lva$SE[4], model1_pv2_lva$SE[4], model1_pv3_lva$SE[4], model1_pv4_lva$SE[4], model1_pv5_lva$SE[4],
                     model1_pv6_lva$SE[4], model1_pv7_lva$SE[4], model1_pv8_lva$SE[4], model1_pv9_lva$SE[4], model1_pv10_lva$SE[4])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[4], model1_pv2_lva$coef[4], model1_pv3_lva$coef[4], model1_pv4_lva$coef[4], 
                         model1_pv5_lva$coef[4], model1_pv6_lva$coef[4], model1_pv7_lva$coef[4], model1_pv8_lva$coef[4], 
                         model1_pv9_lva$coef[4], model1_pv10_lva$coef[4]))

se_coef5_lva <- mean(model1_pv1_lva$SE[5], model1_pv2_lva$SE[5], model1_pv3_lva$SE[5], model1_pv4_lva$SE[5], model1_pv5_lva$SE[5],
                     model1_pv6_lva$SE[5], model1_pv7_lva$SE[5], model1_pv8_lva$SE[5], model1_pv9_lva$SE[5], model1_pv10_lva$SE[5])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[5], model1_pv2_lva$coef[5], model1_pv3_lva$coef[5], model1_pv4_lva$coef[5], 
                         model1_pv5_lva$coef[5], model1_pv6_lva$coef[5], model1_pv7_lva$coef[5], model1_pv8_lva$coef[5], 
                         model1_pv9_lva$coef[5], model1_pv10_lva$coef[5]))

se_coef6_lva <- mean(model1_pv1_lva$SE[6], model1_pv2_lva$SE[6], model1_pv3_lva$SE[6], model1_pv4_lva$SE[6], model1_pv5_lva$SE[6],
                     model1_pv6_lva$SE[6], model1_pv7_lva$SE[6], model1_pv8_lva$SE[6], model1_pv9_lva$SE[6], model1_pv10_lva$SE[6])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[6], model1_pv2_lva$coef[6], model1_pv3_lva$coef[6], model1_pv4_lva$coef[6], 
                         model1_pv5_lva$coef[6], model1_pv6_lva$coef[6], model1_pv7_lva$coef[6], model1_pv8_lva$coef[6], 
                         model1_pv9_lva$coef[6], model1_pv10_lva$coef[6]))

se_coef7_lva <- mean(model1_pv1_lva$SE[7], model1_pv2_lva$SE[7], model1_pv3_lva$SE[7], model1_pv4_lva$SE[7], model1_pv5_lva$SE[7],
                     model1_pv6_lva$SE[7], model1_pv7_lva$SE[7], model1_pv8_lva$SE[7], model1_pv9_lva$SE[7], model1_pv10_lva$SE[7])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[7], model1_pv2_lva$coef[7], model1_pv3_lva$coef[7], model1_pv4_lva$coef[7], 
                         model1_pv5_lva$coef[7], model1_pv6_lva$coef[7], model1_pv7_lva$coef[7], model1_pv8_lva$coef[7], 
                         model1_pv9_lva$coef[7], model1_pv10_lva$coef[7]))

se_coef8_lva <- mean(model1_pv1_lva$SE[8], model1_pv2_lva$SE[8], model1_pv3_lva$SE[8], model1_pv4_lva$SE[8], model1_pv5_lva$SE[8],
                     model1_pv6_lva$SE[8], model1_pv7_lva$SE[8], model1_pv8_lva$SE[8], model1_pv9_lva$SE[8], model1_pv10_lva$SE[8])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[8], model1_pv2_lva$coef[8], model1_pv3_lva$coef[8], model1_pv4_lva$coef[8], 
                         model1_pv5_lva$coef[8], model1_pv6_lva$coef[8], model1_pv7_lva$coef[8], model1_pv8_lva$coef[8], 
                         model1_pv9_lva$coef[8], model1_pv10_lva$coef[8]))

se_coef9_lva <- mean(model1_pv1_lva$SE[9], model1_pv2_lva$SE[9], model1_pv3_lva$SE[9], model1_pv4_lva$SE[9], model1_pv5_lva$SE[9],
                     model1_pv6_lva$SE[9], model1_pv7_lva$SE[9], model1_pv8_lva$SE[9], model1_pv9_lva$SE[9], model1_pv10_lva$SE[9])+(1+(1/M))*var(
                       c(model1_pv1_lva$coef[9], model1_pv2_lva$coef[9], model1_pv3_lva$coef[9], model1_pv4_lva$coef[9], 
                         model1_pv5_lva$coef[9], model1_pv6_lva$coef[9], model1_pv7_lva$coef[9], model1_pv8_lva$coef[9], 
                         model1_pv9_lva$coef[9], model1_pv10_lva$coef[9]))

se_coef10_lva <- mean(model1_pv1_lva$SE[10], model1_pv2_lva$SE[10], model1_pv3_lva$SE[10], model1_pv4_lva$SE[10], model1_pv5_lva$SE[10],
                      model1_pv6_lva$SE[10], model1_pv7_lva$SE[10], model1_pv8_lva$SE[10], model1_pv9_lva$SE[10], model1_pv10_lva$SE[10])+(1+(1/M))*var(
                        c(model1_pv1_lva$coef[10], model1_pv2_lva$coef[10], model1_pv3_lva$coef[10], model1_pv4_lva$coef[10], 
                          model1_pv5_lva$coef[10], model1_pv6_lva$coef[10], model1_pv7_lva$coef[10], model1_pv8_lva$coef[10], 
                          model1_pv9_lva$coef[10], model1_pv10_lva$coef[10]))

se_coef11_lva <- mean(model1_pv1_lva$SE[11], model1_pv2_lva$SE[11], model1_pv3_lva$SE[11], model1_pv4_lva$SE[11], model1_pv5_lva$SE[11],
                      model1_pv6_lva$SE[11], model1_pv7_lva$SE[11], model1_pv8_lva$SE[11], model1_pv9_lva$SE[11], model1_pv10_lva$SE[11])+(1+(1/M))*var(
                        c(model1_pv1_lva$coef[11], model1_pv2_lva$coef[11], model1_pv3_lva$coef[11], model1_pv4_lva$coef[11], 
                          model1_pv5_lva$coef[11], model1_pv6_lva$coef[11], model1_pv7_lva$coef[11], model1_pv8_lva$coef[11], 
                          model1_pv9_lva$coef[11], model1_pv10_lva$coef[11]))

model1_se_lva <- rbind(Intercept=se_coef1_lva, MALE=se_coef2_lva, UNDREM=se_coef3_lva, METASUM=se_coef4_lva, JOYREAD=se_coef5_lva, 
                   RESILIENCE=se_coef6_lva, SCREADCOMP=se_coef7_lva, SCREADDIFF=se_coef8_lva, GFOFAIL=se_coef9_lva, WORKMAST=se_coef10_lva, MASTGOAL=se_coef11_lva)


## t-values
model1_t_lva <- model1_coef_lva/model1_se_lva

## p-values
model1_p_lva <- round(2 * pnorm( abs(model1_t_lva), lower.tail=FALSE), 3)


## ICC
model1_ICC_lva <- mean(model1_pv1_lva$ICC, model1_pv2_lva$ICC, model1_pv3_lva$ICC, model1_pv4_lva$ICC, model1_pv5_lva$ICC,
                   model1_pv6_lva$ICC, model1_pv7_lva$ICC, model1_pv8_lva$ICC, model1_pv9_lva$ICC, model1_pv10_lva$ICC)

# School variance
mean(model1_pv1_lva$vars[1], model1_pv2_lva$vars[1], model1_pv3_lva$vars[1], model1_pv4_lva$vars[1], model1_pv5_lva$vars[1],
     model1_pv6_lva$vars[1], model1_pv7_lva$vars[1], model1_pv8_lva$vars[1], model1_pv9_lva$vars[1], model1_pv10_lva$vars[1])

# Student level variance
mean(model1_pv1_lva$vars[2], model1_pv2_lva$vars[2], model1_pv3_lva$vars[2], model1_pv4_lva$vars[2], model1_pv5_lva$vars[2],
     model1_pv6_lva$vars[2], model1_pv7_lva$vars[2], model1_pv8_lva$vars[2], model1_pv9_lva$vars[2], model1_pv10_lva$vars[2])


## AIC
model1_loglik_lva <- mean(model1_pv1_lva$lnl, model1_pv2_lva$lnl, model1_pv3_lva$lnl, model1_pv4_lva$lnl, model1_pv5_lva$lnl,
                      model1_pv6_lva$lnl, model1_pv7_lva$lnl, model1_pv8_lva$lnl, model1_pv9_lva$lnl, model1_pv10_lva$lnl)

model1_AIC_lva <- -2 * model1_loglik_lva + 2 * 11

## BIC
model1_BIC_lva <- log(nrow(pisa2018final_LVA)) * 11 - 2 * model1_loglik_lva


#--------------------------------#
########## Estonia ########

model1_pv1_est <- mix(PV1READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv2_est <- mix(PV2READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv3_est <- mix(PV3READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv4_est <- mix(PV4READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv5_est <- mix(PV5READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv6_est <- mix(PV6READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv7_est <- mix(PV7READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv8_est <- mix(PV8READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv9_est <- mix(PV9READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model1_pv10_est <- mix(PV10READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)

## COEFFICIENTS
# Averages from all 10 PVs
model1_coef_est <- rowMeans(cbind(model1_pv1_est$coef, model1_pv2_est$coef, model1_pv3_est$coef, model1_pv4_est$coef, model1_pv5_est$coef,
                                  model1_pv6_est$coef, model1_pv7_est$coef, model1_pv8_est$coef, model1_pv9_est$coef, model1_pv10_est$coef))
as.data.frame(model1_coef_est)
## SE
M <- 10

se_coef1_est <- mean(model1_pv1_est$SE[1], model1_pv2_est$SE[1], model1_pv3_est$SE[1], model1_pv4_est$SE[1], model1_pv5_est$SE[1],
                     model1_pv6_est$SE[1], model1_pv7_est$SE[1], model1_pv8_est$SE[1], model1_pv9_est$SE[1], model1_pv10_est$SE[1])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[1], model1_pv2_est$coef[1], model1_pv3_est$coef[1], model1_pv4_est$coef[1], 
                         model1_pv5_est$coef[1], model1_pv6_est$coef[1], model1_pv7_est$coef[1], model1_pv8_est$coef[1], 
                         model1_pv9_est$coef[1], model1_pv10_est$coef[1]))

se_coef2_est <- mean(model1_pv1_est$SE[2], model1_pv2_est$SE[2], model1_pv3_est$SE[2], model1_pv4_est$SE[2], model1_pv5_est$SE[2],
                     model1_pv6_est$SE[2], model1_pv7_est$SE[2], model1_pv8_est$SE[2], model1_pv9_est$SE[2], model1_pv10_est$SE[2])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[2], model1_pv2_est$coef[2], model1_pv3_est$coef[2], model1_pv4_est$coef[2], 
                         model1_pv5_est$coef[2], model1_pv6_est$coef[2], model1_pv7_est$coef[2], model1_pv8_est$coef[2], 
                         model1_pv9_est$coef[2], model1_pv10_est$coef[2]))

se_coef3_est <- mean(model1_pv1_est$SE[3], model1_pv2_est$SE[3], model1_pv3_est$SE[3], model1_pv4_est$SE[3], model1_pv5_est$SE[3],
                     model1_pv6_est$SE[3], model1_pv7_est$SE[3], model1_pv8_est$SE[3], model1_pv9_est$SE[3], model1_pv10_est$SE[3])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[3], model1_pv2_est$coef[3], model1_pv3_est$coef[3], model1_pv4_est$coef[3], 
                         model1_pv5_est$coef[3], model1_pv6_est$coef[3], model1_pv7_est$coef[3], model1_pv8_est$coef[3], 
                         model1_pv9_est$coef[3], model1_pv10_est$coef[3]))

se_coef4_est <- mean(model1_pv1_est$SE[4], model1_pv2_est$SE[4], model1_pv3_est$SE[4], model1_pv4_est$SE[4], model1_pv5_est$SE[4],
                     model1_pv6_est$SE[4], model1_pv7_est$SE[4], model1_pv8_est$SE[4], model1_pv9_est$SE[4], model1_pv10_est$SE[4])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[4], model1_pv2_est$coef[4], model1_pv3_est$coef[4], model1_pv4_est$coef[4], 
                         model1_pv5_est$coef[4], model1_pv6_est$coef[4], model1_pv7_est$coef[4], model1_pv8_est$coef[4], 
                         model1_pv9_est$coef[4], model1_pv10_est$coef[4]))

se_coef5_est <- mean(model1_pv1_est$SE[5], model1_pv2_est$SE[5], model1_pv3_est$SE[5], model1_pv4_est$SE[5], model1_pv5_est$SE[5],
                     model1_pv6_est$SE[5], model1_pv7_est$SE[5], model1_pv8_est$SE[5], model1_pv9_est$SE[5], model1_pv10_est$SE[5])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[5], model1_pv2_est$coef[5], model1_pv3_est$coef[5], model1_pv4_est$coef[5], 
                         model1_pv5_est$coef[5], model1_pv6_est$coef[5], model1_pv7_est$coef[5], model1_pv8_est$coef[5], 
                         model1_pv9_est$coef[5], model1_pv10_est$coef[5]))

se_coef6_est <- mean(model1_pv1_est$SE[6], model1_pv2_est$SE[6], model1_pv3_est$SE[6], model1_pv4_est$SE[6], model1_pv5_est$SE[6],
                     model1_pv6_est$SE[6], model1_pv7_est$SE[6], model1_pv8_est$SE[6], model1_pv9_est$SE[6], model1_pv10_est$SE[6])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[6], model1_pv2_est$coef[6], model1_pv3_est$coef[6], model1_pv4_est$coef[6], 
                         model1_pv5_est$coef[6], model1_pv6_est$coef[6], model1_pv7_est$coef[6], model1_pv8_est$coef[6], 
                         model1_pv9_est$coef[6], model1_pv10_est$coef[6]))

se_coef7_est <- mean(model1_pv1_est$SE[7], model1_pv2_est$SE[7], model1_pv3_est$SE[7], model1_pv4_est$SE[7], model1_pv5_est$SE[7],
                     model1_pv6_est$SE[7], model1_pv7_est$SE[7], model1_pv8_est$SE[7], model1_pv9_est$SE[7], model1_pv10_est$SE[7])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[7], model1_pv2_est$coef[7], model1_pv3_est$coef[7], model1_pv4_est$coef[7], 
                         model1_pv5_est$coef[7], model1_pv6_est$coef[7], model1_pv7_est$coef[7], model1_pv8_est$coef[7], 
                         model1_pv9_est$coef[7], model1_pv10_est$coef[7]))

se_coef8_est <- mean(model1_pv1_est$SE[8], model1_pv2_est$SE[8], model1_pv3_est$SE[8], model1_pv4_est$SE[8], model1_pv5_est$SE[8],
                     model1_pv6_est$SE[8], model1_pv7_est$SE[8], model1_pv8_est$SE[8], model1_pv9_est$SE[8], model1_pv10_est$SE[8])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[8], model1_pv2_est$coef[8], model1_pv3_est$coef[8], model1_pv4_est$coef[8], 
                         model1_pv5_est$coef[8], model1_pv6_est$coef[8], model1_pv7_est$coef[8], model1_pv8_est$coef[8], 
                         model1_pv9_est$coef[8], model1_pv10_est$coef[8]))

se_coef9_est <- mean(model1_pv1_est$SE[9], model1_pv2_est$SE[9], model1_pv3_est$SE[9], model1_pv4_est$SE[9], model1_pv5_est$SE[9],
                     model1_pv6_est$SE[9], model1_pv7_est$SE[9], model1_pv8_est$SE[9], model1_pv9_est$SE[9], model1_pv10_est$SE[9])+(1+(1/M))*var(
                       c(model1_pv1_est$coef[9], model1_pv2_est$coef[9], model1_pv3_est$coef[9], model1_pv4_est$coef[9], 
                         model1_pv5_est$coef[9], model1_pv6_est$coef[9], model1_pv7_est$coef[9], model1_pv8_est$coef[9], 
                         model1_pv9_est$coef[9], model1_pv10_est$coef[9]))

se_coef10_est <- mean(model1_pv1_est$SE[10], model1_pv2_est$SE[10], model1_pv3_est$SE[10], model1_pv4_est$SE[10], model1_pv5_est$SE[10],
                      model1_pv6_est$SE[10], model1_pv7_est$SE[10], model1_pv8_est$SE[10], model1_pv9_est$SE[10], model1_pv10_est$SE[10])+(1+(1/M))*var(
                        c(model1_pv1_est$coef[10], model1_pv2_est$coef[10], model1_pv3_est$coef[10], model1_pv4_est$coef[10], 
                          model1_pv5_est$coef[10], model1_pv6_est$coef[10], model1_pv7_est$coef[10], model1_pv8_est$coef[10], 
                          model1_pv9_est$coef[10], model1_pv10_est$coef[10]))

se_coef11_est <- mean(model1_pv1_est$SE[11], model1_pv2_est$SE[11], model1_pv3_est$SE[11], model1_pv4_est$SE[11], model1_pv5_est$SE[11],
                      model1_pv6_est$SE[11], model1_pv7_est$SE[11], model1_pv8_est$SE[11], model1_pv9_est$SE[11], model1_pv10_est$SE[11])+(1+(1/M))*var(
                        c(model1_pv1_est$coef[11], model1_pv2_est$coef[11], model1_pv3_est$coef[11], model1_pv4_est$coef[11], 
                          model1_pv5_est$coef[11], model1_pv6_est$coef[11], model1_pv7_est$coef[11], model1_pv8_est$coef[11], 
                          model1_pv9_est$coef[11], model1_pv10_est$coef[11]))

model1_se_est <- rbind(Intercept=se_coef1_est, MALE=se_coef2_est, UNDREM=se_coef3_est, METASUM=se_coef4_est, JOYREAD=se_coef5_est, 
                       RESILIENCE=se_coef6_est, SCREADCOMP=se_coef7_est, SCREADDIFF=se_coef8_est, GFOFAIL=se_coef9_est, WORKMAST=se_coef10_est, MASTGOAL=se_coef11_est)

## t-values
model1_t_est <- model1_coef_est/model1_se_est

## p-values
model1_p_est <- round(2 * pnorm( abs(model1_t_est), lower.tail=FALSE), 3)


## ICC
model1_ICC_est <- mean(model1_pv1_est$ICC, model1_pv2_est$ICC, model1_pv3_est$ICC, model1_pv4_est$ICC, model1_pv5_est$ICC,
                       model1_pv6_est$ICC, model1_pv7_est$ICC, model1_pv8_est$ICC, model1_pv9_est$ICC, model1_pv10_est$ICC)

# School variance
mean(model1_pv1_est$vars[1], model1_pv2_est$vars[1], model1_pv3_est$vars[1], model1_pv4_est$vars[1], model1_pv5_est$vars[1],
     model1_pv6_est$vars[1], model1_pv7_est$vars[1], model1_pv8_est$vars[1], model1_pv9_est$vars[1], model1_pv10_est$vars[1])

# Student level variance
mean(model1_pv1_est$vars[2], model1_pv2_est$vars[2], model1_pv3_est$vars[2], model1_pv4_est$vars[2], model1_pv5_est$vars[2],
     model1_pv6_est$vars[2], model1_pv7_est$vars[2], model1_pv8_est$vars[2], model1_pv9_est$vars[2], model1_pv10_est$vars[2])


## AIC
model1_loglik_est <- mean(model1_pv1_est$lnl, model1_pv2_est$lnl, model1_pv3_est$lnl, model1_pv4_est$lnl, model1_pv5_est$lnl,
                          model1_pv6_est$lnl, model1_pv7_est$lnl, model1_pv8_est$lnl, model1_pv9_est$lnl, model1_pv10_est$lnl)

model1_AIC_est <- -2 * model1_loglik_est + 2 * 11

## BIC
model1_BIC_est <- log(nrow(pisa2018final_EST)) * 11 - 2 * model1_loglik_est


#--------------------------------#
##### Final combined mode
########## Lithuania #######
model2_pv1 <- mix(PV1READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv2 <- mix(PV2READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv3 <- mix(PV3READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv4 <- mix(PV4READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv5 <- mix(PV5READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv6 <- mix(PV6READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv7 <- mix(PV7READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv8 <- mix(PV8READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv9 <- mix(PV9READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final)
model2_pv10 <- mix(PV10READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                   weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                   data = pisa2018final)



## COEFFICIENTS
# Averages from all 10 PVs
model2_coef <- rowMeans(cbind(model2_pv1$coef, model2_pv2$coef, model2_pv3$coef, model2_pv4$coef, model2_pv5$coef,
                              model2_pv6$coef, model2_pv7$coef, model2_pv8$coef, model2_pv9$coef, model2_pv10$coef))
round(as.data.frame(model2_coef), 3)

## SE
M <- 10

se_coef1 <- mean(model2_pv1$SE[1], model2_pv2$SE[1], model2_pv3$SE[1], model2_pv4$SE[1], model2_pv5$SE[1],
                 model2_pv6$SE[1], model2_pv7$SE[1], model2_pv8$SE[1], model2_pv9$SE[1], model2_pv10$SE[1])+(1+(1/M))*var(
                   c(model2_pv1$coef[1], model2_pv2$coef[1], model2_pv3$coef[1], model2_pv4$coef[1], 
                     model2_pv5$coef[1], model2_pv6$coef[1], model2_pv7$coef[1], model2_pv8$coef[1], 
                     model2_pv9$coef[1], model2_pv10$coef[1]))

se_coef2 <- mean(model2_pv1$SE[2], model2_pv2$SE[2], model2_pv3$SE[2], model2_pv4$SE[2], model2_pv5$SE[2],
                 model2_pv6$SE[2], model2_pv7$SE[2], model2_pv8$SE[2], model2_pv9$SE[2], model2_pv10$SE[2])+(1+(1/M))*var(
                   c(model2_pv1$coef[2], model2_pv2$coef[2], model2_pv3$coef[2], model2_pv4$coef[2], 
                     model2_pv5$coef[2], model2_pv6$coef[2], model2_pv7$coef[2], model2_pv8$coef[2], 
                     model2_pv9$coef[2], model2_pv10$coef[2]))

se_coef3 <- mean(model2_pv1$SE[3], model2_pv2$SE[3], model2_pv3$SE[3], model2_pv4$SE[3], model2_pv5$SE[3],
                 model2_pv6$SE[3], model2_pv7$SE[3], model2_pv8$SE[3], model2_pv9$SE[3], model2_pv10$SE[3])+(1+(1/M))*var(
                   c(model2_pv1$coef[3], model2_pv2$coef[3], model2_pv3$coef[3], model2_pv4$coef[3], 
                     model2_pv5$coef[3], model2_pv6$coef[3], model2_pv7$coef[3], model2_pv8$coef[3], 
                     model2_pv9$coef[3], model2_pv10$coef[3]))

se_coef4 <- mean(model2_pv1$SE[4], model2_pv2$SE[4], model2_pv3$SE[4], model2_pv4$SE[4], model2_pv5$SE[4],
                 model2_pv6$SE[4], model2_pv7$SE[4], model2_pv8$SE[4], model2_pv9$SE[4], model2_pv10$SE[4])+(1+(1/M))*var(
                   c(model2_pv1$coef[4], model2_pv2$coef[4], model2_pv3$coef[4], model2_pv4$coef[4], 
                     model2_pv5$coef[4], model2_pv6$coef[4], model2_pv7$coef[4], model2_pv8$coef[4], 
                     model2_pv9$coef[4], model2_pv10$coef[4]))

se_coef5 <- mean(model2_pv1$SE[5], model2_pv2$SE[5], model2_pv3$SE[5], model2_pv4$SE[5], model2_pv5$SE[5],
                 model2_pv6$SE[5], model2_pv7$SE[5], model2_pv8$SE[5], model2_pv9$SE[5], model2_pv10$SE[5])+(1+(1/M))*var(
                   c(model2_pv1$coef[5], model2_pv2$coef[5], model2_pv3$coef[5], model2_pv4$coef[5], 
                     model2_pv5$coef[5], model2_pv6$coef[5], model2_pv7$coef[5], model2_pv8$coef[5], 
                     model2_pv9$coef[5], model2_pv10$coef[5]))

se_coef6 <- mean(model2_pv1$SE[6], model2_pv2$SE[6], model2_pv3$SE[6], model2_pv4$SE[6], model2_pv5$SE[6],
                 model2_pv6$SE[6], model2_pv7$SE[6], model2_pv8$SE[6], model2_pv9$SE[6], model2_pv10$SE[6])+(1+(1/M))*var(
                   c(model2_pv1$coef[6], model2_pv2$coef[6], model2_pv3$coef[6], model2_pv4$coef[6], 
                     model2_pv5$coef[6], model2_pv6$coef[6], model2_pv7$coef[6], model2_pv8$coef[6], 
                     model2_pv9$coef[6], model2_pv10$coef[6]))

se_coef7 <- mean(model2_pv1$SE[7], model2_pv2$SE[7], model2_pv3$SE[7], model2_pv4$SE[7], model2_pv5$SE[7],
                 model2_pv6$SE[7], model2_pv7$SE[7], model2_pv8$SE[7], model2_pv9$SE[7], model2_pv10$SE[7])+(1+(1/M))*var(
                   c(model2_pv1$coef[7], model2_pv2$coef[7], model2_pv3$coef[7], model2_pv4$coef[7], 
                     model2_pv5$coef[7], model2_pv6$coef[7], model2_pv7$coef[7], model2_pv8$coef[7], 
                     model2_pv9$coef[7], model2_pv10$coef[7]))

se_coef8 <- mean(model2_pv1$SE[8], model2_pv2$SE[8], model2_pv3$SE[8], model2_pv4$SE[8], model2_pv5$SE[8],
                 model2_pv6$SE[8], model2_pv7$SE[8], model2_pv8$SE[8], model2_pv9$SE[8], model2_pv10$SE[8])+(1+(1/M))*var(
                   c(model2_pv1$coef[8], model2_pv2$coef[8], model2_pv3$coef[8], model2_pv4$coef[8], 
                     model2_pv5$coef[8], model2_pv6$coef[8], model2_pv7$coef[8], model2_pv8$coef[8], 
                     model2_pv9$coef[8], model2_pv10$coef[8]))

se_coef9 <- mean(model2_pv1$SE[9], model2_pv2$SE[9], model2_pv3$SE[9], model2_pv4$SE[9], model2_pv5$SE[9],
                 model2_pv6$SE[9], model2_pv7$SE[9], model2_pv8$SE[9], model2_pv9$SE[9], model2_pv10$SE[9])+(1+(1/M))*var(
                   c(model2_pv1$coef[9], model2_pv2$coef[9], model2_pv3$coef[9], model2_pv4$coef[9], 
                     model2_pv5$coef[9], model2_pv6$coef[9], model2_pv7$coef[9], model2_pv8$coef[9], 
                     model2_pv9$coef[9], model2_pv10$coef[9]))

se_coef10 <- mean(model2_pv1$SE[10], model2_pv2$SE[10], model2_pv3$SE[10], model2_pv4$SE[10], model2_pv5$SE[10],
                  model2_pv6$SE[10], model2_pv7$SE[10], model2_pv8$SE[10], model2_pv9$SE[10], model2_pv10$SE[10])+(1+(1/M))*var(
                    c(model2_pv1$coef[10], model2_pv2$coef[10], model2_pv3$coef[10], model2_pv4$coef[10], 
                      model2_pv5$coef[10], model2_pv6$coef[10], model2_pv7$coef[10], model2_pv8$coef[10], 
                      model2_pv9$coef[10], model2_pv10$coef[10]))

se_coef11 <- mean(model2_pv1$SE[11], model2_pv2$SE[11], model2_pv3$SE[11], model2_pv4$SE[11], model2_pv5$SE[11],
                  model2_pv6$SE[11], model2_pv7$SE[11], model2_pv8$SE[11], model2_pv9$SE[11], model2_pv10$SE[11])+(1+(1/M))*var(
                    c(model2_pv1$coef[11], model2_pv2$coef[11], model2_pv3$coef[11], model2_pv4$coef[11], 
                      model2_pv5$coef[11], model2_pv6$coef[11], model2_pv7$coef[11], model2_pv8$coef[11], 
                      model2_pv9$coef[11], model2_pv10$coef[11]))

se_coef12 <- mean(model2_pv1$SE[12], model2_pv2$SE[12], model2_pv3$SE[12], model2_pv4$SE[12], model2_pv5$SE[12],
                  model2_pv6$SE[12], model2_pv7$SE[12], model2_pv8$SE[12], model2_pv9$SE[12], model2_pv10$SE[12])+(1+(1/M))*var(
                    c(model2_pv1$coef[12], model2_pv2$coef[12], model2_pv3$coef[12], model2_pv4$coef[12], 
                      model2_pv5$coef[12], model2_pv6$coef[12], model2_pv7$coef[12], model2_pv8$coef[12], 
                      model2_pv9$coef[12], model2_pv10$coef[12]))

se_coef13 <- mean(model2_pv1$SE[13], model2_pv2$SE[13], model2_pv3$SE[13], model2_pv4$SE[13], model2_pv5$SE[13],
                  model2_pv6$SE[13], model2_pv7$SE[13], model2_pv8$SE[13], model2_pv9$SE[13], model2_pv10$SE[13])+(1+(1/M))*var(
                    c(model2_pv1$coef[13], model2_pv2$coef[13], model2_pv3$coef[13], model2_pv4$coef[13], 
                      model2_pv5$coef[13], model2_pv6$coef[13], model2_pv7$coef[13], model2_pv8$coef[13], 
                      model2_pv9$coef[13], model2_pv10$coef[13]))

se_coef14 <- mean(model2_pv1$SE[14], model2_pv2$SE[14], model2_pv3$SE[14], model2_pv4$SE[14], model2_pv5$SE[14],
                  model2_pv6$SE[14], model2_pv7$SE[14], model2_pv8$SE[14], model2_pv9$SE[14], model2_pv10$SE[14])+(1+(1/M))*var(
                    c(model2_pv1$coef[14], model2_pv2$coef[14], model2_pv3$coef[14], model2_pv4$coef[14], 
                      model2_pv5$coef[14], model2_pv6$coef[14], model2_pv7$coef[14], model2_pv8$coef[14], 
                      model2_pv9$coef[14], model2_pv10$coef[14]))

model2_se <- rbind(Intercept=se_coef1, MALE=se_coef2, UNDREM=se_coef3, METASUM=se_coef4, JOYREAD=se_coef5, 
                   RESILIENCE=se_coef6, SCREADCOMP=se_coef7, SCREADDIFF=se_coef8, GFOFAIL=se_coef9, 
                   WORKMAST=se_coef10, MASTGOAL=se_coef11, WHICH_AREA=se_coef12, PUBLIC=se_coef13, SCHOOL_ESCS=se_coef14)


## t-values
model2_t <- model2_coef/model2_se

## p-values
model2_p <- round(2 * pnorm( abs(model2_t), lower.tail=FALSE), 3)


## ICC
model2_ICC <- mean(model2_pv1$ICC, model2_pv2$ICC, model2_pv3$ICC, model2_pv4$ICC, model2_pv5$ICC,
                   model2_pv6$ICC, model2_pv7$ICC, model2_pv8$ICC, model2_pv9$ICC, model2_pv10$ICC)


# School variance
mean(model2_pv1$vars[1], model2_pv2$vars[1], model2_pv3$vars[1], model2_pv4$vars[1], model2_pv5$vars[1],
     model2_pv6$vars[1], model2_pv7$vars[1], model2_pv8$vars[1], model2_pv9$vars[1], model2_pv10$vars[1])
# Student level variance
mean(model2_pv1$vars[2], model2_pv2$vars[2], model2_pv3$vars[2], model2_pv4$vars[2], model2_pv5$vars[2],
     model2_pv6$vars[2], model2_pv7$vars[2], model2_pv8$vars[2], model2_pv9$vars[2], model2_pv20$vars[2])


## AIC
model2_loglik <- mean(model2_pv1$lnl, model2_pv2$lnl, model2_pv3$lnl, model2_pv4$lnl, model2_pv5$lnl,
                      model2_pv6$lnl, model2_pv7$lnl, model2_pv8$lnl, model2_pv9$lnl, model2_pv10$lnl)

model2_AIC <- -2 * model2_loglik + 2 * 14

## BIC
model2_BIC <- log(nrow(pisa2018final)) * 14 - 2 * model2_loglik


#--------------------------------#
######## Latvia ######

model2_pv1_lva <- mix(PV1READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv2_lva  <- mix(PV2READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv3_lva  <- mix(PV3READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv4_lva  <- mix(PV4READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv5_lva  <- mix(PV5READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv6_lva  <- mix(PV6READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv7_lva  <- mix(PV7READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv8_lva  <- mix(PV8READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv9_lva  <- mix(PV9READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                  weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                  data = pisa2018final_LVA)
model2_pv10_lva  <- mix(PV10READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                   weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                   data = pisa2018final_LVA)

## COEFFICIENTS
# Averages from all 10 PVs
model2_coef_lva  <- rowMeans(cbind(model2_pv1_lva$coef, model2_pv2_lva$coef, model2_pv3_lva$coef, model2_pv4_lva$coef, model2_pv5_lva$coef,
                              model2_pv6_lva$coef, model2_pv7_lva$coef, model2_pv8_lva$coef, model2_pv9_lva$coef, model2_pv10_lva$coef))
round(as.data.frame(model2_coef_lva), 3)

## SE
M <- 10

se_coef1_lva <- mean(model2_pv1_lva$SE[1], model2_pv2_lva$SE[1], model2_pv3_lva$SE[1], model2_pv4_lva$SE[1], model2_pv5_lva$SE[1],
                     model2_pv6_lva$SE[1], model2_pv7_lva$SE[1], model2_pv8_lva$SE[1], model2_pv9_lva$SE[1], model2_pv10_lva$SE[1])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[1], model2_pv2_lva$coef[1], model2_pv3_lva$coef[1], model2_pv4_lva$coef[1], 
                         model2_pv5_lva$coef[1], model2_pv6_lva$coef[1], model2_pv7_lva$coef[1], model2_pv8_lva$coef[1], 
                         model2_pv9_lva$coef[1], model2_pv10_lva$coef[1]))

se_coef2_lva <- mean(model2_pv1_lva$SE[2], model2_pv2_lva$SE[2], model2_pv3_lva$SE[2], model2_pv4_lva$SE[2], model2_pv5_lva$SE[2],
                     model2_pv6_lva$SE[2], model2_pv7_lva$SE[2], model2_pv8_lva$SE[2], model2_pv9_lva$SE[2], model2_pv10_lva$SE[2])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[2], model2_pv2_lva$coef[2], model2_pv3_lva$coef[2], model2_pv4_lva$coef[2], 
                         model2_pv5_lva$coef[2], model2_pv6_lva$coef[2], model2_pv7_lva$coef[2], model2_pv8_lva$coef[2], 
                         model2_pv9_lva$coef[2], model2_pv10_lva$coef[2]))

se_coef3_lva <- mean(model2_pv1_lva$SE[3], model2_pv2_lva$SE[3], model2_pv3_lva$SE[3], model2_pv4_lva$SE[3], model2_pv5_lva$SE[3],
                     model2_pv6_lva$SE[3], model2_pv7_lva$SE[3], model2_pv8_lva$SE[3], model2_pv9_lva$SE[3], model2_pv10_lva$SE[3])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[3], model2_pv2_lva$coef[3], model2_pv3_lva$coef[3], model2_pv4_lva$coef[3], 
                         model2_pv5_lva$coef[3], model2_pv6_lva$coef[3], model2_pv7_lva$coef[3], model2_pv8_lva$coef[3], 
                         model2_pv9_lva$coef[3], model2_pv10_lva$coef[3]))

se_coef4_lva <- mean(model2_pv1_lva$SE[4], model2_pv2_lva$SE[4], model2_pv3_lva$SE[4], model2_pv4_lva$SE[4], model2_pv5_lva$SE[4],
                     model2_pv6_lva$SE[4], model2_pv7_lva$SE[4], model2_pv8_lva$SE[4], model2_pv9_lva$SE[4], model2_pv10_lva$SE[4])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[4], model2_pv2_lva$coef[4], model2_pv3_lva$coef[4], model2_pv4_lva$coef[4], 
                         model2_pv5_lva$coef[4], model2_pv6_lva$coef[4], model2_pv7_lva$coef[4], model2_pv8_lva$coef[4], 
                         model2_pv9_lva$coef[4], model2_pv10_lva$coef[4]))

se_coef5_lva <- mean(model2_pv1_lva$SE[5], model2_pv2_lva$SE[5], model2_pv3_lva$SE[5], model2_pv4_lva$SE[5], model2_pv5_lva$SE[5],
                     model2_pv6_lva$SE[5], model2_pv7_lva$SE[5], model2_pv8_lva$SE[5], model2_pv9_lva$SE[5], model2_pv10_lva$SE[5])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[5], model2_pv2_lva$coef[5], model2_pv3_lva$coef[5], model2_pv4_lva$coef[5], 
                         model2_pv5_lva$coef[5], model2_pv6_lva$coef[5], model2_pv7_lva$coef[5], model2_pv8_lva$coef[5], 
                         model2_pv9_lva$coef[5], model2_pv10_lva$coef[5]))

se_coef6_lva <- mean(model2_pv1_lva$SE[6], model2_pv2_lva$SE[6], model2_pv3_lva$SE[6], model2_pv4_lva$SE[6], model2_pv5_lva$SE[6],
                     model2_pv6_lva$SE[6], model2_pv7_lva$SE[6], model2_pv8_lva$SE[6], model2_pv9_lva$SE[6], model2_pv10_lva$SE[6])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[6], model2_pv2_lva$coef[6], model2_pv3_lva$coef[6], model2_pv4_lva$coef[6], 
                         model2_pv5_lva$coef[6], model2_pv6_lva$coef[6], model2_pv7_lva$coef[6], model2_pv8_lva$coef[6], 
                         model2_pv9_lva$coef[6], model2_pv10_lva$coef[6]))

se_coef7_lva <- mean(model2_pv1_lva$SE[7], model2_pv2_lva$SE[7], model2_pv3_lva$SE[7], model2_pv4_lva$SE[7], model2_pv5_lva$SE[7],
                     model2_pv6_lva$SE[7], model2_pv7_lva$SE[7], model2_pv8_lva$SE[7], model2_pv9_lva$SE[7], model2_pv10_lva$SE[7])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[7], model2_pv2_lva$coef[7], model2_pv3_lva$coef[7], model2_pv4_lva$coef[7], 
                         model2_pv5_lva$coef[7], model2_pv6_lva$coef[7], model2_pv7_lva$coef[7], model2_pv8_lva$coef[7], 
                         model2_pv9_lva$coef[7], model2_pv10_lva$coef[7]))

se_coef8_lva <- mean(model2_pv1_lva$SE[8], model2_pv2_lva$SE[8], model2_pv3_lva$SE[8], model2_pv4_lva$SE[8], model2_pv5_lva$SE[8],
                     model2_pv6_lva$SE[8], model2_pv7_lva$SE[8], model2_pv8_lva$SE[8], model2_pv9_lva$SE[8], model2_pv10_lva$SE[8])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[8], model2_pv2_lva$coef[8], model2_pv3_lva$coef[8], model2_pv4_lva$coef[8], 
                         model2_pv5_lva$coef[8], model2_pv6_lva$coef[8], model2_pv7_lva$coef[8], model2_pv8_lva$coef[8], 
                         model2_pv9_lva$coef[8], model2_pv10_lva$coef[8]))

se_coef9_lva <- mean(model2_pv1_lva$SE[9], model2_pv2_lva$SE[9], model2_pv3_lva$SE[9], model2_pv4_lva$SE[9], model2_pv5_lva$SE[9],
                     model2_pv6_lva$SE[9], model2_pv7_lva$SE[9], model2_pv8_lva$SE[9], model2_pv9_lva$SE[9], model2_pv10_lva$SE[9])+(1+(1/M))*var(
                       c(model2_pv1_lva$coef[9], model2_pv2_lva$coef[9], model2_pv3_lva$coef[9], model2_pv4_lva$coef[9], 
                         model2_pv5_lva$coef[9], model2_pv6_lva$coef[9], model2_pv7_lva$coef[9], model2_pv8_lva$coef[9], 
                         model2_pv9_lva$coef[9], model2_pv10_lva$coef[9]))

se_coef10_lva <- mean(model2_pv1_lva$SE[10], model2_pv2_lva$SE[10], model2_pv3_lva$SE[10], model2_pv4_lva$SE[10], model2_pv5_lva$SE[10],
                      model2_pv6_lva$SE[10], model2_pv7_lva$SE[10], model2_pv8_lva$SE[10], model2_pv9_lva$SE[10], model2_pv10_lva$SE[10])+(1+(1/M))*var(
                        c(model2_pv1_lva$coef[10], model2_pv2_lva$coef[10], model2_pv3_lva$coef[10], model2_pv4_lva$coef[10], 
                          model2_pv5_lva$coef[10], model2_pv6_lva$coef[10], model2_pv7_lva$coef[10], model2_pv8_lva$coef[10], 
                          model2_pv9_lva$coef[10], model2_pv10_lva$coef[10]))

se_coef11_lva <- mean(model2_pv1_lva$SE[11], model2_pv2_lva$SE[11], model2_pv3_lva$SE[11], model2_pv4_lva$SE[11], model2_pv5_lva$SE[11],
                      model2_pv6_lva$SE[11], model2_pv7_lva$SE[11], model2_pv8_lva$SE[11], model2_pv9_lva$SE[11], model2_pv10_lva$SE[11])+(1+(1/M))*var(
                        c(model2_pv1_lva$coef[11], model2_pv2_lva$coef[11], model2_pv3_lva$coef[11], model2_pv4_lva$coef[11], 
                          model2_pv5_lva$coef[11], model2_pv6_lva$coef[11], model2_pv7_lva$coef[11], model2_pv8_lva$coef[11], 
                          model2_pv9_lva$coef[11], model2_pv10_lva$coef[11]))

se_coef12_lva <- mean(model2_pv1_lva$SE[12], model2_pv2_lva$SE[12], model2_pv3_lva$SE[12], model2_pv4_lva$SE[12], model2_pv5_lva$SE[12],
                      model2_pv6_lva$SE[12], model2_pv7_lva$SE[12], model2_pv8_lva$SE[12], model2_pv9_lva$SE[12], model2_pv10_lva$SE[12])+(1+(1/M))*var(
                        c(model2_pv1_lva$coef[12], model2_pv2_lva$coef[12], model2_pv3_lva$coef[12], model2_pv4_lva$coef[12], 
                          model2_pv5_lva$coef[12], model2_pv6_lva$coef[12], model2_pv7_lva$coef[12], model2_pv8_lva$coef[12], 
                          model2_pv9_lva$coef[12], model2_pv10_lva$coef[12]))

se_coef13_lva <- mean(model2_pv1_lva$SE[13], model2_pv2_lva$SE[13], model2_pv3_lva$SE[13], model2_pv4_lva$SE[13], model2_pv5_lva$SE[13],
                      model2_pv6_lva$SE[13], model2_pv7_lva$SE[13], model2_pv8_lva$SE[13], model2_pv9_lva$SE[13], model2_pv10_lva$SE[13])+(1+(1/M))*var(
                        c(model2_pv1_lva$coef[13], model2_pv2_lva$coef[13], model2_pv3_lva$coef[13], model2_pv4_lva$coef[13], 
                          model2_pv5_lva$coef[13], model2_pv6_lva$coef[13], model2_pv7_lva$coef[13], model2_pv8_lva$coef[13], 
                          model2_pv9_lva$coef[13], model2_pv10_lva$coef[13]))

se_coef14_lva <- mean(model2_pv1_lva$SE[14], model2_pv2_lva$SE[14], model2_pv3_lva$SE[14], model2_pv4_lva$SE[14], model2_pv5_lva$SE[14],
                      model2_pv6_lva$SE[14], model2_pv7_lva$SE[14], model2_pv8_lva$SE[14], model2_pv9_lva$SE[14], model2_pv10_lva$SE[14])+(1+(1/M))*var(
                        c(model2_pv1_lva$coef[14], model2_pv2_lva$coef[14], model2_pv3_lva$coef[14], model2_pv4_lva$coef[14], 
                          model2_pv5_lva$coef[14], model2_pv6_lva$coef[14], model2_pv7_lva$coef[14], model2_pv8_lva$coef[14], 
                          model2_pv9_lva$coef[14], model2_pv10_lva$coef[14]))


model2_se_lva <- rbind(Intercept=se_coef1_lva, MALE=se_coef2_lva, UNDREM=se_coef3_lva, METASUM=se_coef4_lva, JOYREAD=se_coef5_lva, 
                   RESILIENCE=se_coef6_lva, SCREADCOMP=se_coef7_lva, SCREADDIFF=se_coef8_lva, GFOFAIL=se_coef9_lva, 
                   WORKMAST=se_coef10_lva, MASTGOAL=se_coef11_lva, WHICH_AREA=se_coef12_lva, PUBLIC=se_coef13_lva, SCHOOL_ESCS=se_coef14_lva)



## t-values
model2_t_lva <- model2_coef_lva/model2_se_lva

## p-values
model2_p_lva <- round(2 * pnorm( abs(model2_t_lva), lower.tail=FALSE), 3)


## ICC
model2_ICC_lva <- mean(model2_pv1_lva$ICC, model2_pv2_lva$ICC, model2_pv3_lva$ICC, model2_pv4_lva$ICC, model2_pv5_lva$ICC,
                   model2_pv6_lva$ICC, model2_pv7_lva$ICC, model2_pv8_lva$ICC, model2_pv9_lva$ICC, model2_pv10_lva$ICC)


# School variance
mean(model2_pv1_lva$vars[1], model2_pv2_lva$vars[1], model2_pv3_lva$vars[1], model2_pv4_lva$vars[1], model2_pv5_lva$vars[1],
     model2_pv6_lva$vars[1], model2_pv7_lva$vars[1], model2_pv8_lva$vars[1], model2_pv9_lva$vars[1], model2_pv10_lva$vars[1])

# Student level variance
mean(model2_pv1_lva$vars[2], model2_pv2_lva$vars[2], model2_pv3_lva$vars[2], model2_pv4_lva$vars[2], model2_pv5_lva$vars[2],
     model2_pv6_lva$vars[2], model2_pv7_lva$vars[2], model2_pv8_lva$vars[2], model2_pv9_lva$vars[2], model2_pv10_lva$vars[2])



## AIC
model2_loglik_lva <- mean(model2_pv1_lva$lnl, model2_pv2_lva$lnl, model2_pv3_lva$lnl, model2_pv4_lva$lnl, model2_pv5_lva$lnl,
                      model2_pv6_lva$lnl, model2_pv7_lva$lnl, model2_pv8_lva$lnl, model2_pv9_lva$lnl, model2_pv10_lva$lnl)

model2_AIC_lva <- -2 * model2_loglik_lva + 2 * 14

## BIC
model2_BIC_lva <- log(nrow(pisa2018final_LVA)) * 14 - 2 * model2_loglik_lva

#--------------------------------#
########## Estonia

model2_pv1_est <- mix(PV1READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                      weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                      data = pisa2018final_EST)
model2_pv2_est  <- mix(PV2READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv3_est  <- mix(PV3READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv4_est  <- mix(PV4READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv5_est  <- mix(PV5READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv6_est  <- mix(PV6READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv7_est  <- mix(PV7READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv8_est  <- mix(PV8READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv9_est  <- mix(PV9READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                       weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                       data = pisa2018final_EST)
model2_pv10_est  <- mix(PV10READ~MALE+UNDREM+METASUM+JOYREAD+RESILIENCE+SCREADCOMP+SCREADDIFF+ GFOFAIL+WORKMAST+MASTGOAL+WHICH_AREA+PUBLIC+SCHOOL_ESCS+(1|CNTSCHID), 
                        weights = c("cluster_weights", "W_SCHGRNRABWT"), 
                        data = pisa2018final_EST)

names(pisa2018final)

## COEFFICIENTS
# Averages from all 10 PVs

model2_coef_est  <- rowMeans(cbind(model2_pv1_est$coef, model2_pv2_est$coef, model2_pv3_est$coef, model2_pv4_est$coef, model2_pv5_est$coef,
                                   model2_pv6_est$coef, model2_pv7_est$coef, model2_pv8_est$coef, model2_pv9_est$coef, model2_pv10_est$coef))

round(as.data.frame(model2_coef_est), 3)

## SE
M <- 10

se_coef1_est <- mean(model2_pv1_est$SE[1], model2_pv2_est$SE[1], model2_pv3_est$SE[1], model2_pv4_est$SE[1], model2_pv5_est$SE[1],
                     model2_pv6_est$SE[1], model2_pv7_est$SE[1], model2_pv8_est$SE[1], model2_pv9_est$SE[1], model2_pv10_est$SE[1])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[1], model2_pv2_est$coef[1], model2_pv3_est$coef[1], model2_pv4_est$coef[1], 
                         model2_pv5_est$coef[1], model2_pv6_est$coef[1], model2_pv7_est$coef[1], model2_pv8_est$coef[1], 
                         model2_pv9_est$coef[1], model2_pv10_est$coef[1]))

se_coef2_est <- mean(model2_pv1_est$SE[2], model2_pv2_est$SE[2], model2_pv3_est$SE[2], model2_pv4_est$SE[2], model2_pv5_est$SE[2],
                     model2_pv6_est$SE[2], model2_pv7_est$SE[2], model2_pv8_est$SE[2], model2_pv9_est$SE[2], model2_pv10_est$SE[2])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[2], model2_pv2_est$coef[2], model2_pv3_est$coef[2], model2_pv4_est$coef[2], 
                         model2_pv5_est$coef[2], model2_pv6_est$coef[2], model2_pv7_est$coef[2], model2_pv8_est$coef[2], 
                         model2_pv9_est$coef[2], model2_pv10_est$coef[2]))

se_coef3_est <- mean(model2_pv1_est$SE[3], model2_pv2_est$SE[3], model2_pv3_est$SE[3], model2_pv4_est$SE[3], model2_pv5_est$SE[3],
                     model2_pv6_est$SE[3], model2_pv7_est$SE[3], model2_pv8_est$SE[3], model2_pv9_est$SE[3], model2_pv10_est$SE[3])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[3], model2_pv2_est$coef[3], model2_pv3_est$coef[3], model2_pv4_est$coef[3], 
                         model2_pv5_est$coef[3], model2_pv6_est$coef[3], model2_pv7_est$coef[3], model2_pv8_est$coef[3], 
                         model2_pv9_est$coef[3], model2_pv10_est$coef[3]))

se_coef4_est <- mean(model2_pv1_est$SE[4], model2_pv2_est$SE[4], model2_pv3_est$SE[4], model2_pv4_est$SE[4], model2_pv5_est$SE[4],
                     model2_pv6_est$SE[4], model2_pv7_est$SE[4], model2_pv8_est$SE[4], model2_pv9_est$SE[4], model2_pv10_est$SE[4])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[4], model2_pv2_est$coef[4], model2_pv3_est$coef[4], model2_pv4_est$coef[4], 
                         model2_pv5_est$coef[4], model2_pv6_est$coef[4], model2_pv7_est$coef[4], model2_pv8_est$coef[4], 
                         model2_pv9_est$coef[4], model2_pv10_est$coef[4]))

se_coef5_est <- mean(model2_pv1_est$SE[5], model2_pv2_est$SE[5], model2_pv3_est$SE[5], model2_pv4_est$SE[5], model2_pv5_est$SE[5],
                     model2_pv6_est$SE[5], model2_pv7_est$SE[5], model2_pv8_est$SE[5], model2_pv9_est$SE[5], model2_pv10_est$SE[5])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[5], model2_pv2_est$coef[5], model2_pv3_est$coef[5], model2_pv4_est$coef[5], 
                         model2_pv5_est$coef[5], model2_pv6_est$coef[5], model2_pv7_est$coef[5], model2_pv8_est$coef[5], 
                         model2_pv9_est$coef[5], model2_pv10_est$coef[5]))

se_coef6_est <- mean(model2_pv1_est$SE[6], model2_pv2_est$SE[6], model2_pv3_est$SE[6], model2_pv4_est$SE[6], model2_pv5_est$SE[6],
                     model2_pv6_est$SE[6], model2_pv7_est$SE[6], model2_pv8_est$SE[6], model2_pv9_est$SE[6], model2_pv10_est$SE[6])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[6], model2_pv2_est$coef[6], model2_pv3_est$coef[6], model2_pv4_est$coef[6], 
                         model2_pv5_est$coef[6], model2_pv6_est$coef[6], model2_pv7_est$coef[6], model2_pv8_est$coef[6], 
                         model2_pv9_est$coef[6], model2_pv10_est$coef[6]))

se_coef7_est <- mean(model2_pv1_est$SE[7], model2_pv2_est$SE[7], model2_pv3_est$SE[7], model2_pv4_est$SE[7], model2_pv5_est$SE[7],
                     model2_pv6_est$SE[7], model2_pv7_est$SE[7], model2_pv8_est$SE[7], model2_pv9_est$SE[7], model2_pv10_est$SE[7])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[7], model2_pv2_est$coef[7], model2_pv3_est$coef[7], model2_pv4_est$coef[7], 
                         model2_pv5_est$coef[7], model2_pv6_est$coef[7], model2_pv7_est$coef[7], model2_pv8_est$coef[7], 
                         model2_pv9_est$coef[7], model2_pv10_est$coef[7]))

se_coef8_est <- mean(model2_pv1_est$SE[8], model2_pv2_est$SE[8], model2_pv3_est$SE[8], model2_pv4_est$SE[8], model2_pv5_est$SE[8],
                     model2_pv6_est$SE[8], model2_pv7_est$SE[8], model2_pv8_est$SE[8], model2_pv9_est$SE[8], model2_pv10_est$SE[8])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[8], model2_pv2_est$coef[8], model2_pv3_est$coef[8], model2_pv4_est$coef[8], 
                         model2_pv5_est$coef[8], model2_pv6_est$coef[8], model2_pv7_est$coef[8], model2_pv8_est$coef[8], 
                         model2_pv9_est$coef[8], model2_pv10_est$coef[8]))

se_coef9_est <- mean(model2_pv1_est$SE[9], model2_pv2_est$SE[9], model2_pv3_est$SE[9], model2_pv4_est$SE[9], model2_pv5_est$SE[9],
                     model2_pv6_est$SE[9], model2_pv7_est$SE[9], model2_pv8_est$SE[9], model2_pv9_est$SE[9], model2_pv10_est$SE[9])+(1+(1/M))*var(
                       c(model2_pv1_est$coef[9], model2_pv2_est$coef[9], model2_pv3_est$coef[9], model2_pv4_est$coef[9], 
                         model2_pv5_est$coef[9], model2_pv6_est$coef[9], model2_pv7_est$coef[9], model2_pv8_est$coef[9], 
                         model2_pv9_est$coef[9], model2_pv10_est$coef[9]))

se_coef10_est <- mean(model2_pv1_est$SE[10], model2_pv2_est$SE[10], model2_pv3_est$SE[10], model2_pv4_est$SE[10], model2_pv5_est$SE[10],
                      model2_pv6_est$SE[10], model2_pv7_est$SE[10], model2_pv8_est$SE[10], model2_pv9_est$SE[10], model2_pv10_est$SE[10])+(1+(1/M))*var(
                        c(model2_pv1_est$coef[10], model2_pv2_est$coef[10], model2_pv3_est$coef[10], model2_pv4_est$coef[10], 
                          model2_pv5_est$coef[10], model2_pv6_est$coef[10], model2_pv7_est$coef[10], model2_pv8_est$coef[10], 
                          model2_pv9_est$coef[10], model2_pv10_est$coef[10]))

se_coef11_est <- mean(model2_pv1_est$SE[11], model2_pv2_est$SE[11], model2_pv3_est$SE[11], model2_pv4_est$SE[11], model2_pv5_est$SE[11],
                      model2_pv6_est$SE[11], model2_pv7_est$SE[11], model2_pv8_est$SE[11], model2_pv9_est$SE[11], model2_pv10_est$SE[11])+(1+(1/M))*var(
                        c(model2_pv1_est$coef[11], model2_pv2_est$coef[11], model2_pv3_est$coef[11], model2_pv4_est$coef[11], 
                          model2_pv5_est$coef[11], model2_pv6_est$coef[11], model2_pv7_est$coef[11], model2_pv8_est$coef[11], 
                          model2_pv9_est$coef[11], model2_pv10_est$coef[11]))

se_coef12_est <- mean(model2_pv1_est$SE[12], model2_pv2_est$SE[12], model2_pv3_est$SE[12], model2_pv4_est$SE[12], model2_pv5_est$SE[12],
                      model2_pv6_est$SE[12], model2_pv7_est$SE[12], model2_pv8_est$SE[12], model2_pv9_est$SE[12], model2_pv10_est$SE[12])+(1+(1/M))*var(
                        c(model2_pv1_est$coef[12], model2_pv2_est$coef[12], model2_pv3_est$coef[12], model2_pv4_est$coef[12], 
                          model2_pv5_est$coef[12], model2_pv6_est$coef[12], model2_pv7_est$coef[12], model2_pv8_est$coef[12], 
                          model2_pv9_est$coef[12], model2_pv10_est$coef[12]))

se_coef13_est <- mean(model2_pv1_est$SE[13], model2_pv2_est$SE[13], model2_pv3_est$SE[13], model2_pv4_est$SE[13], model2_pv5_est$SE[13],
                      model2_pv6_est$SE[13], model2_pv7_est$SE[13], model2_pv8_est$SE[13], model2_pv9_est$SE[13], model2_pv10_est$SE[13])+(1+(1/M))*var(
                        c(model2_pv1_est$coef[13], model2_pv2_est$coef[13], model2_pv3_est$coef[13], model2_pv4_est$coef[13], 
                          model2_pv5_est$coef[13], model2_pv6_est$coef[13], model2_pv7_est$coef[13], model2_pv8_est$coef[13], 
                          model2_pv9_est$coef[13], model2_pv10_est$coef[13]))

se_coef14_est <- mean(model2_pv1_est$SE[14], model2_pv2_est$SE[14], model2_pv3_est$SE[14], model2_pv4_est$SE[14], model2_pv5_est$SE[14],
                      model2_pv6_est$SE[14], model2_pv7_est$SE[14], model2_pv8_est$SE[14], model2_pv9_est$SE[14], model2_pv10_est$SE[14])+(1+(1/M))*var(
                        c(model2_pv1_est$coef[14], model2_pv2_est$coef[14], model2_pv3_est$coef[14], model2_pv4_est$coef[14], 
                          model2_pv5_est$coef[14], model2_pv6_est$coef[14], model2_pv7_est$coef[14], model2_pv8_est$coef[14], 
                          model2_pv9_est$coef[14], model2_pv10_est$coef[14]))


model2_se_est <- rbind(Intercept=se_coef1_est, MALE=se_coef2_est, UNDREM=se_coef3_est, METASUM=se_coef4_est, JOYREAD=se_coef5_est, 
                       RESILIENCE=se_coef6_est, SCREADCOMP=se_coef7_est, SCREADDIFF=se_coef8_est, GFOFAIL=se_coef9_est, 
                       WORKMAST=se_coef10_est, MASTGOAL=se_coef11_est, WHICH_AREA=se_coef12_est, PUBLIC=se_coef13_est, SCHOOL_ESCS=se_coef14_est)


## t-values
model2_t_est <- model2_coef_est/model2_se_est

## p-values
model2_p_est <- round(2 * pnorm( abs(model2_t_est), lower.tail=FALSE), 3)


## ICC
model2_ICC_est <- mean(model2_pv1_est$ICC, model2_pv2_est$ICC, model2_pv3_est$ICC, model2_pv4_est$ICC, model2_pv5_est$ICC,
                       model2_pv6_est$ICC, model2_pv7_est$ICC, model2_pv8_est$ICC, model2_pv9_est$ICC, model2_pv10_est$ICC)

# School variance
mean(model2_pv1_est$vars[1], model2_pv2_est$vars[1], model2_pv3_est$vars[1], model2_pv4_est$vars[1], model2_pv5_est$vars[1],
     model2_pv6_est$vars[1], model2_pv7_est$vars[1], model2_pv8_est$vars[1], model2_pv9_est$vars[1], model2_pv10_est$vars[1])
# Student level variance
mean(model2_pv1_est$vars[2], model2_pv2_est$vars[2], model2_pv3_est$vars[2], model2_pv4_est$vars[2], model2_pv5_est$vars[2],
     model2_pv6_est$vars[2], model2_pv7_est$vars[2], model2_pv8_est$vars[2], model2_pv9_est$vars[2], model2_pv10_est$vars[2])



## AIC
model2_loglik_est <- mean(model2_pv1_est$lnl, model2_pv2_est$lnl, model2_pv3_est$lnl, model2_pv4_est$lnl, model2_pv5_est$lnl,
                          model2_pv6_est$lnl, model2_pv7_est$lnl, model2_pv8_est$lnl, model2_pv9_est$lnl, model2_pv10_est$lnl)

model2_AIC_est <- -2 * model2_loglik_est + 2 * 14

## BIC
model2_BIC_est <- log(nrow(pisa2018final_EST)) * 14 - 2 * model2_loglik_est



# Please install the following packages if you do not have them installed. 
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(micEconAids)
library(tidyr)
library(data.table)
library(AER)
library(plm)
library(imputeTS)
library(systemfit)
library(lfe)

#--------------------------------------------------------------------------------------------------------------------------------#
# Regression: Estimating the Import Demand Equations (Source Differentiated AIDS)
#--------------------------------------------------------------------------------------------------------------------------------#
rm(list=ls())
source('doRepFunc.r')

PriceGap <- fread('../Data/0_Data_PriceGap_Update.csv', stringsAsFactors = FALSE) %>%
  mutate(Time = as.Date(paste0(Time, '01'), format = '%Y%m%d')) %>%
  dplyr::select(1,2,6) %>%
  mutate(GapRatio = ifelse(GapRatio <= 0, 100, 100+GapRatio))

IntlPriceDat <- fread('../Data/0_InlPrice_Clean.csv', stringsAsFactors = FALSE) %>%
  mutate(Time = as.Date(Time, format = '%Y-%m-%d')) %>%
  dplyr::rename(IntlPrice = Value)

RegDat <- fread(input = '../Data/0_RegressionDatSource_long_clean_Update2018.csv', stringsAsFactors = FALSE) %>%
  mutate(Time = as.Date(Time, format = '%Y-%m-%d')) %>%
  left_join(., PriceGap, by = c('Time', 'Commodity')) %>%
  left_join(., IntlPriceDat, by = c('Time', 'Commodity', 'Country')) %>%
  mutate(ImportPrice = ImportPrice*GapRatio/100) %>%
  dplyr::select(-GapRatio) %>%
  ungroup() %>%
  mutate(Commodity = case_when(Commodity == 'Maize' ~ 'M',
                               Commodity == 'Rice' ~ 'R',
                               Commodity == 'Wheat' ~ 'W')) %>%
  dplyr::rename(QT = Quantity, VA = Value, Pr = ImportPrice) %>%
  group_by(Time) %>%
  mutate(EShare = VA/sum(VA)) %>%
  gather(variable, value, -(Commodity:Country)) %>%
  unite(temp, Commodity, Country, variable) %>%
  spread(temp, value) %>%
  as.data.frame() %>%
  mutate(Exp = ifelse(year(Time) <= 2012, M_USA_VA + R_THI_VA + R_VIN_VA + W_USA_VA + W_AUS_VA + W_CAN_VA,
                      M_USA_VA + M_UKR_VA + M_RES_VA  + R_THI_VA + R_VIN_VA + R_PAK_VA + R_RES_VA + W_USA_VA + W_AUS_VA + W_CAN_VA + W_RES_VA))

UnrestrictDat <- RegDat %>%
  dplyr::filter(year(Time) > 2012) %>%
  mutate(UKR_Dummy = ifelse(year(Time) >= 2015, 1, 0),
         Month = month(Time),
         Quarter = case_when(Month <= 3 ~ 1,
                             Month > 3 & Month <= 6 ~ 2,
                             Month > 6 & Month <= 9 ~ 3,
                             Month > 9 ~ 4),
         UKRRatio = M_UKR_Pr/M_UKR_IntlPrice)

UKRRatioMean <- mean(dplyr::filter(UnrestrictDat, M_UKR_Impute == 0)$UKRRatio)
UnrestrictDat <- mutate(UnrestrictDat, M_UKR_Pr = ifelse(M_UKR_Impute == 1, UKRRatioMean*M_UKR_IntlPrice, M_UKR_Pr))

PriceNames <- c("M_USA_Pr", "M_UKR_Pr", "M_RES_Pr",
                "R_THI_Pr", "R_VIN_Pr", "R_PAK_Pr", "R_RES_Pr",
                "W_USA_Pr", "W_AUS_Pr", "W_CAN_Pr", "W_RES_Pr")

ShareNames <- c("M_USA_EShare", "M_UKR_EShare", "M_RES_EShare",
                "R_THI_EShare", "R_VIN_EShare", "R_PAK_EShare", "R_RES_EShare",
                "W_USA_EShare", "W_AUS_EShare", "W_CAN_EShare", "W_RES_EShare")

basePrice <- list(shares = colMeans(UnrestrictDat[, ShareNames]),
                  prices = colMeans(UnrestrictDat[, PriceNames])) # Base shares and base prices.
PriceIndexName <- 'Ls'
PriceIndex <- micEconAids::aidsPx(PriceIndexName, PriceNames, shareNames = ShareNames,
                                  data = UnrestrictDat, base = basePrice)


# Base share is the first row of observation in the micEcon package. I change it to average shares.
UnrestrictDat$PriceIndex <- as.numeric(log(UnrestrictDat[['Exp']]) - PriceIndex)

# Alternative specification with dummies
eq1 <- M_USA_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + M_UKR_Impute + UKR_Dummy + factor(Quarter)

eq2 <- M_UKR_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + M_UKR_Impute + UKR_Dummy + factor(Quarter)

eq3 <- M_RES_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + M_UKR_Impute + UKR_Dummy + factor(Quarter)

eq4 <- R_THI_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

eq5 <- R_VIN_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

eq6 <- R_PAK_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

eq7 <- R_RES_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

eq8 <- W_USA_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

eq9 <- W_AUS_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

eq10 <- W_CAN_EShare ~ log(M_USA_Pr) + log(M_UKR_Pr) + log(M_RES_Pr) + log(R_THI_Pr) + log(R_VIN_Pr) + log(R_PAK_Pr) + log(R_RES_Pr) +
  log(W_USA_Pr) + log(W_AUS_Pr) + log(W_CAN_Pr) + log(W_RES_Pr) + PriceIndex + UKR_Dummy + factor(Quarter)

# Homogeneity Restriction plus Symmetry Restriction.
# Sum of Gamma is zero for each equation.
ResMatHomo <- c('eq1_log(M_USA_Pr) + eq1_log(M_UKR_Pr) + eq1_log(M_RES_Pr) + eq1_log(R_THI_Pr) + eq1_log(R_VIN_Pr) +
                eq1_log(R_PAK_Pr) + eq1_log(R_RES_Pr) + eq1_log(W_USA_Pr) + eq1_log(W_AUS_Pr) + eq1_log(W_CAN_Pr) + eq1_log(W_RES_Pr) = 0',
                
                'eq2_log(M_USA_Pr) + eq2_log(M_UKR_Pr) + eq2_log(M_RES_Pr) + eq2_log(R_THI_Pr) + eq2_log(R_VIN_Pr) +
                eq2_log(R_PAK_Pr) + eq2_log(R_RES_Pr) + eq2_log(W_USA_Pr) + eq2_log(W_AUS_Pr) + eq2_log(W_CAN_Pr) + eq2_log(W_RES_Pr) = 0',
                
                'eq3_log(M_USA_Pr) + eq3_log(M_UKR_Pr) + eq3_log(M_RES_Pr) + eq3_log(R_THI_Pr) + eq3_log(R_VIN_Pr) +
                eq3_log(R_PAK_Pr) + eq3_log(R_RES_Pr) + eq3_log(W_USA_Pr) + eq3_log(W_AUS_Pr) + eq3_log(W_CAN_Pr) + eq3_log(W_RES_Pr) = 0',
                
                'eq4_log(M_USA_Pr) + eq4_log(M_UKR_Pr) + eq4_log(M_RES_Pr) + eq4_log(R_THI_Pr) + eq4_log(R_VIN_Pr) +
                eq4_log(R_PAK_Pr) + eq4_log(R_RES_Pr) + eq4_log(W_USA_Pr) + eq4_log(W_AUS_Pr) + eq4_log(W_CAN_Pr) + eq4_log(W_RES_Pr) = 0',
                
                'eq5_log(M_USA_Pr) + eq5_log(M_UKR_Pr) + eq5_log(M_RES_Pr) + eq5_log(R_THI_Pr) + eq5_log(R_VIN_Pr) +
                eq5_log(R_PAK_Pr) + eq5_log(R_RES_Pr) + eq5_log(W_USA_Pr) + eq5_log(W_AUS_Pr) + eq5_log(W_CAN_Pr) + eq5_log(W_RES_Pr) = 0',
                
                'eq6_log(M_USA_Pr) + eq6_log(M_UKR_Pr) + eq6_log(M_RES_Pr) + eq6_log(R_THI_Pr) + eq6_log(R_VIN_Pr) +
                eq6_log(R_PAK_Pr) + eq6_log(R_RES_Pr) + eq6_log(W_USA_Pr) + eq6_log(W_AUS_Pr) + eq6_log(W_CAN_Pr) + eq6_log(W_RES_Pr) = 0',
                
                'eq7_log(M_USA_Pr) + eq7_log(M_UKR_Pr) + eq7_log(M_RES_Pr) + eq7_log(R_THI_Pr) + eq7_log(R_VIN_Pr) +
                eq7_log(R_PAK_Pr) + eq7_log(R_RES_Pr) + eq7_log(W_USA_Pr) + eq7_log(W_AUS_Pr) + eq7_log(W_CAN_Pr) + eq7_log(W_RES_Pr) = 0',
                
                'eq8_log(M_USA_Pr) + eq8_log(M_UKR_Pr) + eq8_log(M_RES_Pr) + eq8_log(R_THI_Pr) + eq8_log(R_VIN_Pr) +
                eq8_log(R_PAK_Pr) + eq8_log(R_RES_Pr) + eq8_log(W_USA_Pr) + eq8_log(W_AUS_Pr) + eq8_log(W_CAN_Pr) + eq8_log(W_RES_Pr) = 0',
                
                'eq9_log(M_USA_Pr) + eq9_log(M_UKR_Pr) + eq9_log(M_RES_Pr) + eq9_log(R_THI_Pr) + eq9_log(R_VIN_Pr) +
                eq9_log(R_PAK_Pr) + eq9_log(R_RES_Pr) + eq9_log(W_USA_Pr) + eq9_log(W_AUS_Pr) + eq9_log(W_CAN_Pr) + eq9_log(W_RES_Pr) = 0',
                
                'eq10_log(M_USA_Pr) + eq10_log(M_UKR_Pr) + eq10_log(M_RES_Pr) + eq10_log(R_THI_Pr) + eq10_log(R_VIN_Pr) +
                eq10_log(R_PAK_Pr) + eq10_log(R_RES_Pr) + eq10_log(W_USA_Pr) + eq10_log(W_AUS_Pr) + eq10_log(W_CAN_Pr) + eq10_log(W_RES_Pr) = 0')


# Above for homogeneity; below for symmestry.
ResMatSym <- c('eq1_log(M_UKR_Pr) - eq2_log(M_USA_Pr) = 0',
               'eq1_log(M_RES_Pr) - eq3_log(M_USA_Pr) = 0',
               'eq1_log(R_THI_Pr) - eq4_log(M_USA_Pr) = 0',
               'eq1_log(R_VIN_Pr) - eq5_log(M_USA_Pr) = 0',
               'eq1_log(R_PAK_Pr) - eq6_log(M_USA_Pr) = 0',
               'eq1_log(R_RES_Pr) - eq7_log(M_USA_Pr) = 0',
               'eq1_log(W_USA_Pr) - eq8_log(M_USA_Pr) = 0',
               'eq1_log(W_AUS_Pr) - eq9_log(M_USA_Pr) = 0',
               'eq1_log(W_CAN_Pr) - eq10_log(M_USA_Pr) = 0',
               
               'eq2_log(M_RES_Pr) - eq3_log(M_UKR_Pr) = 0',
               'eq2_log(R_THI_Pr) - eq4_log(M_UKR_Pr) = 0',
               'eq2_log(R_VIN_Pr) - eq5_log(M_UKR_Pr) = 0',
               'eq2_log(R_PAK_Pr) - eq6_log(M_UKR_Pr) = 0',
               'eq2_log(R_RES_Pr) - eq7_log(M_UKR_Pr) = 0',
               'eq2_log(W_USA_Pr) - eq8_log(M_UKR_Pr) = 0',
               'eq2_log(W_AUS_Pr) - eq9_log(M_UKR_Pr) = 0',
               'eq2_log(W_CAN_Pr) - eq10_log(M_UKR_Pr) = 0',
               
               'eq3_log(R_THI_Pr) - eq4_log(M_RES_Pr) = 0',
               'eq3_log(R_VIN_Pr) - eq5_log(M_RES_Pr) = 0',
               'eq3_log(R_PAK_Pr) - eq6_log(M_RES_Pr) = 0',
               'eq3_log(R_RES_Pr) - eq7_log(M_RES_Pr) = 0',
               'eq3_log(W_USA_Pr) - eq8_log(M_RES_Pr) = 0',
               'eq3_log(W_AUS_Pr) - eq9_log(M_RES_Pr) = 0',
               'eq3_log(W_CAN_Pr) - eq10_log(M_RES_Pr) = 0',
               
               'eq4_log(R_VIN_Pr) - eq5_log(R_THI_Pr) = 0',
               'eq4_log(R_PAK_Pr) - eq6_log(R_THI_Pr) = 0',
               'eq4_log(R_RES_Pr) - eq7_log(R_THI_Pr) = 0',
               'eq4_log(W_USA_Pr) - eq8_log(R_THI_Pr) = 0',
               'eq4_log(W_AUS_Pr) - eq9_log(R_THI_Pr) = 0',
               'eq4_log(W_CAN_Pr) - eq10_log(R_THI_Pr) = 0',
               
               'eq5_log(R_PAK_Pr) - eq6_log(R_VIN_Pr) = 0',
               'eq5_log(R_RES_Pr) - eq7_log(R_VIN_Pr) = 0',
               'eq5_log(W_USA_Pr) - eq8_log(R_VIN_Pr) = 0',
               'eq5_log(W_AUS_Pr) - eq9_log(R_VIN_Pr) = 0',
               'eq5_log(W_CAN_Pr) - eq10_log(R_VIN_Pr) = 0',
               
               'eq6_log(R_RES_Pr) - eq7_log(R_PAK_Pr) = 0',
               'eq6_log(W_USA_Pr) - eq8_log(R_PAK_Pr) = 0',
               'eq6_log(W_AUS_Pr) - eq9_log(R_PAK_Pr) = 0',
               'eq6_log(W_CAN_Pr) - eq10_log(R_PAK_Pr) = 0',
               
               'eq7_log(W_USA_Pr) - eq8_log(R_RES_Pr) = 0',
               'eq7_log(W_AUS_Pr) - eq9_log(R_RES_Pr) = 0',
               'eq7_log(W_CAN_Pr) - eq10_log(R_RES_Pr) = 0',
               
               'eq8_log(W_AUS_Pr) - eq9_log(W_USA_Pr) = 0',
               'eq8_log(W_CAN_Pr) - eq10_log(W_USA_Pr) = 0',
               
               'eq9_log(W_CAN_Pr) - eq10_log(W_AUS_Pr) = 0')

ExtraRes <- c('eq1_M_UKR_Impute + eq2_M_UKR_Impute + eq3_M_UKR_Impute = 0',
              'eq1_UKR_Dummy + eq2_UKR_Dummy + eq3_UKR_Dummy + eq4_UKR_Dummy + eq5_UKR_Dummy + eq6_UKR_Dummy + eq7_UKR_Dummy + eq8_UKR_Dummy + eq9_UKR_Dummy + eq10_UKR_Dummy = 0')

# Non-restricted regression.
AIDSEst <- systemfit(formula = list(eq1, eq2, eq3, eq4, eq5, eq6, eq7, eq8, eq9, eq10),
                     data = UnrestrictDat, method = 'SUR',
                     #restrict.matrix = ExtraRes,
                     control = systemfit.control(maxiter = 100))
# summary(AIDSEst)


# Restricted regression.
AIDSEstRes <- systemfit(formula = list(eq1, eq2, eq3, eq4, eq5, eq6, eq7, eq8, eq9, eq10),
                        data = UnrestrictDat, method = 'SUR',
                        restrict.matrix = c(ResMatHomo, ResMatSym, ExtraRes),
                        control = systemfit.control(maxiter = 100))
summary(AIDSEstRes)

linearHypothesis(AIDSEst, hypothesis.matrix = ResMatHomo)
linearHypothesis(AIDSEst, hypothesis.matrix = ResMatSym)

# DW critical value for 9 regressors and 45 obs, 95% (1.1, 2.0)
for (i in 1:10){
  test <- AIDSEstRes$eq[[i]]$residuals
  cat(sum((test-dplyr::lag(test))^2, na.rm = TRUE)/sum(test^2),'%%')
  print(coefficients(lm(test~dplyr::lag(test)))[2])
}

NumEqs <- 10 + 1

# Get elasticities.
CoefEst <- GetCoef(AIDSEst)
CoefEstRes <- GetCoef(AIDSEstRes)

AggShareDat <- UnrestrictDat %>%
  dplyr::select(Time, ShareNames, Exp) %>%
  dplyr::filter(year(Time) >= 2013) %>%
  dplyr::select(-Time) %>%
  colMeans()

SubShareDat <- fread('../Data/0_RegressionDatSource_long_clean_Update2018.csv', stringsAsFactors = FALSE) %>%
  dplyr::filter(!(Commodity == 'Maize' & Country == 'Others')) %>%
  mutate(Time = as.Date(Time, format = '%Y-%m-%d')) %>%
  group_by(Commodity, Time) %>%
  mutate(Share =  Value/sum(Value)) %>%
  ungroup() %>%
  mutate(Commodity = substr(Commodity, 1, 1)) %>%
  dplyr::filter(year(Time) > 2012) %>%
  dplyr::select(Commodity, Time, Country, Share) %>%
  unite(temp, Commodity, Country) %>%
  mutate(temp = paste0(temp, '_EShare')) %>%
  spread(temp, Share) %>%
  dplyr::select(Time, ShareNames) %>%
  as.data.frame() %>%
  dplyr::filter(year(Time) >= 2013) %>%
  dplyr::select(-Time) %>%
  colMeans()


test1 <- GetElasCorrect10Eqs(ShareNames = ShareNames, CoefEst = CoefEstRes, AggShare = AggShareDat[ShareNames],
                             SubShare = SubShareDat, e1= -257.8*1000000/AggShareDat[NumEqs+1], b1 = c(-0.16, -0.79, -1.01), BaseShare = basePrice$shares)

test2 <- GetElasCorrect10Eqs(ShareNames = ShareNames, CoefEst = CoefEstRes, AggShare = AggShareDat[ShareNames],
                             SubShare = SubShareDat, e1= -257.8*1000000/AggShareDat[NumEqs+1], b1 = c(0, 0, 0), BaseShare = basePrice$shares)

diag(test1)
diag(test2)

# Simulate for the standard deviations.
library(mvtnorm)
nSim <- 5000
set.seed(1992)
ParaSim <- rmvnorm(n=nSim, mean=coef(AIDSEstRes), sigma=vcov(AIDSEstRes), method = 'svd')
CoefSim <- lapply(1:nSim, function(i) GetCoef(ParaSim[i,], sim = TRUE))

# Get coefficients.
test <- lapply(CoefSim, '[[', 3)
gammaM <- data.frame(matrix(NA, NumEqs, NumEqs))
for (i in 1:NumEqs){
  for (j in 1:NumEqs){
    gammaM[i,j] <- round(mean(unlist(lapply(test, '[[', i, j))), 2)
  }
}

gammaMSd <- data.frame(matrix(NA, NumEqs, NumEqs))
for (i in 1:NumEqs){
  for (j in 1:NumEqs){
    gammaMSd[i,j] <- paste0('(', round(sd(unlist(lapply(test, '[[', i, j))), 2), ')')
  }
}

gammaTex <- data.frame(matrix(NA, NumEqs*2, NumEqs))
gammaTex[seq(1, NumEqs*2, 2), ] <- gammaM
gammaTex[seq(2, NumEqs*2, 2), ] <- gammaMSd
row.names(gammaTex) <- c('Log price of1', 'USA maize', 'Log price of2', 'UKR maize', 'Log price of3', 'ROW maize', 'Log price of4', 'THI rice', 'Log price of5', 'VIN rice',
                         'Log price of6', 'PAK rice', 'Log price of7', 'ROW rice', 'Log price of8', 'USA wheat', 'Log price of9', 'AUS wheat', 'Log price of10', 'CAN wheat',
                         'Log price of11', 'ROW wheat')
colnames(gammaTex) <- c('USA', 'UKR', 'RES', 'THI', 'VIN', 'PAK', 'RES', 'USA', 'AUS', 'CAN', 'RES')
stargazer(gammaTex, summary = FALSE, rownames = T)

test2 <- lapply(CoefSim, '[[', 2)

betaImputeTex <- data.frame(matrix(NA, 2, NumEqs))
for (i in 1:NumEqs){
  betaImputeTex[1,i] <- round(mean(unlist(lapply(test2, '[[', i))), 2)
}

for (i in 1:NumEqs){
  betaImputeTex[2,i] <- paste0('(', round(sd(unlist(lapply(test2, '[[', i))), 2), ')')
}

stargazer(betaImputeTex, summary = FALSE, rownames = T)


# Restricted
ElasSim1 <- lapply(1:nSim, function(i) GetElasCorrect10Eqs(ShareNames, CoefSim[[i]], AggShare = AggShareDat[ShareNames],
                                                           SubShare = SubShareDat, e1= -257.8*1000000/AggShareDat[NumEqs+1], b1 = c(-0.16, -0.79, -1.01), BaseShare = basePrice$shares))
# Unrestricted
ElasSim2 <- lapply(1:nSim, function(i) GetElasCorrect10Eqs(ShareNames, CoefSim[[i]], AggShare = AggShareDat[ShareNames],
                                                           SubShare = SubShareDat, e1= -257.8*1000000/AggShareDat[NumEqs+1], b1 = c(0, 0, 0), BaseShare = basePrice$shares))

# SD of Price Elasticity
#PriceElasSim1 <- lapply(ElasSim1, '[[', 1)
PriceElasMean1 <- matrix(sapply(1:NumEqs^2, function(i) mean(unlist(lapply(ElasSim1, '[[', i)))), NumEqs, NumEqs)
PriceElasSd1 <- matrix(sapply(1:NumEqs^2, function(i) sd(unlist(lapply(ElasSim1, '[[', i)))), NumEqs, NumEqs)
PriceElasPvalue1 <- matrix(sapply(1:NumEqs^2, function(i) 2*pt(-abs(PriceElasMean1[i]/PriceElasSd1[i]), df = AIDSEstRes$eq[[1]]$df.residual)), NumEqs, NumEqs)

#PriceElasSim2 <- lapply(ElasSim2, '[[', 1)
PriceElasMean2 <- matrix(sapply(1:NumEqs^2, function(i) mean(unlist(lapply(ElasSim2, '[[', i)))), NumEqs, NumEqs)
PriceElasSd2 <- matrix(sapply(1:NumEqs^2, function(i) sd(unlist(lapply(ElasSim2, '[[', i)))), NumEqs, NumEqs)
PriceElasPvalue2 <- matrix(sapply(1:NumEqs^2, function(i) 2*pt(-abs(PriceElasMean2[i]/PriceElasSd2[i]), df = AIDSEstRes$eq[[1]]$df.residual)), NumEqs, NumEqs)
#round(diag(PriceElasPvalue2),2)
#rowSums(PriceElasMean2)

IDNames <- c('Maize-USA', 'Maize-Ukraine', 'Maize-RES','Rice-Thailand', 'Rice-VietNam', 'Rice-Pakistan', 'Rice-RES', 'Wheat-USA', 'Wheat-Australia', 'Wheat-Canada', 'Wheat-RES')

ElasOutM <- data.frame(matrix(NA, NumEqs, 3)) %>%
  'colnames<-'(c('ID', 'Elas1', 'Elas2')) %>%
  mutate(ID = IDNames,
         Elas1 = diag(PriceElasMean1), Elas2 = diag(PriceElasMean2)) %>%
  gather(Variable, Elas, 2:3)

ElasOut <- data.frame(matrix(NA, NumEqs, 3)) %>%
  'colnames<-'(c('ID', 'Elas1', 'Elas2')) %>%
  mutate(ID = IDNames,
         Elas1 = diag(PriceElasSd1),
         Elas2 = diag(PriceElasSd2)) %>%
  gather(Variable, ElasSd, 2:3) %>%
  left_join(., ElasOutM, by = c('ID', 'Variable'))
# mutate(Elas = if_else(grepl('Maize', ID) & Variable == 'Elas1', 0 , Elas),
#        ElasSd = if_else(grepl('Maize', ID) & Variable == 'Elas1', 0 , ElasSd))


gelas <-
  ggplot(data = ElasOut) +
  geom_bar(aes(ID, Elas, fill = Variable), position = position_dodge(), stat = 'identity') +
  geom_text(aes(ID, 0.1, label = round(Elas, 1), group = Variable), position = position_dodge(width = 0.8), size = 3.5, vjust = 0.1) +
  geom_errorbar(aes(ID, ymin = (Elas- 1.692*ElasSd), ymax = (Elas+ 1.692*ElasSd), group = Variable),
                position = position_dodge(width = 0.8), width = 0.15) +
  theme_classic() +
  labs(x = '', y = 'Own price elasticity of import demand') +
  scale_fill_manual(values = c("grey", "grey40"),
                    breaks = c('Elas1', 'Elas2'),
                    labels = c('Restricted', 'Unrestricted')) +
  scale_y_continuous(breaks = seq(3, -7, by = -1)) +
  scale_x_discrete(limits = IDNames,
                   labels = c('Maize-Ukraine' = 'Ukraine', 'Maize-USA' = 'U.S.', 'Maize-RES' = 'ROW',
                              'Rice-Pakistan' = 'Pakistan', 'Rice-Thailand' = 'Thailand', 'Rice-RES' = 'ROW',
                              'Rice-VietNam' = 'Vietnam', 'Wheat-Australia' = 'Australia', 'Wheat-RES' = 'ROW',
                              'Wheat-Canada' = 'Canada', 'Wheat-USA' = 'U.S.')) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = 'black', size = 12, face = 'plain'),
        axis.text = element_text(color = 'black', size = 12, face = 'plain'),
        axis.title.y = element_text(color = 'black', size = 12, face = 'plain')) +
  annotate("text", label ='Maize', x = 2, y = 2, color = "black", size = 4) +
  annotate("segment", x = 0.5, xend = 3.5, y = 1.8, yend = 1.8, color = "black", size = 0.5, arrow=arrow(length = unit(0.1, 'inches'), ends = "both")) +
  annotate("text", label ='Rice', x = 5.5, y = 2, color = "black", size = 4) +
  annotate("segment", x = 3.6, xend = 7.4, y = 1.8, yend = 1.8, color = "black", size = 0.5, arrow=arrow(length = unit(0.1, 'inches'), ends = "both")) +
  annotate("text", label ='Wheat', x = 9.5, y = 2, color = "black", size = 4) +
  annotate("segment", x = 7.6, xend = 11.4, y = 1.8, yend = 1.8, color = "black", size = 0.5, arrow=arrow(length = unit(0.1, 'inches'), ends = "both"))

gelas

ggsave('Fig_ElasEst_TheoryResRev2.png', gelas, width = 220, height = 160, units = 'mm')


# Weighted sum of price elasticity
AveElas <- c(weighted.mean(diag(PriceElasMean2)[1:3], SubShareDat[1:3]), weighted.mean(diag(PriceElasMean2)[4:7], SubShareDat[4:7]),
             weighted.mean(diag(PriceElasMean2)[8:11], SubShareDat[8:11]))
AveElasSd <- c(weighted.mean(diag(PriceElasSd2)[1:3], SubShareDat[1:3]), weighted.mean(diag(PriceElasSd2)[4:7], SubShareDat[4:7]),
               weighted.mean(diag(PriceElasSd2)[8:11], SubShareDat[8:11]))



#---------------------------------------------------------------------------------------------------------------------------------------------#
# Project the import demand without the import protections.
#---------------------------------------------------------------------------------------------------------------------------------------------#
PriceGap15 <- PriceGap %>%
  dplyr::filter(year(Time) == 2017) %>%
  group_by(Commodity) %>%
  summarise(GapRatio = mean(GapRatio)) %>%
  mutate(GapRatio = 100/GapRatio - 1) # domestic price is at denominator.

Quant15 <- RegDat %>%
  dplyr::filter(year(Time) == 2017) %>%
  dplyr::select(gsub("EShare", "QT", ShareNames)) %>%
  colSums()

PriceGap15M <- matrix(c(rep(PriceGap15$GapRatio[1], 3), rep(PriceGap15$GapRatio[2], 4), rep(PriceGap15$GapRatio[3], 4)), 11, 1)

Quant15Proj <- lapply(1:nSim, function(i) (ElasSim2[[i]] %*% PriceGap15M + 1)*Quant15)
Quant15ProjMean <- sapply(1:NumEqs, function(i) mean(unlist(lapply(Quant15Proj, '[[', i))))
Quant15ProjSd <- sapply(1:NumEqs, function(i) sd(unlist(lapply(Quant15Proj, '[[', i))))


QuantProjVis <-  data.frame(Source = gsub("EShare", "QT", ShareNames),
                            Base = Quant15/1000000, Mean = Quant15ProjMean/1000000, # in million tonnes
                            Sd = Quant15ProjSd/1000000) %>%
  gather(Scenario, Value, 2:3) %>%
  mutate(Country = substr(Source, 3, 5),
         Commodity = substr(Source, 1, 1),
         Sd = ifelse(Scenario == 'Base', 0, Sd))

ValueProjVis <- UnrestrictDat %>%
  dplyr::filter(year(Time) == 2017) %>%
  dplyr::select(contains('VA')) %>%
  colSums() %>%
  as.data.frame() %>%
  'colnames<-'(c('ObsValue')) %>%
  mutate(Country = substr(row.names(.), 3, 5),
         Commodity = substr(row.names(.), 1, 1)) %>%
  right_join(., QuantProjVis, by = c('Country', 'Commodity')) %>%
  mutate(AvePrice = ObsValue/(1000000*Value))

ValueProjVis[(NumEqs+1):nrow(ValueProjVis), 'AvePrice'] <- ValueProjVis[1:NumEqs, 'AvePrice']
ValueProjVis <- ValueProjVis %>%
  mutate(ImportV = Value*AvePrice)
#ValueProjVis[9:10, 'ImportV'] <- ValueProjVis[9:10, 'ImportV']*7.2/sum(ValueProjVis[9:10, 'Value']) # Correct for over quota import

label_size <- 12

g <-
  ValueProjVis %>%
  mutate(Source = case_when(Source == 'M_UKR_QT' ~ 'Maize-Ukraine',
                            Source == 'M_USA_QT' ~ 'Maize-USA',
                            Source == 'M_RES_QT' ~ 'Maize-RES',
                            Source == 'R_PAK_QT' ~ 'Rice-Pakistan',
                            Source == 'R_THI_QT' ~ 'Rice-Thailand',
                            Source == 'R_VIN_QT' ~ 'Rice-VietNam',
                            Source == 'R_RES_QT' ~ 'Rice-RES',
                            Source == 'W_AUS_QT' ~ 'Wheat-Australia',
                            Source == 'W_CAN_QT' ~ 'Wheat-Canada',
                            Source == 'W_USA_QT' ~ 'Wheat-USA',
                            Source == 'W_RES_QT' ~ 'Wheat-RES')) %>%
  ggplot(data = .) +
  geom_bar(mapping = aes(Source, ImportV, fill = Scenario),
           stat = 'identity',
           position = position_dodge(width = NULL)) +
  geom_text(aes(Source, 40,label = round(ImportV, 0), group = Scenario),
            position = position_dodge(0.9)) +
  geom_errorbar(aes(x= Source, ymin=ImportV-1.692*Sd*AvePrice, ymax=ImportV+1.692*Sd*AvePrice, group = Scenario),
                colour="black", width=.15, size = 0.4,
                position = position_dodge(0.9)) +
  scale_fill_manual(values = c('Base' = 'grey', 'Mean' = "grey40"),
                    breaks = c("Base", "Mean"),
                    labels = c("Observed", "Simulated")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.title = element_text(size = label_size, colour = 'black'),
        axis.text = element_text(size = label_size, colour = 'black'),
        legend.text = element_text(size = label_size, colour = 'black'),
        plot.margin = margin(1,0,0,0, unit = 'cm')) +
  labs(x = '', y = 'Import value, million USD') +
  scale_x_discrete(limits = IDNames,
                   labels = c('Maize-Ukraine' = 'Ukraine', 'Maize-USA' = 'U.S.', 'Maize-RES' = 'ROW',
                              'Rice-Pakistan' = 'Pakistan', 'Rice-Thailand' = 'Thailand',  'Rice-RES' = 'ROW',
                              'Rice-VietNam' = 'Vietnam', 'Wheat-Australia' = 'Australia',
                              'Wheat-Canada' = 'Canada', 'Wheat-USA' = 'U.S.', 'Wheat-RES' = 'ROW')) +
  annotate("text", label ='Maize', x = 2, y = 1430, color = "black", size = 4) +
  annotate("segment", x = 0.5, xend = 3.4, y = 1400, yend = 1400, color = "black", size = 0.5, arrow=arrow(length = unit(0.1, 'inches'), ends = "both")) +
  annotate("text", label ='Rice', x = 5.5, y = 1430, color = "black", size = 4) +
  annotate("segment", x = 3.5, xend = 7.4, y = 1400, yend = 1400, color = "black", size = 0.5, arrow=arrow(length = unit(0.1, 'inches'), ends = "both")) +
  annotate("text", label ='Wheat', x = 9.5, y = 1430, color = "black", size = 4) +
  annotate("segment", x = 7.6, xend = 11.4, y = 1400, yend = 1400, color = "black", size = 0.5, arrow=arrow(length = unit(0.1, 'inches'), ends = "both"))

g

ggsave('Fig_DisAgg_ValueProject_TheoryResRev2.png', g, width = 220, height = 160, units = 'mm')


AggQuantProjVis <- QuantProjVis %>%
  group_by(Commodity, Scenario) %>%
  summarise(Value = sum(Value), Sd = sum(Sd)) %>%
  ungroup() %>%
  mutate(Commodity = case_when(Commodity == 'M' ~ 'Maize',
                               Commodity == 'R' ~ 'Rice',
                               Commodity == 'W' ~ 'Wheat')) %>%
  mutate(Overquota = case_when(Commodity == 'Wheat' ~ 9.636,
                               Commodity == 'Maize' ~ 7.2,
                               Commodity == 'Rice' ~ 5.32))

OverQuotaDat <- AggQuantProjVis %>%
  dplyr::filter(Scenario == 'Mean', Commodity != 'Total') %>%
  mutate(Dif = if_else(Value - Overquota >= 0, Value - Overquota, 0))

AggTotal <- AggQuantProjVis %>%
  group_by(Scenario) %>%
  summarise(Value = sum(Value), Sd = sum(Sd), Overquota = sum(Overquota)) %>%
  mutate(Overquota = 0) %>%
  cbind(., data.frame(Commodity = 'Total'))

if(sum(OverQuotaDat$Dif) > 0) { # Substratc the import quantity over the quota
  AggTotal$Value[2] =  AggTotal$Value[2] - sum(OverQuotaDat$Dif);
  AggTotal$Sd = 0
}

AggQuantProjVis <- rbind(AggQuantProjVis, AggTotal) %>%
  mutate(Sd = ifelse(Scenario == 'Base', NA, Sd)) %>%
  mutate(Upper = Value+1.692*Sd)

g1 <- ggplot(data = AggQuantProjVis) +
  geom_bar(mapping = aes(Commodity, Value, fill = Scenario),
           stat = 'identity',
           position = position_dodge(width = NULL)) +
  geom_text(aes(Commodity, 0.4, label = round(Value, 1), group = Scenario),
            position = position_dodge(0.9)) +
  geom_errorbar(aes(x= Commodity, ymin=Value-1.692*Sd, ymax=Upper, group = Scenario),
                colour="black", width=.15, size = 0.4,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c('Maize', 'Rice', 'Wheat', 'Total')) +
  scale_fill_manual(values = c('Base' = 'grey', 'Mean' = "grey40"),
                    breaks = c("Base", "Mean"),
                    labels = c("Observed", "Simulated"),
                    guide = guide_legend(reverse = FALSE)) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, by = 4)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.title = element_text(size = label_size, colour = 'black'),
        axis.text = element_text(size = label_size, colour = 'black'),
        legend.text = element_text(size = label_size, colour = 'black')) +
  labs(x = '', y = 'Import quantity, million tonnes') +
  geom_segment(aes(x = 0.5, xend = 1.5, y = 7.2, yend = 7.2), size = 0.6, lty = 2) +
  geom_segment(aes(x = 1.5, xend = 2.5, y = 5.32, yend = 5.32), size = 0.6, lty = 2) +
  geom_segment(aes(x = 2.5, xend = 3.5, y = 9.636, yend = 9.636), size = 0.6, lty = 2) +
  geom_segment(aes(x = 1.5, xend = 1.5, y = 7.2, yend = 5.32), size = 0.6, lty = 2) +
  geom_segment(aes(x = 2.5, xend = 2.5, y = 5.32, yend = 9.636), size = 0.6, lty = 2)

g1

ggsave('Fig_Agg_QuantProject_TheoryResRev2.png', g1, width = 180, height = 160, units = 'mm')



AggValueProjVis <- ValueProjVis %>%
  group_by(Commodity, Scenario) %>%
  summarise(ImportV = sum(ImportV)/1000, Sd = sum(Sd*AvePrice)/1000) %>%
  ungroup() %>%
  mutate(Commodity = case_when(Commodity == 'M' ~ 'Maize',
                               Commodity == 'R' ~ 'Rice',
                               Commodity == 'W' ~ 'Wheat'))

AggTotal <- AggValueProjVis %>%
  group_by(Scenario) %>%
  summarise(ImportV = sum(ImportV), Sd = sum(Sd)) %>%
  cbind(., data.frame(Commodity = 'Total'))


AggValueProjVis <- rbind(AggValueProjVis, AggTotal) %>%
  mutate(Sd = ifelse(Scenario == 'Base', NA, Sd))

g1 <-
  ggplot(data = AggValueProjVis) +
  geom_bar(mapping = aes(Commodity, ImportV, fill = Scenario),
           stat = 'identity',
           position = position_dodge(width = NULL)) +
  geom_text(aes(Commodity, 0.2,label = round(ImportV, 1), group = Scenario),
            position = position_dodge(0.9)) +
  geom_errorbar(aes(x= Commodity, ymin=ImportV-1.692*Sd, ymax=ImportV+1.692*Sd, group = Scenario),
                colour="black", width=.15, size = 0.4,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c('Maize', 'Rice', 'Wheat', 'Total')) +
  scale_fill_manual(values = c('Base' = 'grey', 'Mean' = "grey40"),
                    breaks = c("Base", "Mean"),
                    labels = c("Observed", "Simulated"),
                    guide = guide_legend(reverse = FALSE)) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.title = element_text(size = label_size, colour = 'black'),
        axis.text = element_text(size = label_size, colour = 'black'),
        legend.text = element_text(size = label_size, colour = 'black'),
        plot.margin = margin(1,0,0,0, unit = 'cm')) +
  labs(x = '', y = 'Import value, billion USD')

g1

ggsave('Fig_Agg_ValueProject_TheoryResRev2.png', g1, width = 180, height = 160, units = 'mm')



x <- lapply(c(2013:2017), ProjQuotaFillRateFunc2) %>%
  bind_rows() %>%
  mutate(UpperRate = 100*Upper/Overquota, LowerRate = 100*Lower/Overquota,
         MiddlePoint = UpperRate/2 + LowerRate/2)


g <-
  ggplot(data = x) +
  geom_point(aes(Year, MiddlePoint, lty = Scenario), pch = 1) +
  geom_line(aes(Year, MiddlePoint, lty = Scenario)) +
  geom_errorbar(data = filter(x, Scenario == 'Mean'), aes(Year, ymin = LowerRate, ymax = UpperRate), size = 0.5, width = 0.2) +
  scale_y_continuous(breaks = seq(0, 180, by = 20)) +
  geom_hline(yintercept = 100, lty = 1, size = 0.5) +
  labs(x = '', y = 'Quota fill rates (%)') +
  facet_wrap(~ Commodity, ncol = 2, scales = 'free') +
  scale_linetype_manual(values = c('Base' = 1, 'Mean' = 2),
                        labels = c('Observed', 'Simulated'),
                        name = '') +
  theme(strip.background = element_rect(fill = 'grey80'),
        axis.title = element_text(color = 'black', size = 12),
        axis.text = element_text(color = 'black', size = 12),
        strip.text = element_text(color = 'black', size = 12),
        legend.position = 'bottom',
        legend.text = element_text(color = 'black', size = 12))
g

ann_text <- data.frame(Year = 2015.8, MiddlePoint = 150, lab = "(At-quota importing)",
                       Commodity = factor('Maize', levels = c("Maize","Rice","Wheat")))

g <- g + geom_text(data = ann_text, aes(Year, MiddlePoint), label = expression(italic(at-quota~importing)), size = 4)

g

ggsave('Fig_QuotaFillRates2.png', g, width = 260, height = 240, units = 'mm')


# Draw other graphs.
#----------------------------------------------------------------------------------------------------------------------------#
QuotaFillDat <- read.csv('comtrade_china_world.csv', stringsAsFactors = FALSE) %>%
  filter(Trade.Flow == 'Import', Year >= 2004) %>%
  dplyr::select(Year, Netweight..kg.,Commodity.Code) %>%
  rename(Year = Year, Value = Netweight..kg., Commodity = Commodity.Code) %>%
  mutate(TotalQuota = case_when(Commodity == '1001'~9.636,
                                Commodity == '1005'~7.2,
                                Commodity == '1006'~5.32),
         PrivateQuota = case_when(Commodity == '1001'~9.636*0.1,
                                  Commodity == '1005'~7.2*0.4,
                                  Commodity == '1006'~5.32*0.5),
         Commodity = case_when(Commodity == '1001'~'Wheat',
                               Commodity == '1005'~'Maize',
                               Commodity == '1006'~'Rice'),
         Value = as.numeric(Value)/1000000000)

MOCData2017 <- QuotaFillDat[c(1:3), ] %>%
  mutate(Year = 2017, Value = c(4.3, 2.8, 4))

g <- rbind(QuotaFillDat, MOCData2017) %>%
  mutate(QuotaFill = 100*Value/TotalQuota,
         PrivateQuotaFill = 100*Value/PrivateQuota) %>%
  ggplot(data = .) +
  geom_bar(aes(Year, QuotaFill, fill = Commodity), stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(2005, 2017, by = 2)) +
  scale_fill_manual(values = c('grey', 'grey40', 'black')) +
  labs(x = ' ', y = 'Quota fill rates (%)') +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = 'sans'),
        legend.direction = 'horizontal',
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = 'black', family = 'sans'),
        axis.title = element_text(size = 12, color = 'black', family = 'sans'))

g

ggsave('Fig_QuotaFillRate_Text.png', g, width = 180, height = 150, units = 'mm')


#-------------------------------------------------------------------------------------------------------------------------------#
# Draw the annual tariff equivalents of the tariff quota administration.
g_nra <-
  fread('imputed_price_gap_update.csv') %>%
  mutate(year = substr(time,1,4)) %>%
  group_by(year, commodity) %>%
  summarise(value = mean(gap_ratio)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_line(aes(year, value, lty = commodity, group = commodity)) +
  geom_point(aes(year, value, lty = commodity, group = commodity)) +
  theme_classic() +
  scale_y_continuous(limits = c(-40, 60),
                     breaks = seq(-40, 60, by = 10)) +
  scale_x_continuous(breaks = seq(2009, 2017, by = 1)) +
  geom_hline(yintercept = 0, colour = "black") +
  annotate("text", x = c(2016.5, 2016.5, 2016.5), y = c(21, 36, 55),
           label = c('Maize',"Rice", 'Wheat'),
           size = 3.5) +
  labs(x = '', y = 'Tariff equivalents (%)') +
  theme(legend.position = 'none',
        axis.text = element_text(color = 'black', size = 11))

ggsave('9_fig9_chinanra.png', g_nra, width = 180, height = 160, units = 'mm')
#-------------------------------------------------------------------------------------------------------------------------------#




## Plot the price data.
currency <- fread(input = 'Currency_IMF.csv') %>%
  mutate(Date = as.Date(Date, format = '%d-%b-%Y'),
         Date = format(Date, "%Y%m")) %>%
  group_by(Date) %>%
  summarise(Currency = mean(Currency, na.rm = TRUE))

AnnuanCurrency <- currency %>%
  mutate(Year = substr(Date, 1, 4)) %>%
  group_by(Year) %>%
  summarise(Currency = mean(Currency))

PriceDat <- fread('imputed_price_gap_update.csv') %>%
  dplyr::select(2:5) %>%
  mutate(time = as.character(time)) %>%
  left_join(., currency, by = c('time' = 'Date')) %>%
  mutate(domestic = domestic/Currency, international = international/Currency) %>%
  dplyr::select(-Currency) %>%
  gather(Market, Price, 3:4) %>%
  mutate(Time = as.Date(paste0(time, '01'), format = '%Y%m%d')) %>%
  dplyr::select(-time)

DomPriceDat <- data.frame(matrix(NA, 30, 1))
DomPriceDat$PriceSupport <- c(1840, 1940, 2140, 2500, 2700, 2760, 2760, 138*20, 136*20, 126*20,
                              1660, 1720, 1860, 2040, 2240, 2360, 2360, 118*20, 118*20, 115*20,
                              1500, 1700, 1980, 2120, 2240, 2240, 2000, NA, NA, NA)
DomPriceDat$Year <- rep(c(2009:2018), 3)
DomPriceDat$ExChangeRate <- rep(c(6.83, 6.77, 6.46, 6.31, 6.2, 6.14, 6.23, 6.64, 6.76, NA), 3)
DomPriceDat$commodity <- c(rep('RICE', 10), rep('WHEAT', 10), rep('MAIZE', 10))

PriceDat2 <- PriceDat %>%
  mutate(Year = year(Time)) %>%
  left_join(., DomPriceDat, by = c('commodity', 'Year')) %>%
  mutate(PriceSupportUSD = PriceSupport/ExChangeRate) %>%
  select(commodity, Market, Time, Price, PriceSupportUSD) %>%
  gather(Variable, Value, 4:5) %>%
  filter(!(Market == 'Domestic' & Variable == 'PriceSupportUSD')) %>%
  mutate(Market = ifelse(Variable == 'PriceSupportUSD', 'Support', Market))


g1 <-
  ggplot() +
  geom_line(data = dplyr::filter(PriceDat2, commodity == 'MAIZE', Market != 'Support'),
            aes(Time, Value, lty = Market), size = 0.5) +
  geom_step(data = dplyr::filter(PriceDat2, commodity == 'MAIZE', Market == 'Support'),
            aes(Time, Value), size = 0.4, lty = 'dotdash') +
  labs(x = ' ', y = 'Price (USD/MT)', title = '(a) Maize') +
  scale_x_date(date_labels="%Y", date_breaks = '1 year', limits = c(as.Date('2009-01-01', '%Y-%m-%d'), as.Date('2017-12-01', '%Y-%m-%d'))) +
  scale_y_continuous(limits = c(200, 460), breaks = seq(200, 460, 40)) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  #  theme_classic() +
  theme(strip.background = element_rect(colour="white", fill="white"),
        axis.text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        title = element_text(size = 11, face = 'plain')) +
  annotate("text", label ='Domestic price', x = as.Date("2016-01-01"), y = 420, color = "black", size =3.5) +
  annotate("text", label ='World price', x = as.Date("2015-02-01"), y = 240, color = "black", size =3.5) +
  annotate("text", label ='Support price', x = as.Date("2011-01-01"), y = 242, color = "black", size =3.5)
g1

g2 <-
  ggplot() +
  geom_line(data = dplyr::filter(PriceDat2, commodity == 'RICE', Market != 'Support'),
            aes(Time, Value, lty = Market), size = 0.5) +
  geom_step(data = dplyr::filter(PriceDat2, commodity == 'RICE', Market == 'Support'),
            aes(Time, Value), size = 0.4, lty = 'dotdash') +
  labs(x = ' ', y = 'Price (USD/MT)', title = '(b) Rice') +
  scale_x_date(date_labels="%Y", date_breaks = '1 year', limits = c(as.Date('2009-01-01', '%Y-%m-%d'), as.Date('2017-12-01', '%Y-%m-%d'))) +
  scale_y_continuous(limits = c(240, 760), breaks = seq(240, 760, 80)) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  #theme_classic() +
  theme(strip.background = element_rect(colour="white", fill="white"),
        axis.text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        title = element_text(size = 11)) +
  annotate("text", label ='Domestic price', x = as.Date("2016-01-01"), y = 700, color = "black", size =3.5) +
  annotate("text", label ='World price', x = as.Date("2015-06-01"), y = 520, color = "black", size =3.5) +
  annotate("text", label ='Support price', x = as.Date("2012-01-01"), y = 315, color = "black", size =3.5)
g2

g3 <-
  ggplot() +
  geom_line(data = dplyr::filter(PriceDat2, commodity == 'WHEAT', Market != 'Support'),
            aes(Time, Value, lty = Market), size = 0.5) +
  geom_step(data = dplyr::filter(PriceDat2, commodity == 'WHEAT', Market == 'Support'),
            aes(Time, Value), size = 0.4, lty = 'dotdash') +
  labs(x = ' ', y = 'Price (USD/MT)', title = '(c) Wheat') +
  scale_x_date(date_labels="%Y", date_breaks = '1 year', limits = c(as.Date('2009-01-01', '%Y-%m-%d'), as.Date('2017-12-01', '%Y-%m-%d'))) +
  scale_y_continuous(limits = c(240, 560), breaks = seq(240, 560, 80)) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  #theme_classic() +
  theme(strip.background = element_rect(colour="white", fill="white"),
        axis.text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        title = element_text(size = 11)) +
  annotate("text", label ='Domestic price', x = as.Date("2015-06-01"), y = 520, color = "black", size =3.5) +
  annotate("text", label ='World price', x = as.Date("2015-06-01"), y = 270, color = "black", size =3.5) +
  annotate("text", label ='Support price', x = as.Date("2012-01-01"), y = 280, color = "black", size =3.5)

g3
gg <- grid.arrange(g1, g2, g3, nrow = 2 )

ggsave('Fig_PriceNPR.png', gg, width = 240, height = 220, units = 'mm')



#--------------------------------------------------------------------------------------------------------------------------------#
# Figure, import shares from top importing countries during 2009-2017.
ImportDat <- read.csv('0_Data_ChinaMonthGrainImport_Clean_Update2017.csv') # monthly imports of staples

YearImportDat <- ImportDat %>%
  group_by(Year, Commodity, Country) %>%
  summarise(ImportValue = sum(Value, na.rm = TRUE)) %>%
  dplyr::mutate(Major = case_when((Country == 'USA' & Commodity == 'Maize') | (Country == 'UKR' & Commodity == 'Maize') ~ 1,
                                  (Country == 'AUS' & Commodity == 'Wheat') | (Country == 'CAN' & Commodity == 'Wheat') | (Country == 'USA' & Commodity == 'Wheat')~ 1,
                                  (Country == 'VIN' & Commodity == 'Rice') | (Country == 'THI' & Commodity == 'Rice') | (Country == 'PAK' & Commodity == 'Rice') ~ 1,
                                  Country == 'ALL' ~ 2,
                                  TRUE ~ 0)) %>%
  dplyr::filter(Major != 0, Year >= 2013) %>%
  group_by(Year, Commodity, Major) %>%
  summarise(ImportValue = sum(ImportValue, na.rm = TRUE)) %>%
  spread(Major, ImportValue) %>%
  mutate(Ratio = 100*`1`/`2`) %>%
  ggplot() +
  geom_bar(aes(Year, Ratio, group = Commodity,
               fill = Commodity),
           stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values = c('grey', 'grey40', 'black')) +
  labs(x = ' ', y = 'Import Shares by Major Partners (%)') +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = 'sans'),
        legend.direction = 'horizontal',
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = 'black', family = 'sans'),
        axis.title = element_text(size = 12, color = 'black', family = 'sans'))

ggsave('Fig_MajorImportShares.png', YearImportDat, width = 180, height = 160, units = 'mm')


# ------------------------------------------------------------------------------------------------------------------------------------ #
# Analysis of the discrepancy.
# ------------------------------------------------------------------------------------------------------------------------------------ #
rm(list = ls())
library(ggplot2)

ChinaImportDat <- fread('ChinaImportFromPartner.csv') %>%
  mutate(ID = 'ChinaReportImport') %>%
  filter(Year <= 2016, `Commodity Code` == 1006)

PartnerExportDat <- fread('PartnerExportToChina.csv') %>%
  mutate(ID = 'PartnerReportExport') %>%
  filter(Year <= 2016, `Commodity Code` == 1006)

ChinaImportDatAgg <- ChinaImportDat %>%
  filter(`Partner Code` != 0) %>%
  group_by(Year, `Commodity Code`, `Partner Code`) %>%
  summarise(ChinaQuant = as.numeric(sum(`Netweight (kg)`)),
            ChinaValue = as.numeric(sum(Value)))

PartnerExportDatAgg <- PartnerExportDat %>%
  group_by(Year, `Commodity Code`, `Reporter Code`) %>%
  summarise(PartnerQuant = as.numeric(sum(`Netweight (kg)`, na.rm = T)),
            PartnerValue = as.numeric(sum(Value, na.rm = T)))

CompareDat <- ChinaImportDatAgg %>%
  full_join(., PartnerExportDatAgg, by = c('Year', 'Commodity Code', 'Partner Code' = 'Reporter Code')) %>%
  ungroup() %>%
  mutate(DifValue = PartnerValue - ChinaValue,
         DifQuant = PartnerQuant - ChinaQuant) %>%
  rename(Commodity = `Commodity Code`) %>%
  mutate(Commodity = as.character(Commodity),
         Commodity = case_when(Commodity == '1001' ~ 'Wheat',
                               Commodity == '1005' ~ 'Maize',
                               Commodity == '1006' ~ 'Rice'))

AggCompareDat <- CompareDat %>%
  group_by(Year, Commodity) %>%
  summarise(DifValueAll = sum(DifValue, na.rm = TRUE),
            DifQuantAll = sum(DifQuant, na.rm = TRUE))

AggCompareDatCty <- CompareDat %>%
  group_by(Year, Commodity, `Partner Code`) %>%
  mutate(DifValueAll = sum(DifValue, na.rm = TRUE),
         DifQuantAll = sum(DifQuant, na.rm = TRUE)) %>%
  filter(`Partner Code` %in% c(104, 704)) %>%
  ungroup() %>%
  rename(Country = `Partner Code`) %>%
  mutate(Country = case_when(Country == 104 ~ 'Vietnam',
                             Country == 704 ~ 'Myanmar'))


g1 <-
  ggplot() +
  #geom_bar(data = AggCompareDat, aes(Year, DifValueAll/1000000), stat = 'identity', fill = 'white', col = 'black') +
  geom_bar(data = AggCompareDatCty, aes(Year, DifValueAll/1000000, col = Country, fill = Country), stat = 'identity') +
  scale_color_manual(values = c('grey60', 'grey10'),
                     breaks = c('Myanmar', 'Vietnam')) +
  scale_fill_manual(values = c('grey60', 'grey10'),
                    breaks = c('Myanmar', 'Vietnam')) +
  theme_classic() +
  labs(x = '', y = 'Import value, million dollars') +
  theme(axis.text = element_text(size = 12, color = 'black', family = 'sans'),
        axis.title = element_text(size = 12, color = 'black', family = 'sans'),
        legend.position = 'bottom',
        legend.title = element_blank())

g1

ggsave('Fig_TradeMisvoice.png', g1, width = 180, height = 160, units = 'mm')



ProjQuotaFillRateFunc <- function(SelectYear){
PriceGap15 <- PriceGap %>%
  dplyr::filter(year(Time) == SelectYear) %>%
  group_by(Commodity) %>%
  summarise(GapRatio = mean(GapRatio)) %>%
  mutate(GapRatio = 100/GapRatio - 1) # domestic price is at denominator.

Quant15 <- RegDat %>%
  dplyr::filter(year(Time) == SelectYear) %>%
  dplyr::select(gsub("EShare", "QT", ShareNames)) %>%
  colSums()

PriceGap15M <- matrix(c(rep(PriceGap15$GapRatio[1], 2), rep(PriceGap15$GapRatio[2], 3), rep(PriceGap15$GapRatio[3], 3)), 8, 1)

Quant15Proj <- lapply(1:nSim, function(i) (ElasSim2[[i]] %*% PriceGap15M + 1)*Quant15)
Quant15ProjMean <- sapply(1:8, function(i) mean(unlist(lapply(Quant15Proj, '[[', i))))
Quant15ProjSd <- sapply(1:8, function(i) sd(unlist(lapply(Quant15Proj, '[[', i))))


QuantProjVis <-  data.frame(Source = gsub("EShare", "QT", ShareNames),
                            Base = Quant15/1000000, Mean = Quant15ProjMean/1000000, # in million tonnes
                            Sd = Quant15ProjSd/1000000) %>%
  gather(Scenario, Value, 2:3) %>%
  mutate(Country = substr(Source, 3, 5),
         Commodity = substr(Source, 1, 1),
         Sd = ifelse(Scenario == 'Base', 0, Sd))


AggQuantProjVis <- QuantProjVis %>%
  group_by(Commodity, Scenario) %>%
  summarise(Value = sum(Value), Sd = sum(Sd)) %>%
  ungroup() %>%
  mutate(Commodity = case_when(Commodity == 'M' ~ 'Maize',
                               Commodity == 'R' ~ 'Rice',
                               Commodity == 'W' ~ 'Wheat')) %>%
  mutate(Overquota = case_when(Commodity == 'Wheat' ~ 9.636,
                               Commodity == 'Maize' ~ 7.2,
                               Commodity == 'Rice' ~ 5.32)) %>%
  mutate(Upper = Value + 1.692*Sd, Lower = Value - 1.692*Sd, Year = SelectYear)
  # mutate(Upper = ifelse(Upper >= Overquota, Overquota, Upper),
  #        Lower = ifelse(Lower >= Overquota, Overquota, Lower))
AggQuantProjVis
}

ProjQuotaFillRateFunc2 <- function(SelectYear){
  PriceGap15 <- PriceGap %>%
    dplyr::filter(year(Time) == SelectYear) %>%
    group_by(Commodity) %>%
    summarise(GapRatio = mean(GapRatio)) %>%
    mutate(GapRatio = 100/GapRatio - 1) # domestic price is at denominator.

  Quant15 <- RegDat %>%
    dplyr::filter(year(Time) == SelectYear) %>%
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


  AggQuantProjVis <- QuantProjVis %>%
    group_by(Commodity, Scenario) %>%
    summarise(Value = sum(Value), Sd = sum(Sd)) %>%
    ungroup() %>%
    mutate(Commodity = case_when(Commodity == 'M' ~ 'Maize',
                                 Commodity == 'R' ~ 'Rice',
                                 Commodity == 'W' ~ 'Wheat')) %>%
    mutate(Overquota = case_when(Commodity == 'Wheat' ~ 9.636,
                                 Commodity == 'Maize' ~ 7.2,
                                 Commodity == 'Rice' ~ 5.32)) %>%
    mutate(Upper = Value + 1.692*Sd, Lower = Value - 1.692*Sd, Year = SelectYear)
  # mutate(Upper = ifelse(Upper >= Overquota, Overquota, Upper),
  #        Lower = ifelse(Lower >= Overquota, Overquota, Lower))
  AggQuantProjVis
}


GetCoef <- function(EstObject, sim = FALSE){
  # c(which(grepl('Impute', names(coef(AIDSEstRes)))), which(grepl('Quarter', names(coef(AIDSEstRes)))))
  if(isTRUE(sim)){
    EstCoef = EstObject
  }else{
    EstCoef <- coef(EstObject)
  }
  AlphaEst <- c(EstCoef[grepl('Intercept', names(EstCoef))],
                1-sum(EstCoef[grepl('Intercept', names(EstCoef))]))
  BetaEst <- c(EstCoef[grepl('PriceIndex', names(EstCoef))],
               -sum(EstCoef[grepl('PriceIndex', names(EstCoef))]))
  ImputeEst <- c(EstCoef[grepl('Impute', names(EstCoef))],
                 - sum(EstCoef[grepl('Impute', names(EstCoef))]))

  num <- sum(grepl('eq1_log', names(EstCoef)))
  GammaEst <- matrix(NA, num, num)
  for (i in seq(1, num-1)){
    GammaEst[i,] <- EstCoef[grepl(paste0('eq',i,'_log'), names(EstCoef))]
  }
  GammaEst[num,] <- -colSums(GammaEst[-num, ])

  SeasonEst <- matrix(NA, num, sum(grepl('eq1_factor', names(EstCoef))))
  for (i in seq(1, num-1)){
    SeasonEst[i,] <- EstCoef[grepl(paste0('eq',i,'_factor'), names(EstCoef))]
  }
  SeasonEst[num,] <- -colSums(SeasonEst[-num, ])

  names(AlphaEst)[num] <- paste0('eq', num, '_(Intercept)')
  names(BetaEst)[num] <- paste0('eq', num, '_PriceIndex')

  out <- list(alpha = AlphaEst, beta = BetaEst, gamma = GammaEst, season = SeasonEst, impute = ImputeEst)
  out
}


GetElas <- function(UnrestrictDat, ShareNames, CoefEst, AggShare, SubShare, AggExp, a1, b2){
  num <- length(ShareNames)
  KDM <- matrix(0, num, num) # Kroneker Delta matrix
  diag(KDM) <- 1
  AveExpShare <- matrix(colMeans(UnrestrictDat[, ShareNames]), 1, num)
  BetaM <- matrix(CoefEst$beta, num, 1)
  TempM <- CoefEst$gamma - BetaM %*% AveExpShare
  for (i in c(1:num)){
    TempM[i, ] <- TempM[i, ]/AveExpShare[1, i]
  }
  PriceElas <- -KDM + TempM
  # Get expenditure elasticity estimates.
  ExpElas <- 1 + CoefEst$beta/colMeans(UnrestrictDat[, ShareNames])
  list(PriceElas = PriceElas, ExpElas = ExpElas)
}

GetElasUpdate <- function(ShareNames, CoefEst, AggShare, SubShare, AggExp, a1, b2, BaseShare){
  num <- length(ShareNames)
  ones <- matrix(1, num, 1)
  KDM <- matrix(0, num, num) # Kroneker Delta matrix
  diag(KDM) <- 1

  BetaM <- matrix(CoefEst$beta, num, 1)
  BaseShareM <- matrix(BaseShare, 1, num)
  GammaM <- CoefEst$gamma

  SubShareM1 <- diag(num)
  diag(SubShareM1) <- 1+b2*SubShare
  Element1 <- GammaM %*% SubShareM1

  Element2 <- matrix(CoefEst$beta *(a1/AggExp-1) * BaseShare *(1+b2*SubShare), 1, num)

  AggShareM <- diag(num)
  diag(AggShareM) <- 1/AggShare

  PriceElas <- -KDM + AggShareM %*% (Element1 + ones%*%Element2)
  print(ones%*%Element2)
  # Get expenditure elasticity estimates.
  # ExpElas <- 1 + CoefEst$beta/colMeans(UnrestrictDat[, ShareNames])
  list(PriceElas = PriceElas)
  #list(PriceElas = PriceElas, ExpElas = ExpElas)
}

GetElasCorrect <- function(ShareNames, CoefEst, AggShare, SubShare, AggExp, a1, b1, BaseShare){
  num = length(ShareNames)
  DeltaM <- matrix(0, num, num)
  diag(DeltaM) <- 1     # Delta matrix

  TauM <- matrix(0, num, num)   # Tau matrix.
  TauM[c(1, 2), c(1, 2)] <- matrix(1, 2, 1) %*% matrix(SubShare[1:2]*b1, 1, 2)
  TauM[c(3: 5), c(3: 5)] <- matrix(1, 3, 1) %*% matrix(SubShare[3:5]*b1, 1, 3)
  TauM[c(6: 8), c(6: 8)] <- matrix(1, 3, 1) %*% matrix(SubShare[6:8]*b1, 1, 3)

  TauDiagM <- diag(num)
  diag(TauDiagM) <- SubShare*b1 + 1

  x = a1/AggExp

  AggShareM <- diag(num)
  diag(AggShareM) <- 1/AggShare[1:num]

  -DeltaM - TauM + (AggShareM %*% (CoefEst$gamma + (x-1)*matrix(CoefEst$beta, num, 1) %*% matrix(BaseShare, 1, num)) +
                      x*matrix(1, num, 1) %*% matrix(BaseShare, 1, num)) %*% TauDiagM
}


GetElasCorrect_Elas <- function(ShareNames, CoefEst, AggShare, SubShare, e1, b1, BaseShare){
  b1_vector <- c(rep(b1[1], 2), rep(b1[2], 3), rep(b1[3], 3))
  num = length(ShareNames)
  DeltaM <- matrix(0, num, num)
  diag(DeltaM) <- 1     # Delta matrix

  TauM <- matrix(0, num, num)   # Tau matrix.
  TauM[c(1, 2), c(1, 2)] <- matrix(1, 2, 1) %*% matrix(SubShare[1:2]*b1_vector[1:2], 1, 2)
  TauM[c(3: 5), c(3: 5)] <- matrix(1, 3, 1) %*% matrix(SubShare[3:5]*b1_vector[3:5], 1, 3)
  TauM[c(6: 8), c(6: 8)] <- matrix(1, 3, 1) %*% matrix(SubShare[6:8]*b1_vector[6:8], 1, 3)

  TauDiagM <- diag(num)
  diag(TauDiagM) <- SubShare*b1_vector + 1

  x = e1

  AggShareM <- diag(num)
  diag(AggShareM) <- 1/AggShare[1:num]

  -DeltaM - TauM + (AggShareM %*% (CoefEst$gamma + (x-1)*matrix(CoefEst$beta, num, 1) %*% matrix(BaseShare, 1, num)) +
                      x*matrix(1, num, 1) %*% matrix(BaseShare, 1, num)) %*% TauDiagM
}

GetElasCorrect10Eqs <- function(ShareNames, CoefEst, AggShare, SubShare, e1, b1, BaseShare){
  b1_vector <- c(rep(b1[1], 3), rep(b1[2], 4), rep(b1[3], 4))
  num = length(ShareNames)
  DeltaM <- matrix(0, num, num)
  diag(DeltaM) <- 1     # Delta matrix

  TauM <- matrix(0, num, num)   # Tau matrix.
  TauM[c(1: 3), c(1: 3)] <- matrix(1, 3, 1) %*% matrix(SubShare[1:3]*b1_vector[1:3], 1, 3)
  TauM[c(4: 7), c(4: 7)] <- matrix(1, 4, 1) %*% matrix(SubShare[4:7]*b1_vector[4:7], 1, 4)
  TauM[c(8: 11), c(8: 11)] <- matrix(1, 4, 1) %*% matrix(SubShare[8:11]*b1_vector[8:11], 1, 4)

  TauDiagM <- diag(num)
  diag(TauDiagM) <- SubShare*b1_vector + 1

  x = e1

  AggShareM <- diag(num)
  diag(AggShareM) <- 1/AggShare[1:num]

  -DeltaM - TauM + (AggShareM %*% (CoefEst$gamma + (x-1)*matrix(CoefEst$beta, num, 1) %*% matrix(BaseShare, 1, num)) +
                      x*matrix(1, num, 1) %*% matrix(BaseShare, 1, num)) %*% TauDiagM
}


ProjFunc <- function(e1){
  nSim <- 1000
  set.seed(1992)
  ParaSim <- rmvnorm(n=nSim, mean=coef(AIDSEstRes), sigma=vcov(AIDSEstRes), method = 'svd')
  CoefSim <- lapply(1:nSim, function(i) GetCoef(ParaSim[i,], sim = TRUE))

  ElasSim2 <- lapply(1:nSim, function(i) GetElasCorrect_Elas(ShareNames, CoefSim[[i]], AggShare = AggShareDat[ShareNames],
                                                             SubShare = SubShareDat, e1= e1, b1 = 0, BaseShare = basePrice$shares)) # Choose e1.

  PriceElasMean2 <- matrix(sapply(1:64, function(i) mean(unlist(lapply(ElasSim2, '[[', i)))), 8, 8)
  PriceElasSd2 <- matrix(sapply(1:64, function(i) sd(unlist(lapply(ElasSim2, '[[', i)))), 8, 8)
  PriceElasPvalue2 <- matrix(sapply(1:64, function(i) 2*pt(-abs(PriceElasMean2[i]/PriceElasSd2[i]),df = 45)), 8, 8) # Update degree of freedom

  IDNames <- c('Maize-USA', 'Maize-Ukraine', 'Rice-Thailand', 'Rice-VietNam', 'Rice-Pakistan', 'Wheat-USA', 'Wheat-Australia', 'Wheat-Canada')

  AveElas <- c(weighted.mean(diag(PriceElasMean2)[1:2], SubShareDat[1:2]), weighted.mean(diag(PriceElasMean2)[3:5], SubShareDat[3:5]),
               weighted.mean(diag(PriceElasMean2)[6:8], SubShareDat[6:8]))
  AveElasSd <- c(weighted.mean(diag(PriceElasSd2)[1:2], SubShareDat[1:2]), weighted.mean(diag(PriceElasSd2)[3:5], SubShareDat[3:5]),
                 weighted.mean(diag(PriceElasSd2)[6:8], SubShareDat[6:8]))

  PriceGap15 <- PriceGap %>%
    dplyr::filter(year(Time) == 2015) %>%
    group_by(Commodity) %>%
    summarise(GapRatio = mean(GapRatio)) %>%
    mutate(GapRatio = 100/GapRatio - 1)

  Quant15 <- RegDat %>%
    dplyr::filter(year(Time) == 2015) %>%
    dplyr::select(gsub("EShare", "QT", ShareNames)) %>%
    colSums()

  PriceGap15M <- matrix(c(rep(PriceGap15$GapRatio[1], 2), rep(PriceGap15$GapRatio[2], 3), rep(PriceGap15$GapRatio[3], 3)), 8, 1)

  Quant15Proj <- lapply(1:nSim, function(i) (ElasSim2[[i]]%*% PriceGap15M + 1)*Quant15)
  Quant15ProjMean <- sapply(1:8, function(i) mean(unlist(lapply(Quant15Proj, '[[', i))))
  Quant15ProjSd <- sapply(1:8, function(i) sd(unlist(lapply(Quant15Proj, '[[', i))))

  QuantProjVis <-  data.frame(Source = gsub("EShare", "QT", ShareNames),
                              Base = Quant15/1000000, Mean = Quant15ProjMean/1000000, # in million tonnes
                              Sd = Quant15ProjSd/1000000) %>%
    gather(Scenario, Value, 2:3) %>%
    mutate(Country = substr(Source, 3, 5),
           Commodity = substr(Source, 1, 1),
           Sd = ifelse(Scenario == 'Base', 0, Sd))

  AggQuantProjVis <- QuantProjVis %>%
    group_by(Commodity, Scenario) %>%
    summarise(Value = sum(Value), Sd = sum(Sd)) %>%
    ungroup() %>%
    mutate(Commodity = case_when(Commodity == 'M' ~ 'Maize',
                                 Commodity == 'R' ~ 'Rice',
                                 Commodity == 'W' ~ 'Wheat'),
           Elas = e1) %>%
    mutate(Overquota = case_when(Commodity == 'Wheat' ~ 9.636,
                                 Commodity == 'Maize' ~ 7.2,
                                 Commodity == 'Rice' ~ 5.32)) %>%
    dplyr::filter(Scenario == 'Mean')
  AggQuantProjVis
}



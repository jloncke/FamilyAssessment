########################################
########################################
###### ~~~~~~~ SIMULATION ~~~~~~~ ######
########################################
########################################

###### ~~~~~~~ data creating model ~~~~~~~ ######
data.model <- '
## GROUP 1
# Family effect
FE1 =~ 1*TM + 1*TF + 1*TS + 1*MT + 1*MF + 1*MS + 1*FT + 1*FM + 1*FS + 1*ST + 1*SM + 1*SF

# Actor effects
A.H1 =~ 1*TM + 1*TF + 1*TS
A.M1 =~ 1*MT + 1*MF + 1*MS
A.V1 =~ 1*FT + 1*FM + 1*FS
A.Z1 =~ 1*ST + 1*SM + 1*SF

# Partner effects
P.H1 =~ 1*MT + 1*FT + 1*ST
P.M1 =~ 1*TM + 1*FM + 1*SM
P.V1 =~ 1*TF + 1*MF + 1*SF
P.Z1 =~ 1*TS + 1*MS + 1*FS

# Variance labels
# G1
FE1  ~~ 0.5*FE1
A.H1 ~~ 0.5*A.H1
A.M1 ~~ 0.5*A.M1
A.V1 ~~ 0.5*A.V1
A.Z1 ~~ 0.5*A.Z1
P.H1 ~~ 0.05*P.H1
P.M1 ~~ 0.05*P.M1
P.V1 ~~ 0.05*P.V1
P.Z1 ~~ 0.05*P.Z1

# Generalized reciprocity:
# G1
A.H1 ~~ 0.05*P.H1
A.M1 ~~ 0.05*P.M1 
A.V1 ~~ 0.05*P.V1
A.Z1 ~~ 0.05*P.Z1

# Dyadic reciprocity:
# G1
TM ~~ 0.02*MT
TF ~~ 0.02*FT
TS ~~ 0.02*ST
MF ~~ 0.02*FM
MS ~~ 0.02*SM
FS ~~ 0.02*SF

## Compute structured means
# Define labels for subsequent constraints
# G1
FE1 ~ 2*1
A.H1 ~ 0.1*1
A.M1 ~ -0.1*1
A.V1 ~ 0.1*1
A.Z1 ~ -0.1*1
P.H1 ~ 0.1*1
P.M1 ~ -0.1*1
P.V1 ~ 0.1*1
P.Z1 ~ -0.1*1

# G1
TM ~~ 0.3*TM
TF ~~ 0.3*TF
TS ~~ 0.3*TS
MT ~~ 0.3*MT
MF ~~ 0.3*MF
MS ~~ 0.3*MS
FT ~~ 0.3*FT
FM ~~ 0.3*FM
FS ~~ 0.3*FS
ST ~~ 0.3*ST
SM ~~ 0.3*SM
SF ~~ 0.3*SF

# set summarys of observed variables to zero
# G1
TM ~ -0.025*1 
TF ~ -0.025*1 
TS ~ 0.050*1 

MT ~ 0.05*1 
MF ~ -0.025*1 
MS ~ -0.025*1 

FM ~ 0.050*1 
FT ~ -0.025*1 
FS ~ -0.025*1 

SM ~ -0.025*1 
SF ~ 0.05*1 
ST ~ -0.025*1 
'

###### ~~~~~~~ SRM model -- CFA ~~~~~~~ ######
SRM.model <- '
## GROUP 1
# Family effect
FE1 =~ 1*TM + 1*TF + 1*TS + 1*MT + 1*MF + 1*MS + 1*FT + 1*FM + 1*FS + 1*ST + 1*SM + 1*SF

# Actor effects
A.H1 =~ 1*TM + 1*TF + 1*TS
A.M1 =~ 1*MT + 1*MF + 1*MS
A.V1 =~ 1*FT + 1*FM + 1*FS
A.Z1 =~ 1*ST + 1*SM + 1*SF

# Partner effects
P.H1 =~ 1*MT + 1*FT + 1*ST
P.M1 =~ 1*TM + 1*FM + 1*SM
P.V1 =~ 1*TF + 1*MF + 1*SF
P.Z1 =~ 1*TS + 1*MS + 1*FS

RTM =~ 1*TM
RTF =~ 1*TF
RTS =~ 1*TS
RMT =~ 1*MT
RMF =~ 1*MF
RMS =~ 1*MS
RFT =~ 1*FT
RFM =~ 1*FM
RFS =~ 1*FS
RST =~ 1*ST
RSM =~ 1*SM
RSF =~ 1*SF


# Variance labels
# G1
FE1  ~~ varFE1*FE1
A.H1 ~~ varAH1*A.H1
A.M1 ~~ varAM1*A.M1
A.V1 ~~ varAV1*A.V1
A.Z1 ~~ varAZ1*A.Z1
P.H1 ~~ varPH1*P.H1
P.M1 ~~ varPM1*P.M1
P.V1 ~~ varPV1*P.V1
P.Z1 ~~ varPZ1*P.Z1
RTM ~~ VAR.HM1*RTM
RTF ~~ VAR.HV1*RTF
RTS ~~ VAR.HZ1*RTS
RMT ~~ VAR.MH1*RMT
RMF ~~ VAR.MV1*RMF
RMS ~~ VAR.MZ1*RMS
RFT ~~ VAR.VH1*RFT
RFM ~~ VAR.VM1*RFM
RFS ~~ VAR.VZ1*RFS
RST ~~ VAR.ZH1*RST
RSM ~~ VAR.ZM1*RSM
RSF ~~ VAR.ZV1*RSF

# Generalized reciprocity:
# G1
A.H1 ~~ GR.H*P.H1
A.M1 ~~ GR.M*P.M1 
A.V1 ~~ GR.V*P.V1
A.Z1 ~~ GR.Z*P.Z1

# Dyadic reciprocity:
# G1
RTM ~~ DR.MH*RMT
RTF ~~ DR.VH*RFT
RTS ~~ DR.HZ*RST
RMF ~~ DR.VM*RFM
RMS ~~ DR.ZM*RSM
RFS ~~ DR.ZV*RSF

## Compute structured means
# Define labels for subsequent constraints
# G1
FE1 ~ means.FE1*1
A.H1 ~ means.AH1*1
A.M1 ~ means.AM1*1
A.V1 ~ means.AV1*1
A.Z1 ~ means.AZ1*1
P.H1 ~ means.PH1*1
P.M1 ~ means.PM1*1
P.V1 ~ means.PV1*1
P.Z1 ~ means.PZ1*1

# G1
TM ~~ 0*TM
TF ~~ 0*TF
TS ~~ 0*TS
MT ~~ 0*MT
MF ~~ 0*MF
MS ~~ 0*MS
FT ~~ 0*FT
FM ~~ 0*FM
FS ~~ 0*FS
ST ~~ 0*ST
SM ~~ 0*SM
SF ~~ 0*SF

# set summarys of observed variables to zero
# G1
TM ~ 0*1
TF ~ 0*1
TS ~ 0*1
MT ~ 0*1
MF ~ 0*1
MS ~ 0*1
FM ~ 0*1
FT ~ 0*1
FS ~ 0*1
SM ~ 0*1
SF ~ 0*1
ST ~ 0*1

RTM ~ means.RHM1*1
RTF ~ means.RHV1*1
RTS ~ means.RHZ1*1
RMT ~ means.RMH1*1
RMF ~ means.RMV1*1
RMS ~ means.RMZ1*1
RFM ~ means.RVM1*1
RFT ~ means.RVH1*1
RFS ~ means.RVZ1*1
RSM ~ means.RZM1*1
RSF ~ means.RZV1*1
RST ~ means.RZH1*1

# Constraints
# G1
0 == means.AH1 + means.AM1 + means.AV1 + means.AZ1
0 == means.PH1 + means.PM1 + means.PV1 + means.PZ1
0 == means.RHM1 + means.RHV1 + means.RHZ1
0 == means.RMH1 + means.RMV1 + means.RMZ1
0 == means.RVH1 + means.RVM1 + means.RVZ1
0 == means.RZH1 + means.RZM1 + means.RZV1
0 == means.RMH1 + means.RVH1 + means.RVZ1
0 == means.RHM1 + means.RVM1 + means.RZM1
0 == means.RHV1 + means.RMV1 + means.RZV1
0 == means.RHZ1 + means.RMZ1 + means.RVZ1
'

###### ~~~~~~~ Number of loops ~~~~~~~ ######
loops <- 1000

###### ~~~~~~~ Empty vectors ~~~~~~~ ######
var_FE_a <- numeric()
var_AM_a <- numeric()
var_AV_a <- numeric()
var_AZ_a <- numeric()
var_AH_a <- numeric()
var_PM_a <- numeric()
var_PV_a <- numeric()
var_PZ_a <- numeric()
var_PH_a <- numeric()

var_FE_anew <- numeric()
var_AM_anew <- numeric()
var_AV_anew <- numeric()
var_AZ_anew <- numeric()
var_AH_anew <- numeric()
var_PM_anew <- numeric()
var_PV_anew <- numeric()
var_PZ_anew <- numeric()
var_PH_anew <- numeric()
var_RTM_anew <- numeric()
var_RTF_anew <- numeric()
var_RTS_anew <- numeric()
var_RMT_anew <- numeric()
var_RMF_anew <- numeric()
var_RMS_anew <- numeric()
var_RFT_anew <- numeric()
var_RFM_anew <- numeric()
var_RFS_anew <- numeric()
var_RST_anew <- numeric()
var_RSM_anew <- numeric()
var_RSF_anew <- numeric()

mean_FE_a <- numeric()
mean_AM_a <- numeric()
mean_AV_a <- numeric()
mean_AZ_a <- numeric()
mean_AH_a <- numeric()
mean_PM_a <- numeric()
mean_PV_a <- numeric()
mean_PZ_a <- numeric()
mean_PH_a <- numeric()
mean_RTM_a <- numeric()
mean_RTF_a <- numeric()
mean_RTS_a <- numeric()
mean_RMT_a <- numeric()
mean_RMS_a <- numeric()
mean_RMF_a <- numeric()
mean_RFT_a <- numeric()
mean_RFS_a <- numeric()
mean_RFM_a <- numeric()
mean_RST_a <- numeric()
mean_RSF_a <- numeric()
mean_RSM_a <- numeric()

p_anoFE <- numeric()
p_anoAM <- numeric()
p_anoAV <- numeric()
p_anoAH <- numeric()
p_anoAZ <- numeric()
p_anoPM <- numeric()
p_anoPV <- numeric()
p_anoPH <- numeric()
p_anoPZ <- numeric()
p_anoRTM <- numeric()
p_anoRTF <- numeric()
p_anoRTS <- numeric()
p_anoRMT <- numeric()
p_anoRMS <- numeric()
p_anoRMF <- numeric()
p_anoRFM <- numeric()
p_anoRFS <- numeric()
p_anoRFT <- numeric()
p_anoRSM <- numeric()
p_anoRST <- numeric()
p_anoRSF <- numeric()

p_anonewFE <- numeric()
p_anonewAM <- numeric()
p_anonewAV <- numeric()
p_anonewAH <- numeric()
p_anonewAZ <- numeric()
p_anonewPM <- numeric()
p_anonewPV <- numeric()
p_anonewPH <- numeric()
p_anonewPZ <- numeric()
p_anonewRTM <- numeric()
p_anonewRTF <- numeric()
p_anonewRTS <- numeric()
p_anonewRMT <- numeric()
p_anonewRMF <- numeric()
p_anonewRMS <- numeric()
p_anonewRFM <- numeric()
p_anonewRFT <- numeric()
p_anonewRFS <- numeric()
p_anonewRSM <- numeric()
p_anonewRST <- numeric()
p_anonewRSF <- numeric() 

p_anooldFE <- numeric()
p_anooldAM <- numeric()
p_anooldAV <- numeric()
p_anooldAH <- numeric()
p_anooldAZ <- numeric()
p_anooldPM <- numeric()
p_anooldPV <- numeric()
p_anooldPH <- numeric()
p_anooldPZ <- numeric()
p_anooldRTM <- numeric()
p_anooldRTF <- numeric()
p_anooldRTS <- numeric()
p_anooldRMT <- numeric()
p_anooldRMF <- numeric()
p_anooldRMS <- numeric()
p_anooldRFM <- numeric()
p_anooldRFT <- numeric()
p_anooldRFS <- numeric()
p_anooldRSM <- numeric()
p_anooldRST <- numeric()
p_anooldRSF <- numeric() 

con_anoFE <- numeric()
con_anoAM <- numeric()
con_anoAV <- numeric()
con_anoAH <- numeric()
con_anoAZ <- numeric()
con_anoPM <- numeric()
con_anoPV <- numeric()
con_anoPH <- numeric()
con_anoPZ <- numeric()
con_anoRTM <- numeric()
con_anoRTF <- numeric()
con_anoRTS <- numeric()
con_anoRMT <- numeric()
con_anoRMF <- numeric()
con_anoRMS <- numeric()
con_anoRFT <- numeric()
con_anoRFM <- numeric()
con_anoRFS <- numeric()
con_anoRSM <- numeric()
con_anoRSF <- numeric()
con_anoRST <- numeric()

con_anonewFE <- numeric()
con_anonewAM <- numeric()
con_anonewAV <- numeric()
con_anonewAH <- numeric()
con_anonewAZ <- numeric()
con_anonewPM <- numeric()
con_anonewPV <- numeric()
con_anonewPH <- numeric()
con_anonewPZ <- numeric()
con_anonewRTM <- numeric()
con_anonewRTF <- numeric()
con_anonewRTS <- numeric()
con_anonewRMT <- numeric()
con_anonewRMF <- numeric()
con_anonewRMS <- numeric()
con_anonewRFT <- numeric()
con_anonewRFM <- numeric()
con_anonewRFS <- numeric()
con_anonewRSM <- numeric()
con_anonewRSF <- numeric()
con_anonewRST <- numeric()

con_anooldFE <- numeric()
con_anooldAM <- numeric()
con_anooldAV <- numeric()
con_anooldAH <- numeric()
con_anooldAZ <- numeric()
con_anooldPM <- numeric()
con_anooldPV <- numeric()
con_anooldPH <- numeric()
con_anooldPZ <- numeric()
con_anooldRTM <- numeric()
con_anooldRTF <- numeric()
con_anooldRTS <- numeric()
con_anooldRMT <- numeric()
con_anooldRMF <- numeric()
con_anooldRMS <- numeric()
con_anooldRFT <- numeric()
con_anooldRFM <- numeric()
con_anooldRFS <- numeric()
con_anooldRSM <- numeric()
con_anooldRSF <- numeric()
con_anooldRST <- numeric()

N <- 151 # 51 - 151 - 301 - 501

for(i in 1:loops){
  set.seed(i)
  # simulate data
  data_Cook <- simulateData(data.model, sample.nobs  = N)
  # take random family out of the dataset
  id <- sample(seq(1:N), 1)
  data_Cook_rep <- data_Cook[-id,]
  
  # fit SRM 
  fit <- lavaan(SRM.model, data=data_Cook_rep)
  eff1 <- as.data.frame(parameterEstimates(fit))
  eff1$f <- paste(eff1$lhs, eff1$op, eff1$rhs)
  
  # Regression fs
  Regression <- lavPredict(fit)
  
  # Means of latent variables
  EETA <- lavaan:::computeEETA(lavmodel = fit@Model, lavsamplestats = fit@SampleStats)[[1]]
  # Covariance matrix latent variables
  VETA <- lavaan:::computeVETA(lavmodel = fit@Model)[[1]]
  # Lambda matrix
  LAMBDA <- lavaan:::computeLAMBDA(lavmodel = fit@Model, remove.dummy.lv = FALSE)[[1]]
  # Residual covariance matrix
  THETA <- lavaan:::computeTHETA(lavmodel=fit@Model)[[1]]
  # Model-implied covariance matrix
  Sigma.hat <- lavaan:::computeSigmaHat(lavmodel = fit@Model)[[1]]
  
  # expected values of latent variables
  mean_FE <- eff1[eff1$f=="FE1 ~1 ",]$est
  mean_AH <- eff1[eff1$f=="A.H1 ~1 ",]$est
  mean_AV <- eff1[eff1$f=="A.V1 ~1 ",]$est
  mean_AM <- eff1[eff1$f=="A.M1 ~1 ",]$est
  mean_AZ <- eff1[eff1$f=="A.Z1 ~1 ",]$est
  mean_PH <- eff1[eff1$f=="P.H1 ~1 ",]$est
  mean_PV <- eff1[eff1$f=="P.V1 ~1 ",]$est
  mean_PM <- eff1[eff1$f=="P.M1 ~1 ",]$est
  mean_PZ <- eff1[eff1$f=="P.Z1 ~1 ",]$est
  
  mean_RTM <- eff1[eff1$f=="RTM ~1 ",]$est
  mean_RTF <- eff1[eff1$f=="RTF ~1 ",]$est
  mean_RTS <- eff1[eff1$f=="RTS ~1 ",]$est
  mean_RMT <- eff1[eff1$f=="RMT ~1 ",]$est
  mean_RMF <- eff1[eff1$f=="RMF ~1 ",]$est
  mean_RMS <- eff1[eff1$f=="RMS ~1 ",]$est
  mean_RFM <- eff1[eff1$f=="RFM ~1 ",]$est
  mean_RFT <- eff1[eff1$f=="RFT ~1 ",]$est
  mean_RFS <- eff1[eff1$f=="RFS ~1 ",]$est
  mean_RSM <- eff1[eff1$f=="RSM ~1 ",]$est
  mean_RSF <- eff1[eff1$f=="RSF ~1 ",]$est
  mean_RST <- eff1[eff1$f=="RST ~1 ",]$est
  
  # Expected values of observed variables/dyadic values
  EY_TM <- mean_FE + mean_AH + mean_PM + mean_RTM
  EY_TF <- mean_FE + mean_AH + mean_PV + mean_RTF
  EY_TS <- mean_FE + mean_AH + mean_PZ + mean_RTS
  EY_MT <- mean_FE + mean_AM + mean_PH + mean_RMT
  EY_MF <- mean_FE + mean_AM + mean_PV + mean_RMF
  EY_MS <- mean_FE + mean_AM + mean_PZ + mean_RMS 
  EY_FM <- mean_FE + mean_AV + mean_PM + mean_RFM
  EY_FT <- mean_FE + mean_AV + mean_PH + mean_RFT
  EY_FS <- mean_FE + mean_AV + mean_PZ + mean_RFS
  EY_SM <- mean_FE + mean_AZ + mean_PM + mean_RSM
  EY_SF <- mean_FE + mean_AZ + mean_PV + mean_RSF
  EY_ST <- mean_FE + mean_AZ + mean_PH + mean_RST

  EY_1fam <- c(EY_TM,EY_TF,EY_TS,
               EY_MT,EY_MF,EY_MS,
               EY_FT,EY_FM,EY_FS,
               EY_ST,EY_SM,EY_SF)


  # calculation one family's regression fs?
  data.obs1 <- cbind(data_Cook$TM[id], data_Cook$TF[id], data_Cook$TS[id],
                     data_Cook$MT[id], data_Cook$MF[id], data_Cook$MS[id],
                     data_Cook$FT[id], data_Cook$FM[id], data_Cook$FS[id],
                     data_Cook$ST[id], data_Cook$SM[id], data_Cook$SF[id])
  #RES1  <- sweep(data.obs1,  MARGIN = 2L, STATS = EY_1fam,   FUN = "-") # hoe weten we de verwachte waarden?
  fs <- as.data.frame(data.obs1%*%FSC_R)
  
  names(fs) <- names(Regression)
  
  ## ANOVA-scores of the particular family
  FE    <- rowMeans(data_Cook[id,1:12])
  A.M <- 3 * 3/(4*2)*(data_Cook[id,]$MT + data_Cook[id,]$MF + data_Cook[id,]$MS)/3 +
    3/(4*2)*(data_Cook[id,]$FM + data_Cook[id,]$TM + data_Cook[id,]$SM)/3 - 3/2 * FE
  A.V <- 3 * 3/(4*2)*(data_Cook[id,]$FM + data_Cook[id,]$FS + data_Cook[id,]$FT)/3 +
    3/(4*2)*(data_Cook[id,]$MF + data_Cook[id,]$SF + data_Cook[id,]$TF)/3 - 3/2 * FE
  A.Z <- 3 * 3/(4*2)*(data_Cook[id,]$SM + data_Cook[id,]$SF +
                        data_Cook[id,]$ST)/3 +
    3/(4*2)*(data_Cook[id,]$MS + data_Cook[id,]$FS +
               data_Cook[id,]$TS)/3 - 3/2 * FE
  A.H <- 3 * 3/(4*2)*(data_Cook[id,]$TM + data_Cook[id,]$TF +
                        data_Cook[id,]$TS)/3 +
    3/(4*2)*(data_Cook[id,]$MT + data_Cook[id,]$FT +
               data_Cook[id,]$ST)/3 - 3/2 * FE
  P.M <- 3 * 3/(4*2)*(data_Cook[id,]$FM + data_Cook[id,]$SM + data_Cook[id,]$TM)/3 +
    3/(4*2)*(data_Cook[id,]$MF + data_Cook[id,]$MS + data_Cook[id,]$MT)/3 +
    - 3/2 * FE
  P.V <- 3 * 3/(4*2)*(data_Cook[id,]$MF + data_Cook[id,]$SF + data_Cook[id,]$TF)/3 +
    3/(4*2)*(data_Cook[id,]$FM + data_Cook[id,]$FS + data_Cook[id,]$FT)/3 +
    - 3/2 * FE
  P.Z <- 3 * 3/(4*2)*(data_Cook[id,]$MS + data_Cook[id,]$FS + data_Cook[id,]$TS)/3 +
    3/(4*2)*(data_Cook[id,]$SM + data_Cook[id,]$SF + data_Cook[id,]$ST)/3 +
    - 3/2 * FE
  P.H <- 3 * 3/(4*2)*(data_Cook[id,]$MT + data_Cook[id,]$FT + data_Cook[id,]$ST)/3  +
    3/(4*2)*(data_Cook[id,]$TM + data_Cook[id,]$TF + data_Cook[id,]$TS)/3 +
    - 3/2 * FE
  RTM <- data_Cook[id,]$TM - (A.H + P.M + FE)
  RTF <- data_Cook[id,]$TF - (A.H + P.V + FE)
  RTS <- data_Cook[id,]$TS - (A.H + P.Z + FE)
  RMT <- data_Cook[id,]$MT - (A.M + P.H + FE)
  RMF <- data_Cook[id,]$MF - (A.M + P.V + FE)
  RMS <- data_Cook[id,]$MS - (A.M + P.Z + FE)
  RFM <- data_Cook[id,]$FM - (A.V + P.M + FE)
  RFT <- data_Cook[id,]$FT - (A.V + P.H + FE)
  RFS <- data_Cook[id,]$FS - (A.V + P.Z + FE)
  RSM <- data_Cook[id,]$SM - (A.Z + P.M + FE)
  RSF <- data_Cook[id,]$SF - (A.Z + P.V + FE)
  RST <- data_Cook[id,]$ST - (A.Z + P.H + FE)
  
  # Weight matrix of ANOVA
  # "TM" "TF" "TS" "MT" "MF" "MS" "FT" "FM" "FS" "ST" "SM" "SF"
  FSC_9 <- matrix(c(1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12, # family effect
                    0.25,0.25,0.25,0,-0.125, -0.125,0, -0.125, -0.125,0, -0.125, -0.125, # A.H.
                    0, -0.125, -0.125,0.25,0.25,0.25, -0.125,0, -0.125, -0.125,0, -0.125, # A.M
                    -0.125, -0.125,0, -0.125, -0.125,0, -0.125, -0.125,0,0.25,0.25,0.25, # A.Z
                    -0.125,0, -0.125, -0.125,0, -0.125,0.25,0.25,0.25, -0.125, -0.125,0, # A.V
                    0,0,0,0.25,-0.125, -0.125,0.25, -0.125, -0.125,0.25, -0.125, -0.125, # P.H
                    0.25, -0.125, -0.125,0,0,0, -0.125,0.25, -0.125, -0.125,0.25, -0.125,# P.M
                    -0.125, -0.125,0.25, -0.125, -0.125,0.25, -0.125, -0.125,0.25,0,0,0, # P.Z
                    -0.125,0.25, -0.125, -0.125,0.25, -0.125,0,0,0, -0.125, -0.125,0.25, # P.V
                    5/12, -5/24, -5/24, -1/12, 1/24,  1/24, 1/24, -5/24, 1/6, 1/24,  -5/24, 1/6, # RHM
                    -5/24, 5/12,-5/24, 1/24, -5/24, 1/6, -1/12, 1/24,  1/24, 1/24,  1/6, -5/24, # RHV
                    -5/24, -5/24, 5/12, 1/24, 1/6, -5/24,1/24,1/6 , -5/24, -1/12, 1/24,1/24, # RHZ
                    -1/12, 1/24, 1/24, 5/12, -5/24, -5/24, -5/24,  1/24, 1/6, -5/24, 1/24, 1/6,# RMH
                    1/24, -5/24, 1/6, -5/24, 5/12, -5/24, 1/24, -1/12, 1/24, 1/6, 1/24, -5/24,# RMV
                    1/24, 1/6, -5/24, -5/24, -5/24, 5/12, 1/6, 1/24, -5/24, 1/24, -1/12, 1/24, # RMZ
                    1/24, -1/12, 1/24, -5/24, 1/24, 1/6, 5/12, -5/24, -5/24, -5/24,  1/6, 1/24,# RVH
                    -5/24, 1/24, 1/6, 1/24, -1/12, 1/24,-5/24,5/12,-5/24, 1/6, -5/24, 1/24, # RVM
                    1/6, 1/24, -5/24, 1/6, 1/24, -5/24, -5/24, -5/24, 5/12,1/24, 1/24, -1/12,  # RVZ
                    1/24, 1/24, -1/12, -5/24, 1/6, 1/24, -5/24,  1/6, 1/24, 5/12,-5/24, -5/24, # RZH
                    -5/24, 1/6, 1/24,1/24, 1/24, -1/12, 1/6, -5/24,1/24, -5/24, 5/12, -5/24,  # RZM
                    1/6,  -5/24, 1/24,1/6, -5/24, 1/24, 1/24, 1/24, -1/12, -5/24, -5/24,5/12  # RZV 
  ), 
  nrow=12, ncol=21)
  FSC_anova <- t(FSC_9)
  SRM_effect <- as.data.frame(t(FSC_anova%*%t(as.matrix(data_Cook[,1:12]))))
  names(SRM_effect) <- names(Regression)

    FE_group <- c()
    A.H_group <- c()
    A.Z_group <- c()
    A.M_group <- c()
    A.V_group <- c()
    P.M_group <- c()
    P.V_group <- c()
    P.H_group <- c()
    P.Z_group <- c()
    RTM_group <- c()
    RTF_group <- c()
    RTS_group <- c()
    RMT_group <- c()
    RMF_group <- c()
    RMS_group <- c()
    RFM_group <- c()
    RFT_group <- c()
    RFS_group <- c()
    RSM_group <- c()
    RSF_group <- c()
    RST_group <- c()
    
    for (k in 1:length(data_Cook_rep$TM)){
      FE_group[k]    <- rowMeans(data_Cook_rep[k,1:12])
      A.M_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$MT + data_Cook_rep[k,]$MF + data_Cook_rep[k,]$MS)/3 +
        3/(4*2)*(data_Cook_rep[k,]$FM + data_Cook_rep[k,]$TM + data_Cook_rep[k,]$SM)/3 - 3/2 * FE_group[k]
      A.V_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$FM + data_Cook_rep[k,]$FS + data_Cook_rep[k,]$FT)/3 +
        3/(4*2)*(data_Cook_rep[k,]$MF + data_Cook_rep[k,]$SF + data_Cook_rep[k,]$TF)/3 - 3/2 * FE_group[k]
      A.Z_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$SM + data_Cook_rep[k,]$SF +
                                     data_Cook_rep[k,]$ST)/3 +
        3/(4*2)*(data_Cook_rep[k,]$MS + data_Cook_rep[k,]$FS +
                   data_Cook_rep[k,]$TS)/3 - 3/2 * FE_group[k]
      A.H_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$TM + data_Cook_rep[k,]$TF +
                                     data_Cook_rep[k,]$TS)/3 +
        3/(4*2)*(data_Cook_rep[k,]$MT + data_Cook_rep[k,]$FT +
                   data_Cook_rep[k,]$ST)/3 - 3/2 * FE_group[k]
      P.M_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$FM + data_Cook_rep[k,]$SM + data_Cook_rep[k,]$TM)/3 +
        3/(4*2)*(data_Cook_rep[k,]$MF + data_Cook_rep[k,]$MS + data_Cook_rep[k,]$MT)/3 +
        - 3/2 * FE_group[k]
      P.V_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$MF + data_Cook_rep[k,]$SF + data_Cook_rep[k,]$TF)/3 +
        3/(4*2)*(data_Cook_rep[k,]$FM + data_Cook_rep[k,]$FS + data_Cook_rep[k,]$FT)/3 +
        - 3/2 * FE_group[k]
      P.Z_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$MS + data_Cook_rep[k,]$FS + data_Cook_rep[k,]$TS)/3 +
        3/(4*2)*(data_Cook_rep[k,]$SM + data_Cook_rep[k,]$SF + data_Cook_rep[k,]$ST)/3 +
        - 3/2 * FE_group[k]
      P.H_group[k] <- 3 * 3/(4*2)*(data_Cook_rep[k,]$MT + data_Cook_rep[k,]$FT + data_Cook_rep[k,]$ST)/3  +
        3/(4*2)*(data_Cook_rep[k,]$TM + data_Cook_rep[k,]$TF + data_Cook_rep[k,]$TS)/3 +
        - 3/2 * FE_group[k]
      
      RTM_group[k] <- data_Cook_rep[k,]$TM - (A.H_group[k] + P.M_group[k] + FE_group[k])
      RTF_group[k] <- data_Cook_rep[k,]$TF - (A.H_group[k] + P.V_group[k] + FE_group[k])
      RTS_group[k] <- data_Cook_rep[k,]$TS - (A.H_group[k] + P.Z_group[k] + FE_group[k])
      RMT_group[k] <- data_Cook_rep[k,]$MT - (A.M_group[k] + P.H_group[k] + FE_group[k])
      RMF_group[k] <- data_Cook_rep[k,]$MF - (A.M_group[k] + P.V_group[k] + FE_group[k])
      RMS_group[k] <- data_Cook_rep[k,]$MS - (A.M_group[k] + P.Z_group[k] + FE_group[k])
      RFM_group[k] <- data_Cook_rep[k,]$FM - (A.V_group[k] + P.M_group[k] + FE_group[k])
      RFT_group[k] <- data_Cook_rep[k,]$FT - (A.V_group[k] + P.H_group[k] + FE_group[k])
      RFS_group[k] <- data_Cook_rep[k,]$FS - (A.V_group[k] + P.Z_group[k] + FE_group[k])
      RSM_group[k] <- data_Cook_rep[k,]$SM - (A.Z_group[k] + P.M_group[k] + FE_group[k])
      RSF_group[k] <- data_Cook_rep[k,]$SF - (A.Z_group[k] + P.V_group[k] + FE_group[k])
      RST_group[k] <- data_Cook_rep[k,]$ST - (A.Z_group[k] + P.H_group[k] + FE_group[k])
    }

  # variances of latent variables
  var_FE <- eff1[eff1$f=="FE1 ~~ FE1",]$est
  var_AH <- eff1[eff1$f=="A.H1 ~~ A.H1",]$est
  var_AV <- eff1[eff1$f=="A.V1 ~~ A.V1",]$est
  var_AM <- eff1[eff1$f=="A.M1 ~~ A.M1",]$est
  var_AZ <- eff1[eff1$f=="A.Z1 ~~ A.Z1",]$est
  var_PH <- eff1[eff1$f=="P.H1 ~~ P.H1",]$est
  var_PV <- eff1[eff1$f=="P.V1 ~~ P.V1",]$est
  var_PM <- eff1[eff1$f=="P.M1 ~~ P.M1",]$est
  var_PZ <- eff1[eff1$f=="P.Z1 ~~ P.Z1",]$est
  var_RTM <- eff1[eff1$f=="RTM ~~ RTM",]$est
  var_RTF <- eff1[eff1$f=="RTF ~~ RTF",]$est
  var_RTS <- eff1[eff1$f=="RTS ~~ RTS",]$est
  var_RMT <- eff1[eff1$f=="RMT ~~ RMT",]$est
  var_RMF <- eff1[eff1$f=="RMF ~~ RMF",]$est
  var_RMS <- eff1[eff1$f=="RMS ~~ RMS",]$est
  var_RFT <- eff1[eff1$f=="RFT ~~ RFT",]$est
  var_RFM <- eff1[eff1$f=="RFM ~~ RFM",]$est
  var_RFS <- eff1[eff1$f=="RFS ~~ RFS",]$est
  var_RST <- eff1[eff1$f=="RST ~~ RST",]$est
  var_RSM <- eff1[eff1$f=="RSM ~~ RSM",]$est
  var_RSF <- eff1[eff1$f=="RSF ~~ RSF",]$est
  
  
  # MEANS ANOVA SCORES
  mean_FE_a[i] <- mean(FE_group) - mean_FE 
  mean_AM_a[i] <- mean(A.M_group) - mean_AM
  mean_AV_a[i] <- mean(A.V_group) - mean_AV
  mean_AZ_a[i] <- mean(A.Z_group) - mean_AZ
  mean_AH_a[i] <- mean(A.H_group) - mean_AH
  mean_PM_a[i] <- mean(P.M_group) - mean_PM
  mean_PV_a[i] <- mean(P.V_group) - mean_PV
  mean_PZ_a[i] <- mean(P.Z_group) - mean_PZ
  mean_PH_a[i] <- mean(P.H_group) - mean_PH
  
  mean_RTM_a[i] <- mean(RTM_group) - mean_RTM
  mean_RTF_a[i] <- mean(RTF_group) - mean_RTF
  mean_RTS_a[i] <- mean(RTS_group) - mean_RTS
  mean_RMT_a[i] <- mean(RMT_group) - mean_RMT
  mean_RMS_a[i] <- mean(RMS_group) - mean_RMS
  mean_RMF_a[i] <- mean(RMF_group) - mean_RMF
  mean_RFT_a[i] <- mean(RFT_group) - mean_RFT
  mean_RFS_a[i] <- mean(RFS_group) - mean_RFS
  mean_RFM_a[i] <- mean(RFM_group) - mean_RFM
  mean_RST_a[i] <- mean(RST_group) - mean_RST
  mean_RSF_a[i] <- mean(RSF_group) - mean_RSF
  mean_RSM_a[i] <- mean(RSM_group) - mean_RSM
  
  # ------ NAIVE METHOD ------ #
  # Z-scores ANOVA factorscores & p-values
  z_anoFE <- (FE - (mean_FE))/sqrt(var_FE)
  p_anoFE[i] <- 2*pnorm(-abs(z_anoFE))
  
  z_anoAM <- (A.M - (mean_AM))/sqrt(var_AM)
  p_anoAM[i] <- 2*pnorm(-abs(z_anoAM))
  z_anoAV <- (A.V - (mean_AV))/sqrt(var_AV)
  p_anoAV[i] <- 2*pnorm(-abs(z_anoAV))
  z_anoAZ <- (A.Z - (mean_AZ))/sqrt(var_AZ)
  p_anoAZ[i] <- 2*pnorm(-abs(z_anoAZ))
  z_anoAH <- (A.H - (mean_AH))/sqrt(var_AH)
  p_anoAH[i] <- 2*pnorm(-abs(z_anoAH))
  
  z_anoPM <- (P.M - (mean_PM))/sqrt(var_PM)
  p_anoPM[i] <- 2*pnorm(-abs(z_anoPM))
  z_anoPV <- (P.V - (mean_PV))/sqrt(var_PV)
  p_anoPV[i] <- 2*pnorm(-abs(z_anoPV))
  z_anoPZ <- (P.Z - (mean_PZ))/sqrt(var_PZ)
  p_anoPZ[i] <- 2*pnorm(-abs(z_anoPZ))
  z_anoPH <- (P.H - (mean_PH))/sqrt(var_PH)
  p_anoPH[i] <- 2*pnorm(-abs(z_anoPH))
  
  z_anoRTM <- (RTM - mean_RTM)/sqrt(var_RTM)
  p_anoRTM[i] <- 2*pnorm(-abs(z_anoRTM))
  z_anoRTF <- (RTF - mean_RTF)/sqrt(var_RTF)
  p_anoRTF[i] <- 2*pnorm(-abs(z_anoRTF))
  z_anoRTS <- (RTS - mean_RTS)/sqrt(var_RTS)
  p_anoRTS[i] <- 2*pnorm(-abs(z_anoRTS))
  
  z_anoRMT <- (RMT - mean_RMT)/sqrt(var_RMT)
  p_anoRMT[i] <- 2*pnorm(-abs(z_anoRMT))
  z_anoRMS <- (RMS - mean_RMS)/sqrt(var_RMS)
  p_anoRMS[i] <- 2*pnorm(-abs(z_anoRMS))
  z_anoRMF <- (RMF - mean_RMF)/sqrt(var_RMF)
  p_anoRMF[i] <- 2*pnorm(-abs(z_anoRMF))
  
  z_anoRFM <- (RFM - mean_RFM)/sqrt(var_RFM)
  p_anoRFM[i] <- 2*pnorm(-abs(z_anoRFM))
  z_anoRFS <- (RFS - mean_RFS)/sqrt(var_RFS)
  p_anoRFS[i] <- 2*pnorm(-abs(z_anoRFS))
  z_anoRFT <- (RFT - mean_RFT)/sqrt(var_RFT)
  p_anoRFT[i] <- 2*pnorm(-abs(z_anoRFT))
  
  z_anoRSM <- (RSM - mean_RSM)/sqrt(var_RSM)
  p_anoRSM[i] <- 2*pnorm(-abs(z_anoRSM))
  z_anoRST <- (RST - mean_RST)/sqrt(var_RST)
  p_anoRST[i] <- 2*pnorm(-abs(z_anoRST))
  z_anoRSF <- (RSF - mean_RSF)/sqrt(var_RSF)
  p_anoRSF[i] <- 2*pnorm(-abs(z_anoRSF))
  
  # Same conclusion?
  con_anoFE[i] <- ifelse(p_anoFE[i] < 0.05,0,1)
  con_anoAM[i] <- ifelse(p_anoAM[i] < 0.05,0,1)
  con_anoAV[i] <- ifelse(p_anoAV[i] < 0.05,0,1)
  con_anoAZ[i] <- ifelse(p_anoAZ[i] < 0.05,0,1)
  con_anoAH[i] <- ifelse(p_anoAH[i] < 0.05,0,1)
  con_anoPM[i] <- ifelse(p_anoPM[i] < 0.05,0,1)
  con_anoPV[i] <- ifelse(p_anoPV[i] < 0.05,0,1)
  con_anoPZ[i] <- ifelse(p_anoPZ[i] < 0.05,0,1)
  con_anoPH[i] <- ifelse(p_anoPH[i] < 0.05,0,1)
  con_anoRTM[i] <- ifelse(p_anoRTM[i] < 0.05, 0, 1)
  con_anoRTF[i] <- ifelse(p_anoRTF[i] < 0.05, 0, 1)
  con_anoRTS[i] <- ifelse(p_anoRTS[i] < 0.05, 0, 1)
  con_anoRMT[i] <- ifelse(p_anoRMT[i] < 0.05, 0, 1)
  con_anoRMF[i] <- ifelse(p_anoRMF[i] < 0.05, 0, 1)
  con_anoRMS[i] <- ifelse(p_anoRMS[i] < 0.05, 0, 1)
  con_anoRFT[i] <- ifelse(p_anoRFT[i] < 0.05, 0, 1)
  con_anoRFM[i] <- ifelse(p_anoRFM[i] < 0.05, 0, 1)
  con_anoRFS[i] <- ifelse(p_anoRFS[i] < 0.05, 0, 1)
  con_anoRSM[i] <- ifelse(p_anoRSM[i] < 0.05, 0, 1)
  con_anoRSF[i] <- ifelse(p_anoRSF[i] < 0.05, 0, 1)
  con_anoRST[i] <- ifelse(p_anoRST[i] < 0.05, 0, 1)
  
  # ------ NEW METHOD ------ #
  S2 <- t(FSC_9)%*%(LAMBDA%*%VETA%*%t(LAMBDA) + THETA)%*%FSC_9
  
  z_anonewFE <- (FE - (mean_FE))/sqrt(S2[1,1])
  p_anonewFE[i] <- 2*pnorm(-abs(z_anonewFE))
  
  z_anonewAM <- (A.M - (mean_AM))/sqrt(S2[3,3])
  p_anonewAM[i] <- 2*pnorm(-abs(z_anonewAM))
  z_anonewAV <- (A.V - (mean_AV))/sqrt(S2[4,4])
  p_anonewAV[i] <- 2*pnorm(-abs(z_anonewAV))
  z_anonewAZ <- (A.Z - (mean_AZ))/sqrt(S2[5,5])
  p_anonewAZ[i] <- 2*pnorm(-abs(z_anonewAZ))
  z_anonewAH <- (A.H - (mean_AH))/sqrt(S2[2,2])
  p_anonewAH[i] <- 2*pnorm(-abs(z_anonewAH))
  
  z_anonewPM <- (P.M - (mean_PM))/sqrt(S2[7,7])
  p_anonewPM[i] <- 2*pnorm(-abs(z_anonewPM))
  z_anonewPV <- (P.V - (mean_PV))/sqrt(S2[8,8])
  p_anonewPV[i] <- 2*pnorm(-abs(z_anonewPV))
  z_anonewPZ <- (P.Z - (mean_PZ))/sqrt(S2[9,9])
  p_anonewPZ[i] <- 2*pnorm(-abs(z_anonewPZ))
  z_anonewPH <- (P.H - (mean_PH))/sqrt(S2[6,6])
  p_anonewPH[i] <- 2*pnorm(-abs(z_anonewPH))
  
  z_anonewRTM <- (RTM - mean_RTM)/sqrt(S2[10,10])
  p_anonewRTM[i] <- 2*pnorm(-abs(z_anonewRTM))
  z_anonewRTF <- (RTF - mean_RTF)/sqrt(S2[11,11])
  p_anonewRTF[i] <- 2*pnorm(-abs(z_anonewRTF))
  z_anonewRTS <- (RTS - mean_RTS)/sqrt(S2[12,12])
  p_anonewRTS[i] <- 2*pnorm(-abs(z_anonewRTS))
  
  z_anonewRMT <- (RMT - mean_RMT)/sqrt(S2[13,13])
  p_anonewRMT[i] <- 2*pnorm(-abs(z_anonewRMT))
  z_anonewRMF <- (RMF - mean_RMF)/sqrt(S2[14,14])
  p_anonewRMF[i] <- 2*pnorm(-abs(z_anonewRMF))
  z_anonewRMS <- (RMS - mean_RMS)/sqrt(S2[15,15])
  p_anonewRMS[i] <- 2*pnorm(-abs(z_anonewRMS))

  z_anonewRFM <- (RFM - mean_RFM)/sqrt(S2[17,17])
  p_anonewRFM[i] <- 2*pnorm(-abs(z_anonewRFM))
  z_anonewRFT <- (RFT - mean_RFT)/sqrt(S2[16,16])
  p_anonewRFT[i] <- 2*pnorm(-abs(z_anonewRFT))
  z_anonewRFS <- (RFS - mean_RFS)/sqrt(S2[18,18])
  p_anonewRFS[i] <- 2*pnorm(-abs(z_anonewRFS))
  
  z_anonewRSM <- (RSM - mean_RSM)/sqrt(S2[20,20])
  p_anonewRSM[i] <- 2*pnorm(-abs(z_anonewRSM))
  z_anonewRST <- (RST - mean_RST)/sqrt(S2[19,19])
  p_anonewRST[i] <- 2*pnorm(-abs(z_anonewRST))
  z_anonewRSF <- (RSF - mean_RSF)/sqrt(S2[21,21])
  p_anonewRSF[i] <- 2*pnorm(-abs(z_anonewRSF))
  
  con_anonewFE[i] <- ifelse(p_anonewFE[i] < 0.05,0,1)
  con_anonewAM[i] <- ifelse(p_anonewAM[i] < 0.05,0,1)
  con_anonewAV[i] <- ifelse(p_anonewAV[i] < 0.05,0,1)
  con_anonewAZ[i] <- ifelse(p_anonewAZ[i] < 0.05,0,1)
  con_anonewAH[i] <- ifelse(p_anonewAH[i] < 0.05,0,1)
  con_anonewPM[i] <- ifelse(p_anonewPM[i] < 0.05,0,1)
  con_anonewPV[i] <- ifelse(p_anonewPV[i] < 0.05,0,1)
  con_anonewPZ[i] <- ifelse(p_anonewPZ[i] < 0.05,0,1)
  con_anonewPH[i] <- ifelse(p_anonewPH[i] < 0.05,0,1)
  con_anonewRTM[i] <- ifelse(p_anonewRTM[i] < 0.05, 0, 1)
  con_anonewRTF[i] <- ifelse(p_anonewRTF[i] < 0.05, 0, 1)
  con_anonewRTS[i] <- ifelse(p_anonewRTS[i] < 0.05, 0, 1)
  con_anonewRMT[i] <- ifelse(p_anonewRMT[i] < 0.05, 0, 1)
  con_anonewRMF[i] <- ifelse(p_anonewRMF[i] < 0.05, 0, 1)
  con_anonewRMS[i] <- ifelse(p_anonewRMS[i] < 0.05, 0, 1)
  con_anonewRFT[i] <- ifelse(p_anonewRFT[i] < 0.05, 0, 1)
  con_anonewRFM[i] <- ifelse(p_anonewRFM[i] < 0.05, 0, 1)
  con_anonewRFS[i] <- ifelse(p_anonewRFS[i] < 0.05, 0, 1)
  con_anonewRSM[i] <- ifelse(p_anonewRSM[i] < 0.05, 0, 1)
  con_anonewRSF[i] <- ifelse(p_anonewRSF[i] < 0.05, 0, 1)
  con_anonewRST[i] <- ifelse(p_anonewRST[i] < 0.05, 0, 1)
  
  var_FE_anew[i] <- S2[1,1] - var(SRM_effect$FE1)
  var_AM_anew[i] <- S2[3,3] - var(SRM_effect$A.M1)
  var_AV_anew[i] <- S2[4,4] - var(SRM_effect$A.V1)
  var_AZ_anew[i] <- S2[5,5] - var(SRM_effect$A.Z1)
  var_AH_anew[i] <- S2[2,2] - var(SRM_effect$A.H1)
  var_PM_anew[i] <- S2[7,7] - var(SRM_effect$P.M1)
  var_PV_anew[i] <- S2[8,8] - var(SRM_effect$P.V1)
  var_PZ_anew[i] <- S2[9,9] - var(SRM_effect$P.Z1)
  var_PH_anew[i] <- S2[6,6] - var(SRM_effect$P.H1)
  var_RTM_anew[i] <- S2[10,10] - var(SRM_effect$RTM)
  var_RTF_anew[i] <- S2[11,11] - var(SRM_effect$RTF)
  var_RTS_anew[i] <- S2[12,12] - var(SRM_effect$RTS)
  var_RMT_anew[i] <- S2[13,13] - var(SRM_effect$RMT)
  var_RMF_anew[i] <- S2[14,14] - var(SRM_effect$RMF)
  var_RMS_anew[i] <- S2[15,15] - var(SRM_effect$RMS)
  var_RFT_anew[i] <- S2[16,16] - var(SRM_effect$RFT)
  var_RFM_anew[i] <- S2[17,17] - var(SRM_effect$RFM)
  var_RFS_anew[i] <- S2[18,18] - var(SRM_effect$RFS)
  var_RST_anew[i] <- S2[19,19] - var(SRM_effect$RST)
  var_RSM_anew[i] <- S2[20,20] - var(SRM_effect$RSM)
  var_RSF_anew[i] <- S2[21,21] - var(SRM_effect$RSF)
  
  # ------ ORIGINAL METHOD ------ #
  z_anooldFE <- (FE - mean(FE_group))/sqrt(var(FE_group))
  p_anooldFE[i] <- 2*pnorm(-abs(z_anooldFE))
  
  z_anooldAM <- (A.M - mean(A.M_group))/sqrt(var(A.M_group))
  p_anooldAM[i] <- 2*pnorm(-abs(z_anooldAM))
  z_anooldAV <- (A.V - mean(A.V_group))/sqrt(var(A.V_group))
  p_anooldAV[i] <- 2*pnorm(-abs(z_anooldAV))
  z_anooldAZ <- (A.Z - mean(A.Z_group))/sqrt(var(A.Z_group))
  p_anooldAZ[i] <- 2*pnorm(-abs(z_anooldAZ))
  z_anooldAH <- (A.H - mean(A.H_group))/sqrt(var(A.H_group))
  p_anooldAH[i] <- 2*pnorm(-abs(z_anooldAH))
  
  z_anooldPM <- (P.M - mean(P.M_group))/sqrt(var(P.M_group))
  p_anooldPM[i] <- 2*pnorm(-abs(z_anooldPM))
  z_anooldPV <- (P.V - mean(P.V_group))/sqrt(var(P.V_group))
  p_anooldPV[i] <- 2*pnorm(-abs(z_anooldPV))
  z_anooldPZ <- (P.Z - mean(P.Z_group))/sqrt(var(P.Z_group))
  p_anooldPZ[i] <- 2*pnorm(-abs(z_anooldPZ))
  z_anooldPH <- (P.H - mean(P.H_group))/sqrt(var(P.H_group))
  p_anooldPH[i] <- 2*pnorm(-abs(z_anooldPH))
  
  z_anooldRTM <- (RTM - mean(RTM_group))/sqrt(var(RTM_group))
  p_anooldRTM[i] <- 2*pnorm(-abs(z_anooldRTM))
  z_anooldRTF <- (RTF - mean(RTF_group))/sqrt(var(RTF_group))
  p_anooldRTF[i] <- 2*pnorm(-abs(z_anooldRTF))
  z_anooldRTS <- (RTS - mean(RTS_group))/sqrt(var(RTS_group))
  p_anooldRTS[i] <- 2*pnorm(-abs(z_anooldRTS))
  
  z_anooldRMT <- (RMT - mean(RMT_group))/sqrt(var(RMT_group))
  p_anooldRMT[i] <- 2*pnorm(-abs(z_anooldRMT))
  z_anooldRMF <- (RMF - mean(RMF_group))/sqrt(var(RMF_group))
  p_anooldRMF[i] <- 2*pnorm(-abs(z_anooldRMF))
  z_anooldRMS <- (RMS - mean(RMS_group))/sqrt(var(RMS_group))
  p_anooldRMS[i] <- 2*pnorm(-abs(z_anooldRMS))
  
  z_anooldRFM <- (RFM - mean(RFM_group))/sqrt(var(RFM_group))
  p_anooldRFM[i] <- 2*pnorm(-abs(z_anooldRFM))
  z_anooldRFT <- (RFT - mean(RFT_group))/sqrt(var(RFT_group))
  p_anooldRFT[i] <- 2*pnorm(-abs(z_anooldRFT))
  z_anooldRFS <- (RFS - mean(RFS_group))/sqrt(var(RFS_group))
  p_anooldRFS[i] <- 2*pnorm(-abs(z_anooldRFS))
  
  z_anooldRSM <- (RSM - mean(RSM_group))/sqrt(var(RSM_group))
  p_anooldRSM[i] <- 2*pnorm(-abs(z_anooldRSM))
  z_anooldRST <- (RST - mean(RST_group))/sqrt(var(RST_group))
  p_anooldRST[i] <- 2*pnorm(-abs(z_anooldRST))
  z_anooldRSF <- (RSF - mean(RSF_group))/sqrt(var(RSF_group))
  p_anooldRSF[i] <- 2*pnorm(-abs(z_anooldRSF))
  
  con_anooldFE[i] <- ifelse(p_anooldFE[i] < 0.05,0,1)
  con_anooldAM[i] <- ifelse(p_anooldAM[i] < 0.05,0,1)
  con_anooldAV[i] <- ifelse(p_anooldAV[i] < 0.05,0,1)
  con_anooldAZ[i] <- ifelse(p_anooldAZ[i] < 0.05,0,1)
  con_anooldAH[i] <- ifelse(p_anooldAH[i] < 0.05,0,1)
  con_anooldPM[i] <- ifelse(p_anooldPM[i] < 0.05,0,1)
  con_anooldPV[i] <- ifelse(p_anooldPV[i] < 0.05,0,1)
  con_anooldPZ[i] <- ifelse(p_anooldPZ[i] < 0.05,0,1)
  con_anooldPH[i] <- ifelse(p_anooldPH[i] < 0.05,0,1)
  con_anooldRTM[i] <- ifelse(p_anooldRTM[i] < 0.05, 0, 1)
  con_anooldRTF[i] <- ifelse(p_anooldRTF[i] < 0.05, 0, 1)
  con_anooldRTS[i] <- ifelse(p_anooldRTS[i] < 0.05, 0, 1)
  con_anooldRMT[i] <- ifelse(p_anooldRMT[i] < 0.05, 0, 1)
  con_anooldRMF[i] <- ifelse(p_anooldRMF[i] < 0.05, 0, 1)
  con_anooldRMS[i] <- ifelse(p_anooldRMS[i] < 0.05, 0, 1)
  con_anooldRFT[i] <- ifelse(p_anooldRFT[i] < 0.05, 0, 1)
  con_anooldRFM[i] <- ifelse(p_anooldRFM[i] < 0.05, 0, 1)
  con_anooldRFS[i] <- ifelse(p_anooldRFS[i] < 0.05, 0, 1)
  con_anooldRSM[i] <- ifelse(p_anooldRSM[i] < 0.05, 0, 1)
  con_anooldRSF[i] <- ifelse(p_anooldRSF[i] < 0.05, 0, 1)
  con_anooldRST[i] <- ifelse(p_anooldRST[i] < 0.05, 0, 1)  

  print(i)
}

# ACCEPTABLE VALUES
# 0.95 + 1.96*sqrt((0.95*0.05)/1000)
# 0.9635084
# 0.95 - 1.96*sqrt((0.95*0.05)/1000)
# 0.9364916


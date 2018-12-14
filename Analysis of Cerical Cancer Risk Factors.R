#Data Import
library(readr)
MasterFile <- read_csv("GitHub/Independent Project/risk_factors_cervical_cancer_Tidy.csv")

#Load library needed
library(ggplot2)
library(ggpubr)
library(cowplot)
library(dplyr)
library(ggbeeswarm)
library(corrplot)

#Numeric summary
summary(MasterFile)

#initialize variants with proper names


#Misc variables
#Age and Data Visulization, numeric
age <- c(MasterFile$Age)
beeswarm::beeswarm(age)
boxplot(age)
numSexPart <- c(MasterFile$`Number of sexual partners`)
firstIntercourse <- c(MasterFile$`First sexual intercourse`)
#Pregnancies and Data Visulization, discrete
Pregnancies <- c(MasterFile$`Num of pregnancies`)
beeswarm::beeswarm(Pregnancies)
boxplot(Pregnancies)

yearSmoke <- c(MasterFile$`Smokes (years)`)
binarySmoke <- c(MasterFile$Smokes)
packSmoke <- c(MasterFile$`Smokes (packs/year)`)
binaryPills <- c(MasterFile$`Hormonal Contraceptives`)
yearPills <- c(MasterFile$`Hormonal Contraceptives (years)`)
#All Cervical Cancer Diagnosis
Cancer <- c(MasterFile$`Dx:Cancer`)
#Cancer Subtypes
CancerH <- c(MasterFile$`Hinselmann`)
CancerS <- c(MasterFile$`Schiller`)
CancerC <- c(MasterFile$`Citology`)
CancerB <- c(MasterFile$`Biopsy`)

#IUDs
isIUD <- c(MasterFile$IUD)
IUDyears <- c(MasterFile$`IUD (years)`)

#All stds
isSTD <- c(MasterFile$STDs)
numSTD <- c(MasterFile$`STDs (number)`)

#all condylomatosis
STDallCo <- c(MasterFile$`STDs:condylomatosis`)
STDCoc <- c(MasterFile$`STDs:cervical condylomatosis`)
STDCov <- c(MasterFile$`STDs:vaginal condylomatosis`)
STDCovp <- c(MasterFile$`STDs:vulvo-perineal condylomatosis`)
#other STDs
STDsyp <- c(MasterFile$`STDs:syphilis`)
STDpid <- c(MasterFile$`STDs:pelvic inflammatory disease`)
STDher <- c(MasterFile$`STDs:genital herpes`)
STDmc <- c(MasterFile$`STDs:molluscum contagiosum`)
STDaid <- c(MasterFile$`STDs:AIDS`)
STDhiv <- c(MasterFile$`STDs:HIV`)
STDhb <- c(MasterFile$`STDs:Hepatitis B`)
STDhpv <- c(MasterFile$`STDs:HPV`)
#diagnosis
DxHPV <- c(MasterFile$`Dx:HPV`)
DxCIN <- c(MasterFile$`Dx:CIN`)

#identify target variables for analysis.
#allCorrs <- round(cor(MasterFile, use='pairwise'), 1)
#corrplot(allCorrs)
#to exclude the AID diagnosis and cervical condylomatosis since it's all NAs
MasterFile$`STDs:AIDS` <- NULL
MasterFile$`STDs:cervical condylomatosis` <- NULL
View(MasterFile)
allCorrs <- round(cor(MasterFile, use='pairwise'),1)
corrplot(allCorrs)
View(allCorrs)
#correlationMatrix <- data.frame(allCorrs)
write.csv(allCorrs, 'Correlational Matrix.csv')

#narrowed down to variables: Main effector: HPV
lmHPVCancer <- glm(Cancer~DxHPV, family = binomial)
summary(lmHPVCancer)
#plot(lmHPVCancer)

ggplot(MasterFile, aes(x = DxHPV, y = Cancer))+ 
         geom_point() +
         stat_smooth(method = "glm",
                     method.args = list(family = "binomial"),
                     col = "red")
#IUD
lmIUDCancer <- glm(Cancer~isIUD, family = binomial)
summary(lmIUDCancer)
ggplot(MasterFile, aes(x = isIUD, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
#pregnancies: NS
lmPregCancer <- glm(Cancer~Pregnancies, family = binomial)
summary(lmPregCancer)
ggplot(MasterFile, aes(x = Pregnancies, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
#Smoking NS
lmSmokeCancer <- glm(Cancer~binarySmoke, family = binomial)
summary(lmSmokeCancer)
ggplot(MasterFile, aes(x = binarySmoke, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
#HIV NS
lmHIVCancer <- glm(Cancer~STDhiv, family = binomial)
summary(lmHIVCancer)
ggplot(MasterFile, aes(x = STDhiv, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
#Pill use
lmPillCancer <- glm(Cancer~binaryPills, family = binomial)
summary(lmPillCancer)
ggplot(MasterFile, aes(x = binaryPills, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
#all STDs NS
lmSTDCancer <- glm(Cancer~binarySTD, family = binomial)
summary(lmSTDCancer)
ggplot(MasterFile, aes(x = binarySTD, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")

#adding covariant

#HPV+IUD NS
lmHPVIUDCancer <- glm(Cancer~DxHPV+isIUD, family = binomial)
summary(lmHPVIUDCancer)
ggplot(MasterFile, aes(x = DxHPV+isIUD, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
#HPV+HIV NS
lmHPVHIVCancer <- glm(Cancer~DxHPV+STDhiv, family = binomial)
summary(lmHPVHIVCancer)
ggplot(MasterFile, aes(x = DxHPV+STDhiv, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")

#For discrete analysis: Pearson
# #STD
lmnumSTDCancer <- lm(Cancer~numSTD)
summary(lmnumSTDCancer)
ggplot(MasterFile, aes(x = numSTD, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "lm",
              #method.args = list(family = "binomial"),
              col = "red")
# #sex partners
lmnumPartnerCancer <- lm(Cancer~numSexPart)
summary(lmnumPartnerCancer)
ggplot(MasterFile, aes(x = numSexPart, y = Cancer))+ 
  geom_point() +
  stat_smooth(method = "lm",
              #method.args = list(family = "binomial"),
              col = "red")

#for Cancer Subtype analysis: HPV
lmHPVCancerH <- glm(CancerH~DxHPV, family = binomial)
lmHPVCancerS <- glm(CancerS~DxHPV, family = binomial)
lmHPVCancerC <- glm(CancerC~DxHPV, family = binomial)
lmHPVCancerB <- glm(CancerB~DxHPV, family = binomial)
summary(lmHPVCancerH)
summary(lmHPVCancerS)
summary(lmHPVCancerC)
summary(lmHPVCancerB)

par(mfrow=c(2,2))
ggplot(MasterFile, aes(x = DxHPV, y = CancerH))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
ggplot(MasterFile, aes(x = DxHPV, y = CancerS))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
ggplot(MasterFile, aes(x = DxHPV, y = CancerC))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
ggplot(MasterFile, aes(x = DxHPV, y = CancerB))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")

#for Cancer Subtype analysis: IUD
lmIUDCancerH <- glm(CancerH~isIUD, family = binomial)
lmIUDCancerS <- glm(CancerS~isIUD, family = binomial)
lmIUDCancerC <- glm(CancerC~isIUD, family = binomial)
lmIUDCancerB <- glm(CancerB~isIUD, family = binomial)

summary(lmIUDCancerH)
summary(lmIUDCancerS)
summary(lmIUDCancerC)
summary(lmIUDCancerB)

ggplot(MasterFile, aes(x = isIUD, y = CancerH))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
ggplot(MasterFile, aes(x = isIUD, y = CancerS))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
ggplot(MasterFile, aes(x = isIUD, y = CancerC))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")
ggplot(MasterFile, aes(x = isIUD, y = CancerB))+ 
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red")

library(readr)
MasterFile <- read_csv("GitHub/Independent Project/risk_factors_cervical_cancer_Tidy.csv")
#head(MasterFile)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(dplyr)
library(ggbeeswarm)
library(tidyr)

install.packages("norm")
install.packages("tidyverse")

summary(glm())

age <- c(MasterFile$Age)
numSexPart <- c(MasterFile$`Number of sexual partners`)
firstIntercourse <- c(MasterFile$`First sexual intercourse`)
Pregnancies <- C(MasterFile$`Num of pregnancies`)
yearSmoke <- c(MasterFile$`Smokes (years)`)
binarySmoke <- c(MasterFile$Smokes)
packSmoke <- c(MasterFile$`Smokes (packs/year)`)
binaryPills <- c(MasterFile$`Hormonal Contraceptives`)
yearPills <- c(MasterFile$`Hormonal Contraceptives (years)`)



summary(glm(Cancer ~ STD, family = "binomial"))
summary(glm(Cancer ~ binarySTD, family = "binomial"))

df <- data.frame(Cancer, STD, binarySTD)
summary(df)
is(df)
ggplot(data = df,
       aes(y = Cancer,
           x = STD)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family ="binomial"))



#All cervical Cancers
Cancer <- c(MasterFile$`Dx:Cancer`)
#Cancer Subtypes
CancerH <- c(MasterFile$`Hinselmann`)
CancerS <- c(MasterFile$`Schiller`)
CancerC <- c(MasterFile$`Citology`)
CancerB <- c(MasterFile$`Biopsy`)

#IUDs
binaryIUD <- c(MasterFile$IUD)
IUD <- c(MasterFile$`IUD (years)`)

#All stds
binarySTD <- c(MasterFile$STDs)
STD <- c(MasterFile$`STDs (number)`)

#all condylomatosis
STDCo <- c(MasterFile$`STDs:condylomatosis`)
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


#d[is.?(d)] <- "NA"

summary(age)
#plot(age, fill = "lightgray", add = "mean")
#probably bee swarm
beeswarm::beeswarm(age)
boxplot(age)

summary(numSexPart)
plot(numSexPart, fill = "lightgray", add = "mean")
beeswarm::beeswarm(numSexPart)
boxplot(numSexPart)
#gghistogram(Cancer, fill = "lightgray", add = "mean")
summary(Cancer)
#plot(Cancer)

#data.frame(age, IUD)

#summary(binarySTD)
STDSuite <- data.frame(binarySTD, STD, STDCo, STDCoc,STDCov,STDCovp, STDsyp, STDpid, STDaid, STDher, STDmc, STDhiv, STDhb, STDhpv)
summary(STDSuite)
STDSuite <- tibble::rowid_to_column(STDSuite, "ID")
#STDSuite %>% drop_na(STDSuite)
STDSuiteCleaned <- STDSuite[complete.cases(STDSuite), ]
summary(STDSuiteCleaned)




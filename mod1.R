########## Modelisation logistique du Deficit immunite passive chez les veaux############

###Definition dossier de travail et ouverture des librairies
setwd("C:/Users/Colombine/Desktop/GIMAT/Stage CIRAD/FHCC/TestGitHub/testlundi/repotestR")
library(prettyR)
library(psych)
library(dplyr)
library(mgcv)
library(ResourceSelection)
library(usethis)

### Selection des veaux allaintants dans le tableau de donnees

str(COLOSTRUM2)
C<-COLOSTRUM2
CV<-subset(C,C$GROUPE=="V")

### Definition de la variable reponse DTIP 

CV$DTIP<-ifelse(CV$PLASL_IGG<15,1,0)
table(CV$DTIP,exclude=NULL)


### Nouvelle variable MIGG1 : masse IgG 

CV$MIGG1<-CV$BU1_VOL*CV$COL_IGG


### Simplification de la variable PARITE par la creation d'une nouvelle variable RVELC

class(CV$PARITE)
summary(CV$PARITE)
par(mfrow=c(1,2))
boxplot(CV$PARITE,main="Parite")
hist(CV$PARITE,xlab="Parite des meres",main="Representation de la parit?")
#75% des veaux venant mere de parite de 1 a 4


#CV$RVEL<-case_when(CV$PARITE==1~1,CV$PARITE==2~2,CV$PARITE==3~3,CV$PARITE>=4~4)
#summary(CV$RVEL)

CV$RVELC <-ifelse(CV$PARITE>4,4,CV$PARITE)
table(CV$RVELC)


### Nouveau data.frame sans les valeurs manquantes pour les variables suivantes
### COLMODE,BU1_VOL,COL_IGG,MIGG1,MODNAIS et RVELC 

Complete<-complete.cases(CV[,c(2:4,11,13,14)])
summary(Complete)
CVC<-subset(CV,Complete=="TRUE", )
#184 observations

### Description des variables 

## BU1_VOL

par(mfrow=c(1,3))
class(CVC$BU1_VOL)
describe.numeric(CVC$BU1_VOL)
hist(CVC$BU1_VOL,xlab="Volume",main="Volume")
boxplot(CVC$BU1_VOL,main="Volume")
qqnorm(CVC$BU1_VOL)
qqline(CVC$BU1_VOL)
shapiro.test(CVC$BU1_VOL)

## COL_IGG

class(CVC$COL_IGG)
describe.numeric(CVC$COL_IGG)
hist(CVC$COL_IGG,xlab="concentration",main="Concentration")
boxplot(CVC$COL_IGG,main="Concentration")
qqnorm(CVC$COL_IGG)
qqline(CVC$COL_IGG)
shapiro.test(CVC$COL_IGG)


## MIGG1

class(CVC$MIGG1)
describe.numeric(CVC$MIGG1)
hist(CVC$MIGG1,xlab="masse",main="Masse")
boxplot(CVC$MIGG1)
qqnorm(CVC$MIGG1)
qqline(CVC$MIGG1)
shapiro.test(CVC$MIGG1)

## COLMODE

class(CVC$COLMODE)
par(mfrow=c(1,1))
CVC$COLMODE<-as.factor(CVC$COLMODE)
class(CVC$COLMODE)
describe.factor(CVC$COLMODE)
barplot(table(CVC$COLMODE),main="Effectifs administration colostrum")

## MODNAIS
class(CVC$MODNAIS)
CVC$MODNAIS<-as.factor(CVC$MODNAIS)
class(CVC$MODNAIS)
describe.factor(CVC$MODNAIS)
par(mfrow=c(1,1))
barplot(table(CVC$MODNAIS),xlab = "Modalite de naissance", ylab="Effectif", main ="Effectif par modalite de naissance")

## RVEL

class(CVC$RVELC)
CVC$RVELC<-as.factor(CVC$RVELC)
class(CVC$RVELC)
describe.factor(CVC$RVELC)
par(mfrow=c(1,1))
barplot(table(CVC$RVELC),xlab = "Parite", ylab="Effectif", main ="Effectif par parite")

## DTIP

class(CVC$DTIP)
table(CVC$DTIP)
barplot(table(CVC$DTIP),xlab ="deficit",ylab="Effectif",main="Deficit colostral ")

### Description DTIP selon les variables 

## DTIP f(MODNAIS)

par(mfrow=c(1,1))
Tbr<-table(CVC$DTIP,CVC$MODNAIS)
Tbr
barplot(Tbr,beside=T,legend=levels(unique(CVC$DTIP)),main="Mode de naissance et deficit",xlab="Mode de naissance",ylab="Concentration")
comp<-chisq.test(Tbr)
chisq.test(Tbr)
comp$expected

## DTIP f(RVELC)

par(mfrow=c(1,1))
Tbr<-table(CVC$DTIP,CVC$RVELC)
Tbr
barplot(Tbr,beside=T,legend=levels(unique(CVC$DTIP)),main="Parit? et deficit",xlab="Parite",ylab="effectif")
comp<-chisq.test(Tbr)
chisq.test(Tbr)
comp$expected

## DTIP f(COLMODE)

par(mfrow=c(1,1))
Tbr<-table(CVC$DTIP,CVC$COLMODE)
Tbr
barplot(Tbr,beside=T,legend=levels(unique(CVC$DTIP)),main="Mode administration et deficit",xlab="Mode",ylab="effectif")
comp<-chisq.test(Tbr)
chisq.test(Tbr)
comp$expected

## DTIP f(BU1_VOL)

par(mfrow=c(1,2))
describeBy(CVC$BU1_VOL,CVC$DTIP)
boxplot(CVC$BU1_VOL~CVC$DTIP,xlab="Deficit",ylab="Volume",main="Volume de colostrum et d?ficit")
hist(CVC$BU1_VOL[CVC$DTIP=="0"],xlab="Volume",ylim=c(0,50),col=("grey"),main="Volume")
hist(CVC$BU1_VOL[CVC$DTIP=="1"],xlab="Volume",col=("white"),add=T)
wilcox.test(CVC$BU1_VOL[CVC$DTIP=="1"],CVC$BU1_VOL[CVC$DTIP=="0"]) 

## DTIP f(COL_IGG)

par(mfrow=c(1,2))
boxplot(CVC$COL_IGG~CVC$DTIP,xlab="Deficit",ylab="Concentration",main="Concentration IgG et d?ficit")
plot(density(CVC$COL_IGG[CVC$DTIP=="0"]), col = "red",main="Concentration IgG")
lines(density(CVC$COL_IGG[CVC$DTIP=="1"]), col = "blue")
describeBy(CVC$COL_IGG,CVC$DTIP)

#Representation histogramme
hist(CVC$COL_IGG[CVC$DTIP==0], xlab="Concentration", ylab="Effectif",main="Concentration IgG dans groupe deficit")
hist(CVC$COL_IGG[CVC$DTIP==1], xlab="Concentration", ylab="Effectif",main="Concentration IgG dans groupe deficit",col="green",add=T)

#Normalite 
qqnorm(CVC$COL_IGG[CVC$DTIP==0],main="QQplot Groupe 0")
qqline(CVC$COL_IGG[CVC$DTIP==0])
qqnorm(CVC$COL_IGG[CVC$DTIP==1],main="QQplot Groupe 1")
qqline(CVC$COL_IGG[CVC$DTIP==1])
#Approximation normale
#Egalit? des variances
var.test(CVC$COL_IGG[CVC$DTIP==1],CVC$COL_IGG[CVC$DTIP==0])
#Var in?gale
t.test(CVC$COL_IGG[CVC$DTIP==1],CVC$COL_IGG[CVC$DTIP==0],var.equal = F)

## DTIP f(MIGG1)

xplot(CVC$MIGG1~CVC$DTIP,xlab="Deficit",ylab="MASSE",main="Masse IgG et d?ficit")
plot(density(CVC$MIGG1[CVC$DTIP=="1"]), col = "red",main="Masse IgG")
lines(density(CVC$MIGG1[CVC$DTIP=="0"]), col = "blue")
describeBy(CVC$MIGG1,CVC$DTIP)
var.test(CVC$MIGG1[CVC$DTIP==1],CVC$MIGG1[CVC$DTIP==0])
t.test(CVC$COL_IGG[CVC$DTIP==1],CVC$COL_IGG[CVC$DTIP==0])


### Verification colinearites entre certaines variables quantitatives (graphiquement uniquement)


plot(CVC$MIGG1~CVC$BU1_VOL,main="Masse = f(volume)")
plot(CVC$MIGG1~CVC$COL_IGG,main="Masse = f(concentration)")
plot(CVC$BU1_VOL~CVC$COL_IGG,main="Volume = f(concentration)")
library(lmtest)
harvtest(CVC$BU1_VOL~ CVC$MIGG1)
harvtest(CVC$BU1_VOL~ CVC$COL_IGG)
harvtest(CVC$MIGG1~ CVC$COL_IGG)


### Modele univarie pour chaque variable : selection pour p<0.3

## MIGG1

CVC$DTIP1<-as.numeric(CVC$DTIP)
table(CVC$DTIP1)
UnivMIGG<-glm(formula=CVC$DTIP1~CVC$MIGG1, family=binomial(link="logit"))
summary(UnivMIGG)

## COL_IGG

UnivCI<-glm(formula=CVC$DTIP1~CVC$COL_IGG, family=binomial(link="logit"))
summary(UnivCI)

## COLMODE
UnivCOL<-glm(formula=CVC$DTIP1~CVC$COLMODE, family=binomial(link="logit"))
summary(UnivCOL)
ex1<-glm(DTIP~ COLMODE, data=CVC, family = binomial(link = "logit"))
drop1(ex1, test="LRT")

## MODNAIS

UnivN<-glm(formula=CVC$DTIP1~CVC$MODNAIS, family=binomial(link="logit"))
summary(UnivN)
ex1<-glm(DTIP~ COLMODE, data=CVC, family = binomial(link = "logit"))
drop1(ex1, test="LRT")

## BU1_VOL

UnivBU<-glm(formula=CVC$DTIP1~CVC$BU1_VOL, family=binomial(link="logit"))
summary(UnivBU)

## RVELC

UnivR<-glm(formula=CVC$DTIP1~CVC$RVELC, family=binomial(link="logit"))
summary(UnivR)



### 2 modeles realises : COL_IGG + BU1_VOL ou avec MIGG1 

modele0<-glm(formula=CVC$DTIP1~CVC$RVELC+CVC$BU1_VOL+CVC$COL_IGG+CVC$MODNAIS+CVC$COLMODE, family=binomial(link="logit"))
summary(modele0)
modele0bis<-glm(formula=CVC$DTIP1~CVC$RVELC+CVC$MIGG1+CVC$MODNAIS+CVC$COLMODE, family=binomial(link="logit"))
summary(modele0bis)
#Comparaison des AIC : choix modele0

### Elagage du modele par methode pas a pas descendante : etude sans interaction

modele0<-glm(formula=CVC$DTIP1~CVC$RVELC+CVC$BU1_VOL+CVC$COL_IGG+CVC$MODNAIS+CVC$COLMODE, family=binomial(link="logit"))
summary(modele0)

model01<-update(modele0,~.-CVC$MODNAIS)
summary(model01)
anova(modele0,model01,test="LRT")

model02<-update(model01,~.-CVC$COLMODE)
summary(model02)
anova(model01,model02,test="LRT")

model03<-update(model02,~.-CVC$BU1_VOL)
summary(model03)
anova(model02,model03,test="LRT")
### Etude des interactions

model03i<-glm(formula=CVC$DTIP1~CVC$RVELC*CVC$COL_IGG, family=binomial(link="logit"))
summary(model03i)
anova(model03,model03i,test="LRT")
#Interaction significative



### Verification log-linearite des variables quantitatives continues 

par(mfrow=c(1,1))
model03g<-gam(CVC$DTIP1~CVC$RVELC+s(CVC$COL_IGG),family=binomial)
plot.gam(model03g)
#Lineaire en echelle log

### Test d'adequation 

hoslem.test(CVC$DTIP1,model03$fitted.values,g=10)

### Interpretation des OR

COEF<-coef(model03)
COEF
STD<-summary(model03)$coefficient[,2]
OR<-exp(COEF)
OR
ORinf<-exp(COEF[-1]-1.96*STD[-1])
ORinf
ORsup<-exp(COEF[-1]+1.96*STD[-1])
ORsup



### Changement ordre facteur
CVC$RVELC<-relevel(CVC$RVELC, ref="3")  
levels(CVC$RVELC)
model03b <- glm(CVC$DTIP1 ~ CVC$RVELC+CVC$COL_IGG, family = binomial)
summary(model03b)
COEFb<-model03b$coefficients
STDb<-summary(model03b)$coefficient[,2]
ORb<-exp(COEFb)
ORbinf<-exp(COEFb[-1]-1.96*STDb[-1])
ORbinf
ORbsup<-exp(COEFb[-1]+1.96*STDb[-1])
ORbsup




### Discretisation d'une variable COL_IGG en 4 classes, classe 3 en reference

CVC$COL_IGG1<-ifelse(CVC$COL_IGG<=60,1,0)
CVC$COL_IGG2<-ifelse(60<CVC$COL_IGG & CVC$COL_IGG<=100,1,0)
CVC$COL_IGG3<-ifelse(100<CVC$COL_IGG & CVC$COL_IGG<=150,1,0)
CVC$COL_IGG4<-ifelse(CVC$COL_IGG>150,1,0)
table(CVC$COL_IGG1)
table(CVC$COL_IGG2)
table(CVC$COL_IGG3)
table(CVC$COL_IGG4)

model04<-glm(CVC$DTIP1~CVC$COL_IGG1+CVC$COL_IGG2+CVC$COL_IGG4+CVC$RVELC,family = binomial)
summary(model04)

COEFc<-model04$coefficients
ORc<-exp(COEFc)
STDc<-summary(model04)$coefficient[,2]
ORcinf<-exp(COEFc[-1]-1.96*STDc[-1])
ORcinf
ORcsup<-exp(COEFc[-1]+1.96*STDc[-1])
ORcsup


librabry(usethis)


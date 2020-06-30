
#install.packages("ratte")
#install.packages("randomForest")

library(data.table)
library(caret)
#library(xgboost)
library(Matrix)
library(iml)
library(jtools)
library(ggstance)
library(mgcv)
library(parallel)
library(snow)
library(doSNOW)
library(dotwhisker)
library(pscl)
library(corrplot)
library(gbm)
library(pdp)
library(MASS)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(randomForest)
library(rattle)
#------------------------------------------------------------------------------------------------------------------------------
# 1. DATA IMPORT
  #a. IMPORT RAW DATA
  #b. TRANSFORM CATEGORICAL VARIABLES INTO FACTORS
#------------------------------------------------------------------------------------------------------------------------------

data <- data.table::fread("C:\\Users\\black\\Google Drive\\Python\\Data Science\\Royal London\\DataSet_Clean.csv", sep = ",") %>%
  mutate(lapse_rate=ifelse(EXPOSURE_CNT==0,0,LAPSE_CNT/EXPOSURE_CNT))%>%
  mutate(GENDER=as.factor(GENDER))%>%
  mutate(FACE_AMOUNT_BAND=as.factor(FACE_AMOUNT_BAND))%>%
  mutate(POST_LEVEL_PREMIUM_STRUCTURE=as.factor(POST_LEVEL_PREMIUM_STRUCTURE))%>%
  mutate(RISK_CLASS=as.factor(RISK_CLASS))%>%
  mutate(Duration=DURATION)%>%                  # "Duration is continuous while DURATION is a factor
  mutate(DURATION=as.factor(DURATION))%>%
  mutate(PREMIUM_MODE=as.factor(PREMIUM_MODE)) %>% 
  mutate(AGE_BAND=as.factor(AGE_BAND)) %>%
  filter(EXPOSURE_CNT > 0)

#------------------------------------------------------------------------------------------------------------------------------
# INTRODUCINGE THE CARET PACKAGE
#------------------------------------------------------------------------------------------------------------------------------
#SPLIT DATA INTO TRAIN AND TEST SET
set.seed(3456)
smp_siz = floor(0.8*nrow(data))
trainIndex = sample(seq_len(nrow(data)),size = smp_siz) 
X_train<-data[trainIndex,]
X_test<-data[-trainIndex,]

#------------------------------------------------------------------------------------------------------------------------------
# POISSON MODEL
#------------------------------------------------------------------------------------------------------------------------------

lapse_poison <- glm(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                    + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE 
                    ,data=X_train, family=poisson(link="log"))

summary(lapse_poison)

(1-lapse_poison$deviance/lapse_poison$null.deviance )


lapse_poison_pred <- tibble(exp(predict(lapse_poison, X_test))) %>% 
  rename(pred=`exp(predict(lapse_poison, X_test))`)
lapse_poison_pred <- cbind(lapse_poison_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)


(lapse_MSE= sum((lapse_poison_pred$error)^2)/nrow(lapse_poison_pred))
#------------------------------------------------------------------------------------------------------------------------------
# DECISION TREE
#------------------------------------------------------------------------------------------------------------------------------

lapse_cart <- rpart(lapse_rate ~   GENDER + AGE_BAND + FACE_AMOUNT_BAND + DURATION
               + POST_LEVEL_PREMIUM_STRUCTURE  +  RISK_CLASS + PREMIUM_MODE  +PREM_JUMP_D11_D10,
               weights = X_train$EXPOSURE_CNT,
               data=X_train, method = 'poisson', control=rpart.control(maxdepth = 4, cp=0.005))

lapse_cart

(sum(X_train$LAPSE_CNT)/sum(X_train$EXPOSURE_CNT))
rpart.plot(lapse_cart)



lapse_cart_pred <- tibble(predict(lapse_cart, X_test)) %>% 
  rename(pred=`predict(lapse_cart, X_test)`) %>% 
  mutate(pred=pred*X_test$EXPOSURE_CNT)
lapse_cart_pred <- cbind(lapse_cart_pred,X_test[,c("EXPOSURE_CNT","LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)


(lapse_cart_MSE= sum((lapse_cart_pred$error)^2)/nrow(lapse_cart_pred))

#The best so far!!!!!!


#lets compare to the traditional lapse approach


lapse_cartrate_pred <- tibble(predict(lapse_cart, X_test)) %>% 
rename(cart_pred=`predict(lapse_cart, X_test)`)
lapse_cartrate_pred <- cbind(lapse_cartrate_pred,X_test[,c("FACE_AMOUNT_BAND","DURATION","EXPOSURE_CNT","LAPSE_CNT")]) 

duration_gen_SA_plot <- summarize(group_by(data,DURATION,FACE_AMOUNT_BAND ), EXPOSURE=sum(EXPOSURE_CNT),
                  LAPSE=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE/EXPOSURE)


# # Merge by columns with different names
z <- merge(lapse_cartrate_pred, duration_gen_SA_plot, by.x = c( "DURATION", "FACE_AMOUNT_BAND"), by.y =c( "DURATION", "FACE_AMOUNT_BAND"))

z <- mutate(z,trad_pred=EXPOSURE_CNT*lapse_rate ) %>% 
  mutate(error=trad_pred-LAPSE_CNT)

(lapse_trad_count_MSE= sum((z$error)^2)/nrow(z))

#trditinal approach performs the worst



#------------------------------------------------------------------------------------------------------------------------------
# GBM
#------------------------------------------------------------------------------------------------------------------------------


lapse_gbm <-  gbm(formula = LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ Duration + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                  + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE 
                  ,data=X_train,  distribution ="poisson", n.trees = 160, verbose=TRUE, interaction.depth = 2)


summary(lapse_gbm)

#plot predictions

lapse_gbm_pred <- tibble(predict(lapse_gbm, newdata = X_test, type = "response", n.trees=160)*X_test[,"EXPOSURE_CNT"]) %>% 
  rename(pred=`*...` )
lapse_gbm_pred <- cbind(lapse_gbm_pred,X_test[,"LAPSE_CNT"])%>% 
  rename(count=`X_test[, "LAPSE_CNT"]`  ) %>% 
  mutate(error=pred-count)

#plot(lapse_gbm_pred$pred, lapse_gbm_pred$count)
(lapse_gbm_MSE= sum((lapse_gbm_pred$error)^2)/nrow(lapse_gbm_pred))



par(mar = c(5, 8, 1, 1))
summary(
  lapse_gbm, 
  cBars = 15,
  method = relative.influence, 
  las = 2
)


#Plot of Response variable
plot(lapse_gbm,i="Duration") 
#Inverse relation with lstat variable

plot(lapse_gbm,i="PREM_JUMP_D11_D10") 
#as the average number of rooms increases the the price increases


#------------------------------------------------------------------------------------------------------------------------------
# RL DATA
#------------------------------------------------------------------------------------------------------------------------------


data <- data.table::fread("C:\\Users\\black\\Google Drive\\Python\\Data Science\\Royal London\\Training\\Data\\Protection_Lapse_Deaths_1.csv", sep = ",") %>%
  mutate(target=ENDSTATE/EXPOSURE)%>%
  mutate(STARTDATE=as.Date(STARTDATE,"%m/%d/%y"))%>%
  mutate(ENDDATE=as.Date(ENDDATE,"%m/%d/%y"))%>%
  mutate(DOB1=as.Date(DOB1,"%m/%d/%y"))%>%
  mutate(DOB2=as.Date(DOB2,"%m/%d/%y"))%>% 
  mutate(DURATION=EXPOSURE)%>% 
  mutate(Sex1=as.factor(Sex1))%>%
  mutate(LAPSE=as.factor(ENDSTATE))%>%
  mutate(JL_Status=as.factor(JL_Status))%>%
  mutate(Smoker=as.factor(Smoker))%>%
  mutate(Benefit_Type=as.factor(Benefit_Type)) %>% 
  mutate(CI_Cover=as.factor(CI_Cover)) %>% 
  mutate(CI_Waiver=as.factor(CI_Waiver)) %>% 
  mutate(employmentStatus_1=as.factor(employmentStatus_1)) %>% 
  mutate(Occupation_1=as.factor(Occupation_1)) %>% 
  mutate(Industry_1=as.factor(Industry_1)) %>% 
  mutate(Postcode_district=as.factor(`Postcode district`)) %>% 
  mutate(Region=as.factor(Region)) %>% 
  mutate(MosaicGroup=as.factor(MosaicGroup)) %>% 
  mutate(MosaicType=as.integer(MosaicType)) %>% 
  mutate(FSSGroup=as.factor(FSSGroup)) %>%  
  
  mutate(FSSType=as.factor(FSSType)) %>%  
  mutate(IFA_NO=as.factor(IFA_NO)) %>%  
  mutate(IFA_Postcode_district=as.factor(IFA_Postcode_district)) %>%  
  mutate(IFA_Region=as.factor(IFA_Region)) %>%  
  mutate(Remote=as.factor(Remote)) %>%  
  mutate(Commission=as.factor(Commission)) %>%  
  
  mutate(RLAgentSegment=as.factor(RLAgentSegment)) %>%
  mutate(OpenToNB=as.factor(OpenToNB)) %>%
  mutate(Sex2=as.factor(Sex2)) %>%
  mutate(Smoking_Status_Current2=as.factor(Smoking_Status_Current2)) %>%
  mutate(Occupation_2=as.factor(Occupation_2)) %>%
  mutate(employmentStatus_2=as.factor(employmentStatus_2)) %>%
  mutate(Industry_2=as.factor(Industry_2)) %>% 
  filter(EXPOSURE>1.1)


#LAPSE RATES BY DURATION
p <-ggplot(summarize(group_by(data, DURATION),  target=mean(target)), aes(DURATION, target, fill=DURATION))
p +geom_bar(stat = "identity")


#LAPSE RATES BY GENDER
p <-ggplot(summarize(group_by(data, Sex1),  target=mean(target)), aes(Sex1, target, fill=Sex1))
p +geom_bar(stat = "identity")


#LAPSE RATES BY SOMEKER
p <-ggplot(summarize(group_by(data, Smoker),  target=mean(target)), aes(Smoker, target, fill=Smoker))
p +geom_bar(stat = "identity")

#LAPSE RATES BY NB
p <-ggplot(summarize(group_by(data, OpenToNB),  target=mean(target)), aes(OpenToNB, target, fill=OpenToNB))
p +geom_bar(stat = "identity")

p <-ggplot(summarize(group_by(data, Benefit_Type),  target=mean(target)), aes(Benefit_Type, target, fill=Benefit_Type))
p +geom_bar(stat = "identity")


##plot relevant features 
features<-colnames(data)
features_rel<- c("EXPOSURE", "AGE", "Term", "Original_SA")


for( i in features_rel ){
  
  p<-ggplot(filter(data,Original_SA<5000000),aes_string(x=i,fill="LAPSE"))+geom_histogram(bins=50,alpha=0.8,colour='black')
  print(p)
}

#------------------------------------------------------------------------------------------------------------------------------
# 4. POISSON REGRESSION
#4a. *****PREDICTION INTERVALS
#------------------------------------------------------------------------------------------------------------------------------

#SPLIT DATA INTO TRAIN AND TEST SET
set.seed(3456)
smp_siz = floor(0.8*nrow(data))
trainIndex = sample(seq_len(nrow(data)),size = smp_siz) 
X_train<-data[trainIndex,]
X_test<-data[-trainIndex,]




lapse_poison<-glm(ENDSTATE~ offset(log(EXPOSURE))+
                    
                    Sex1+ AGE +Original_SA+ Premium+JL_Status+Smoker + Benefit_Type+             
                    CI_Cover+ CI_Waiver+ Term+ employmentStatus_1+ Industry_1+ Region+  
                    + FinancialComplexity_FSS_C+   Remote+ Commission+ OpenToNB   + DURATION,
                  
                  data=X_train,family=poisson(link="log"))


summary(lapse_poison)

lapse_poison_pred <- tibble(exp(predict(lapse_poison, X_test))) %>% 
  rename(pred=`exp(predict(lapse_poison, X_test))`)
lapse_poison_pred <- cbind(lapse_poison_pred,X_test[,c("ENDSTATE","EXPOSURE")])%>% 
  mutate(error=pred-ENDSTATE)


(lapse_MSE= sum((lapse_poison_pred$error)^2)/nrow(lapse_poison_pred))


#------------------------------------------------------------------------------------------------------------------------------
#GBM
#------------------------------------------------------------------------------------------------------------------------------

lapse_gbm <-  gbm(ENDSTATE~ offset(log(EXPOSURE))+
                    
                    Sex1+ AGE +Original_SA+ Premium+JL_Status+Smoker + Benefit_Type+             
                    CI_Cover+ CI_Waiver+ Term+ employmentStatus_1+ Industry_1+ Region+ MosaicType+ FinancialAssurance_FSS_A+  
                    FinancialLifestyle_FSS_B+ FinancialComplexity_FSS_C+ FinancialFlexibility_FSS_D+ FinancialVitality_FSS_E+  
                    FinancialMaturity_FSS_F+ Status_Mosaic_A+ Lifestage_Mosaic_B+ Experience_Mosaic_C+
                    Culture_Mosaic_D+ Work_Mosaic_E+ Remote+ Commission+ OpenToNB   + DURATION
                  ,data=X_train,  distribution ="poisson", n.trees = 100, verbose=TRUE, interaction.depth = 3)

#plot predictions

lapse_gbm_pred <- tibble(predict(lapse_gbm, newdata = X_test, type = "response", n.trees=100)*X_test[,"EXPOSURE"]) %>% 
  rename(pred=`*...` )
lapse_gbm_pred <- cbind(lapse_gbm_pred,X_test[,"ENDSTATE"])%>% 
  rename(count=`X_test[, "ENDSTATE"]`  ) %>% 
  mutate(error=pred-count)

#plot(lapse_gbm_pred$pred, lapse_gbm_pred$count)
(lapse_gbm_MSE= sum((lapse_gbm_pred$error)^2)/nrow(lapse_gbm_pred))

summary(lapse_gbm)



#try fit a model with the important variables
#GAM with Smooth duration



#------------------------------------------------------------------------------------------------------------------------------
#MODEL SELECTION
#------------------------------------------------------------------------------------------------------------------------------

nothing<-glm(ENDSTATE~ offset(log(EXPOSURE))+1,
                  data=X_train,family=poisson(link="log"))

summary(nothing)

forwards = step(nothing,scope=list(lower=formula(nothing),upper=formula(lapse_poison)), direction="forward")
summary(forwards)

#------------------------------------------------------------------------------------------------------------------------------
TREES
#------------------------------------------------------------------------------------------------------------------------------

lapse_cart <- rpart(target ~  Sex1+ AGE +Original_SA+ Premium+JL_Status+Smoker + Benefit_Type+             
                      CI_Cover+ CI_Waiver+ Term+ employmentStatus_1+ Industry_1+ Region+ MosaicType+ FinancialAssurance_FSS_A+  
                      FinancialLifestyle_FSS_B+ FinancialComplexity_FSS_C+ FinancialFlexibility_FSS_D+ FinancialVitality_FSS_E+  
                      FinancialMaturity_FSS_F+ Status_Mosaic_A+ Lifestage_Mosaic_B+ Experience_Mosaic_C+
                      Culture_Mosaic_D+ Work_Mosaic_E+ Remote+ Commission+ OpenToNB   + DURATION +Occupation_1,
                    weights = X_train$EXPOSURE,
                    data=X_train, method = 'poisson', control=rpart.control(maxdepth = 4, cp=0.005))



z <- tibble(lapse_cart$splits)

(sum(X_train$LAPSE_CNT)/sum(X_train$EXPOSURE_CNT))
rpart.plot(lapse_cart)



lapse_cart_pred <- tibble(predict(lapse_cart, X_test)) %>% 
  rename(pred=`predict(lapse_cart, X_test)`) %>% 
  mutate(pred=pred*X_test$EXPOSURE_CNT)
lapse_cart_pred <- cbind(lapse_cart_pred,X_test[,c("EXPOSURE_CNT","LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)


(lapse_cart_MSE= sum((lapse_cart_pred$error)^2)/nrow(lapse_cart_pred))


z <- tibble(lapse_cart$where)

y <- cbind(X_train,z ) %>% 
  rename(node=`lapse_cart$where`) %>% 
  mutate(node=as.factor(node))

y <- summarise(group_by(y,node), ENDSTATE=sum(ENDSTATE),EXPOSURE=sum(EXPOSURE) ) %>% 
  mutate(lapse_rate=ENDSTATE/EXPOSURE)


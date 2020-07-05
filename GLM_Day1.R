
#install.packages("rpart.plot")

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
#------------------------------------------------------------------------------------------------------------------------------
# 1. DATA IMPORT
  #a. IMPORT RAW DATA
  #b. TRANSFORM CATEGORICAL VARIABLES INTO FACTORS
#------------------------------------------------------------------------------------------------------------------------------
#data 
#https://www.soa.org/resources/experience-studies/2014/research-2014-post-level-shock/


#####data pipeline slide here






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
#data tabualtion summarize group by


gen_plot <- summarize(group_by(data, GENDER),  lapse_rate=mean(lapse_rate))


duration_plot <- summarize(group_by(data, DURATION),  lapse_rate=mean(lapse_rate))


duration_gen_plot <- summarize(group_by(data, DURATION, GENDER),  lapse_rate=mean(lapse_rate))


duration_gen_SA_plot <- summarize(group_by(data, FACE_AMOUNT_BAND,DURATION,  DURATION),  lapse_rate=mean(lapse_rate))

#typical lapse analysis



#------------------------------------------------------------------------------------------------------------------------------
# 3. DATA VISUALISATION - EXPLORATORY DATA ANALYSIS
  #3a. SHOW GRAPHS OF RELATIONSHIPS BETWEEN PREDICTOTS AND LAPSE RATE
  #3b. TRANSFORM CATEGORICAL VARIABLES INTO FACTORS
  #3. SHOW CORRELATIONS BETWEEN VARIABLE
#------------------------------------------------------------------------------------------------------------------------------


#LAPSE RATES BY DURATION
plot <- summarize(group_by(data,DURATION ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
          LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
          mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)

ggplot(summarize(group_by(plot, DURATION),  lapse_rate=mean(lapse_rate)),
       aes(DURATION, lapse_rate, fill=DURATION))+
geom_bar(stat = "identity")




 
 
 #non linear relationship with Duration - consider using duration as a factor
 #fitting duration as a continous variable would result in a very poor fit as
 #the relationship will be highly non-linear

 
 #LAPSE RATES BY GENDER

plot <- summarize(group_by(data,GENDER ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                   LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
                   mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)
 
ggplot(summarize(group_by(plot, GENDER),  lapse_rate=mean(lapse_rate)), aes(GENDER, lapse_rate, fill=GENDER))+
geom_bar(stat = "identity")



#LAPSE RATES BY SUM ASSURED

plot <- summarize(group_by(data,FACE_AMOUNT_BAND ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                  LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
                  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)

p <-ggplot(summarize(group_by(plot, FACE_AMOUNT_BAND),  lapse_rate=mean(lapse_rate)), aes(FACE_AMOUNT_BAND, lapse_rate, fill=FACE_AMOUNT_BAND))
p +geom_bar(stat = "identity")


#LAPSE RATES BY PREMIUM_MODE

plot <- summarize(group_by(data,PREMIUM_MODE ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                  LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
                  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)
p <-ggplot(summarize(group_by(plot, PREMIUM_MODE),  lapse_rate=mean(lapse_rate)), aes(PREMIUM_MODE, lapse_rate, fill=PREMIUM_MODE))
p +geom_bar(stat = "identity")


#LAPSE RATES BY structurew
plot <- summarize(group_by(data,POST_LEVEL_PREMIUM_STRUCTURE ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                  LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)

ggplot(plot, aes(POST_LEVEL_PREMIUM_STRUCTURE, lapse_rate, fill=POST_LEVEL_PREMIUM_STRUCTURE))+
geom_bar(stat = "identity")

#LAPSE RATES BY AGE_BAND
plot <- summarize(group_by(data,AGE_BAND ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                  LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)

ggplot(plot, aes(AGE_BAND, lapse_rate, fill=AGE_BAND))+
  geom_bar(stat = "identity")


#LAPSE RATES BY RISK_CLASS
plot <- summarize(group_by(data,RISK_CLASS ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                  LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)

ggplot(plot, aes(RISK_CLASS, lapse_rate, fill=RISK_CLASS))+
  geom_bar(stat = "identity")



#LAPSE RATES BY JUMP SIZE

plot <- mutate(data, PREM_JUMP_D11_D10=as.integer(PREM_JUMP_D11_D10))%>%
  mutate(PREM_JUMP_D11_D10=as.factor(PREM_JUMP_D11_D10))

  plot <- summarize(group_by(plot,PREM_JUMP_D11_D10 ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
            LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)
  
  
ggplot(summarize(group_by(plot, PREM_JUMP_D11_D10),  lapse_rate=mean(lapse_rate)), aes(PREM_JUMP_D11_D10, lapse_rate, fill=PREM_JUMP_D11_D10))+
geom_bar(stat = "identity")




q <- mutate(data, PREM_JUMP_D11_D10=as.integer(PREM_JUMP_D11_D10))%>%
  mutate(PREM_JUMP_D11_D10=as.factor(PREM_JUMP_D11_D10))

q <- summarize(group_by(q,PREM_JUMP_D11_D10,GENDER, FACE_AMOUNT_BAND ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
               LAPSE_CNT=sum(LAPSE_CNT),Duration=mean(Duration),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)

ggplot(data=q)+
  geom_point(alpha=0.7, mapping = aes(x=Duration, y=lapse_rate, size = EXPOSURE_AMT, color=GENDER))  
  
#split by FACE_AMOUNT_BAND


ggplot(data=q)+
  geom_point(alpha=0.7, mapping = aes(x=Duration, y=lapse_rate,  color=GENDER)) + 
  facet_wrap(~ FACE_AMOUNT_BAND, nrow = 2)

rm(q)
rm(plot)

gc()




#------------------------------------------------------------------------------------------------------------------------------
# 4. SIMPLE LINEAR MODEL
  #4a. *****PREDICTION INTERVALS
#------------------------------------------------------------------------------------------------------------------------------
lapse_mod <- lm(lapse_rate ~DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND+ POST_LEVEL_PREMIUM_STRUCTURE 
                 + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE , data=data)


summary(lapse_mod)

summary(lm)



effect_plot(lapse_mod, pred = DURATION, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )
effect_plot(lapse_mod, pred = PREMIUM_MODE, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "CUD Bright" )
effect_plot(lapse_mod, pred = PREM_JUMP_D11_D10, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Raainbow" )
effect_plot(lapse_mod, pred = POST_LEVEL_PREMIUM_STRUCTURE, interval = TRUE, plot.points = FALSE, y.label="lapse rate" )
effect_plot(lapse_mod, pred =  RISK_CLASS, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )


hist(data$lapse_rate, xlim=c(0,1 ), breaks=50, ylim=c(0,30000))


hist(lapse_mod$fitted.values)

#non-normal distribution

#plot_coefs(lapse_mod, coefs=c("PREMIUM_MODE"))


#plot_summs



#------------------------------------------------------------------------------------------------------------------------------
#poisson model
#------------------------------------------------------------------------------------------------------------------------------

#POISSSON can be used with both grouped and ungrouped data
#If individual responses are poisson then the sum is also posson




#SPLIT INTO TEST TRAIN

set.seed(3456)
smp_siz = floor(0.75*nrow(data))
trainIndex = sample(seq_len(nrow(data)),size = smp_siz) 
X_train<-data[trainIndex,]
X_test<-data[-trainIndex,]


hist(X_train$LAPSE_CNT, xlim=c(0,1999 ), breaks=50, ylim=c(0,30000))

#note sero inflated data

#FIT MODEL
#need to allo for exposure
lapse_poison <- glm(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ Duration + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                    + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE 
                    ,data=X_train, family=poisson(link="log"))

#for a well fitting modelthe residual deviance should be close to the degrees of freedom
summary(lapse_poison)
#All terms significant - this is suspicious

#rule of thumb pseudo R^2 = 1-residual_dev/null_dev
(1-lapse_poison$deviance/lapse_poison$null.deviance )


#IMPACT OF FACTORS
# how does a unit increase in explanatory variables impact the lapse rates
effect_plot(lapse_poison, pred = Duration, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )
effect_plot(lapse_poison, pred = PREMIUM_MODE, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "CUD Bright" )
effect_plot(lapse_poison, pred = RISK_CLASS, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )
effect_plot(lapse_poison, pred = GENDER, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )


#visualising regression results

lapse_poison_coef <- broom::tidy(lapse_poison) %>% 
  mutate(estimate=exp(estimate))
  
  #filter(p.value<0.001 )

dwplot(lapse_poison_coef, show_intercept = TRUE)


#predictions
#NOTE THESE ARE THE LOG OF THE COUNTS

preds <- predict(lapse_poison, X_test)
#view(preds)

#MEAN SQAURE ERROR
(lapse_poison_MSE= sum((lapse_poison_pred$error)^2)/nrow(lapse_poison_pred))


#visualising predictions results


lapse_poison_pred <- tibble(exp(predict(lapse_poison, X_test))) %>% 
 rename(pred=`exp(predict(lapse_poison, X_test))`)
lapse_poison_pred <- cbind(lapse_poison_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)


ggplot(data=lapse_poison_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")

#how do predictions vary by duration rates vary by DURATION
ggplot(data=lapse_poison_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ DURATION, nrow = 4)

#####THIS IS A VERY POOR MODEL

#### LETS TRY FITTING TO DURATION AS A FACTOR

#LAPSE RATES BY DURATION
plot <- summarize(group_by(data,DURATION ), EXPOSURE_CNT=sum(EXPOSURE_CNT),
                  LAPSE_CNT=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE_CNT/EXPOSURE_CNT)
ggplot(summarize(group_by(plot, DURATION),  lapse_rate=mean(lapse_rate)), aes(DURATION, lapse_rate, fill=DURATION))+
  geom_bar(stat = "identity")


#"DURATION" is a factor 

lapse_poison <- glm(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                    + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE 
                    ,data=X_train, family=poisson(link="log"))



summary(lapse_poison)

(1-lapse_poison$deviance/lapse_poison$null.deviance )
#THIS MODEL LOOKS MUCH BETTER

effect_plot(lapse_poison, pred = DURATION, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )
effect_plot(lapse_poison, pred = PREMIUM_MODE, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "CUD Bright" )
effect_plot(lapse_poison, pred = RISK_CLASS, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )
effect_plot(lapse_poison, pred = GENDER, interval = TRUE, plot.points = FALSE, y.label="lapse rate", colors = "Rainbow" )

lapse_poison_pred <- tibble(exp(predict(lapse_poison, X_test))) %>% 
  rename(pred=`exp(predict(lapse_poison, X_test))`)
lapse_poison_pred <- cbind(lapse_poison_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION", "EXPOSURE_CNT","lapse_rate")])%>%
  mutate(pred_rate=pred/EXPOSURE_CNT)
  mutate(error=pred-LAPSE_CNT)
  
##############################################
  #######################################
  #################################
#alternative plot  
X_test$pred <-predict(lapse_poison, X_test, type="response")
byPred <- aggregate(pred ~ DURATION, data = X_test, FUN = sum)
byObsv <- aggregate(LAPSE_CNT ~ DURATION, data = X_test, FUN = sum)
AERatio <- byObsv[,2]/byPred[,2] 


plot(AERatio,xlab="DURATION", ylab="AE Ratio", xaxt='n', ylim=c(0.9,1.1),
     pch=18)
title("A/E vs. DURATION")
#axis(1, at=1:4,labels=c("NS-Annual","NS-Monthly","SM-Anual","SM-Monthly"), las=0)
abline(1,0,col="red")

  

#MEAN SQAURE ERROR
(lapse_poison_MSE= sum((lapse_poison_pred$error)^2)/nrow(lapse_poison_pred))

#THIS MODEL LOOKS MUCH BETTER


ggplot(data=lapse_poison_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")


#THIS MODEL LOOKS MUCH BETTER

#how do predictions vary by duration rates vary by DURATION

ggplot(data=lapse_poison_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ DURATION, nrow = 4)




#Lets try a negative binomial

#------------------------------------------------------------------------------------------------------------------------------
#negative binomial model
#------------------------------------------------------------------------------------------------------------------------------



lapse_poison_nb <- glm.nb (LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                    + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE 
                    ,data=X_train, link="log")

summary(lapse_poison_nb)


lapse_poison_nb_pred <- tibble(exp(predict(lapse_poison_nb, X_test))) %>% 
  rename(pred=`exp(predict(lapse_poison_nb, X_test))`)
lapse_poison_nb_pred <- cbind(lapse_poison_nb_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)

#MEAN SQAURE ERROR
(lapse_poison_nb_MSE= sum((lapse_poison_nb_pred$error)^2)/nrow(lapse_poison_nb_pred))


#in this case it looks like the poisson is better than the negative binomial distribution


#------------------------------------------------------------------------------------------------------------------------------
#zero inflated poisson model
#------------------------------------------------------------------------------------------------------------------------------

#MOTIVATION
#Sometimes the number of zeros is greater that would be prediced by the poisson or negative binomial

hist(X_train$LAPSE_CNT, xlim=c(0,1999 ), breaks=50, ylim=c(0,3000))


#recall that we fitted a poisson model but how do prediction vary between those who lapsed and those who didnt

#first add an indicator variable for those who lapsed 1 or zero

lapse_poison_pred <- mutate(lapse_poison_pred, zero=as.factor(ifelse(LAPSE_CNT==0,1,0)))

ggplot(data=lapse_poison_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ zero, nrow = 1)

# 
lapse_poison_zero <- zeroinfl(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                              + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE,
                              X_train)
summary(lapse_poison_zero)


#remove insignificant zero model terms
#we can model the zero part seperately

# 
lapse_poison_zero <- zeroinfl(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                              + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE
                              | offset(log(EXPOSURE_CNT))+DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                              + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 + PREMIUM_MODE,
                              X_train)
summary(lapse_poison_zero)


lapse_poison_zero_pred <- tibble(predict(lapse_poison_zero, X_test)) %>% 
  rename(pred=`predict(lapse_poison_zero, X_test)`)
lapse_poison_zero_pred <- cbind(lapse_poison_zero_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)

ggplot(data=lapse_poison_zero_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")


#MEAN SQAURE ERROR
(lapse_poison_zero_MSE= sum((lapse_poison_zero_pred$error)^2)/nrow(lapse_poison_zero_pred))



#------------------------------------------------------------------------------------------------------------------------------
# GAM
#------------------------------------------------------------------------------------------------------------------------------


#LAPSE RATES BY JUMP SIZE
X_train2 <- mutate(X_train, PREM_JUMP_D11_D10=as.integer(PREM_JUMP_D11_D10))%>%
  mutate(PREM_JUMP_D11_D10=as.factor(PREM_JUMP_D11_D10))
p <-ggplot(summarize(group_by(X_train2, PREM_JUMP_D11_D10),  lapse_rate=mean(lapse_rate)), aes(PREM_JUMP_D11_D10, lapse_rate, fill=PREM_JUMP_D11_D10))
p +geom_bar(stat = "identity")



lapse_gam <-mgcv::bam(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ DURATION + GENDER + s(AGE_BAND) + FACE_AMOUNT_BAND
                      + POST_LEVEL_PREMIUM_STRUCTURE + s(PREM_JUMP_D11_D10) +  RISK_CLASS + PREMIUM_MODE 
                      ,data=X_train, family=poisson(link="log"))


summary(lapse_gam)


plot(lapse_gam,select=1)

#note we see the impact of age

plot(lapse_gam,select=2)


#note we see the impact of escalation

#gam.check(lapse_gam)



#plot predictions


lapse_poison_gam_pred <- tibble(exp(predict(lapse_gam, X_test))) %>% 
  rename(pred=`exp(predict(lapse_gam, X_test))`)

lapse_poison_gam_pred <- cbind(lapse_poison_gam_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)

plot(lapse_poison_gam_pred$pred, lapse_poison_gam_pred$LAPSE_CNT)
(lapse_poison_gam_MSE= sum((lapse_poison_gam_pred$error)^2)/nrow(lapse_poison_gam_pred))

#best model thus far

#------------------------------------------------------------------------------------------------------------------------------
# add interactions see plot with lapse by duration and jump
#------------------------------------------------------------------------------------------------------------------------------

lapse_poison_int <- glm(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+  GENDER + AGE_BAND + FACE_AMOUNT_BAND + DURATION
                    + POST_LEVEL_PREMIUM_STRUCTURE  +  RISK_CLASS + PREMIUM_MODE  +PREM_JUMP_D11_D10
                    + POST_LEVEL_PREMIUM_STRUCTURE*PREM_JUMP_D11_D10
                    ,data=X_train, family=poisson(link="log"))


summary(lapse_poison_int)


(1-lapse_poison_int$deviance/lapse_poison_int$null.deviance )

lapse_poison_int_coef <- broom::tidy(lapse_poison_int) %>% 
  mutate(estimate=exp(estimate))

#filter(p.value<0.001 )

#dwplot(lapse_poison_int,lapse_poison, show_intercept = TRUE)
dwplot(lapse_poison_int_coef, show_intercept = TRUE)



#plot predictions


lapse_poison_int_pred <- tibble(exp(predict(lapse_poison_int, X_test))) %>% 
  rename(pred=`exp(predict(lapse_poison_int, X_test))`)
lapse_poison_int_pred <- cbind(lapse_poison_int_pred,X_test[,c("LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)

ggplot(data=lapse_poison_int_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")



(lapse_poison_int_MSE= sum((lapse_poison_int_pred$error)^2)/nrow(lapse_poison_int_pred))


#The interaction has improved the model


#------------------------------------------------------------------------------------------------------------------------------
# MODEL SELECTION
#------------------------------------------------------------------------------------------------------------------------------


# stepwise <- glm(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+  GENDER + AGE_BAND + FACE_AMOUNT_BAND+DURATION+PREM_JUMP_D11_D10
#                         + POST_LEVEL_PREMIUM_STRUCTURE  +  RISK_CLASS + PREMIUM_MODE + PREM_JUMP_D11_D10*DURATION+
#                   POST_LEVEL_PREMIUM_STRUCTURE*PREM_JUMP_D11_D10
#                   
#                         ,data=X_train, family=poisson(link="log"))


stepwise_f <- glm(LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ 1
                
                ,data=X_train, family=poisson(link="log"))


z <- step(stepwise_f, direction ="forward", scope = formula(stepwise)  )

summary(stepwise)




#------------------------------------------------------------------------------------------------------------------------------
# INTRODUCINGE THE CARET PACKAGE
#------------------------------------------------------------------------------------------------------------------------------


#SPLIT DATA INTO TRAIN AND TEST SET
set.seed(3456)
trainIndex <- createDataPartition(data$GENDER, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)

#create test/train split
X_train<-data[trainIndex,]
X_test<-data[-trainIndex,]

#k-fold cross validation
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = FALSE,
  #sampling = "up"
)

cl <- makeCluster(10,type = "SOCK")
registerDoSNOW(cl)



lapse <- train(lapse_rate ~   GENDER + AGE_BAND + FACE_AMOUNT_BAND + DURATION
                               + POST_LEVEL_PREMIUM_STRUCTURE  +  RISK_CLASS + PREMIUM_MODE  +PREM_JUMP_D11_D10
                               + POST_LEVEL_PREMIUM_STRUCTURE*PREM_JUMP_D11_D10, weights = X_train$EXPOSURE
                               ,data=X_train, method="glm",family=poisson(link = "log" )   ,trControl=fitControl)

#can use weights to model the lapse rate directly instead of offset

stopCluster(cl)

summary(lapse)


plot(varImp(lapse))



lapse_pred <- tibble(predict(lapse, X_test)) %>% 
  rename(pred=`predict(lapse, X_test)`) %>% 
  mutate(pred=pred*X_test$EXPOSURE_CNT)
  
lapse_pred <- cbind(lapse_pred,X_test[,c("EXPOSURE_CNT","LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)

ggplot(data=lapse_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")


(lapse_MSE= sum((lapse_pred$error)^2)/nrow(lapse_pred))





#------------------------------------------------------------------------------------------------------------------------------
# TREE BASED METHODS RPART 
#------------------------------------------------------------------------------------------------------------------------------

lapse_cart <- rpart(lapse_rate ~   GENDER + AGE_BAND + FACE_AMOUNT_BAND + DURATION
               + POST_LEVEL_PREMIUM_STRUCTURE  +  RISK_CLASS + PREMIUM_MODE  +PREM_JUMP_D11_D10,
               weights = X_train$EXPOSURE_CNT
               ,data=X_train, method = 'poisson', control=rpart.control(maxdepth = 4, cp=0.005))

lapse_cart

(sum(X_train$LAPSE_CNT)/sum(X_train$EXPOSURE_CNT))

rpart.plot(lapse_cart)
#this  tells us how to group the data!


#lets try the counts
#Y has a column for exposure and one for counts
Y=as.matrix( X_train[,c("EXPOSURE_CNT","LAPSE_CNT")])

lapse_cart_count <- rpart(Y  ~   GENDER + AGE_BAND + FACE_AMOUNT_BAND + DURATION
                    + POST_LEVEL_PREMIUM_STRUCTURE  +  RISK_CLASS + PREMIUM_MODE  +PREM_JUMP_D11_D10,
                    data=X_train, method = 'poisson', control=rpart.control(maxdepth = 4, cp=0.005))

summary(lapse_cart_count, cp=0.005)

rpart.plot(lapse_cart_count)

#results are equivalent

lapse_cart_count_pred <- tibble(predict(lapse_cart_count, X_test)) %>% 
  rename(pred=`predict(lapse_cart_count, X_test)`) %>% 
  mutate(pred=pred*X_test$EXPOSURE_CNT)

lapse_cart_count_pred <- cbind(lapse_cart_count_pred,X_test[,c("EXPOSURE_CNT","LAPSE_CNT","FACE_AMOUNT_BAND","DURATION")])%>% 
  mutate(error=pred-LAPSE_CNT)

ggplot(data=lapse_cart_count_pred,  aes(x=LAPSE_CNT, y=pred))+
  geom_point()+
  geom_smooth(method="lm")


(lapse_cart_count_MSE= sum((lapse_cart_count_pred$error)^2)/nrow(lapse_cart_count_pred))

#The best so far!!!!!!


#lets compare to the traditional lapse approach


lapse_cart_rate_pred <- tibble(predict(lapse_cart_count, X_test)) %>% 
  rename(cart_pred=`predict(lapse_cart_count, X_test)`)


lapse_cart_rate_pred <- cbind(lapse_cart_rate_pred,X_test[,c("FACE_AMOUNT_BAND","DURATION","EXPOSURE_CNT","LAPSE_CNT")]) 



duration_gen_SA_plot <- summarize(group_by(data,DURATION,FACE_AMOUNT_BAND ), EXPOSURE=sum(EXPOSURE_CNT),
                  LAPSE=sum(LAPSE_CNT),EXPOSURE_AMT=sum(EXPOSURE_AMT)) %>% 
  mutate(lapse_rate=LAPSE/EXPOSURE)


# # Merge by columns with different names
z <- merge(lapse_cart_rate_pred, duration_gen_SA_plot, by.x = c( "DURATION", "FACE_AMOUNT_BAND"), by.y =c( "DURATION", "FACE_AMOUNT_BAND"))

z <- mutate(z,trad_pred=EXPOSURE_CNT*lapse_rate ) %>% 
  mutate(error=trad_pred-LAPSE_CNT)


(lapse_trad_count_MSE= sum((z$error)^2)/nrow(z))

#trditinal approach performs the worst


#------------------------------------------------------------------------------------------------------------------------------
# RANDOM FOREST
#------------------------------------------------------------------------------------------------------------------------------






#------------------------------------------------------------------------------------------------------------------------------
# GBM
#------------------------------------------------------------------------------------------------------------------------------
#http://uc-r.github.io/gbm_regression

lapse_gbm <-  gbm(formula = LAPSE_CNT ~ offset(log(EXPOSURE_CNT))+ DURATION + GENDER + AGE_BAND + FACE_AMOUNT_BAND
                  + POST_LEVEL_PREMIUM_STRUCTURE + PREM_JUMP_D11_D10 +  RISK_CLASS + PREMIUM_MODE 
                  ,data=X_train,  distribution ="poisson", n.trees = 160, verbose=TRUE, interaction.depth = 2)

#plot predictions

lapse_gbm_pred <- tibble(predict(lapse_gbm, newdata = X_test, type = "response", n.trees=300)*X_test[,"EXPOSURE_CNT"]) %>% 
  rename(pred=`*...` )
lapse_gbm_pred <- cbind(lapse_gbm_pred,X_test[,"LAPSE_CNT"])%>% 
  rename(count=`X_test[, "LAPSE_CNT"]`  ) %>% 
  mutate(error=pred-count)
plot(lapse_gbm_pred$pred, lapse_gbm_pred$count)
(lapse_gbm_MSE= sum((lapse_gbm_pred$error)^2)/nrow(lapse_gbm_pred))



par(mar = c(5, 8, 1, 1))
summary(
  lapse_gbm, 
  cBars = 10,
  method = relative.influence, 
  las = 2
)


#Plot of Response variable with lstat variable
plot(lapse_gbm,i="DURATION") 
#Inverse relation with lstat variable

plot(lapse_gbm,i="PREM_JUMP_D11_D10") 
#as the average number of rooms increases the the price increases



partial(lapse_gbm,  pred.var = "PREM_JUMP_D11_D10", n.trees=lapse_gbm$n.trees, plot = TRUE, rug = TRUE,plot.engine = "ggplot2") 



























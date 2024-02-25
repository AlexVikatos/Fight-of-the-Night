#Logistic Model#

#AUTHOR: ALEX VIKATOS

#BIBLIOGRAPHY : AN INTRODUCTION TO STATISTICAL LEARNING
#               WITH APPLICATIONS IN R by Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirami.


##CHECKING Residual Deviances for each variable

explanotory_list=colnames(data[,c(5,6,7,8,9,11:29)])
explanotory_list
final=NULL
for (i in explanotory_list){
  formula_str=paste("FOTN",i,sep="~")
  model=glm(formula_str,data=data,family="binomial")
  Residual_deviance=summary(model)$deviance
  final_element=list(Explanatory_variable=i,Model=model,Residual_Deviance=Residual_deviance)
  final=c(final,list(final_element))
}
final
#################################################################################
#################################################################################
###############      P   R   O   X   I   R    O    ##############################
#################################################################################
#################################################################################
#################################################################################  


#ME OLES TIS METAVLITES EKTOS TOY DATE GIA EUNOITOUS LOGOUS,KRATISA TO YEAR.
data$FOTN=as.factor(data$FOTN)
logistic=glm(FOTN~sig_strikes_landed+control_time_accuracy+method+division+stance+
               total_comp_time+sub_attempts+takedowns_landed+control+
               year+gender,data=data,family="binomial")
summary(logistic)
#SIG STRIKES Statistical Signifance
#ALL THE OTHER P-VALUES ARE RELATIVELY LARGE.

#checking the p-values of the coefficients
coef(logistic)
summary(logistic)$coef
##too many categorical variables##

##predicting the probability##
logistic.probs=predict(logistic,type="response")
#show me the 10 first probs
logistic.probs[1:10]
contrasts(data$FOTN)

#i am creating a vector with 2846 elements of No
logistic.pred=rep("No",2846)
logistic.pred[logistic.probs>0.5]="Yes"

##CONFUSION MATRIX##
table(logistic.pred,data$FOTN)

(2608+4)/2846
#0.9177 the diagonal elements of the confusion matrix indicates correct prediction

(190+43)/2846
#0.0818 the off diagonal elements of the confusion matrix indicates represent incorrect predictions


## THE ISSUE HERE IS THAT MY MODEL CANT PREDICT MORE THAN 4 FOTNs
## BECAUSE AS YOU CAN SEE AT THE DIAGONAL MATRIX ONLY 4 "YES" ARE CORRECT
## THAT MIGHT BE BECAUSE OF THE P-VALUES OF SOME OF THE VARIABLES OR BECAUSE OF THE 
## SMALL SAMPLE OF FOTNs(only 195) INSTEAD TO A LARGE NUMBER OF noFOTNs(2846-195)



##SAME MODEL##
##DIFFERENT METHOD##
#training and test subsets##
train=(data$year<2020)
data2020=data[!train,]
head(data2020)

dim(data2020)
#942 stiles 29 grammes

#glm.fits=glm(formula =FOTN~sig_strikes_landed+total_comp_time,data=data,family="binomial",
            #subset=train)


glm.fits=glm(formula = FOTN ~ sig_strikes_landed + control_time_accuracy + 
                method + division + stance + total_comp_time + sub_attempts + 
                takedowns_landed + control + year + gender, family = "binomial", 
              data = data, subset = train)
glm.probs=predict(glm.fits,data2020,type="response")
summary(glm.fits)
glm.pred=rep("No",942)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2020])
#vrika 13 FOTNS
mean(glm.pred==data$FOTN[data$year>=2020])
#0.937
mean(glm.pred!=data$FOTN[data$year>=2020])
#0.062
#################################################################################
####################        E    N     D      ###################################
###############      P   R   O   X   I   R    O    ##############################
#################################################################################
#################################################################################
#################################################################################  

#START OVER#
#1.MODEL CREATION
#2.MODEL SELECTION#

#1
mod1=glm(FOTN~division+stance+method+total_comp_time+round+knockdowns+
           sub_attempts+control+takedowns_landed+sig_strikes_landed+
           total_strikes_landed+head_strikes_landed+body_strikes_landed+
           leg_strikes_landed+distance_strikes_landed+clinch_strikes_landed+
           ground_strikes_landed+year+gender,data=data,family="binomial")
summary(mod1)
str(data)

#STEPWISE REGRESSION#
#TREXW TO MODELO ME BACKWARD
#AFAIREI ENA PREDICTOR TH FORA
#META SIGKRINEI TO AIC TOU MODELOU ME OLA TA ALLA
#EPILEGEI AUTO ME TO XAMILOTERO AIC
library(MASS)
mod_step=stepAIC(mod1,direction="backward",trace=FALSE)
mod_step
summary(mod_step)

#glm(formula = FOTN ~ method + total_comp_time + knockdowns + 
#sub_attempts + sig_strikes_landed + total_strikes_landed + 
  #head_strikes_landed + year, family = "binomial", data = data)



#BOOTSTRAP REGRESSION#
#bootstrap resampling with replacement method
#epilegei tous pio suxnous predictors pou ekane select h stepwise
install.packages("bootStepAIC")
library(bootStepAIC)
?boot.stepAIC
mod1
mod_boot=boot.stepAIC(mod1,data,B=50)
mod_boot

#Final Model:
#FOTN ~ method + total_comp_time + knockdowns + sub_attempts + 
#  sig_strikes_landed + total_strikes_landed + head_strikes_landed + 
# year


mod2=glm(FOTN ~ method + total_comp_time + knockdowns + sub_attempts + 
           sig_strikes_landed + total_strikes_landed + head_strikes_landed + 
           year,data=data,family="binomial")
summary(mod2)

#ARA EPILEGW TO MOD2 GIATI EXEI AIC=1126 ENANTI TOU MOD1 POU EXEI AIC=1139



#Check for Multicollinearity of predictor variables.Problem of 
#multicollinarity happens when two or more predictors that are highly correlated
#This may increase the standard error of the coefficient and reduce the reliability of
#estimated coefficients

#VIF >=5 WOULD SUGGEST HIGH CORRELATION
install.packages("car")
library(car)
vif(mod2)
vif(mod_step) 

#PARATIRW OTI EXW GVIF>5 SE SIG_STRIKES_LANDED,HEAD_STRIKES_LANDED,TOTAL_STRIKES_LANDED
#ALLA VGAZEI GIVEN THE CONTEXT NA SIMVENEI AUTO OPOTE DEN KSERW AN XRIAZETE NA KANW 
#ADRESS TO THEMA.

##FUTURE WORK MIGHT CREATE/COMBINE(existing variables) NEW VARIABLES SUCH AS A INDEX OF THE LEVEL OF THE ACCURACY
#OF THE ABOVE VARIABLES THAT MIGHT RESOLVE THE ISSUE OF MULTICOLLINEARITY.





#PREDICTION METHODS FOR MOD1 AND MOD2

#METHOD.1 FOR MOD2
##predicting the probability##
logistic.probs=predict(mod2,type="response")
#show me the 10 first probs
logistic.probs[1:10]
contrasts(data$FOTN)

#i am creating a vector with 2846 elements of No
logistic.pred=rep("No",2846)
logistic.pred[logistic.probs>0.5]="Yes"

##CONFUSION MATRIX##
table(logistic.pred,data$FOTN)

#METHOD.2 FOR MOD2
glm.probs=predict(mod2,data2020,type="response")
summary(mod2)
glm.pred=rep("No",942)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2020])


#METHOD.1 FOR MOD1
##predicting the probability##
logistic.probs=predict(mod1,type="response")
#show me the 10 first probs
logistic.probs[1:10]
contrasts(data$FOTN)

#i am creating a vector with 2846 elements of No
logistic.pred=rep("No",2846)
logistic.pred[logistic.probs>0.5]="Yes"


#i am creating a vector with 2846 elements of No
logistic.pred=rep("No",2846)
logistic.pred[logistic.probs>0.5]="Yes"

##CONFUSION MATRIX##
table(logistic.pred,data$FOTN)


#METHOD.2 FOR MOD1
glm.probs=predict(mod1,data2020,type="response")
summary(mod1)
glm.pred=rep("No",942)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2020])






###AFTER VIF TEST##
#STEPWISE
#glm(formula = FOTN ~ method + total_comp_time + knockdowns + 
 #     sub_attempts + sig_strikes_landed + total_strikes_landed + 
  #    head_strikes_landed + year, family = "binomial", data = data)

mod_step1=glm(formula = FOTN ~ method + total_comp_time + knockdowns + 
                sub_attempts + total_strikes_landed + 
                head_strikes_landed + year, family = "binomial", data = data)
vif(mod_step1)                
#i can see that the VIF values have dropped significantly
#lets do some predictions with the mod_step1


glm.probs=predict(mod_step1,data2020,type="response")
summary(mod_step1)
glm.pred=rep("No",942)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2020])

glm.probs=predict(mod_step,data2020,type="response")
summary(mod1)
glm.pred=rep("No",942)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2020])















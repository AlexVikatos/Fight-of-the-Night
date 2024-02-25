#Logistic Model#

#AUTHOR: ALEX VIKATOS

#BIBLIOGRAPHY : AN INTRODUCTION TO STATISTICAL LEARNING
#               WITH APPLICATIONS IN R by Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirami.

##CHECKING Residual Deviances for each variable

explanotory_list=colnames(data[,c(5,6,7,8,9,11:144)])
explanotory_list
final=NULL
data$FOTN=as.factor(data$FOTN)
for (i in explanotory_list){
  formula_str=paste("FOTN",i,sep="~")
  model=glm(formula_str,data=data,family="binomial")
  Residual_deviance=summary(model)$deviance
  final_element=list(Explanatory_variable=i,Model=model,Residual_Deviance=Residual_deviance)
  final=c(final,list(final_element))
}
final
str(data)
data
data$division=as.factor(data$division)
data$method=as.factor(data$method)
data$stance=as.factor(data$stance)
data$stance2=as.factor(data$stance2)
#####FULL MODEL#####
####################
####################



dataformodel=data[,c(5,6,7,8,9,10,12:144)]
str(dataformodel)
dataformodel$FOTN
dataformodel$FOTN=as.factor(dataformodel$FOTN)
logistic=glm(FOTN~.,data=dataformodel,family="binomial")
summary(logistic)
par(mfrow=c(2,2))
plot(logistic)
logistic4=glm(FOTN~leg_strikes_landed2,data=dataformodel,family="binomial")

##predicting the probability##
logistic.probs=predict(logistic,type="response")
#show me the 10 first probs
logistic.probs[1:10]
contrasts(dataformodel$FOTN)
?contrasts

#i am creating a vector with 2846 elements of No
logistic.pred=rep("No",2846)
logistic.pred[logistic.probs>0.5]="Yes"

##CONFUSION MATRIX##
table(logistic.pred,dataformodel$FOTN)
yfull1=cbind(logistic.pred,dataformodel$FOTN)
yfull1
#8 true positives

str(data)
#DIFFERENT
##SAME MODEL##
##DIFFERENT METHOD##
#training and test subsets##
train=(dataformodel$year<2021)
data2021=dataformodel[!train,]
head(data2020)

dim(data2021)
table(data2021$FOTN)
data2021

glm.fits=glm(FOTN~.,data=dataformodel,family="binomial")
plot(glm.fits)

glm.probs=predict(glm.fits,data2021,type="response")
summary(glm.fits)
glm.pred=rep("No",498)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2021])
#14 true positives
yfull=cbind(glm.pred,data[data$year>=2021,c("FOTN","fighter","fighter2")])
yfull

table(data$FOTN,data$year)



########END#########
#####FULL MODEL#####
####################
####################


#stepwise#

library(MASS)
mod_step=stepAIC(logistic,direction="backward",trace=FALSE)
mod_step
summary(mod_step)
plot(mod_step)

glm.probs=predict(mod_step,data2020,type="response")
summary(glm.fits)
glm.pred=rep("No",942)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2020])
#15 true positives
ystep=cbind(glm.pred,data[data$year>=2020,c("FOTN","fighter","fighter2")])
ystep


##RIDGE-LASSO##
install.packages("glmnet")
library(glmnet)
#a=1 Lasso Regression
#a=0 Ridge


###LASSO##
table(dataformodel$FOTN,dataformodel$year)
dataformodel$FOTN=as.numeric(dataformodel$FOTN)
dataformodel$FOTN[dataformodel$FOTN==1]=0
dataformodel$FOTN[dataformodel$FOTN==2]=1
dataformodel$FOTN

#define the response variable
response = na.omit(dataformodel$FOTN)
predictors = as.matrix(na.omit(dataformodel[, -which(names(dataformodel) == "FOTN")]))


#train and test data
train_data=dataformodel[dataformodel$year<2020,]
test_data=dataformodel[dataformodel$year>2020,]

x <- as.matrix(train_data[, -which(names(train_data) == "FOTN")])
y <- train_data$FOTN

#I HAD NAs.
complete_rows <- complete.cases(x, y)
x <- x[complete_rows, ]
y <- y[complete_rows,]
y

#perform k-fold cross-validation to find optimal lamda value
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)
best_lamda=lasso_model$lambda.min
best_lamda

#plot of MSE's vs lamda values
plot(lasso_model)


best_model=glmnet(x,y,lamda=best_lamda)
best_model
plot(best_model)
coef(best_model)


# Make predictions on the test data and evaluate the model's performance
x_test <- as.matrix(test_data[, -which(names(test_data) == "FOTN")])
y_test <- na.omit(test_data$FOTN)


predictions=predict(best_model,s=best_lamda,newx = x_test,type="response")
predictions

glm.pred=rep("No",498)
glm.pred[predictions>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2021])
#4 TRUE POSITIVES




############13/10/2023#######
str(dataformodel)
logistic1=glm(FOTN~division+method+total_comp_time+round+
              sum_knockdowns+sum_sub_attempts+sum_takedowns_landed+
               sum_takedowns_attempts+sum_sig_strikes_landed+sum_sig_strikes_attempts+
               sum_head_strikes_landed+sum_head_strikes_attempts+sum_reversals+sum_total_strikes_landed+
               sum_total_strikes_attempts+total_control+sum_body_strikes_landed+
               sum_body_strikes_attempts+sum_leg_strikes_landed+
               sum_distance_strikes_landed+sum_distance_strikes_attempts+
               sum_clinch_strikes_landed+sum_clinch_strikes_attempts+
               contol_accuracy,data=dataformodel,family="binomial")
plot(logistic1)
coef(logistic1)
logistic1
aov(logistic1)
#time for prediction



glm.probs=predict(logistic1,data2021,type="response")
summary(logistic1)
glm.pred=rep("No",498)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2021])
#7 true positives
yfull=cbind(glm.pred,data[data$year>=2021,c("FOTN","fighter","fighter2")])
yfull






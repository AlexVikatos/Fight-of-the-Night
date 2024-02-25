##FULL MODEL
##SCRIPT ONE 

head(dataformodel)
str(dataformodel)
dataformodel$division=as.factor(dataformodel$division)
dataformodel$method=as.factor(dataformodel$method)
dataformodel$FOTN=as.factor(dataformodel$FOTN)
dataformodel$stance=as.factor(dataformodel$stance)
dataformodel$stance2=as.factor(dataformodel$stance2)
dataformodel$gender=as.factor(dataformodel$gender)

##REMOVE STANCE2.1 FROM DATAFORMODEL
# df = subset(mydata, select = -c(x,z) )
df=subset(dataformodel,select = -c(stance2.1))
str(df)
print(df)
full=glm(FOTN~.,data = df,family="binomial")
summary(full)
na.omit(df)




###IT SEEMS LIKE HAVING NAs VALUES AT SOME OF THE INTERCEPTS IS NORMAL IN THIS CASE
###SINCE I HAVE Perfect Colinearity FOR EXAMPLE THE SUM_SIG_STRIKES_LANDED IS DESCRIBED
###BY THE SUM OF SIG_STRIKES_LANDED AND SIG_STRIKES_LANDED2


###WE WILL CONTINUE OUR PREDICTION EVEN WITH THAT "BROKEN" MODEL AND THEN
###WE WILL MAKE A FORWARD VARIABLE SELECTION,A BACKWARDS VARIABLE SELECTION
###A BOOTSTRAP VARIABLE SELECTION AND FINALLY WITH A LASSO.AFTER THAT ITS
###ABOUT MODEL SELECTION TIME.


##PREDICTION 
#training and test subsets##
train=(df$year<2021)
data2021=df[!train,]
head(data2021)

dim(data2021)
table(data2021$FOTN)
data2021


###FULL MODEL

glm.probs=predict(full,data2021,type="response")
summary(full)
glm.pred=rep("No",498)
glm.pred[glm.probs>0.5]="Yes"
glm.pred
#confussion matrix#
table(glm.pred,data$FOTN[data$year>=2021])
#14 true positives so our full model is eligible to find correctly 14 out of 32 "fight of the night" awards.
yfull=cbind(glm.pred,data[data$year>=2021,c("FOTN","fighter","fighter2")])
yfull #see which of these fights.



list=colnames(dataformodel)
list
list=list[1:116]
list
formula_string <- paste("FOTN ~", paste(list, collapse = " + "))
formula_string
dataformodel$FOTN=as.factor(dataformodel$FOTN)
full2=glm(FOTN ~ division + stance + stance2 + method + total_comp_time + round + knockdowns + sub_attempts + reversals + control + takedowns_landed + takedowns_attempts + sig_strikes_landed + sig_strikes_attempts + total_strikes_landed + total_strikes_attempts + head_strikes_landed + head_strikes_attempts + body_strikes_landed + body_strikes_attempts + leg_strikes_landed + leg_strikes_attempts + distance_strikes_landed + distance_strikes_attempts + clinch_strikes_landed + clinch_strikes_attempts + ground_strikes_landed + ground_strikes_attempts + takedowns_accuracy + sig_strikes_accuracy + total_strikes_accuracy + head_strikes_accuracy + body_strikes_accuracy + leg_strikes_accuracy + distance_strikes_accuracy + clinch_strikes_accuracy + ground_strikes_accuracy + takedowns_def + sig_strikes_def + total_strikes_def + sub_attempts_per_min + takedowns_landed_per_min + takedowns_attempts_per_min + sig_strikes_landed_per_min + sig_strikes_attempts_per_min + total_strikes_landed_per_min + total_strikes_attempts_per_min + head_strikes_landed_per_min + head_strikes_attempts_per_min + body_strikes_landed_per_min + body_strikes_attempts_per_min + leg_strikes_landed_per_min + leg_strikes_attempts_per_min + distance_strikes_landed_per_min + distance_strikes_attempts_per_min + clinch_strikes_landed_per_min + clinch_strikes_attempts_per_min + ground_strikes_landed_per_min + ground_strikes_attempts_per_min + stance2.1 + knockdowns2 + sub_attempts2 + reversals2 + control2 + takedowns_landed2 + takedowns_attempts2 + sig_strikes_landed2 + sig_strikes_attempts2 + total_strikes_landed2 + total_strikes_attempts2 + head_strikes_landed2 + head_strikes_attempts2 + body_strikes_landed2 + body_strikes_attempts2 + leg_strikes_landed2 + leg_strikes_attempts2 + distance_strikes_landed2 + distance_strikes_attempts2 + clinch_strikes_landed2 + clinch_strikes_attempts2 + ground_strikes_landed2 + ground_strikes_attempts2 + takedowns_accuracy2 + total_strikes_accuracy2 + sig_strikes_accuracy2 + head_strikes_accuracy2 + body_strikes_accuracy2 + leg_strikes_accuracy2 + distance_strikes_accuracy2 + clinch_strikes_accuracy2 + ground_strikes_accuracy2 + takedowns_def2 + sig_strikes_def2 + total_strikes_def2 + sub_attempts_per_min2 + takedowns_landed_per_min2 + takedowns_attempts_per_min2 + sig_strikes_landed_per_min2 + sig_strikes_attempts_per_min2 + total_strikes_landed_per_min2 + total_strikes_attempts_per_min2 + head_strikes_landed_per_min2 + head_strikes_attempts_per_min2 + body_strikes_landed_per_min2 + body_strikes_attempts_per_min2 + leg_strikes_landed_per_min2 + leg_strikes_attempts_per_min2 + distance_strikes_landed_per_min2 + distance_strikes_attempts_per_min2 + clinch_strikes_landed_per_min2 + clinch_strikes_attempts_per_min2 + ground_strikes_landed_per_min2 + ground_strikes_attempts_per_min2 + year + gender,data =dataformodel ,family="binomial")
summary(full2)

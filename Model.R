##MODELO##
##CHECK CLASS BIAS
table(data$FOTN)
#One way to address the problem of class bias is to draw the 0’s and 1’s 
#for the trainingData (development sample) in equal proportions.
#In doing so, we will put rest of the inputData not included
#for training into testData (validation sample).
#As a result, the size of development sample will be smaller that validation,
#which is okay, because, there are large number of observations over 2000.

dataYES=data[data$FOTN=="Yes",]
dataNO=data[data$FOTN=="No",]
?set.seed()
set.seed(100)

data_YES_training_rows <- sample(1:nrow(dataYES), 0.7*nrow(dataYES))  # FOTN=YES for training
data_NO_training_rows <- sample(1:nrow(dataNO), 0.7*nrow(dataNO))  #FOTN=NOfor training.

#Pick as many NO as YES
training_YES <- dataYES[data_YES_training_rows, ]  
training_NO <- dataNO[data_NO_training_rows, ]
trainingData <- rbind(training_YES, training_NO)  # row bind the 1's and 0's 


##TEST DATA
test_YES <- dataYES[-data_YES_training_rows, ]
test_NO <- dataNO[-data_NO_training_rows, ]
testData <- rbind(test_YES, test_NO)  # row bind the 1's and 0's 




logitMod <- glm(FOTN ~method+total_comp_time+round+knockdowns+sub_attempts+control+
                  takedowns_landed+sig_strikes_landed+total_strikes_landed+FOTN+gender,data=trainingData, family=binomial(link="logit"))
summary(logitMod)
predicted <- predict(logitMod, testData, type="response")

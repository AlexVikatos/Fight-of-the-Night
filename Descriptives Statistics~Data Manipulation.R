#AUTHOR : ALEX VIKATOS



df=read.csv(file.choose())  #UFC_REVAMP2
head(df)
str(df)
#creation of the SUM variables 
for (i in 1:2846){
  df$sum_knockdowns[i]=df$knockdowns[i]+df$knockdowns2[i]
  df$sum_sub_attempts[i]=df$sub_attempts[i]+df$sub_attempts2[i]
  df$sum_takedowns_landed[i]=df$takedowns_landed[i]+df$takedowns_landed2[i]
  df$sum_takedowns_attempts[i]=df$takedowns_attempts[i]+df$takedowns_attempts2[i]
  df$sum_sig_strikes_landed[i]=df$sig_strikes_landed[i]+df$sig_strikes_landed2[i]
  df$sum_sig_strikes_attempts[i]=df$sig_strikes_attempts[i]+df$sig_strikes_attempts2[i]
  df$sum_head_strikes_landed[i]=df$head_strikes_landed[i]+df$head_strikes_attempts2[i]
  df$sum_head_strikes_attempts[i]=df$head_strikes_attempts[i]+df$head_strikes_attempts2[i]
  df$sum_reversals[i]=df$reversals[i]+df$reversals2[i]
  df$sum_total_strikes_landed[i]=df$total_strikes_landed[i]+df$total_strikes_landed2[i]
  df$sum_total_strikes_attempts[i]=df$total_strikes_attempts[i]+df$total_strikes_attempts2[i]
  df$total_control[i]=df$control[i]+df$control2[i]
  df$sum_body_strikes_landed[i]=df$body_strikes_landed[i]+df$body_strikes_landed2[i]
  df$sum_body_strikes_attempts[i]=df$body_strikes_attempts[i]+df$body_strikes_attempts2[i]
  df$sum_leg_strikes_attempts[i]=df$leg_strikes_attempts[i]+df$leg_strikes_attempts2[i]
  df$sum_leg_strikes_landed[i]=df$leg_strikes_landed[i]+df$leg_strikes_landed2[i]
  df$sum_distance_strikes_landed[i]=df$distance_strikes_landed[i]+df$distance_strikes_landed2[i]
  df$sum_distance_strikes_attempts[i]=df$distance_strikes_attempts[i]+df$distance_strikes_attempts2[i]
  df$sum_clinch_strikes_landed[i]=df$clinch_strikes_landed[i]+df$clinch_strikes_landed2[i]
  df$sum_clinch_strikes_attempts[i]=df$clinch_strikes_attempts[i]+df$clinch_strikes_attempts2[i]
  df$sum_ground_strikes_landed[i]=df$ground_strikes_landed2[i]+df$ground_strikes_landed[i]
  df$sum_ground_strikes_attempts[i]=df$ground_strikes_attempts[i]+df$ground_strikes_attempts2[i]
}
str(df)


#control accuracy
for (i in 1:2846){
  df$contol_accuracy[i]=df$total_control[i]/df$total_comp_time[i]
}

######RENAME###
data=df

#epilegw tuxaia 195 paratiriseis 
data_195=data[sample(1:2846,195,replace=FALSE),]
str(data_195)

#ftiaxnw to neo dataset me mono ta FOTN(fight of the night)
FOTNonly=df[df$FOTN=="Yes",]
head(FOTNonly)
FOTNonly$method=as.factor(FOTNonly$method)

#ftiaxnw to neo dataset XWRIS ta FOTN(fight of the night)
noFOTN=df[df$FOTN=="No",]
head(noFOTN)
noFOTN$method=as.factor(noFOTN$method)

#noFOTN alla me 195 paratiriseis
noFOTN_195=noFOTN[sample(1:2652,195,replace=FALSE),]
str(noFOTN_195)

##Descriptives Statistics-relational statistics##
#1.descriptives function gia oles tis posotikes olwn twn dataframes

#2.Barplots gia tis katigorikes metavlites (method,gender)
#kai mones alla kai me tis posotikes(sig_strikes_landed,sub_attemps,takedowns_landed)
#se ola ta dataframes(FOTN,data,noFOTN)


#3. Istogrammata-Density Plots gia tis posotikes(sig_strikes_landed klp )
#alla kai se sundiasmo me tis katigorikes(method,gender,division klp)

#4.SXESEIS ANA 2
#gender~FOTN(2 katigorikes metaksu tous)
#gender~sig_strikes_landed(katigoriki+posotiki)
#gender~takedowns_landed(katigoriki+posotiki)
#FOTN~sig_stikees_landed(katigoriki+posotiki)
#FOTN~takedowns_landed(katigoriki+posotiki)

##POIOS FIGHTER EXEI TA PIO POLLA FOTN 
t=table(FOTNonly$fighter)
FOTNonly[FOTNonly$fighter=="Dustin Poirier",]
##VLEPW OTI EINAI O DUSTIN POIRIER


##ΠΕΡΙΓΡΑΦΙΚΗ ΣΤΑΤΙΣΤΙΚΗ##
descriptives=function(x){
  n=length(x)
  mu=mean(x)
  med=median(x)
  q1=quantile(x,0.25)
  q2=quantile(x,0.75)
  sd=sd(x)
  cv=sd(x)/mean(x)
  kurtosis=sum((x-mu)^4)/(n*sd^4)-3
  symmetry=sum((x-mu)^3)/(n*sd^3)
  d=c(mu,med,q1,q2,sd,cv,kurtosis,symmetry)
  names(d)=c("mesos oros","median","Q1","Q2","SD","CV","kurtosis","symmetry")
  return(d)
}


#1.
#data
descriptives(na.omit(data$total_comp_time))
descriptives(na.omit(data$sum_sig_strikes_landed))
descriptives(na.omit(data$sum_knockdowns))
descriptives(na.omit(data$sum_total_strikes_landed))
descriptives(na.omit(data$sum_head_strikes_landed))

#FOTNonly
descriptives(na.omit(FOTNonly$sum_total_comp_time))
descriptives(na.omit(FOTNonly$sum_sig_strikes_landed))
descriptives(na.omit(FOTNonly$sum_knockdowns))
descriptives(na.omit(FOTNonly$total_strikes_landed))
descriptives(na.omit(FOTNonly$sum_head_strikes_landed))

#noFOTN_195
descriptives(na.omit(noFOTN_195$total_comp_time))
descriptives(na.omit(noFOTN_195$sum_sig_strikes_landed))
descriptives(na.omit(noFOTN_195$sum_knockdowns))
descriptives(na.omit(noFOTN_195$sum_total_strikes_landed))
descriptives(na.omit(noFOTN_195$sum_head_strikes_landed))

#data_195
descriptives(na.omit(data_195$total_comp_time))
descriptives(na.omit(data_195$sum_sig_strikes_landed))
descriptives(na.omit(data_195$sum_knockdowns))
descriptives(na.omit(data_195$sum_total_strikes_landed))
descriptives(na.omit(data_195$sum_head_strikes_landed))

#2
#1 barplots for method kai gender kai FOTN
## data
install.packages("ggthemes")
library(ggthemes)
library(ggplot2)
#barplot gia to posa FIGHTS htan FOTN kai posa oxi apo to 2016-2021
ggplot(data=na.omit(data),aes(x=FOTN))+
  geom_bar(color="red",fill="cadetblue")+
  labs(title="Count of FOTN")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

#barplot gia to posa fights htan male kai posa female apo to 2016-2021
ggplot(data=na.omit(data),aes(x=gender))+
  geom_bar(color="red",fill="cadetblue")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(title="Count of Male and Female Fights")+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

#barplot gia na doume ta way of wins
ggplot(data=na.omit(data),aes(x=method))+
  geom_bar(color="red",fill="cadetblue")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(title="Count of Ways of fight endings")+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

table(data$method=="DQ") #7 dq match endings

#FOTNonly
#barplot FOTNonly~method
ggplot(data=na.omit(FOTNonly),aes(x=method))+
  geom_bar(color="red",fill="cadetblue")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(title="Count of Ways of fight endings only for the FOTN")+
  theme_wsj(color="gray")+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

#barplot FOTNony~gender
ggplot(data=na.omit(FOTNonly),aes(x=gender))+
  geom_bar(color="red",fill="cadetblue")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(title="Count of FOTNs group by Gender")+
  theme_wsj(color="gray")+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
#noFOTN
#barplot noFOTN~gender

ggplot(data=na.omit(noFOTN_195),aes(x=gender))+
  geom_bar(color="red",fill="cadetblue")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(title="Count of noFOTNs group by Gender")+
  theme_wsj(color="grey")+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

#barplot noFOTN~method
ggplot(data=na.omit(noFOTN_195),aes(x=method))+
  geom_bar(color="red",fill="cadetblue")+ geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(title="Count of wins of noFOTNs fights")+
  theme_wsj(color="grey")+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

##COLOR SET UP##
#########################
#########################
#########################
# Load the ggplot2 library
library(ggplot2)
# Get a list of named colors in ggplot2
color_list <- colors()
# Print the list of named colors
print(color_list)

custom_colors <- c("S-DEC" = "palegreen",  # Replace with your desired color codes
                   "U-DEC" = "navyblue",
                   "SUB" = "orange2",
                   "KO/TKO"="red3","M-DEC"="cadetblue")

###################################
###################################
###################################
##FOTN
#barplot gia methods analoga to gender
ggplot(data=na.omit(FOTNonly),aes(x=gender,fill=method))+
  geom_bar(position=position_dodge())+scale_fill_manual(values = custom_colors)+
  geom_text(stat="count",aes(label=..count..),position=position_dodge(width=0.9),vjust=-0.1)+
  theme_wsj()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor=element_blank())+
  labs(title = "Ways of fight endings for FOTNs per Gender",
       y="Count",x="Gender")+facet_wrap(~method)

#DATA
#barplot gia methods analoga to gender
ggplot(data=na.omit(data),aes(x=gender,fill=method))+
  geom_bar(position=position_dodge())+scale_fill_manual(values = custom_colors)+
  geom_text(stat = 'count', aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.1)+theme_wsj()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor=element_blank())+
  labs(title = "Ways of fight endings for FOTNs and NOFOTNs per Gender",
       y="Count",x="Gender")+facet_wrap(~method)


#noFOTN_195
#barplot gia ways of endings of noFOTNS grouped by Gender

ggplot(data=na.omit(noFOTN_195),aes(x=gender,fill=method))+
  geom_bar(position=position_dodge())+scale_fill_manual(values = custom_colors)+
  geom_text(stat = 'count', aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.1)+
  theme_wsj()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor=element_blank())+
  labs(title = "Ways of fight endings for noFOTNs per Gender",
       y="Count",x="Gender")+facet_wrap(~method)

##HISTOGRAMS##
##FOR DATA##
###HISTOGRAMS FOR THE WHOLE DATASET
ggplot(data,aes(x=total_control))+
  geom_density(aes(fill=division),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Division",x="Time of Control",
       y="Frequency")+facet_wrap(~division)


ggplot(data,aes(x=total_control))+
  geom_density(aes(fill=method),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Method",x="Time of Control",
       y="Frequency")+facet_wrap(~method)

ggplot(data,aes(x=total_control))+
  geom_density(aes(fill=gender),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Gender",x="Time of Control",
       y="Frequency")+facet_wrap(~gender)

ggplot(data,aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=gender),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by Gender",x="Significant Strikes Landed",
       y="Frequency")+facet_wrap(~gender)


ggplot(na.omit(data),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=FOTN),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes for FOTN(yes/no)",x="Significant strikes landed",
       y="Frequency")+facet_wrap(~FOTN)

ggplot(na.omit(data),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=round),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by finishing round",x="Significant strikes landed",
       y="Frequency")+facet_wrap(~round)

ggplot(na.omit(data),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=method),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for significant stirkes landed group by method",x="Significant strikes landed",
       y="Frequency")+facet_wrap(~method)

##FOR FOTNonly##
###HISTOGRAMS FOR THE FOTNonly DATASET
ggplot(na.omit(FOTNonly),aes(x=total_control))+
  geom_density(aes(fill=division),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Division",x="Time of Control",
       y="Frequency")+facet_wrap(~division)


ggplot(na.omit(FOTNonly),aes(x=total_control))+
  geom_density(aes(fill=method),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Method",x="Time of Control",
       y="Frequency")+facet_wrap(~method)

ggplot(na.omit(FOTNonly),aes(x=total_control))+
  geom_density(aes(fill=gender),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Gender",x="Time of Control",
       y="Frequency")+facet_wrap(~gender)

ggplot(na.omit(FOTNonly),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=gender),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by Gender",x="Significant Strikes Landed",
       y="Frequency")+facet_wrap(~gender)


ggplot(na.omit(FOTNonly),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=as.factor(round)),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by finishing round",x="Significant strikes landed",
       y="Frequency")

ggplot(na.omit(FOTNonly),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=method),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for significant stirkes landed group by method",x="Significant strikes landed",
       y="Frequency")+facet_wrap(~method)

##FOR noFOTN_195##
###HISTOGRAMS FOR THE noFOTN_195 DATASET
ggplot(na.omit(noFOTN_195),aes(x=total_control))+
  geom_density(aes(fill=division),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Controlgroup by Division",x="Time of Control",
       y="Frequency")


ggplot(na.omit(noFOTN_195),aes(x=total_control))+
  geom_density(aes(fill=method),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Method",x="Time of Control",
       y="Frequency")+facet_wrap(~method)

ggplot(na.omit(noFOTN_195),aes(x=total_control))+
  geom_density(aes(fill=gender),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control group by Gender",x="Time of Control",
       y="Frequency")+facet_wrap(~gender)

ggplot(na.omit(noFOTN_195),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=gender),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by Gender",x="Significant Strikes Landed",
       y="Frequency")+facet_wrap(~gender)


ggplot(na.omit(noFOTN_195),aes(x=sum_sig_strikes_landed))+
  geom_density(aes(fill=as.factor(round)),color="#e9ecef",alpha=0.9)+
  theme_wsj()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by finishing round",x="Significant strikes landed",
       y="Frequency")

ggplot(na.omit(noFOTN_195),aes(x=sig_strikes_landed))+
  geom_density(aes(fill=method),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for significant stirkes landed group by method",x="Significant strikes landed",
       y="Frequency")+facet_wrap(~method)

#4.SXESEIS ANA 2
#gender~FOTN(2 katigorikes metaksu tous)
#gender~sig_strikes_landed(katigoriki+posotiki)
#gender~takedowns_landed(katigoriki+posotiki)
#FOTN~sig_stikees_landed(katigoriki+posotiki)
#FOTN~takedowns_landed(katigoriki+posotiki)
#FOTN~time_spent_standing(katigoriki+posotiki)
#time_spent_standing~sig_strikes_landing(posotiki+posotiki)
library(ggplot2)
##1.gender~sig_strikes_landed##
#for data
library(nortest)
by(data$sum_sig_strikes_landed,data$gender,lillie.test)
#no normal
wilcox.test(sum_sig_strikes_landed~gender,data=data)
#aporiptw Ho ara uparxei megalh diafora metaksu 
#twn significant strikes lander metaksu twn male kai female fights
boxplot(data$sig_strikes_landed~data$gender,xlab="Gender",ylab="significant strikes",
        col=c("cadetblue","orange"),main="Boxplot for Sig_strikes group by Gender")
legend("topright",legend = c("Men","Women"),fill=c("cadetblue","orange"))
median(data$sum_sig_strikes_landed[data$gender=="Men"])#70
median(data$sum_sig_strikes_landed[data$gender=="Women"]) #95
median(data$sum_total_strikes_landed[data$gender=="Men"]) #106
median(data$sum_total_strikes_landed[data$gender=="Women"]) #153

#paratirw oti pio polles akraies times exoun ta Men fights 
#megalitero IQR exoun ta women fights kata ligo
#kai h diamesos twn women fights einai ligo pio psila 

#2.gender~takedowns_landed
#for data
library(nortest)
by(data$sum_takedowns_landed,data$gender,lillie.test)
#no_normal
wilcox.test(sum_takedowns_landed~gender,data=data)
#aporiptw Ho ara uparxei diafora stis diamesous
ggplot(data, aes(x = gender, y = sum_takedowns_landed, fill = gender)) +
  geom_boxplot() +theme_wsj()+
  scale_fill_manual(values = c("Men" = "cadetblue", "Women" = "red")) +
  labs(x = "Gender", y = "Takedowns Landed", title = "Boxplot for Takedowns Landed grouped by Gender")

#3.FOTN~sig_strikes_landed
#data
library(ggthemes)
by(data$sum_sig_strikes_landed,data$FOTN,lillie.test)
#aporiptw thn kanonikotita
wilcox.test(sum_sig_strikes_landed~FOTN,data=data)
#aporiptw Ho ara diafora sta sig strikes gia gia noFOTN kai FOTN fights
ggplot(na.omit(data),aes(x=FOTN,y=sum_sig_strikes_landed,fill=FOTN))+
  geom_boxplot()+labs(title="Significant Strikes: Fight of the Night (FOTN) vs. Non-FOTN - Boxplot Analysis")+theme_stata()+
  scale_fill_manual(values=c("Yes"="cadetblue","No"="red"))+
  labs(x="Fight of the Night",y="Significant strikes landed")


#4.FOTN~takedowns_landed
by(data$sum_takedowns_landed,data$FOTN,lillie.test)
#aporiptw kanonikotita
wilcox.test(sum_takedowns_landed~FOTN,data=data)
#den aporiptw Ho ara de uparxei diafora diameswn
ggplot(na.omit(data),aes(x=FOTN,y=takedowns_landed,fill=FOTN))+
  geom_boxplot() +theme_wsj()+labs(title = "Takedowns landed group by FOTN")  

#5.FOTN~sub_attemps
by(data$sum_sub_attempts,data$FOTN,lillie.test)
#aporiptw Ho ara no-normal
wilcox.test(sum_sub_attempts~FOTN,data=data)
#den aporiptw Ho ara den uparxei diafora diameswn
ggplot(na.omit(data), aes(x = FOTN, y = sub_attempts, fill = FOTN)) +
  geom_boxplot() +theme_wsj()+
  scale_fill_manual(values = c("Yes" = "cadetblue", "No" = "red")) +
  labs(x = "FOTN", y = "Submission Attempts", title = "Submission Attempts groub by FOTN")
#genika paratiroume oti ta submission attempts einai polu liga kai stis 2 periptwseis
#alla sta FOTN paratiroume elaxista kai malista ektos apo kapoies akraies times
#kai to IQR einai mideniko


cor.test(data$total_control,data$sum_sig_strikes_landed,method = "spearman")

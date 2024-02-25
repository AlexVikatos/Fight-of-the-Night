ufc=read.csv(file.choose(),header=T)
ufc$year <- as.numeric(substr(ufc$date, start = nchar(ufc$date) - 3, stop = nchar(ufc$date)))
head(ufc)


##ARXIZW KAI FTIAXNW TO DATA SET GIA TA YEARS 2016,2017,2018,2019,2020 kai 2021
str(ufc)
ufcnew=ufc[ufc$year==2016|ufc$year==2017|ufc$year==2018|ufc$year==2019|ufc$year==2020|ufc$year==2021,]
head(ufcnew)
#paratirw oti exw diploeggrafes opote tha diagrapsw tis grammes pou exoun result=0 
ufcnew=ufcnew[!ufcnew$result==0,]
ufcnew

ufcnew[ufcnew$fighter=="Khabib Nurmagomedov",]
ufcnew
###write.csv(ufcnew, "C:\\Users\\ALEX\\Desktop\\ufcnew.csv", row.names=FALSE) ALLAKSA LAPTOP OPOTE ALLAZEI TO PATH




#afou eftiaksa to neo dataset apo to excel giati prosthesa mia nea metavliti to FOTN(fight of the night) 
data=read.csv(file.choose(),header=T)
str(data)
head(data)
na.omit(data)
data$FOTN=as.factor(data$FOTN)
levels(data$FOTN)=c("No","Yes")
head(data)
data$gender=as.factor(ifelse(substr(data$division,1,5)=="Women","Women","Men"))
data$year=as.factor(data$year)
head(data)
str(data)

##ftiaxnw to teliko excel file pou tha doulepsw
#write.csv(data,"C:\\Users\\USER\\Desktop\\data.csv", row.names=FALSE)

## CREATION OF NEW VARIABLES AND DELETING VARIABLES I DONT NEED ##
data$insignificant_strikes_landed=data$total_strikes_landed-data$sig_strikes_landed

data=data[,-c(15,17,19,21,23,25,29,31,32,33)]
data=data[,-21]
str(data)

data=read.csv(file.choose())


#ftiaxnw to neo dataset me mono ta FOTN(fight of the night)
FOTNonly=data[data$FOTN=="Yes",]
head(FOTNonly)
FOTNonly$method=as.factor(FOTNonly$method)

#ftiaxnw to neo dataset XWRIS ta FOTN(fight of the night)
noFOTN=data[data$FOTN=="No",]
head(noFOTN)
noFOTN$method=as.factor(noFOTN$method)


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

#GIA TO FOTNonly DATASET
descriptives(na.omit(FOTNonly$knockdowns))
descriptives(na.omit(FOTNonly$sub_attempts))
descriptives(na.omit(FOTNonly$control))
descriptives(na.omit(FOTNonly$takedowns_landed))
descriptives(na.omit(FOTNonly$sig_strikes_landed))
descriptives(na.omit(FOTNonly$total_strikes_landed))
descriptives(na.omit(FOTNonly$head_strikes_landed))
descriptives(na.omit(FOTNonly$body_strikes_landed))
descriptives(na.omit(FOTNonly$leg_strikes_landed))
descriptives(na.omit(FOTNonly$clinch_strikes_landed))
descriptives(na.omit(FOTNonly$ground_strikes_landed))
tab1=prop.table(table(FOTNonly$method))
tab=prop.table(table(FOTNonly$gender))
barplot(tab1)
barplot(tab)




install.packages("ggplot2")
library(ggplot2)
##BARPLOT METHOD OF WINS
ggplot(data=na.omit(FOTNonly), aes(x=method)) +
  geom_bar(color="cadetblue",fill=rgb(0.3219,0.3,0.5,0.7)) +
  labs(title="Count of Win Methods",
       x="Method",
       y="Count")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
##BARPLOT GENDER COUNT
ggplot(data=na.omit(FOTNonly),aes(x=gender))+
  geom_bar(color="blue",fill=rgb(0.1,0.3,0.5,0.7))+
  labs(title="FOTN per GENDER",
       x="Gender",y="Count")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
##BARPLOT GIA FOTN H OXI STO DATA
ggplot(data=na.omit(data),aes(x=FOTN))+
  geom_bar(color="green")+
  labs(title="Count of FOTN")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())


##COUNT OF FOTN PER YEAR
?aggregate()
FOTNCOUNT=aggregate(FOTN~year,data=FOTNonly,length)
ggplot(data=na.omit(FOTNonly),aes(x=as.factor(year)))+
  geom_bar(color="green",fill=rgb(1,2,3,4,5,6))+
  labs(title = "Number of Wins by Method per Year",
      y="Count",x="Year")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

##HISTOGRAMS FOR FOTNonly##

ggplot(FOTNonly,aes(x=knockdowns))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Knockdowns",x="Number of Knockdowns",
       y="Frequency")

ggplot(FOTNonly,aes(x=control))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(FOTNonly,aes(x=sig_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant Strikes Landed",x="Number of Significant Stikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=takedowns_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Takedowns Landed",x="Number of Takedowns Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=head_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Headstrikes Landed",x="Number of Headstrikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=total_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Total Strikes Landed",x="Number of Total Strikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=body_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Bodystrikes Landed",x="Number of Bodystrikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=leg_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Legstrikes Landed",x="Number of Legstrikes  Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=distance_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Distance Strikes Landed",x="Number of Distance Strikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=ground_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Ground Strikes Landed",x="Number of Ground Strikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=clinch_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Clinch Strikes Landed",x="Number of Clinch Strikes Landed",
       y="Frequency")

ggplot(FOTNonly,aes(x=ground_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Ground Strikes Landed",x="Number of Ground Strikes Landed",
       y="Frequency")


#GIA TO data DATASET
#epilegw tuxaia 195 paratiriseis 
data_195=data[sample(1:2846,195,replace=FALSE),]
str(data_195)


descriptives(na.omit(data_195$knockdowns))
descriptives(na.omit(data_195$sub_attempts))
descriptives(na.omit(data_195$control))
descriptives(na.omit(data_195$takedowns_landed))
descriptives(na.omit(data_195$sig_strikes_landed))
descriptives(na.omit(data_195$total_strikes_landed))
descriptives(na.omit(data_195$head_strikes_landed))
descriptives(na.omit(data_195$body_strikes_landed))
descriptives(na.omit(data_195$leg_strikes_landed))
descriptives(na.omit(data_195$clinch_strikes_landed))
descriptives(na.omit(data_195$ground_strikes_landed))
tab1=prop.table(table(data_195$method))
tab=prop.table(table(data_195$gender))


library(ggplot2)
##BARPLOT METHOD OF WINS FOR DATA_195
ggplot(data=na.omit(data_195), aes(x=method)) +
  geom_bar(color="cadetblue",fill=rgb(0.3219,0.3,0.5,0.7)) +
  labs(title="Count of Win Methods",
       x="Method",
       y="Count")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
##BARPLOT GENDER COUNT
ggplot(data=na.omit(data_195),aes(x=gender))+
  geom_bar(color="blue",fill=rgb(0.1,0.3,0.5,0.7))+
  labs(title="FOTN per GENDER",
       x="Gender",y="Count")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
##BARPLOT GIA FOTN H OXI STO DATA
ggplot(data=na.omit(data),aes(x=FOTN))+
  geom_bar(color="green")+
  labs(title="Count of FOTN")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())


##COUNT OF FOTN PER YEAR
?aggregate()
FOTNCOUNT=aggregate(FOTN~year,data=FOTNonly,length)
FOTNCOUNT
ggplot(data=na.omit(FOTNonly),aes(x=as.factor(year)))+
  geom_bar(color="green",fill=rgb(1,2,3,4,5,6))+
  labs(title = "Number of Wins by Method per Year",
       y="Count",x="Year")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

##HISTOGRAMS FOR data_195##

ggplot(data_195,aes(x=knockdowns))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Knockdowns",x="Number of Knockdowns",
       y="Frequency")

ggplot(data_195,aes(x=control))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data_195,aes(x=control))+
  geom_density(aes(fill=factor(gender)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data_195,aes(x=control))+
  geom_density(aes(fill=factor(division)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data_195,aes(x=control))+
  geom_density(aes(fill=factor(stance)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data_195,aes(x=sig_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant Strikes Landed",x="Number of Significant Stikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=takedowns_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Takedowns Landed",x="Number of Takedowns Landed",
       y="Frequency")

ggplot(data_195,aes(x=head_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Headstrikes Landed",x="Number of Headstrikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=total_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Total Strikes Landed",x="Number of Total Strikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=body_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Bodystrikes Landed",x="Number of Bodystrikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=leg_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Legstrikes Landed",x="Number of Legstrikes  Landed",
       y="Frequency")

ggplot(data_195,aes(x=distance_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Distance Strikes Landed",x="Number of Distance Strikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=ground_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Ground Strikes Landed",x="Number of Ground Strikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=clinch_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Clinch Strikes Landed",x="Number of Clinch Strikes Landed",
       y="Frequency")

ggplot(data_195,aes(x=ground_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Ground Strikes Landed",x="Number of Ground Strikes Landed",
       y="Frequency")
  
#GIA TO noFOTN DATASET
str(noFOTN)
noFOTN_195=noFOTN[sample(1:2652,195,replace=FALSE),]
str(noFOTN_195)



descriptives(na.omit(noFOTN_195$knockdowns))
descriptives(na.omit(noFOTN_195$sub_attempts))
descriptives(na.omit(noFOTN_195$control))
descriptives(na.omit(noFOTN_195$takedowns_landed))
descriptives(na.omit(noFOTN_195$sig_strikes_landed))
descriptives(na.omit(noFOTN_195$total_strikes_landed))
descriptives(na.omit(noFOTN_195$head_strikes_landed))
descriptives(na.omit(noFOTN_195$body_strikes_landed))
descriptives(na.omit(noFOTN_195$leg_strikes_landed))
descriptives(na.omit(noFOTN_195$clinch_strikes_landed))
descriptives(na.omit(noFOTN_195$ground_strikes_landed))
tab1=prop.table(table(noFOTN_195$method))
tab=prop.table(table(noFOTN_195$gender))


library(ggplot2)
##BARPLOT METHOD OF WINS FOR DATA_195
ggplot(data=na.omit(data_195), aes(x=method)) +
  geom_bar(color="cadetblue",fill=rgb(0.3219,0.3,0.5,0.7)) +
  labs(title="Count of Win Methods",
       x="Method",
       y="Count")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
##BARPLOT GENDER COUNT
ggplot(data=na.omit(data_195),aes(x=gender))+
  geom_bar(color="blue",fill=rgb(0.1,0.3,0.5,0.7))+
  labs(title="FOTN per GENDER",
       x="Gender",y="Count")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())
##BARPLOT GIA FOTN H OXI STO DATA
ggplot(data=na.omit(data),aes(x=FOTN))+
  geom_bar(color="green")+
  labs(title="Count of FOTN")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())


##COUNT OF FOTN PER YEAR
?aggregate()
FOTNCOUNT=aggregate(FOTN~year,data=FOTNonly,length)
FOTNCOUNT
ggplot(data=na.omit(FOTNonly),aes(x=as.factor(year)))+
  geom_bar(color="green",fill=rgb(1,2,3,4,5,6))+
  labs(title = "Number of Wins by Method per Year",
       y="Count",x="Year")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())

##HISTOGRAMS FOR data_195##

ggplot(noFOTN_195,aes(x=knockdowns))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Knockdowns",x="Number of Knockdowns",
       y="Frequency")

ggplot(noFOTN_195,aes(x=control))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(noFOTN_195,aes(x=sig_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant Strikes Landed",x="Number of Significant Stikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=takedowns_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Takedowns Landed",x="Number of Takedowns Landed",
       y="Frequency")

ggplot(data_195,aes(x=head_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Headstrikes Landed",x="Number of Headstrikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=total_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Total Strikes Landed",x="Number of Total Strikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=body_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Bodystrikes Landed",x="Number of Bodystrikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=leg_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Legstrikes Landed",x="Number of Legstrikes  Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=distance_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Distance Strikes Landed",x="Number of Distance Strikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=ground_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Ground Strikes Landed",x="Number of Ground Strikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=clinch_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Clinch Strikes Landed",x="Number of Clinch Strikes Landed",
       y="Frequency")

ggplot(noFOTN_195,aes(x=ground_strikes_landed))+
  geom_histogram(fill="#69b3a2",color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram of Ground Strikes Landed",x="Number of Ground Strikes Landed",
       y="Frequency")




###HISTOGRAMS FOR THE WHOLE DATASET
ggplot(data,aes(x=control))+
  geom_density(aes(fill=factor(division)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data,aes(x=control))+
  geom_density(aes(fill=factor(na.omit(stance))),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data,aes(x=control))+
  geom_density(aes(fill=factor(method)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data,aes(x=control))+
  geom_density(aes(fill=factor(gender)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Control",x="Time of Control",
       y="Frequency")

ggplot(data,aes(x=sig_strikes_landed))+
  geom_density(aes(fill=factor(gender)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes per Gender",x="Significant Strikes Landed",
       y="Frequency")


ggplot(data,aes(x=sig_strikes_landed))+
  geom_density(aes(fill=factor(FOTN)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes for FOTN(yes/no)",x="Significant strikes landed",
       y="Frequency")

ggplot(data,aes(x=sig_strikes_landed))+
  geom_density(aes(fill=factor(round)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for Significant strikes group by finishing round",x="Significant strikes landed",
       y="Frequency")

ggplot(data,aes(x=sig_strikes_landed))+
  geom_density(aes(fill=factor(method)),color="#e9ecef",alpha=0.9)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor=element_blank())+
  labs(title="Histogram for significant stirkes landed per method of win",x="Significant strikes landed",
       y="Frequency")

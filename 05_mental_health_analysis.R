rm(list=ls())
library(FactoMineR)
library(sandwich)
library(aod)
library(haven)
library(AER)
library(factoextra)
library(dplyr)
library(stargazer)

setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\processed_data')
full.data.set<-read.csv('mental_health_labor_mkt.CSV')

#subset to get rid of null values in the GHQs
full.data.set<-subset(full.data.set,GHQ1!=(-1))
full.data.set<-subset(full.data.set,GHQ1!=(-2))
full.data.set<-subset(full.data.set,GHQ1!=(-9))
full.data.set<-subset(full.data.set,GHQ1!=(-8))
full.data.set<-subset(full.data.set,GHQ1!=(-7))

full.data.set<-subset(full.data.set,GHQ2!=(-1))
full.data.set<-subset(full.data.set,GHQ2!=(-2))
full.data.set<-subset(full.data.set,GHQ2!=(-9))
full.data.set<-subset(full.data.set,GHQ2!=(-8))
full.data.set<-subset(full.data.set,GHQ2!=(-7))

full.data.set<-subset(full.data.set,GHQ3!=(-1))
full.data.set<-subset(full.data.set,GHQ3!=(-2))
full.data.set<-subset(full.data.set,GHQ3!=(-9))
full.data.set<-subset(full.data.set,GHQ3!=(-8))
full.data.set<-subset(full.data.set,GHQ3!=(-7))

full.data.set<-subset(full.data.set,GHQ4!=(-1))
full.data.set<-subset(full.data.set,GHQ4!=(-2))
full.data.set<-subset(full.data.set,GHQ4!=(-9))
full.data.set<-subset(full.data.set,GHQ4!=(-8))
full.data.set<-subset(full.data.set,GHQ4!=(-7))

full.data.set<-subset(full.data.set,GHQ5!=(-1))
full.data.set<-subset(full.data.set,GHQ5!=(-2))
full.data.set<-subset(full.data.set,GHQ5!=(-9))
full.data.set<-subset(full.data.set,GHQ5!=(-8))
full.data.set<-subset(full.data.set,GHQ5!=(-7))

full.data.set<-subset(full.data.set,GHQ6!=(-1))
full.data.set<-subset(full.data.set,GHQ6!=(-2))
full.data.set<-subset(full.data.set,GHQ6!=(-9))
full.data.set<-subset(full.data.set,GHQ6!=(-8))
full.data.set<-subset(full.data.set,GHQ6!=(-7))

full.data.set<-subset(full.data.set,GHQ7!=(-1))
full.data.set<-subset(full.data.set,GHQ7!=(-2))
full.data.set<-subset(full.data.set,GHQ7!=(-9))
full.data.set<-subset(full.data.set,GHQ7!=(-8))
full.data.set<-subset(full.data.set,GHQ7!=(-7))

full.data.set<-subset(full.data.set,GHQ8!=(-1))
full.data.set<-subset(full.data.set,GHQ8!=(-2))
full.data.set<-subset(full.data.set,GHQ8!=(-9))
full.data.set<-subset(full.data.set,GHQ8!=(-8))
full.data.set<-subset(full.data.set,GHQ8!=(-7))

full.data.set<-subset(full.data.set,GHQ9!=(-1))
full.data.set<-subset(full.data.set,GHQ9!=(-2))
full.data.set<-subset(full.data.set,GHQ9!=(-9))
full.data.set<-subset(full.data.set,GHQ9!=(-8))
full.data.set<-subset(full.data.set,GHQ9!=(-7))

full.data.set<-subset(full.data.set,GHQ10!=(-1))
full.data.set<-subset(full.data.set,GHQ10!=(-2))
full.data.set<-subset(full.data.set,GHQ10!=(-9))
full.data.set<-subset(full.data.set,GHQ10!=(-8))
full.data.set<-subset(full.data.set,GHQ10!=(-7))

full.data.set<-subset(full.data.set,GHQ11!=(-1))
full.data.set<-subset(full.data.set,GHQ11!=(-2))
full.data.set<-subset(full.data.set,GHQ11!=(-9))
full.data.set<-subset(full.data.set,GHQ11!=(-8))
full.data.set<-subset(full.data.set,GHQ11!=(-7))

full.data.set<-subset(full.data.set,GHQ12!=(-1))
full.data.set<-subset(full.data.set,GHQ12!=(-2))
full.data.set<-subset(full.data.set,GHQ12!=(-9))
full.data.set<-subset(full.data.set,GHQ12!=(-8))
full.data.set<-subset(full.data.set,GHQ12!=(-7))

#also get rid of emplyoment nulls
full.data.set<-subset(full.data.set,employment_stat!=97)
full.data.set<-subset(full.data.set,occupation>0)

#sex race and marital nulls
full.data.set<-subset(full.data.set,occupation>0)
full.data.set<-subset(full.data.set,marital_stat>0)

######################################################################
#This section of code is going to create some mental health variables#
#Gathergood's measure
#Gathergood's measure with a different cutoff
#Gathergood's measure with quantiles
######################################################################


mental.health.vars<-as.data.frame(matrix(nrow=nrow(full.data.set),ncol=4))
colnames(mental.health.vars)<-c('GG','GG_diff_cutoff','GG_quantile','PCA')

#get the GG measure
#convert these to ones and zeros (need to subtract 2 first to make sure that we're adjusting for the factors)
GQ1.norm<-round((full.data.set[,'GHQ1'])/4)
GQ2.norm<-round((full.data.set[,'GHQ2'])/4)
GQ3.norm<-round((full.data.set[,'GHQ3'])/4)
GQ4.norm<-round((full.data.set[,'GHQ4'])/4)
GQ5.norm<-round((full.data.set[,'GHQ5'])/4)
GQ6.norm<-round((full.data.set[,'GHQ6'])/4)
GQ7.norm<-round((full.data.set[,'GHQ7'])/4)
GQ8.norm<-round((full.data.set[,'GHQ8'])/4)
GQ9.norm<-round((full.data.set[,'GHQ9'])/4)
GQ10.norm<-round((full.data.set[,'GHQ10'])/4)
GQ11.norm<-round((full.data.set[,'GHQ11'])/4)
GQ12.norm<-round((full.data.set[,'GHQ12'])/4)

#thereby get a 'mental health' variable
mental.health.vars[,'GG']<-as.data.frame(GQ1.norm+GQ2.norm+GQ3.norm+GQ4.norm+GQ5.norm+
                                           GQ6.norm+GQ7.norm+GQ8.norm+GQ9.norm+GQ10.norm+
                                           GQ11.norm+GQ12.norm)
summary(mental.health.vars[,'GG'])
hist(mental.health.vars[,'GG'])

#get the GG_diff_cutoff measure 
#convert these to ones and zeros (need to subtract 2 first to make sure that we're adjusting for the factors)
GQ1.norm<-round((full.data.set[,'GHQ1'])/3)
GQ2.norm<-round((full.data.set[,'GHQ2'])/3)
GQ3.norm<-round((full.data.set[,'GHQ3'])/3)
GQ4.norm<-round((full.data.set[,'GHQ4'])/3)
GQ5.norm<-round((full.data.set[,'GHQ5'])/3)
GQ6.norm<-round((full.data.set[,'GHQ6'])/3)
GQ7.norm<-round((full.data.set[,'GHQ7'])/3)
GQ8.norm<-round((full.data.set[,'GHQ8'])/3)
GQ9.norm<-round((full.data.set[,'GHQ9'])/3)
GQ10.norm<-round((full.data.set[,'GHQ10'])/3)
GQ11.norm<-round((full.data.set[,'GHQ11'])/3)
GQ12.norm<-round((full.data.set[,'GHQ12'])/3)

#thereby get a 'mental health' variable
mental.health.vars[,'GG_diff_cutoff']<-as.data.frame(GQ1.norm+GQ2.norm+GQ3.norm+GQ4.norm+GQ5.norm+
                                           GQ6.norm+GQ7.norm+GQ8.norm+GQ9.norm+GQ10.norm+
                                           GQ11.norm+GQ12.norm)
summary(mental.health.vars[,'GG_diff_cutoff'])
hist(mental.health.vars[,'GG_diff_cutoff'])

#get the GG quantile measure 
#convert these to ones and zeros (need to subtract 2 first to make sure that we're adjusting for the factors)
GQ1.norm<-as.numeric(full.data.set[,'GHQ1']>median(unlist(full.data.set[,'GHQ1'])))
GQ2.norm<-as.numeric(full.data.set[,'GHQ2']>median(unlist(full.data.set[,'GHQ2'])))
GQ3.norm<-as.numeric(full.data.set[,'GHQ3']>median(unlist(full.data.set[,'GHQ3'])))
GQ4.norm<-as.numeric(full.data.set[,'GHQ4']>median(unlist(full.data.set[,'GHQ4'])))
GQ5.norm<-as.numeric(full.data.set[,'GHQ5']>median(unlist(full.data.set[,'GHQ5'])))
GQ6.norm<-as.numeric(full.data.set[,'GHQ6']>median(unlist(full.data.set[,'GHQ6'])))
GQ7.norm<-as.numeric(full.data.set[,'GHQ7']>median(unlist(full.data.set[,'GHQ7'])))
GQ8.norm<-as.numeric(full.data.set[,'GHQ8']>median(unlist(full.data.set[,'GHQ8'])))
GQ9.norm<-as.numeric(full.data.set[,'GHQ9']>median(unlist(full.data.set[,'GHQ9'])))
GQ10.norm<-as.numeric(full.data.set[,'GHQ10']>median(unlist(full.data.set[,'GHQ10'])))
GQ11.norm<-as.numeric(full.data.set[,'GHQ11']>median(unlist(full.data.set[,'GHQ11'])))
GQ12.norm<-as.numeric(full.data.set[,'GHQ12']>median(unlist(full.data.set[,'GHQ12'])))

#thereby get a 'mental health' variable
mental.health.vars[,'GG_quantile']<-as.data.frame(GQ1.norm+GQ2.norm+GQ3.norm+GQ4.norm+GQ5.norm+
                                                       GQ6.norm+GQ7.norm+GQ8.norm+GQ9.norm+GQ10.norm+
                                                       GQ11.norm+GQ12.norm)
summary(mental.health.vars[,'GG_quantile'])
hist(mental.health.vars[,'GG_quantile'])



#sense check: what does a PCA say
PCA.mental.health<-prcomp(full.data.set[,5:16])
mental.health.vars[,'PCA']<-PCA.mental.health$x[,1]

#correlation matrices
cor.matrix<-matrix(ncol=4,nrow=4)
for (val in seq(from=1,to=4)){
  for (val.2 in seq(from=1,to=4)){
    cor.matrix[val,val.2]<-cor(mental.health.vars[,val],mental.health.vars[,val.2])
  }
}

#stitch this on
full.data.set<-cbind(full.data.set,mental.health.vars)

##############################################

#I also want to check against the jpy measure#

##############################################
full.data.set[,'wave_number']<-as.numeric(full.data.set[,'wave_number'])
full.data.set[,'JPY_measure']<--full.data.set[,'JPY_measure']
US.data.set<-subset(full.data.set,as.numeric(wave_number)>18)

mental.health.vars.US<-select(US.data.set,JPY_measure:PCA)

cor.matrix<-matrix(ncol=5,nrow=5)
for (val in seq(from=1,to=5)){
  for (val.2 in seq(from=1,to=5)){
    cor.matrix[val,val.2]<-cor(mental.health.vars.US[,val],mental.health.vars.US[,val.2])
  }
}

colnames(cor.matrix)<-colnames(mental.health.vars.US)
rownames(cor.matrix)<-colnames(mental.health.vars.US)

#now subset to get rid of NAs in age
full.data.set<-subset(full.data.set,age>=25)
full.data.set<-subset(full.data.set,age<=75)

############################################

#more subsetting and adding in some dummies#

############################################

full.data.set<-subset(full.data.set,is.na(race)==FALSE)
full.data.set<-subset(full.data.set,race>0)
full.data.set<-subset(full.data.set,education>0)
full.data.set<-subset(full.data.set,education<96)
full.data.set<-subset(full.data.set,helps_disables>=0)
full.data.set<-subset(full.data.set,hh_income>=0)
full.data.set<-subset(full.data.set,household_ownerships>=0)
full.data.set<-subset(full.data.set,no_kids>=0)

#need to create some race dummies: black, Asian
race.dummies<-as.data.frame(matrix(data=0,nrow=nrow(full.data.set),ncol=2))
colnames(race.dummies)<-c('black','asian')
race.dummies[,'black']<-ifelse((full.data.set[,'race']==2|full.data.set[,'race']==3|full.data.set[,'race']==4)&full.data.set[,'wave_number']<19,1,race.dummies[,'black'])
race.dummies[,'black']<-ifelse((full.data.set[,'race']==14|full.data.set[,'race']==15|full.data.set[,'race']==16)&full.data.set[,'wave_number']>18,1,race.dummies[,'black'])

race.dummies[,'asian']<-ifelse((full.data.set[,'race']==5|full.data.set[,'race']==6|
                                   full.data.set[,'race']==7|full.data.set[,'race']==8)&full.data.set[,'wave_number']<19,1,race.dummies[,'asian'])
race.dummies[,'asian']<-ifelse((full.data.set[,'race']==9|full.data.set[,'race']==10|
                                   full.data.set[,'race']==11|full.data.set[,'race']==12)&full.data.set[,'wave_number']>18,1,race.dummies[,'asian'])

#create a home_owner dummy
home.owner.dummy<-as.data.frame(matrix(data=0,nrow=nrow(full.data.set),ncol=1))
colnames(home.owner.dummy)<-'home_owner_dummy'
home.owner.dummy[,'home_owner_dummy']<-ifelse((full.data.set[,'household_ownerships']==1|full.data.set[,'household_ownerships']==2)&full.data.set[,'wave_number']<19,1,home.owner.dummy[,'home_owner_dummy'])
home.owner.dummy[,'home_owner_dummy']<-ifelse((full.data.set[,'household_ownerships']==1|full.data.set[,'household_ownerships']==2|full.data.set[,'household_ownerships']==3
)&full.data.set[,'wave_number']>18,1,home.owner.dummy[,'home_owner_dummy'])

#and an 'adjusted hh_income'
adjusted.hh.income<-as.data.frame((full.data.set[,'hh_income']-full.data.set[,'monthly_labor_income'])*full.data.set[,'conversion_factor'])
colnames(adjusted.hh.income)<-'adjusted_hh_income'

full.data.set<-cbind(full.data.set, race.dummies ,home.owner.dummy,adjusted.hh.income)

#okay. I want to see some basic correlations

#start by scaling all the variables to have the same mean and variance

for (val in seq(from=1,to=5,by=1)){
  #create a dataset to use in the regression
  var<-colnames(mental.health.vars.US)[val]
  #calculate mean and stdev
  df<-full.data.set[complete.cases(full.data.set[,var]),var]
  stand.dev<-sqrt(var(df))
  full.data.set[,var]<-full.data.set[,var]/stand.dev
  df<-full.data.set[complete.cases(full.data.set[,var]),var]
  mean.df<-mean(df)
  full.data.set[,var]<-full.data.set[,var]-mean.df+0.5
 
}

#I want to replace the above 6's for the JPY measure with NAs
full.data.set[,'JPY_measure']<-ifelse(full.data.set[,'JPY_measure']>=5.9,NA,full.data.set[,'JPY_measure'])


#################
#graph the distributions
png('histograms_mh.png')
par(mfrow=c(2,3))
hist(full.data.set[,'JPY_measure'],main='SF12')
hist(full.data.set[,'GG'],main='cutoff 2')
hist(full.data.set[,'GG_diff_cutoff'],main='cutoff 1')
hist(full.data.set[,'GG_quantile'],main='cutoff median')
hist(full.data.set[,'PCA'],main='PCA')
dev.off()
#################

#######################################################################
#first create a regression which shows age effects, job status effects#
#######################################################################

age.factor<-as.data.frame(as.factor(full.data.set[,'age']))
colnames(age.factor)<-'age_factor'
full.data.set<-cbind(full.data.set,age.factor)
wave.factor<-as.data.frame(as.factor(full.data.set[,'wave_number']))
colnames(wave.factor)<-'wave_factor'
full.data.set<-cbind(full.data.set,wave.factor)

png('age_effects_mental_health.png')
plot(main='age effects on mental health',xlab='age',ylab='mental health',type='n',x=0,y=0,xlim=c(1,51),ylim=c(-0.5,1.5))
col.vector<-c('red','blue','green','black','purple')

#for each of my measures of mental health
for (val in seq(from=1,to=5,by=1)){
#create a dataset to use in the regression
var<-colnames(mental.health.vars.US)[val]  
  
df<-full.data.set
colnames(df)[match(var,colnames(df))]<-'mental_health'

#get rid of NAs
df[,'mental_health']<-ifelse(is.na(df[,'mental_health'])==FALSE,df[,'mental_health'],-100)
df<-subset(df,mental_health>-100)

#run a regression
age.profile.regression<-lm(mental_health~factor(age_factor)+factor(wave_factor),data=df)

coefs<-age.profile.regression$coefficients
coefs[2:52]<-coefs[2:52]+coefs[1]
points(coefs[1:51],col=col.vector[val])

}
mh.lables<-c('SF12', 'cutoff 2','cutoff 1','cutoff med', 'PCA')
legend('topright',legend=mh.lables,fill=col.vector)
dev.off()

#create some dummies
job.stat.dummies<-as.data.frame(matrix(data=0,nrow=nrow(full.data.set),ncol=2))
colnames(job.stat.dummies)<-c('unemployment','sick')                                
job.stat.dummies[,'unemployment']<-ifelse(full.data.set[,'employment_stat']==3,1,0)
job.stat.dummies[,'sick']<-ifelse(full.data.set[,'employment_stat']==8,1,0)
full.data.set<-cbind(full.data.set,job.stat.dummies)

#also create an age-squared
age.squared<-as.data.frame(full.data.set[,'age']^2)
colnames(age.squared)<-'age_squared'
full.data.set<-cbind(full.data.set,age.squared)

#also create a matrix to store regression results
job.stat.results<-matrix(nrow=5,ncol=4)

#for each of my measures of mental health
for (val in seq(from=1,to=5,by=1)){
  #create a dataset to use in the regression
  var<-colnames(mental.health.vars.US)[val]  
  
  df<-full.data.set
  colnames(df)[match(var,colnames(df))]<-'mental_health'
  
  #get rid of NAs
  df[,'mental_health']<-ifelse(is.na(df[,'mental_health'])==FALSE,df[,'mental_health'],-100)
  df<-subset(df,mental_health>-100)
  
  #run a regression
  job.status.eff<-lm(mental_health~unemployment+sick+factor(wave_factor)+age+age_squared,data=df)
  
  job.status.eff.2<-lm(mental_health~unemployment+sick+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
    factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=df)
  
  
  coefs<-job.status.eff$coefficients
  job.stat.results[val,1]<-coefs[2]
  job.stat.results[val,2]<-coefs[3]
  coefs<-job.status.eff.2$coefficients
  job.stat.results[val,3]<-coefs[2]
  job.stat.results[val,4]<-coefs[3]
  
}
job.stat.results<-as.data.frame(job.stat.results)
colnames(job.stat.results)<-c('unemployment','incapacitated','unemployment with controls','incapacitated with controls')
rownames(job.stat.results)<-mh.lables

################################################################
#Other basic demographics: education, sex, race, marital status#
################################################################
dem.data.set<-full.data.set


demographics.reg.1<-lm(JPY_measure~factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                       factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=dem.data.set)
demographics.reg.2<-lm(GG~factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                         factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=dem.data.set)
demographics.reg.3<-lm(GG_diff_cutoff~factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                         factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=dem.data.set)
demographics.reg.4<-lm(GG_quantile~factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                         factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=dem.data.set)
demographics.reg.5<-lm(PCA~factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                         factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=dem.data.set)

######################################################################################
#okay, amongst those who are working, I want to know how income affects mental health#
######################################################################################


employed.data<-subset(full.data.set,employment_stat==2)
employed.data<-subset(employed.data,monthly_labor_income>0)
employed.data<-subset(employed.data,full_time>=0)

income.effect.results<-matrix(nrow=5,ncol=20)

#######################
#Let's make some plots#
#######################

#some plots
employed.data.last<-subset(employed.data,wave_factor==28)

png('income_and_health_plot.png')
par(mfrow=c(2,3))
plot(employed.data.last[,'JPY_measure'],employed.data.last[,'monthly_labor_income'],main='SF12',xlab='mental health',ylab='income')
plot(employed.data.last[,'GG'],employed.data.last[,'monthly_labor_income'],main='cutoff 2',xlab='mental health',ylab='income')
plot(employed.data.last[,'GG_diff_cutoff'],employed.data.last[,'monthly_labor_income'],main='cutoff 1',xlab='mental health',ylab='income')
plot(employed.data.last[,'GG_quantile'],employed.data.last[,'monthly_labor_income'],main='cutoff median',xlab='mental health',ylab='income')
plot(employed.data.last[,'PCA'],employed.data.last[,'monthly_labor_income'],main='PCA',xlab='mental health',ylab='income')
dev.off()

#I want to see log wages
log.income<-as.data.frame(log(employed.data[,'monthly_labor_income']))
colnames(log.income)<-'log_monthly_income'
employed.data<-cbind(employed.data,log.income)


#let's start by just looking at labor income vs. mental health
#for each measure
for (val in seq(from=1,to=5,by=1)){
  var<-colnames(mental.health.vars.US)[val]  
  
  df<-employed.data
  colnames(df)[match(var,colnames(df))]<-'mental_health'
  
  #get rid of NAs
  df[,'mental_health']<-ifelse(is.na(df[,'mental_health'])==FALSE,df[,'mental_health'],-100)
  df<-subset(df,mental_health>-100)
  
  df[,'log_monthly_income']<-log(12)+df[,'log_monthly_income']
  
  #create a mental_health sqaured variable
  mh.squared<-as.data.frame((df[,'mental_health'])^2)
  colnames(mh.squared)<-'mental_health_squared'
  df<-cbind(df,mh.squared)
  
  #run the reg
  income.effect.reg<-lm(monthly_labor_income~mental_health+mental_health_squared+
                          factor(sex)+black+asian+
                          +age+age_squared+factor(wave_number),data=df)
  
  #store the results
  coefs<-income.effect.reg$coefficients
  income.effect.results[val,1]<-coef(income.effect.reg)[2]
  income.effect.results[val,6]<-coef(income.effect.reg)[3]
  income.effect.results[val,11]<-summary(income.effect.reg)$coefficients[2,4]
  income.effect.results[val,16]<-summary(income.effect.reg)$coefficients[3,4]
  
  #now run the reg controlling for occupation
  income.effect.reg<-lm(monthly_labor_income~mental_health+mental_health_squared+factor(sex)+black+asian+
                          age+age_squared+factor(wave_number)+factor(occupation),data=df)
  coefs<-income.effect.reg$coefficients
  income.effect.results[val,2]<-coef(income.effect.reg)[2]
  income.effect.results[val,7]<-coef(income.effect.reg)[3]
  income.effect.results[val,12]<-summary(income.effect.reg)$coefficients[2,4]
  income.effect.results[val,17]<-summary(income.effect.reg)$coefficients[3,4]
  
  #now run the reg controlling for occupation
  income.effect.reg<-lm(monthly_labor_income~mental_health+mental_health_squared+factor(sex)+black+asian+
                          age+age_squared+factor(full_time),data=df)
  coefs<-income.effect.reg$coefficients
  income.effect.results[val,3]<-coef(income.effect.reg)[2]
  income.effect.results[val,8]<-coef(income.effect.reg)[3]
  income.effect.results[val,13]<-summary(income.effect.reg)$coefficients[2,4]                                             
  income.effect.results[val,18]<-summary(income.effect.reg)$coefficients[3,4]
  
  #and full time/part time
  income.effect.reg<-lm(monthly_labor_income~mental_health+mental_health_squared+factor(sex)+black+asian+
                          age+age_squared+factor(occupation)+factor(full_time),data=df)
  coefs<-income.effect.reg$coefficients
  income.effect.results[val,4]<-coef(income.effect.reg)[2]
  income.effect.results[val,9]<-coef(income.effect.reg)[3]
  income.effect.results[val,14]<-summary(income.effect.reg)$coefficients[2,4]
  income.effect.results[val,19]<-summary(income.effect.reg)$coefficients[3,4]
  
  #and probability of part time
  df[,'full_time']<-df[,'full_time']-1
  part.time.reg<-glm(full_time~mental_health+mental_health_squared+factor(sex)+black+asian+
                       age+age_squared,family=binomial(probit),data=df)
  coefs<-part.time.reg$coefficients
  income.effect.results[val,5]<-exp(coef(part.time.reg)[2])
  income.effect.results[val,10]<-exp(coef(part.time.reg)[3])
  income.effect.results[val,15]<-summary(part.time.reg)$coefficients[2,4]
  income.effect.results[val,20]<-summary(part.time.reg)$coefficients[3,4]
}

#get mean income
mean.income<-median(employed.data[,'monthly_labor_income'])
res.matrix<-matrix(nrow=5,ncol=4)
#for each row
for (val in seq(from=1,to=5,by=1)){
  #and each column
  for (val.1 in seq(from=1,to=4,by=1)){
    res.matrix[val,val.1]<-income.effect.results[val,val.1]+2*income.effect.results[val,val.1]*1.5
  }
}

#name these results
rownames(res.matrix)<-row.names(job.stat.results)
colnames(res.matrix)<-c('standard controls','plus occupation','plus full time','plus full time and occ')

#I want to divide by mean
res.matrix<-100*(res.matrix/mean.income)

#full time ajustment
fta<-35/25
employed.data<-subset(employed.data,full_time>0)

#for all the people who work part time
part.timers<-subset(employed.data,full_time==2)
#replace their labor income with adjusted labor income

employed.data<-subset(employed.data,weekly_hours>0)

wage<-as.data.frame(employed.data[,'monthly_labor_income']/employed.data[,'weekly_hours'])
colnames(wage)<-'wage'

employed.data<-cbind(employed.data,wage)

plot(employed.data[,'age'],employed.data[,'wage'])

#we only want those who are under the age of 65
employed.data<-subset(employed.data,age<=65)

#now let's run a regression of wage against age
log.wage<-as.data.frame(log(employed.data[,'wage']))
colnames(log.wage)<-'log_wage'
employed.data<-cbind(employed.data,log.wage)
wage.reg<-lm(log_wage~age+age_squared+factor(wave_number),data=employed.data)
#add the residuals in here
residuals<-as.data.frame(wage.reg$residuals)
colnames(residuals)<-'residuals'
employed.data<-cbind(employed.data,residuals)

mental.health.inequality<-lm(JPY_measure~residuals+factor(wave_number)+age+age_squared,data=employed.data)

inequality.results<-matrix(nrow=5,ncol=2)
employed.data[,'occupation']<-as.factor(employed.data[,'occupation'])

#for each of my measures of mental health
for (val in seq(from=1,to=5,by=1)){
  #create a dataset to use in the regression
  var<-colnames(mental.health.vars.US)[val]  
  
  df<-employed.data
  colnames(df)[match(var,colnames(df))]<-'mental_health'
  
  #get rid of NAs
  df[,'mental_health']<-ifelse(is.na(df[,'mental_health'])==FALSE,df[,'mental_health'],-100)
  df<-subset(df,mental_health>-100)
  
  #run a regression
  mental.health.inequality<-lm(mental_health~log_wage+factor(wave_number)+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                                 factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=df)
  
  coefs<-mental.health.inequality$coefficients
  inequality.results[val,1]<-coefs[2]
  
  occ.reg<-lm(mental_health~log_wage+factor(occupation)+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=df)
  
  coefs<-occ.reg$coefficients
  inequality.results[val,2]<-coefs[2]
  
}

#visualize the extent of this
plot(employed.data[,'residuals'],predict(occ.reg))
inequality.results<-as.data.frame(inequality.results)
rownames(inequality.results)<-mh.lables
colnames(inequality.results)<-c('standard controls','plus occupational dummies controls')

inequality.results*(max(employed.data[,'log_wage'])-min(employed.data[,'log_wage']))

###############################
#Explaining wage distributions#
###############################

wage.distribution.res<-matrix(nrow=5,ncol=6)

for (val in seq(from=1,to=5,by=1)){
  #create a dataset to use in the regression
  var<-colnames(mental.health.vars.US)[val]  
  
  df<-employed.data
  colnames(df)[match(var,colnames(df))]<-'mental_health'
  
  #get rid of NAs
  df[,'mental_health']<-ifelse(is.na(df[,'mental_health'])==FALSE,df[,'mental_health'],-100)
  df<-subset(df,mental_health>-100)
  
  init.reg<-lm(log_wage~mental_health+age+age_squared,data=df)
  
  HR.variance.matrix<-vcovHC(init.reg, type = "HC1")
  
  wage.distribution.res[val,1]<-coef(init.reg)[2]
  wage.distribution.res[val,4]<-coef(init.reg)[2]/sqrt(diag(HR.variance.matrix)[2])
  
  init.reg<-lm(log_wage~mental_health+age+age_squared+
                 factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                 factor(no_kids)+factor(helps_disables)+adjusted_hh_income,data=df)
  
  HR.variance.matrix<-vcovHC(init.reg, type = "HC1")
  
  wage.distribution.res[val,2]<-coef(init.reg)[2]
  wage.distribution.res[val,5]<-coef(init.reg)[2]/sqrt(diag(HR.variance.matrix)[2])
  
  init.reg<-lm(log_wage~mental_health+age+age_squared+
                 factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
                 factor(no_kids)+factor(helps_disables)+adjusted_hh_income+factor(occupation),data=df)
  
  HR.variance.matrix<-vcovHC(init.reg, type = "HC1")
  
  wage.distribution.res[val,3]<-coef(init.reg)[2]
  wage.distribution.res[val,6]<-coef(init.reg)[2]/sqrt(diag(HR.variance.matrix)[2])
  
}

#for reference, what's a standard deviation of log wages
sqrt(var(employed.data[,'log_wage']))
hist(employed.data[,'log_wage'])

###########################
#occupational health risks#
###########################

occ.reg<-lm(PCA~residuals+factor(occupation)+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
              factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=employed.data)
#let's see if this is robust to an F-test
HR.variance.matrix<-vcovHC(occ.reg, type = "HC1")
#we have n restrictions
val<-length(levels(employed.data[,'occupation']))
#get a restrictions matrix
R<-matrix(data=0,nrow=val,ncol=val)
diag(R)<-1
R<-cbind(matrix(data=0,nrow=val,ncol=2),R,matrix(data=0,nrow=val,ncol=length(coef(occ.reg))-2-val))
#and a q matrix
q<-matrix(data=0,nrow=val,ncol=1)
#store the F value
F.occupations<-((t(R%*%coef(occ.reg)-q)%*%solve(R%*%HR.variance.matrix%*%t(R))%*%(R%*%coef(occ.reg)-q)))/(val-1)

#upload occupation data, to cluster and then re-run regressions

occ.data<-read.csv('occ_class_data.CSV')
#get mental health dummies
occ.data[,'GHQ1']<-ifelse(occ.data[,'GHQ1']==4|occ.data[,'GHQ1']==3,1,0)
occ.data[,'GHQ2']<-ifelse(occ.data[,'GHQ2']==4|occ.data[,'GHQ2']==3,1,0)
occ.data[,'GHQ3']<-ifelse(occ.data[,'GHQ3']==4|occ.data[,'GHQ3']==3,1,0)
occ.data[,'GHQ4']<-ifelse(occ.data[,'GHQ4']==4|occ.data[,'GHQ4']==3,1,0)
occ.data[,'GHQ5']<-ifelse(occ.data[,'GHQ5']==4|occ.data[,'GHQ5']==3,1,0)
occ.data[,'GHQ6']<-ifelse(occ.data[,'GHQ6']==4|occ.data[,'GHQ6']==3,1,0)
occ.data[,'GHQ7']<-ifelse(occ.data[,'GHQ7']==4|occ.data[,'GHQ7']==3,1,0)
occ.data[,'GHQ8']<-ifelse(occ.data[,'GHQ8']==4|occ.data[,'GHQ8']==3,1,0)
occ.data[,'GHQ9']<-ifelse(occ.data[,'GHQ9']==4|occ.data[,'GHQ9']==3,1,0)
occ.data[,'GHQ10']<-ifelse(occ.data[,'GHQ10']==4|occ.data[,'GHQ10']==3,1,0)
occ.data[,'GHQ11']<-ifelse(occ.data[,'GHQ11']==4|occ.data[,'GHQ11']==3,1,0)
occ.data[,'GHQ12']<-ifelse(occ.data[,'GHQ12']==4|occ.data[,'GHQ12']==3,1,0)

occ.data[,'wkaut1']<-ifelse(occ.data[,'wkaut1']==1|occ.data[,'wkaut1']==2,1,0)
occ.data[,'wkaut2']<-ifelse(occ.data[,'wkaut2']==1|occ.data[,'wkaut2']==2,1,0)
occ.data[,'wkaut3']<-ifelse(occ.data[,'wkaut3']==1|occ.data[,'wkaut3']==2,1,0)
occ.data[,'wkaut4']<-ifelse(occ.data[,'wkaut4']==1|occ.data[,'wkaut4']==2,1,0)
occ.data[,'wkaut5']<-ifelse(occ.data[,'wkaut5']==1|occ.data[,'wkaut5']==2,1,0)

occ.data[,'depenth1']<-ifelse(occ.data[,'depenth1']==4|occ.data[,'depenth1']==5,1,0)
occ.data[,'depenth2']<-ifelse(occ.data[,'depenth2']==4|occ.data[,'depenth2']==5,1,0)
occ.data[,'depenth3']<-ifelse(occ.data[,'depenth3']==4|occ.data[,'depenth3']==5,1,0)
occ.data[,'depenth4']<-ifelse(occ.data[,'depenth4']==4|occ.data[,'depenth4']==5,1,0)
occ.data[,'depenth5']<-ifelse(occ.data[,'depenth5']==4|occ.data[,'depenth5']==5,1,0)
occ.data[,'depenth6']<-ifelse(occ.data[,'depenth6']==4|occ.data[,'depenth6']==5,1,0)

occ.list<-unique(occ.data[,'occupation'])
occ.count<-seq(from=1,to=length(occ.list),by=1)
occ.cluster.data<-cbind(occ.list,matrix(nrow=length(occ.list),ncol=24))
#for each occupation, get the average of each measure
count.to.23<-seq(from=1,to=23,by=1)
for (val in occ.count){
  occ<-occ.list[val]
  df<-subset(occ.data,occupation==occ)
  for (val.1 in count.to.23){
  occ.cluster.data[val,1+val.1]<-mean(df[,val.1+3])
  occ.cluster.data[val,25]<-nrow(df)
  }
}
rownames(occ.cluster.data)<-occ.data[match(occ.cluster.data[,1],occ.data[,'occupation']),'occupation.name']
N.cluster<-occ.cluster.data[,25]

#calculate the distance matrix
dist.matrix.occ<-dist(occ.cluster.data[,2:24],method='euclidian')
#use this to cluster
occ.cluster<-hclust(dist.matrix.occ,method='centroid')
#cut this
occ.cluster.cut<-cutree(occ.cluster,k=10)
#plot it
png('cluster_ex.png')
plot(occ.cluster,main='Occupational Clusters',cex=0.5,xlab = 'Occupations',ylab='Distance')
rect.hclust(occ.cluster , k = 2, border = 2:6)
dev.off()

#create a matrix to store results in
cluster.results<-matrix(nrow=length(occ.list)-1,ncol=16)
#16 is 5x r-squared, F-test, average t-stat + 1
cluster.results[,1]<-seq(from=2,to=length(occ.list),by=1)
#now for each possible cluster
for (val in cluster.results[,1]){
  #cluster
  cluster.cut<-as.data.frame(cutree(occ.cluster,k=val))
  #Use this to get a some clustered data
  clustered.occs<-as.data.frame(cluster.cut[match(employed.data[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(employed.data,clustered.occs)
  
  df<-subset(df,is.na(clustered_occs)==FALSE)
  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df
    
    #create a dataset to use in the regression
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
  
    #get rid of NAs
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,mental_health>-100)  
    
  
    reg<-lm(mental_health~0+factor(clustered_occs)+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
            factor(no_kids)+factor(helps_disables)+residuals+adjusted_hh_income+age+age_squared,data=df1)  
    #store the R-squared
    cluster.results[val-1,1+(val.1-1)*3+1]<-summary(reg)$r.squared
  
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(reg, type = "HC1")
    #store the p value
    cluster.results[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix, b=coef(reg), Terms = 2:val, df = NULL, verbose = FALSE)$result)[3,]
    #and the max minus min
    cluster.results[val-1,1+(val.1-1)*3+3]<-max(coef(reg)[1:val])-min(coef(reg)[1:val])
  }
}

###################################################################################################################
plot(cluster.results[,1],cluster.results[,6],ylim=c(4,50))
lines(cluster.results[,1],cluster.results[,9])
lines(cluster.results[,1],cluster.results[,12])
lines(cluster.results[,1],cluster.results[,15])

png('occupational_predictors_of_mental_health_significant.png')
plot(cluster.results[,1],cluster.results[,3],ylim=c(0,1),main='Occupational predictors of mental health, P value',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(cluster.results[,1],cluster.results[,6],col=col.vector[2])
lines(cluster.results[,1],cluster.results[,9],col=col.vector[3])
lines(cluster.results[,1],cluster.results[,12],col=col.vector[4])
lines(cluster.results[,1],cluster.results[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)
dev.off()

png('occupational_predictors_of_mental_health.png')
plot(cluster.results[,1],cluster.results[,4],ylim=c(0,1.5),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(cluster.results[,1],cluster.results[,7],col=col.vector[2])
lines(cluster.results[,1],cluster.results[,10],col=col.vector[3])
lines(cluster.results[,1],cluster.results[,13],col=col.vector[4])
lines(cluster.results[,1],cluster.results[,16],col=col.vector[5])
legend('bottomright',legend=mh.lables,fill = col.vector,horiz=TRUE)
dev.off()

#######################################
# Now do this with K-means clustering #
#######################################
#First proof of concept for K-menas clustering

kmean.cluster<-kmeans(occ.cluster.data[,2:24], 4, iter.max=1000, algorithm = 'Lloyd')
#run a pca for this
kmean.cluster.pca<-prcomp(occ.cluster.data[,2:24])
pcs<-kmean.cluster.pca$x[,1:2]
plot(pcs[,1],pcs[,2])

png('kmeans_eg.png')
fviz_cluster(kmean.cluster, data = occ.cluster.data[,2:24],
             palette = c("red", "blue", "black",'purple','green'), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
dev.off()
######################################
#create a matrix to store results in
K.cluster.results<-matrix(nrow=length(occ.list)-1,ncol=16)
#16 is 5x r-squared, F-test, average t-stat + 1
K.cluster.results[,1]<-seq(from=2,to=length(occ.list),by=1)
#now for each possible cluster
for (val in K.cluster.results[,1]){
  #cluster
  
  kmean.cluster<-kmeans(occ.cluster.data[,2:24], val, iter.max=1000, algorithm = 'Lloyd')
  cluster.key<-as.data.frame(kmean.cluster$cluster)
  
  clustered.occs<-as.data.frame(cluster.key[match(employed.data[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(employed.data,clustered.occs)
  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df
    
    #create a dataset to use in the regression
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
    
    #get rid of NAs
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,mental_health>-100)  
    
    
    reg<-lm(mental_health~0+factor(clustered_occs)+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+asian+home_owner_dummy+
              factor(no_kids)+factor(helps_disables)+residuals+adjusted_hh_income+age+age_squared,data=df1)  
    #store the R-squared
    K.cluster.results[val-1,1+(val.1-1)*3+1]<-summary(reg)$r.squared
    
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(reg, type = "HC1")
    #get a restrictions matrix
    R<-matrix(data=0,nrow=val,ncol=val)
    diag(R)<-1
    R<-cbind(matrix(data=0,nrow=val,ncol=2),R,matrix(data=0,nrow=val,ncol=length(coef(reg))-2-val))
    #and a q matrix
    q<-matrix(data=0,nrow=val,ncol=1)
    #store the F value
    #K.cluster.results[val-1,1+(val.1-1)*3+2]<-((t(R%*%coef(reg)-q)%*%solve(R%*%HR.variance.matrix%*%t(R))%*%(R%*%coef(reg)-q)))/(val-1)
    
    #store the P value
    K.cluster.results[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix, b=coef(reg), Terms = 2:val, df = NULL, verbose = FALSE)$result)[3,]
    
    #and the max minus min
    K.cluster.results[val-1,1+(val.1-1)*3+3]<-max(coef(reg)[1:val])-min(coef(reg)[1:val])
  }
}

png('occupational_predictors_of_mental_health_significant_K.png')
plot(K.cluster.results[,1],K.cluster.results[,3],ylim=c(0,0.1),main='Occupational predictors of mental health, P value',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(K.cluster.results[,1],K.cluster.results[,6],col=col.vector[2])
lines(K.cluster.results[,1],K.cluster.results[,9],col=col.vector[3])
lines(K.cluster.results[,1],K.cluster.results[,12],col=col.vector[4])
lines(K.cluster.results[,1],K.cluster.results[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)
dev.off()

png('occupational_predictors_of_mental_health_K.png')
plot(K.cluster.results[,1],K.cluster.results[,4],ylim=c(0,2),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(K.cluster.results[,1],K.cluster.results[,7],col=col.vector[2])
lines(K.cluster.results[,1],K.cluster.results[,10],col=col.vector[3])
lines(K.cluster.results[,1],K.cluster.results[,13],col=col.vector[4])
lines(K.cluster.results[,1],K.cluster.results[,16],col=col.vector[5])
legend('topleft',legend=mh.lables,fill = col.vector,horiz=TRUE)
dev.off()

##############################

#group some results by person#

##############################

people<-unique(full.data.set[,'pid'])
#start by creating some wave-specific waves
for (val in seq(from=1,to=28,by=1)){
  df<-subset(full.data.set,wave_number==val)
  df.name<-paste('data.wave.',val,sep='')
  assign(df.name,df)
}
wave.data.list<-list(data.wave.1,data.wave.2,data.wave.3,data.wave.4,data.wave.5,data.wave.6,data.wave.7,data.wave.8,data.wave.9,
                     data.wave.10,data.wave.11,data.wave.12,data.wave.13,data.wave.14,data.wave.15,data.wave.16,data.wave.17,data.wave.18,
                     data.wave.19,data.wave.20,data.wave.21,data.wave.22,data.wave.23,data.wave.24,data.wave.25,data.wave.26,data.wave.27,
                     data.wave.28)
#now stack them together so that each wave (from 28 down to 1) is paired with the one before it
for (val in seq(from=2,to=28,by=1)){
df1<-as.data.frame(wave.data.list[val])
df<-as.data.frame(wave.data.list[val-1])
#match them
df<-df[match(df1[,'pid'],df[,'pid']),]
colnames(df1)<-paste(colnames(df),'_prime',sep='')
df<-cbind(df,df1)
df.name<-paste('waves',val,'_and_',val-1,sep='')
assign(df.name,df)
}

#stack them ontop of each other
wave.paired.data<-rbind(waves28_and_27,waves27_and_26,waves26_and_25,waves25_and_24,waves24_and_23,waves23_and_22,waves22_and_21,waves21_and_20,waves20_and_19,
                        waves19_and_18,waves18_and_17,waves17_and_16,waves16_and_15,waves15_and_14,waves14_and_13,waves13_and_12,waves12_and_11,
                        waves11_and_10,waves10_and_9,waves9_and_8,waves8_and_7,waves7_and_6,waves6_and_5,waves5_and_4,waves4_and_3,
                        waves3_and_2,waves2_and_1)
#kill NAs
wave.paired.data<-subset(wave.paired.data,is.na(pid)==FALSE)

#we need to get changes in mental health
first.MH.prime<-match('JPY_measure_prime',colnames(wave.paired.data))
last.MH.prime<-match('PCA_prime',colnames(wave.paired.data))

first.MH<-match('JPY_measure',colnames(wave.paired.data))
last.MH<-match('PCA',colnames(wave.paired.data))

delta.mental.health<-wave.paired.data[,first.MH.prime:last.MH.prime]-wave.paired.data[,first.MH:last.MH]
colnames(delta.mental.health)<-c("delta_JPY_measure", "delta_GG", "delta_GG_diff_cutoff",
                                 "delta_GG_quantile","delta_PCA")
  

wave.paired.data<-cbind(wave.paired.data,delta.mental.health)

hist(wave.paired.data[,'delta_JPY_measure'])
hist(wave.paired.data[,'delta_GG'])
hist(wave.paired.data[,'delta_GG_diff_cutoff'])
hist(wave.paired.data[,'delta_GG_quantile'])
hist(wave.paired.data[,'delta_PCA'])
#note: JPY measure and PCA have the nicest looking histograms.

##########################################

#employed data, re-do the occupation test#

##########################################

wave.paired.data.employed<-subset(wave.paired.data,employment_stat==2)
cluster.results.delta<-matrix(nrow=length(occ.list)-1,ncol=5)
cluster.results.delta[,1]<-seq(from=2,to=length(occ.list),by=1)

#run regressions for each measure of mental health
delta.reg.JPY<-lm(delta_JPY_measure~factor(wave_number)+factor(marital_stat)+
                     factor(sex)+factor(education)+black+asian+home_owner_dummy+
                     factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=wave.paired.data.employed)

delta.reg.GG<-lm(delta_GG~factor(wave_number)+factor(marital_stat)+
                    factor(sex)+factor(education)+black+asian+home_owner_dummy+
                    factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=wave.paired.data.employed)

delta.reg.GG.diff.cutoff<-lm(delta_GG_diff_cutoff~factor(wave_number)+factor(marital_stat)+
                   factor(sex)+factor(education)+black+asian+home_owner_dummy+
                   factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=wave.paired.data.employed)

delta.reg.GG.quantile<-lm(delta_GG_quantile~factor(wave_number)+factor(marital_stat)+
                               factor(sex)+factor(education)+black+asian+home_owner_dummy+
                               factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=wave.paired.data.employed)

delta.reg.PCA<-lm(delta_PCA~factor(wave_number)+factor(marital_stat)+
                            factor(sex)+factor(education)+black+asian+home_owner_dummy+
                            factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=wave.paired.data.employed)
#notes: it's pretty much just marital status and (sometimes) education which is significant in these regressions. (Could look at changes too)

#Now I want to look at occupational effects on changes in mental health
delta.cluster.results<-matrix(nrow=length(occ.list)-1,ncol=16)
#16 is 5x r-squared, F-test, average t-stat + 1
delta.cluster.results[,1]<-seq(from=2,to=length(occ.list),by=1)

for (val in delta.cluster.results[,1]){
  #cluster
  cluster.cut<-as.data.frame(cutree(occ.cluster,k=val))
  #Use this to get a some clustered data
  clustered.occs<-as.data.frame(cluster.cut[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(wave.paired.data.employed,clustered.occs)
  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df
    
    #create a dataset to use in the regression
    var<-colnames(delta.mental.health)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'delta_mental_health'
    
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
    
    #get rid of NAs
    df1[,'delta_mental_health']<-ifelse(is.na(df1[,'delta_mental_health'])==FALSE,df1[,'delta_mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    reg<-lm(delta_mental_health~0+factor(clustered_occs)+mental_health+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+
              asian+home_owner_dummy+factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=df1)  
    #store the R-squared
    delta.cluster.results[val-1,1+(val.1-1)*3+1]<-summary(reg)$r.squared
    
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(reg, type = "HC1")
    #get a restrictions matrix
    R<-matrix(data=0,nrow=val,ncol=val)
    diag(R)<-1
    R<-cbind(matrix(data=0,nrow=val,ncol=2),R,matrix(data=0,nrow=val,ncol=length(coef(reg))-2-val))
    #and a q matrix
    q<-matrix(data=0,nrow=val,ncol=1)
    #store the F value
    delta.cluster.results[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix, b=coef(reg), Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    #and the max minus min
    delta.cluster.results[val-1,1+(val.1-1)*3+3]<-max(coef(reg)[1:val])-min(coef(reg)[1:val])
  }
}

png('occ_effect_delta_hm.png')
plot(delta.cluster.results[,1],delta.cluster.results[,4],ylim=c(0,2),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(delta.cluster.results[,1],delta.cluster.results[,7],col=col.vector[2])
lines(delta.cluster.results[,1],delta.cluster.results[,10],col=col.vector[3])
lines(delta.cluster.results[,1],delta.cluster.results[,13],col=col.vector[4])
lines(delta.cluster.results[,1],delta.cluster.results[,16],col=col.vector[5])
legend('topleft',legend=mh.lables,fill = col.vector,horiz=TRUE)
dev.off()

png('occ_effect_delta_hm_Ftest.png')
plot(delta.cluster.results[,1],delta.cluster.results[,3],ylim=c(0,1),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(delta.cluster.results[,1],delta.cluster.results[,6],col=col.vector[2])
lines(delta.cluster.results[,1],delta.cluster.results[,9],col=col.vector[3])
lines(delta.cluster.results[,1],delta.cluster.results[,12],col=col.vector[4])
lines(delta.cluster.results[,1],delta.cluster.results[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)
dev.off()

#############################
#and K-means


#Now I want to look at occupational effects on changes in mental health
K.delta.cluster.results<-matrix(nrow=9,ncol=16)
#16 is 5x r-squared, F-test, average t-stat + 1
K.delta.cluster.results[,1]<-seq(from=2,to=10,by=1)

for (val in K.delta.cluster.results[,1]){
  #cluster
  kmean.cluster<-kmeans(occ.cluster.data[,2:24], val, iter.max=1000, algorithm = 'Lloyd')
  cluster.key<-as.data.frame(kmean.cluster$cluster)
  
  clustered.occs<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(wave.paired.data.employed,clustered.occs)
  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df
    
    #create a dataset to use in the regression
    var<-colnames(delta.mental.health)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'delta_mental_health'
    
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
    
    #get rid of NAs
    df1[,'delta_mental_health']<-ifelse(is.na(df1[,'delta_mental_health'])==FALSE,df1[,'delta_mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    #draw first 1000 people
    
    sample<-unique(df1[,'pid'])[1:1000]
    
    df.sample<-subset(df1,pid %in% sample)
    
    reg<-lm(delta_mental_health~0+factor(clustered_occs)+mental_health+factor(wave_number)+factor(marital_stat)+factor(education)+
              home_owner_dummy+factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared+factor(pid),data=df.sample)  
    #store the R-squared
    K.delta.cluster.results[val-1,1+(val.1-1)*3+1]<-summary(reg)$r.squared
    
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(reg, type = "HC1")
    #store the P value
    K.delta.cluster.results[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix[1:val,1:val], b=coef(reg)[1:val], Terms = 2:val, df = NULL, verbose = FALSE)$result)[3,]
    #and the max minus min
    K.delta.cluster.results[val-1,1+(val.1-1)*3+3]<-max(coef(reg)[1:val])-min(coef(reg)[1:val])
  }
}

png('K_occ_effect_delta_hm.png')
plot(K.delta.cluster.results[,1],K.delta.cluster.results[,4],ylim=c(0,2),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,7],col=col.vector[2])
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,10],col=col.vector[3])
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,13],col=col.vector[4])
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,16],col=col.vector[5])
legend('topleft',legend=mh.lables,fill = col.vector,horiz=TRUE)
dev.off()

png('K_occ_effect_delta_hm_Ftest.png')
plot(K.delta.cluster.results[,1],K.delta.cluster.results[,3],ylim=c(0,1),
     main='Occupational predictors of mental health p value',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,6],col=col.vector[2])
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,9],col=col.vector[3])
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,12],col=col.vector[4])
lines(K.delta.cluster.results[,1],K.delta.cluster.results[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)
dev.off()

####################
#pick an (essentially random) specification: 3 clusters, all measures

kmean.cluster<-kmeans(occ.cluster.data[,2:24], 5, iter.max=1000, algorithm = 'Lloyd')
cluster.key<-as.data.frame(kmean.cluster$cluster)

clustered.occs<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
colnames(clustered.occs)<-'clustered_occs'
df<-cbind(wave.paired.data.employed,clustered.occs)

  reg.SF12<-lm(delta_JPY_measure~factor(clustered_occs)+JPY_measure+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+
            asian+home_owner_dummy+factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=df)  


##########################

#what about the variance?#

##########################


#Now I want to look at occupational effects on changes in mental health
K.delta.cluster.var.results<-matrix(nrow=length(occ.list)-1,ncol=16)
#16 is 5x r-squared, F-test, average t-stat + 1
K.delta.cluster.var.results[,1]<-seq(from=2,to=length(occ.list),by=1)

for (val in K.delta.cluster.var.results[,1]){
  #cluster
  kmean.cluster<-kmeans(occ.cluster.data[,2:24], val, iter.max=1000, algorithm = 'Lloyd')
  cluster.key<-as.data.frame(kmean.cluster$cluster)
  
  clustered.occs<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(wave.paired.data.employed,clustered.occs)
  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df
    
    #create a dataset to use in the regression
    var<-colnames(delta.mental.health)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'delta_mental_health'
    
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
    
    #get rid of NAs
    df1[,'delta_mental_health']<-ifelse(is.na(df1[,'delta_mental_health'])==FALSE,df1[,'delta_mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    df1[,'delta_mental_health']<-(df1[,'delta_mental_health']-mean(df1[,'delta_mental_health']))^2
    
    reg<-lm(delta_mental_health~factor(clustered_occs)+mental_health+factor(wave_number)+factor(marital_stat)+factor(sex)+factor(education)+black+
              asian+home_owner_dummy+factor(no_kids)+factor(helps_disables)+adjusted_hh_income+age+age_squared,data=df1)  
    #store the R-squared
    K.delta.cluster.var.results[val-1,1+(val.1-1)*3+1]<-summary(reg)$r.squared
    
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(reg, type = "HC1")
    #store the P value
    K.delta.cluster.var.results[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix, b=coef(reg), Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    #and the max minus min
    K.delta.cluster.var.results[val-1,1+(val.1-1)*3+3]<-max(coef(reg)[2:val])-min(coef(reg)[2:val])
  }
}

png('K_occ_effect_delta_hm_var.png')
plot(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,4],ylim=c(0,3),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,7],col=col.vector[2])
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,10],col=col.vector[3])
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,13],col=col.vector[4])
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,16],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)
dev.off()

png('K_occ_effect_delta_hm_Ftest_var.png')
plot(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,3],main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,6],col=col.vector[2])
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,9],col=col.vector[3])
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,12],col=col.vector[4])
lines(K.delta.cluster.var.results[,1],K.delta.cluster.var.results[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)
dev.off()


################################

#frequency of occupation shifts#

################################

#Start by looking at the plot by different numbers of clusters
kmean.cluster<-kmeans(occ.cluster.data[,2:24], 3, iter.max=1000, algorithm = 'Lloyd')
#run a pca for this
kmean.cluster.pca<-prcomp(occ.cluster.data[,2:24])
pcs<-kmean.cluster.pca$x[,1:2]
plot(pcs[,1],pcs[,2])

fviz_cluster(kmean.cluster, data = occ.cluster.data[,2:24],
             palette = c("red", "blue", "black",'purple','green'), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

loadings<-kmean.cluster.pca$rotation

loading.plot.dt<-t(cbind(loadings[,1],loadings[,2],loadings[,3]))
colnames(loading.plot.dt)<-colnames(occ.data)[4:26]
png('pca_weights.png')
barplot(loading.plot.dt,beside=TRUE,main='Principle component factor loadings',
        xlab='variables',col=col.vector[1:3],cex.axis=1,cex.names=0.75,las=2)
legend('bottomright',legend=c('one','two','three'),fill=col.vector[1:3])
dev.off()

#alright, probability of shift between occupations
cluster.key<-as.data.frame(kmean.cluster$cluster)

clustered.occs<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
colnames(clustered.occs)<-'clustered_occs'

#we also need clustered occs prime
clustered.occs.prime<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation_prime'],occ.cluster.data[,1]),1])
colnames(clustered.occs.prime)<-'clustered_occs_prime'

df<-cbind(wave.paired.data.employed,clustered.occs,clustered.occs.prime)
df<-subset(df,clustered_occs!=clustered_occs_prime|clustered_occs==clustered_occs_prime)


df.switches<-subset(df,clustered_occs!=clustered_occs_prime)
nrow(df.switches)/nrow(df)
#at 3 clusters, 18% of people switch occupation each period.


####################
#TSLS specification#
####################

#start with the wavepaired data employed, and kill people with no region
wave.paired.data.employed<-subset(wave.paired.data.employed,region>=0)

TSLS.cluster.res<-matrix(nrow=9,ncol=16)
TSLS.cluster.res[,1]<-seq(from=2,to=10,by=1)
cluster.res.no.switchers<-matrix(nrow=9,ncol=5)

for (val in TSLS.cluster.res[,1]){
  #cluster
  kmean.cluster<-kmeans(occ.cluster.data[,2:24], val, iter.max=1000, algorithm = 'Lloyd')
  cluster.key<-as.data.frame(kmean.cluster$cluster)
  
  #################################################################
  #fviz_cluster(kmean.cluster, data = occ.cluster.data[,2:24],
  #             palette = c("red", "blue", "black",'purple','green','yellow','pink','grey'), 
  #             geom = "point",
  #            ellipse.type = "convex", 
  #             ggtheme = theme_bw()
  #)
  #################################################################
  
  clustered.occs<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(wave.paired.data.employed,clustered.occs)
  #we also need to get clustered_occs_prime
  clustered.occs.prime<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation_prime'],occ.cluster.data[,1]),1])
  colnames(clustered.occs.prime)<-'clustered_occs_prime'
  df<-cbind(df,clustered.occs.prime)
  
  df<-subset(df,is.na(clustered_occs)==FALSE)
  
  #now need to create an instrument, which is predicted occupation in each year and in each region
  
  df.with.instruments<-as.data.frame(cbind(matrix(nrow=0,ncol=ncol(df)+length(occ.list))))
  colnames(df.with.instruments)<-c(colnames(df),paste('share_in_occ_',seq(from=1,to=length(occ.list),by=1),sep=''))
  
  #for each year
  wave.count<-unique(df[,'wave_number'])
  
  for (w in wave.count){
    
  df1<-subset(df,wave_number==w)  
  
  occ.share.key<-as.data.frame(matrix(ncol=length(occ.list),nrow=nrow(df1)))
  colnames(occ.share.key)<-paste('share_in_occ_',seq(from=1,to=length(occ.list),by=1),sep='')
  
  #for each cluster
  for (val.1 in seq(from=1,to=val,by=1)){
  
  occ.share.key[,val.1]<-nrow(subset(df1,clustered_occs==val.1))/nrow(df1)
  
  }
  
  df1<-cbind(df1,occ.share.key)
  
  #add this to the dataset
  df.with.instruments<-rbind(df.with.instruments,df1)
  
  }
  
  #I want to get a matrix of no switchers
  df.switches.1<-subset(df.with.instruments,clustered_occs!=clustered_occs_prime)
  
  ever.switchers<-unique(df.switches.1[,'pid'])
  
  df.never.switchers<-subset(df.with.instruments,(pid %in% ever.switchers)==FALSE)
  

  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df.with.instruments
    
    df2<-df.never.switchers
    
    #create a dataset to use in the regression
    var<-colnames(delta.mental.health)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'delta_mental_health'
    colnames(df2)[match(var,colnames(df2))]<-'delta_mental_health'
    
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
    
    #get rid of NAs
    df1[,'delta_mental_health']<-ifelse(is.na(df1[,'delta_mental_health'])==FALSE,df1[,'delta_mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    occ.dummy<-as.data.frame(ifelse(df1[,'clustered_occs']==2,1,0))
    colnames(occ.dummy)<-'occ_dummy'
    df1<-cbind(df1,occ.dummy)
    
    #draw first 1000 people
    
    sample<-unique(df1[,'pid'])[1:1000]
    
    df.sample<-subset(df1,pid %in% sample)
    df2<-subset(df2,pid %in% sample)
    
    #get the shares in each occupation
    shares<-as.data.frame(df.sample[,98:(97+val)])
    shares.2<-as.data.frame(df2[,98:(97+val)])
    iv<-ivreg(delta_mental_health~factor(clustered_occs)+mental_health+factor(wave_number)+factor(pid)|mental_health+
              factor(wave_number)+as.matrix(shares)+factor(pid),data=df.sample)
    
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(iv, type = "HC1")
    #get a restrictions matrix
    TSLS.cluster.res[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix, b=coef(iv)[1:nrow(HR.variance.matrix)], Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    #and the max minus min
    TSLS.cluster.res[val-1,1+(val.1-1)*3+3]<-max(coef(iv)[1]+c(0,coef(iv)[2:val]))-min(coef(iv)[1]+c(0,coef(iv)[2:val]))
    
    #how about the first stage? For each of the clusstered occ
    av.F<-0
    av.F.2<-0
    for (val.2 in seq(from=1,to=val)){
    fsr<-lm(factor(clustered_occs)==val.2~as.matrix(shares)+factor(wave_number)+factor(pid),data=df.sample)
    #get the right vcov matrix
    fsr.variance.matrix<-vcovHC(fsr, type = "HC1")
    placebo.reg<-lm(delta_mental_health~as.matrix(shares.2)+factor(wave_number)+factor(pid),data=df2)
    #get the right vcov matrix
    placebo.variance.matrix<-vcovHC(placebo.reg, type = "HC1")
    #update average F-stat
    av.F<-av.F+as.data.frame(wald.test(Sigma=fsr.variance.matrix[1:val,1:val], b=coef(fsr)[1:val], Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    av.F.2<-av.F.2+as.data.frame(wald.test(Sigma=placebo.variance.matrix[1:val,1:val], b=coef(placebo.reg)[1:val], Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    }
    TSLS.cluster.res[val-1,1+(val.1-1)*3+1]<-av.F/val
    cluster.res.no.switchers[val-1,val.1]<-av.F.2/val
  }
}

plot(TSLS.cluster.res[,1],TSLS.cluster.res[,4],ylim=c(0,2),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,7],col=col.vector[2])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,10],col=col.vector[3])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,13],col=col.vector[4])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,16],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)


plot(TSLS.cluster.res[,1],TSLS.cluster.res[,3],main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,6],col=col.vector[2])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,9],col=col.vector[3])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,12],col=col.vector[4])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)

plot(TSLS.cluster.res[,1],TSLS.cluster.res[,2],main='Instrument relevance',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='Average first stage F-stat')
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,5],col=col.vector[2])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,8],col=col.vector[3])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,11],col=col.vector[4])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,14],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)

plot(TSLS.cluster.res[,1],cluster.res.no.switchers[,1],main='Instrument validity',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='Average F-stat')
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,2],col=col.vector[2])
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,3],col=col.vector[3])
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,4],col=col.vector[4])
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,5],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)

############################
#TSLS specification: region#
############################

#start with the wavepaired data employed, and kill people with no region
wave.paired.data.employed<-subset(wave.paired.data.employed,region>=0)

TSLS.cluster.res<-matrix(nrow=9,ncol=16)
TSLS.cluster.res[,1]<-seq(from=2,to=10,by=1)
cluster.res.no.switchers<-matrix(nrow=9,ncol=5)

for (val in TSLS.cluster.res[,1]){
  #cluster
  kmean.cluster<-kmeans(occ.cluster.data[,2:24], val, iter.max=1000, algorithm = 'Lloyd')
  cluster.key<-as.data.frame(kmean.cluster$cluster)
  
  clustered.occs<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation'],occ.cluster.data[,1]),1])
  colnames(clustered.occs)<-'clustered_occs'
  df<-cbind(wave.paired.data.employed,clustered.occs)
  #we also need to get clustered_occs_prime
  clustered.occs.prime<-as.data.frame(cluster.key[match(wave.paired.data.employed[,'occupation_prime'],occ.cluster.data[,1]),1])
  colnames(clustered.occs.prime)<-'clustered_occs_prime'
  df<-cbind(df,clustered.occs.prime)
  
  df<-subset(df,is.na(clustered_occs)==FALSE)
  
  #now need to create an instrument, which is predicted occupation in each year and in each region
  
  df.with.instruments<-as.data.frame(cbind(matrix(nrow=0,ncol=ncol(df)+length(occ.list))))
  colnames(df.with.instruments)<-c(colnames(df),paste('share_in_occ_',seq(from=1,to=length(occ.list),by=1),sep=''))
  
  #for each year
  wave.count<-unique(df[,'wave_number'])
  
  for (w in wave.count){
    
    df1<-subset(df,wave_number==w)  
    
    occ.share.key<-as.data.frame(matrix(ncol=length(occ.list),nrow=nrow(df1)))
    colnames(occ.share.key)<-paste('share_in_occ_',seq(from=1,to=length(occ.list),by=1),sep='')
    
    #for each cluster
    for (val.1 in seq(from=1,to=val,by=1)){
      
      cluster.by.region<-matrix(nrow=12,ncol=1)
      #then for each region, get the employment share
      for (val.2 in unique(wave.paired.data.employed[,'region'])){
        cluster.by.region[val.2,1]<-nrow(subset(df1,clustered_occs==val.1&region==val.2))/nrow(subset(df1,region==val.2))
      }
      occ.share.key[,val.1]<-cluster.by.region[df1[,'region'],1]
    }
    
    df1<-cbind(df1,occ.share.key)
    
    #add this to the dataset
    df.with.instruments<-rbind(df.with.instruments,df1)
    
  }
  
  #I want to get a matrix of no switchers
  df.switches.1<-subset(df.with.instruments,clustered_occs!=clustered_occs_prime)
  
  ever.switchers<-unique(df.switches.1[,'pid'])
  
  df.never.switchers<-subset(df.with.instruments,(pid %in% ever.switchers)==FALSE)
  
  
  
  #then for each measure of mental health
  for (val.1 in seq(from=1,to=5,by=1)){
    
    df1<-df.with.instruments
    
    df2<-df.never.switchers
    
    #create a dataset to use in the regression
    var<-colnames(delta.mental.health)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'delta_mental_health'
    colnames(df2)[match(var,colnames(df2))]<-'delta_mental_health'
    
    var<-colnames(mental.health.vars.US)[val.1]  
    colnames(df1)[match(var,colnames(df1))]<-'mental_health'
    colnames(df2)[match(var,colnames(df2))]<-'mental_health'
    
    #get rid of NAs
    df1[,'delta_mental_health']<-ifelse(is.na(df1[,'delta_mental_health'])==FALSE,df1[,'delta_mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    df1[,'mental_health']<-ifelse(is.na(df1[,'mental_health'])==FALSE,df1[,'mental_health'],-100)
    df1<-subset(df1,delta_mental_health>-100)  
    
    occ.dummy<-as.data.frame(ifelse(df1[,'clustered_occs']==2,1,0))
    colnames(occ.dummy)<-'occ_dummy'
    df1<-cbind(df1,occ.dummy)
    
    sample<-unique(df1[,'pid'])[1:1000]
    
    df.sample<-subset(df1,pid %in% sample)
    df2<-subset(df2,pid %in% sample)
    
    #get the shares in each occupation
    shares<-as.data.frame(df.sample[,98:(97+val)])
    shares.2<-as.data.frame(df2[,98:(97+val)])
    iv<-ivreg(delta_mental_health~factor(clustered_occs)+mental_health+factor(wave_number)+factor(region)+factor(pid)|mental_health+
                +factor(wave_number)+as.matrix(shares)+factor(region)+factor(pid),data=df.sample)
    
    #get the right vcov matrix
    HR.variance.matrix<-vcovHC(iv, type = "HC1")
    #get a restrictions matrix
    TSLS.cluster.res[val-1,1+(val.1-1)*3+2]<-as.data.frame(wald.test(Sigma=HR.variance.matrix, b=coef(iv)[1:nrow(HR.variance.matrix)], Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    #and the max minus min
    TSLS.cluster.res[val-1,1+(val.1-1)*3+3]<-max(coef(iv)[1]+c(0,coef(iv)[2:val]))-min(coef(iv)[1]+c(0,coef(iv)[2:val]))
    
    #how about the first stage? For each of the clusstered occ
    av.F<-0
    av.F.2<-0
    for (val.2 in seq(from=1,to=val)){
      fsr<-lm(factor(clustered_occs)==val.2~as.matrix(shares)+factor(region)+factor(wave_number)+factor(pid),data=df.sample)
      #get the right vcov matrix
      fsr.variance.matrix<-vcovHC(fsr, type = "HC1")
      
      placebo.reg<-lm(delta_mental_health~as.matrix(shares.2)+factor(region)+factor(wave_number)+factor(pid)+mental_health,data=df2)
      #get the right vcov matrix
      placebo.variance.matrix<-vcovHC(placebo.reg, type = "HC1")
      #update average F-stat
      av.F<-av.F+as.data.frame(wald.test(Sigma=fsr.variance.matrix[1:val,1:val], b=coef(fsr)[1:val], Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
      av.F.2<-av.F.2+as.data.frame(wald.test(Sigma=placebo.variance.matrix[1:val,1:val], b=coef(placebo.reg)[1:val], Terms = 2:val,df = NULL, verbose = FALSE)$result)[3,]
    }
    TSLS.cluster.res[val-1,1+(val.1-1)*3+1]<-av.F/val
    cluster.res.no.switchers[val-1,val.1]<-av.F.2/val
  }
}

plot(TSLS.cluster.res[,1],TSLS.cluster.res[,4],ylim=c(0,2),main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='max effect')
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,7],col=col.vector[2])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,10],col=col.vector[3])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,13],col=col.vector[4])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,16],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)


plot(TSLS.cluster.res[,1],TSLS.cluster.res[,3],main='Occupational predictors of mental health',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='p value')
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,6],col=col.vector[2])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,9],col=col.vector[3])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,12],col=col.vector[4])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,15],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)

plot(TSLS.cluster.res[,1],TSLS.cluster.res[,2],main='Instrument relevance',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='Average first stage F-stat',ylim=c(0,1))
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,5],col=col.vector[2])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,8],col=col.vector[3])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,11],col=col.vector[4])
lines(TSLS.cluster.res[,1],TSLS.cluster.res[,14],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)

plot(TSLS.cluster.res[,1],cluster.res.no.switchers[,1],main='Instrument validity',type='l',col=col.vector[1],
     xlab='number of clusters',ylab='Average F-stat',ylim=c(0,1))
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,2],col=col.vector[2])
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,3],col=col.vector[3])
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,4],col=col.vector[4])
lines(TSLS.cluster.res[,1],cluster.res.no.switchers[,5],col=col.vector[5])
legend('topright',legend=mh.lables,fill = col.vector)


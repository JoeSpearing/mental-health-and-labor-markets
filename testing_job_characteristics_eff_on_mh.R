rm(list=ls())
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\processed_data')
library(glmnet)

full.data.set<-read.csv('job_characteristics_and_mental_health.CSV')

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

#subset to get people working
workers.data<-subset(full.data.set,employment_stat==2)

###############################################################
#look at how socioeconomic class matches up against occupation#
###############################################################

workers.data<-subset(workers.data,three_class>=0)

occ.data<-read.csv('occ_class_data.CSV')
occupation.class.table<-as.data.frame(matrix(nrow=length(unique(workers.data[,'occupation'])),ncol=5))
colnames(occupation.class.table)<-c('occupation','occupation_name','perc_in_class_1','perc_in_class_2','perc_in_class_3')

occupation.class.table[,1]<-unique(workers.data[,'occupation'])
occupation.class.table<-as.data.frame(occupation.class.table)

for (val in seq(from=1,to=length(unique(workers.data[,'occupation'])),by=1)){
  occ.no<-occupation.class.table[val,1]
  occ.name<-occ.data[match(occ.no,occ.data[,3]),'occupation.name']
  occupation.class.table[val,2]<-as.character(occ.name)
  
  df<-subset(workers.data,occupation==occ.no)
  occupation.class.table[val,3]<-nrow(subset(df,three_class==1))/nrow(df)
  occupation.class.table[val,4]<-nrow(subset(df,three_class==2))/nrow(df)
  occupation.class.table[val,5]<-nrow(subset(df,three_class==3))/nrow(df)
}

#save this: it's cool
write.csv(occupation.class.table,'occupation_and_class.CSV')

png('job_level_one.png')
par(mar=c(3,20,3,1))
barplot(height=t(as.matrix(occupation.class.table[1:25,3:5])),col=c('red','green','black'),
        las = 2, names.arg=occupation.class.table[1:25,2], axes=TRUE,cex.names=0.5,horiz=TRUE)
legend('top',legend=c('Manager','supervisor','Neither'),fill=c('red','green','black'),horiz=TRUE)
dev.off()

png('job_level_two.png')
par(mar=c(3,20,3,1))
barplot(height=t(as.matrix(occupation.class.table[26:50,3:5])),col=c('red','green','black'),
        las = 2, names.arg=occupation.class.table[26:50,2], axes=TRUE,cex.names=0.5,horiz=TRUE)
legend('top',legend=c('Manager','supervisor','Neither'),fill=c('red','green','black'),horiz=TRUE)
dev.off()

png('job_level_three.png')
par(mar=c(3,20,3,1))
barplot(height=t(as.matrix(occupation.class.table[51:75,3:5])),col=c('red','green','black'),
        las = 2, names.arg=occupation.class.table[51:75,2], axes=TRUE,cex.names=0.5,horiz=TRUE)
legend('top',legend=c('Manager','supervisor','Neither'),fill=c('red','green','black'),horiz=TRUE)
dev.off()

png('job_level_fourth.png')
par(mar=c(3,20,3,1))
barplot(height=t(as.matrix(occupation.class.table[76:97,3:5])),col=c('red','green','black'),
        las = 2, names.arg=occupation.class.table[76:97,2], axes=TRUE,cex.names=0.5,horiz=TRUE)
legend('top',legend=c('Manager','supervisor','Neither'),fill=c('red','green','black'),horiz=TRUE)
dev.off()

###############################################################

workers.data<-subset(workers.data,part_time_available>=0)
workers.data<-subset(workers.data,job_location>=0)
workers.data<-subset(workers.data,ttonlys_available>=0)
workers.data<-subset(workers.data,compressed_week_available>=0)
workers.data<-subset(workers.data, annualisedhrs_available>=0)
workers.data<-subset(workers.data,other_flex_available>=0)
workers.data<-subset(workers.data,inf_flex_available>=0)
workers.data<-subset(workers.data,wkfrhome_available>=0)
workers.data<-subset(workers.data,wkfrhome_use>=0)
workers.data<-subset(workers.data,term_time_use>=0)
workers.data<-subset(workers.data,part_time_use>=0)
workers.data<-subset(workers.data,compressed_week_use>=0)
workers.data<-subset(workers.data,annualisedhrs_use>=0)
workers.data<-subset(workers.data,wkaut1>=0)
workers.data<-subset(workers.data,wkaut2>=0)
workers.data<-subset(workers.data,wkaut3>=0)
workers.data<-subset(workers.data,wkaut4>=0)
workers.data<-subset(workers.data,wkaut5>=0)
workers.data<-subset(workers.data,hrs_per_week>=0)
workers.data<-subset(workers.data,ot_hrs_per_week>=0)
workers.data<-subset(workers.data,depenth1>=0)
workers.data<-subset(workers.data,depenth2>=0)
workers.data<-subset(workers.data,depenth3>=0)
workers.data<-subset(workers.data,depenth4>=0)
workers.data<-subset(workers.data,depenth5>=0)
workers.data<-subset(workers.data,depenth6>=0)
workers.data<-subset(workers.data,managerial_responsibility>=0)

y<-workers.data[,'GG']



#loop over factor variables to turn them into dummies

fac.vars<-c("marital_stat","education","helps_disables",'sex','job_location','wave_number','managerial_responsibility')
            
for (facvar in fac.vars){
facs<-unique(workers.data[,facvar])
facs<-facs[2:length(facs)]
for (fac in facs){
  df<-as.data.frame(ifelse(workers.data[,facvar]==fac,1,0))
  colnames(df)<-paste(facvar,'_factor_',fac,sep='')
  workers.data<-cbind(workers.data,df)
}
}

#same for ordered varialbes
ordered.vars<-c("part_time_available",       "ttonlys_available",         "compressed_week_available", "annualisedhrs_available",  
"other_flex_available",      "inf_flex_available",        "wkfrhome_available",        "part_time_use"     ,       
"wkfrhome_use",              "term_time_use",             "compressed_week_use",       "annualisedhrs_use",        
"wkaut1",                    "wkaut2",                    "wkaut3",                    "wkaut4"    ,               
"wkaut5",                    "ot_hrs_per_week",           "depenth1" ,                
"depenth2",                  "depenth3",                  "depenth4"      ,            "depenth5",                 
"depenth6",                  "job_satisfaction", 'three_class')

for (ovar in ordered.vars){
  df<-as.data.frame(ifelse(workers.data[,ovar]>median(workers.data[,ovar]),1,0))
  colnames(df)<-paste(ovar,'_dummy',sep='')
  workers.data<-cbind(workers.data,df)
}


x<-cbind(workers.data[,69:133],workers.data[,4],workers.data[,4]^2,workers.data[,55],workers.data[,56],workers.data[,30])

#run the lasso

cv.obj<-cv.glmnet(as.matrix(x),y,family="gaussian",alpha=1,nfolds = 10,lambda= (seq(0,1,0.01)))

first.lasso<-glmnet(x,y,family="gaussian",alpha=1,lambda= (seq(0,1,0.01)))
par(mar=c(3,3,3,2))
plot(first.lasso, "lambda", cex = 0.7, cex.axis=0.7, cex.lab=0.7)

optim.lasso<-glmnet(x,y,family="gaussian",alpha=1,lambda=cv.obj$lambda.min)

#lambda.1se is the largest lambda which keeps us within 1 standard error of the minimum

#return the coefficients I'm interested in:
beta<-as.matrix(coef(optim.lasso))
optim.lasso$df

relevantvars<-as.data.frame(beta)
colnames(relevantvars)<-'beta'
relevantvars<-subset(relevantvars,beta!=0)
rownames(relevantvars)

workers.data.new<-as.data.frame(matrix(nrow=1,ncol=ncol(workers.data)+5))
colnames(workers.data.new)[1:ncol(workers.data)]<-colnames(workers.data)
colnames(workers.data.new)[(ncol(workers.data)+1):(ncol(workers.data)+5)]<-c("GG_lag",
                                                                             "GG_diff_cutoff_lag","GG_quantile_lag","PCA_lag", "JPY_measure_lag" )

#create a lagged mental health variable which will help us dis-entangle 'causal effects'
waves<-unique(workers.data[,'wave_number'])[2:length(unique(workers.data[,'wave_number']))]
for (wv in waves){
  #get the df which is just this year
  df<-subset(workers.data,wave_number==wv)
  #and the previous
  df_lag<-subset(workers.data,wave_number==(wv-2))
  #match up the people
  start<-match('GG',colnames(df))
  end<-match('PCA',colnames(df))
  lagged.mh<-df_lag[match(df[,'pid'],df_lag[,'pid']),start:end]
  colnames(lagged.mh)<-paste(colnames(df[,start:end]),'_lag',sep='')
  #stich together
  df<-cbind(df,lagged.mh)
  
  #same with JPY_Meausre
  lagged.mh<-as.data.frame(df_lag[match(df[,'pid'],df_lag[,'pid']),'JPY_measure'])
  colnames(lagged.mh)<-'JPY_measure_lag'
  #stich together
  df<-cbind(df,lagged.mh)
  
  workers.data.new<-rbind(workers.data.new,df)
  
}

workers.data.new<-subset(workers.data.new,is.na(GG_lag)==FALSE)

#now run a lasso with controls:
y<-workers.data.new[,'GG']
x<-cbind(workers.data.new[,71:133],workers.data.new[,4],workers.data.new[,4]^2,workers.data.new[,55],workers.data.new[,56],
         workers.data.new[,30],workers.data.new[,'GG_lag'])

cv.obj.contr<-cv.glmnet(as.matrix(x),y,family="gaussian",alpha=1,nfolds = 10,lambda= (seq(0,1,0.01)))

contr.lasso<-glmnet(x,y,family="gaussian",alpha=1,lambda=cv.obj.contr$lambda.min)
plot(contr.lasso, "lambda", cex = 0.7, cex.axis=0.7, cex.lab=0.7)

#return the coefficients I'm interested in:
beta.2<-as.matrix(coef(contr.lasso))
contr.lasso$df

relevantvars.2<-as.data.frame(beta.2)
colnames(relevantvars.2)<-'beta'
relevantvars.2<-subset(relevantvars.2,beta!=0)
rownames(relevantvars.2)

################################################################################
#now I want to look at the predictors of people being miserable about their job#
################################################################################
  
y<-workers.data[,'depenth1_dummy']

x<-cbind(workers.data[,'age'],workers.data[,'age']^2,workers.data[,'no_kids'],workers.data[,'hrs_per_week'],workers.data[,'ot_hrs_per_week'],
         workers.data[,71:124],workers.data[,132:133])

cv.obj.dep<-cv.glmnet(as.matrix(x),y,family="gaussian",alpha=1,nfolds = 10,lambda= (seq(0,1,0.01)))

dep.lasso<-glmnet(x,y,family="binomial",alpha=1,lambda= cv.obj.dep$lambda.min)

beta.dep<-as.matrix(coef(dep.lasso))

relevantvars.dep<-as.data.frame(beta.dep)
colnames(relevantvars.dep)<-'beta_depenth'

depenth.list<-c('depenth2_dummy','depenth3_dummy','depenth4_dummy','depenth5_dummy','depenth6_dummy')

for (d in depenth.list){
  y<-workers.data[,d]
  
  cv.obj.dep<-cv.glmnet(as.matrix(x),y,family="gaussian",alpha=1,nfolds = 10,lambda= (seq(0,1,0.01)))
  
  dep.lasso<-glmnet(x,y,family="binomial",alpha=1,lambda= cv.obj.dep$lambda.min)
  
  beta.dep<-as.matrix(coef(dep.lasso))
  
  relevantvars.dep1<-as.data.frame(beta.dep)
  colnames(relevantvars.dep1)<-paste('beta_',d,sep='')
  
  relevantvars.dep<-cbind(relevantvars.dep,relevantvars.dep1)
  
}

#get rid of zeros
relevantvars.dep<-subset(relevantvars.dep,beta_depenth!=0|beta_depenth2_dummy!=0|
                           beta_depenth3_dummy!=0|beta_depenth4_dummy!=0|beta_depenth5_dummy!=0|beta_depenth6_dummy!=0)

common.relevantvars.dep<-subset(relevantvars.dep,beta_depenth!=0&beta_depenth2_dummy!=0&
                                  beta_depenth3_dummy!=0&beta_depenth4_dummy!=0&beta_depenth5_dummy!=0&beta_depenth6_dummy!=0)

#make a barplot
col.vector<-c('red','blue','green','black','purple','yellow','bisque')

#rownames(relevantvars.dep)<-c('intercept','age squared','hrs per week','black','asian',
#                              'GCSE highest qual','aids disabled person','female',
#                              'wave 28','informal flexible work','wfh available',
#                              'no autonomy tasks','no autonomy pace',
 #                             'no autonomy manner','no autonomy task order',
  #                            'ot hours','job satisfaction','higher social class')

par(mar=c(12,4,4,4))

barplot(t(as.matrix(common.relevantvars.dep)),beside = TRUE,cex.axis=1,cex.names=0.75,las=2,col=col.vector,
        main='ten best predictors')
legend('bottom',c('tense','uneasy','worried','depressed','gloomy','miserable'),
       fill=col.vector)

rownames(common.relevantvars.dep)

age.squared<-as.data.frame((workers.data[,'age'])^2)
colnames(age.squared)<-'age_squared'
workers.data<-cbind(workers.data,age.squared)

reg.work.factors<-lm(depenth3_dummy~age+age_squared+factor(marital_stat)+factor(education)+home_owner_dummy+
                factor(sex)+factor(wave_number)+
                +adjusted_hh_income+hrs_per_week+ot_hrs_per_week+factor(job_location)+
                  factor(managerial_responsibility)+factor(ttonlys_available)+
                  factor(compressed_week_available)+factor(annualisedhrs_available)+
                  factor(other_flex_available)+factor(inf_flex_available)+
                  factor(wkfrhome_available)+factor(part_time_use)+
                  factor(wkfrhome_use)+factor(term_time_use)+factor(compressed_week_use)+
                  factor(annualisedhrs_use)+factor(wkaut1)+factor(wkaut2)+factor(wkaut3)+
                  factor(wkaut4)+factor(wkaut5)+factor(job_satisfaction)+factor(three_class), data=workers.data)

reg.work.plus.occ<-lm(depenth3_dummy~age+age_squared+factor(marital_stat)+factor(education)+home_owner_dummy+
                        factor(sex)+factor(wave_number)+
                        +adjusted_hh_income+hrs_per_week+ot_hrs_per_week+factor(job_location)+
                        factor(managerial_responsibility)+factor(ttonlys_available)+
                        factor(compressed_week_available)+factor(annualisedhrs_available)+
                        factor(other_flex_available)+factor(inf_flex_available)+
                        factor(wkfrhome_available)+factor(part_time_use)+
                        factor(wkfrhome_use)+factor(term_time_use)+factor(compressed_week_use)+
                        factor(annualisedhrs_use)+factor(wkaut1)+factor(wkaut2)+factor(wkaut3)+
                        factor(wkaut4)+factor(wkaut5)+factor(job_satisfaction)+
                        factor(three_class)+factor(occupation), data=workers.data)

scaled.vars<-as.data.frame(workers.data[,'hrs_per_week']/max(workers.data[,'hrs_per_week']))
                  

colnames(scaled.vars)<-'hours_per_week'

job.characteristics<-prcomp(cbind(scaled.vars[,'hours_per_week'],
                                  workers.data[,'managerial_responsibility_factor_2'],
                                  workers.data[,'managerial_responsibility_factor_3'],  workers.data[,'ttonlys_available_dummy'],workers.data[,'compressed_week_available_dummy'],    
workers.data[,'annualisedhrs_available_dummy'], workers.data[,'other_flex_available_dummy'],          workers.data[,'inf_flex_available_dummy'],           
workers.data[,'wkfrhome_available_dummy'],workers.data[,'part_time_use_dummy'],                 workers.data[,'wkfrhome_use_dummy'],                 
workers.data[,'term_time_use_dummy'], workers.data[,'compressed_week_use_dummy'],           workers.data[,'annualisedhrs_use_dummy'],            
workers.data[,'wkaut1_dummy'], workers.data[,'wkaut2_dummy'],                        workers.data[,'wkaut3_dummy'],                       
workers.data[,'wkaut4_dummy'],  workers.data[,'wkaut5_dummy'],                        workers.data[,'job_satisfaction_dummy'],             
workers.data[,'three_class_dummy']))

pca.weights<-as.data.frame(job.characteristics$rotation)

cumulative.job.charcateristics.variance<-(job.characteristics$sdev)^2

cumulative.job.charcateristics.variance<-cumulative.job.charcateristics.variance/sum(cumulative.job.charcateristics.variance)

for (val in (seq(from=2,to=length(job.characteristics$sdev),by=1))){
  cumulative.job.charcateristics.variance[val]<-cumulative.job.charcateristics.variance[val]+cumulative.job.charcateristics.variance[val-1]
}

job.vars<-c('hours_per_week','managerial_responsibility_factor_2',
                            'managerial_responsibility_factor_3','ttonlys_available_dummy','compressed_week_available_dummy',    
'annualisedhrs_available_dummy', 'other_flex_available_dummy',  'inf_flex_available_dummy',           
'wkfrhome_available_dummy','part_time_use_dummy',  'wkfrhome_use_dummy',                 
'term_time_use_dummy', 'compressed_week_use_dummy', 'annualisedhrs_use_dummy',            
'wkaut1_dummy', 'wkaut2_dummy', 'wkaut3_dummy',                       
'wkaut4_dummy','wkaut5_dummy', 'job_satisfaction_dummy',             
'three_class_dummy')

par(mar=c(12,4,4,4))

barplot(t(pca.weights[,1:5]),beside=TRUE,cex.axis=1,cex.names=0.75,las=2,col=col.vector[1:5],
        main='PCA weights',names.arg = job.vars,las=2)

legend('topleft',c('first PC','second PC','third PC','fourth PC','fifth PC'),fill=col.vector[1:5],horiz=TRUE)

workers.data.with.pca<-cbind(workers.data,job.characteristics$x)

comp.reg.nofe<-lm(monthly_labor_income~age+age_squared+factor(education)+
               PC1+PC2+PC3+PC4+PC5+factor(sex)+factor(race)+factor(full_time)+
               GG+factor(wave_number),data=workers.data.with.pca)

comp.reg<-lm(monthly_labor_income~age+age_squared+
               PC1+PC2+PC3+PC4+PC5++factor(full_time)+factor(pid)+
               GG+factor(wave_number),data=workers.data.with.pca)


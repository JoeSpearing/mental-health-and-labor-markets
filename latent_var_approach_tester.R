rm(list=ls())
library(FactoMineR)
library(sandwich)
library(aod)
library(stargazer)
setwd('C:\\Users\\Joe Spearing\\Documents\\measuring morbidity\\UKDA-6614-spss\\spss\\spss24\\ukhls_wx')
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

#get rid of other null results
full.data.set<-subset(full.data.set,is.na(pid)==FALSE)
full.data.set<-subset(full.data.set,employment_stat>=0)
full.data.set<-subset(full.data.set,age>=0)
full.data.set<-subset(full.data.set,sex>=0)
full.data.set<-subset(full.data.set,race>=0)

race.dummies<-as.data.frame(matrix(data=0,nrow=nrow(full.data.set),ncol=2))
colnames(race.dummies)<-c('black','asian')
race.dummies[,'black']<-ifelse((full.data.set[,'race']==2|full.data.set[,'race']==3|full.data.set[,'race']==4)&full.data.set[,'wave_number']<19,1,race.dummies[,'black'])
race.dummies[,'black']<-ifelse((full.data.set[,'race']==14|full.data.set[,'race']==15|full.data.set[,'race']==16)&full.data.set[,'wave_number']>18,1,race.dummies[,'black'])

race.dummies[,'asian']<-ifelse((full.data.set[,'race']==5|full.data.set[,'race']==6|
                                  full.data.set[,'race']==7|full.data.set[,'race']==8)&full.data.set[,'wave_number']<19,1,race.dummies[,'asian'])
race.dummies[,'asian']<-ifelse((full.data.set[,'race']==9|full.data.set[,'race']==10|
                                  full.data.set[,'race']==11|full.data.set[,'race']==12)&full.data.set[,'wave_number']>18,1,race.dummies[,'asian'])

full.data.set<-cbind(full.data.set,race.dummies)

length(unique(full.data.set[,'pid']))

#########################################
# creating a summary table for the data #
#########################################

summary.table<-as.data.frame(matrix(nrow=20,ncol=4))
rownames(summary.table)<-c('percent male','percent black','percent asian','age','percent employed','percent unemployed','percent incapacitated',
                           'percent retired',
  'concentration','sleep','useful role','making decisions','under strain',
  'overcoming difficulties','enjoy activities','can face problems','depressed',
  'loss of confidence','believe worthless','happiness')
colnames(summary.table)<-c('mean','median','min','max')

summary.table['percent male','mean']<-100*sum(full.data.set$sex==1)/nrow(full.data.set)
summary.table['percent black','mean']<-100*sum(full.data.set$black==1)/nrow(full.data.set)
summary.table['percent asian','mean']<-100*sum(full.data.set$asian==1)/nrow(full.data.set)
summary.table['age','mean']<-mean(full.data.set[,'age'])
summary.table['age','median']<-median(full.data.set[,'age'])
summary.table['age','min']<-min(full.data.set[,'age'])
summary.table['age','max']<-max(full.data.set[,'age'])
summary.table['percent employed','mean']<-100*(sum(full.data.set$employment_stat==1)+sum(full.data.set$employment_stat==2))/nrow(full.data.set)
summary.table['percent unemployed','mean']<-100*(sum(full.data.set$employment_stat==3))/nrow(full.data.set)
summary.table['percent incapacitated','mean']<-100*(sum(full.data.set$employment_stat==8))/nrow(full.data.set)
summary.table['percent retired','mean']<-100*(sum(full.data.set$employment_stat==4))/nrow(full.data.set)

for (val in seq(from=1,to=12,by=1)){
  summary.table[8+val,'mean']<-mean(full.data.set[,5+val])
  summary.table[8+val,'median']<-median(full.data.set[,5+val])
  summary.table[8+val,'min']<-min(full.data.set[,5+val])
  summary.table[8+val,'max']<-max(full.data.set[,5+val])
  
}
summary.table<-round(summary.table,2)

#sum up our measures of mental health to see if the 0 or 48 problem is real
sum.mh.vars<-full.data.set[,'GHQ1']+full.data.set[,'GHQ2']+full.data.set[,'GHQ3']+full.data.set[,'GHQ4']+full.data.set[,'GHQ5']+full.data.set[,'GHQ6']+
  full.data.set[,'GHQ7']+full.data.set[,'GHQ8']+full.data.set[,'GHQ9']+full.data.set[,'GHQ10']+full.data.set[,'GHQ11']+full.data.set[,'GHQ12']

############################################################################
#Alright, now what we're going to do is run this massive fuck-off algorithm#
############################################################################

#we have the following parameters: sigma, alpha 2-4 for each of the 12 measures (1+5*12)=61 in total.
#first create a function which is the differential of the likelihood function w.r.t. all of my coefficients
coef.guess<-c(5,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,
              -9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999,-9999,-15,0,15,9999)

#I'm going to want a "smallest possible number"
eps<-2.220446e-16

#we're going to require some Tauchen-Hussey weights and xs:
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\R_scripts')
TH.data<-read.csv('TH_x_w.CSV')

#set number of grid points:
N<-9

#Given sigma^2, and a number of points, N, construct Y and W
TH<-function(var,N){
  #y is some equally space number of points between a max and min
  y_max<-3.1910*sqrt(var)
  y<-seq(from=-y_max,to=y_max,length.out = N)
  #get the distance between each point
  d<-y[2]-y[1]
  #then get a vector of probabilities
  probs<-y
  for (p in seq(from=2,to=N-1,by=1)){
    probs[p]<-pnorm((y[p]+d/2)/sqrt(var))-pnorm((y[p]-d/2)/sqrt(var))
  }
  probs[1]<-pnorm((y[1]+d/2)/sqrt(var))
  probs[N]<-1-pnorm((y[N]-d/2)/sqrt(var))
  
  
return(cbind(y,probs))
  
}
#numerical normal
norm<-TH(coef.guess[1]^2,N)


#test how good this is
TH.test<-matrix(data=0,nrow=98,ncol=2)
for (val in seq(from=3,to=100,by=1)){
  norm<-TH(coef.guess[1]^2,val)
  TH.test[val-2,1]<-sum(norm[,1]*norm[,2])
  TH.test[val-2,2]<-sum((norm[,1]^2)*norm[,2])
}


#I'm going to use a subset of the data
sample.data.set<-full.data.set[match(sample(full.data.set[,'pid'],size=1000,replace=TRUE),full.data.set[,'pid']),]

#create a function which returns the likelihood function

likelihood_i_function<-function(coef){
  #coef<-coef.guess
  #first get the weights for the normal
  norm<-TH(coef[1]^2)
  #get the relevant dataset
  rel.data<-sample.data.set[,6:17]
  #this is going to be the output:
  likelihood<-0
  #looping over people
  for (val in seq(from=1,to=nrow(rel.data),by=1)){
  #val<-2
  #this gets you the person's data
  per.data<-rel.data[val,] 
  #initialise the likelihood for person it
  likelihood_i<-0
  #also loop over M
  for (val.1 in seq(from=1,to=9,by=1)){
    m<-norm[val.1,'y']
    
    #start by getting 'C'
    C<-log(max(pnorm(coef[as.numeric((per.data[1])+2)]-m)-pnorm(coef[as.numeric((per.data[1])+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[2])+5+2)]-m)-pnorm(coef[as.numeric((per.data[2])+5+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[3])+10+2)]-m)-pnorm(coef[as.numeric((per.data[3])+10+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[4])+15+2)]-m)-pnorm(coef[as.numeric((per.data[4])+15+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[5])+20+2)]-m)-pnorm(coef[as.numeric((per.data[5])+20+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[6])+25+2)]-m)-pnorm(coef[as.numeric((per.data[6])+25+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[7])+30+2)]-m)-pnorm(coef[as.numeric((per.data[7])+30+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[8])+35+2)]-m)-pnorm(coef[as.numeric((per.data[8])+35+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[9])+40+2)]-m)-pnorm(coef[as.numeric((per.data[9])+40+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[10])+45+2)]-m)-pnorm(coef[as.numeric((per.data[10])+45+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[11])+50+2)]-m)-pnorm(coef[as.numeric((per.data[11])+50+1)]-m),eps))+
      log(max(pnorm(coef[as.numeric((per.data[12])+55+2)]-m)-pnorm(coef[as.numeric((per.data[12])+55+1)]-m),eps)) 
    C<-exp(C)*((1/sqrt(2*pi))^12)
    #C<-1
    #use this to update the likelihood for i
    likelihood_i<-likelihood_i+norm[val.1,'weights']*C
    results[val.1,1]<-norm[val.1,'weights']*C
  }
  #now update the total likelihood
  likelihood<-likelihood+log(likelihood_i)
  }
  return(likelihood)
}

likelihood_i_function(coef.guess)

#create a function which returns the first differential w.r.t sigma squared

dif_sigma<-function(coef){
  #coef<-coef.guess
  #first get the weights for the normal
  norm<-TH(coef[1]^2)
  #return the relevant data
  rel.data<-cbind(as.matrix(sample.data.set[,6:17]),matrix(nrow=nrow(sample.data.set),ncol=1))
  #this is going to be the output
  diff.sigma<-0
  #now for each person
  for (val in seq(from=1,to=nrow(rel.data),by=1)){
  #val<-2
  #get their data
    pers.data<-rel.data[val,]
    
    #we want to set their numator and denominator to one
    numerator.sum<-0
    denominator.sum<-0
    
    
    #also loop over m
    for (val.1 in seq(from=1,to=9,by=1)){
      m<-norm[val.1,'y']
      #now get C
      C<-log(max(pnorm(coef[as.numeric((per.data[1])+2)]-m)-pnorm(coef[as.numeric((per.data[1])+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[2])+5+2)]-m)-pnorm(coef[as.numeric((per.data[2])+5+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[3])+10+2)]-m)-pnorm(coef[as.numeric((per.data[3])+10+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[4])+15+2)]-m)-pnorm(coef[as.numeric((per.data[4])+15+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[5])+20+2)]-m)-pnorm(coef[as.numeric((per.data[5])+20+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[6])+25+2)]-m)-pnorm(coef[as.numeric((per.data[6])+25+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[7])+30+2)]-m)-pnorm(coef[as.numeric((per.data[7])+30+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[8])+35+2)]-m)-pnorm(coef[as.numeric((per.data[8])+35+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[9])+40+2)]-m)-pnorm(coef[as.numeric((per.data[9])+40+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[10])+45+2)]-m)-pnorm(coef[as.numeric((per.data[10])+45+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[11])+50+2)]-m)-pnorm(coef[as.numeric((per.data[11])+50+1)]-m),eps))+
        log(max(pnorm(coef[as.numeric((per.data[12])+55+2)]-m)-pnorm(coef[as.numeric((per.data[12])+55+1)]-m),eps)) 
      
      C<-exp(C)*((1/sqrt(2*pi))^12)
      #C<-1
      #get the numerator
      numerator<-C*((m^2)/(coef[1]^3)-1/(coef[1]))
      #and the denominator
      denominator<-C
      
      numerator.sum<-numerator.sum+numerator*norm[val.1,'weights']
      denominator.sum<-denominator.sum+denominator*norm[val.1,'weights']
      #denominator.sum<-1
      
      results[val.1,1]<-numerator*norm[val.1,'weights']
      
    }
    #Now use this to get the fraction for person (it)
    #diff.sigma<-diff.sigma+(numerator.sum/denominator.sum)
    diff.sigma<-diff.sigma+round(numerator.sum/denominator.sum,3)
    rel.data[val,13]<-(numerator.sum/denominator.sum)
  }
  
  return(diff.sigma)
}

dif_sigma(coef.guess)

#########################################################################
#check that this 'works' by comparing this to the numerical differential#
#########################################################################

coef.guess.delta<-coef.guess+matrix(data=c(0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),ncol=1,nrow=length(coef.guess))

((likelihood_i_function(coef.guess.delta)-likelihood_i_function(coef.guess))/0.01)


##########################################################################
#alright, now the function which differentiates w.r.t to the bloody alphas

dif_alphas<-function(coef){
  #coef<-coef.guess
  norm<-TH(coef[1]^2)
  #return the relevant data
  rel.data<-cbind(as.matrix(sample.data.set[,6:17]),matrix(nrow=nrow(sample.data.set),ncol=1))
  #this is going to by the output
  diff.alphas<-matrix(data=0,nrow=36,ncol=1)
  
  #loop over people
  for (val in seq(from=1,to=nrow(rel.data),by=1)){
    #get their data
    pers.data<-rel.data[val,]
    
  #B is the denominator 
  B<-0  
  #and these the numerators
  diff.alphas.it<-matrix(data=0,nrow=36,ncol=1)
  
  #handily it starts as being C from before  
  for (val.1 in seq(from=1,to=9,by=1)){
    m<-norm[val.1,'y']
    #now get C
    C<-log(max(pnorm(coef[(pers.data[1])+2]-m)-pnorm(coef[(pers.data[1])+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[2])+5+2]-m)-pnorm(coef[(pers.data[2])+5+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[3])+10+2]-m)-pnorm(coef[(pers.data[3])+10+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[4])+15+2]-m)-pnorm(coef[(pers.data[4])+15+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[5])+20+2]-m)-pnorm(coef[(pers.data[5])+20+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[6])+25+2]-m)-pnorm(coef[(pers.data[6])+25+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[7])+30+2]-m)-pnorm(coef[(pers.data[7])+30+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[8])+35+2]-m)-pnorm(coef[(pers.data[8])+35+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[9])+40+2]-m)-pnorm(coef[(pers.data[9])+40+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[10])+45+2]-m)-pnorm(coef[(pers.data[10])+45+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[11])+50+2]-m)-pnorm(coef[(pers.data[11])+50+1]-m),eps))+
      log(max(pnorm(coef[(pers.data[12])+55+2]-m)-pnorm(coef[(pers.data[12])+55+1]-m),eps))
    
    C<-exp(C)/((sqrt(2*pi))^12)  
    #thereby get B
    B<-B+norm[val.1,'weights']*C*(1/(sqrt(2*pi*coef[1]^2)))*exp(-(1/2)*(m^2)/(coef[1]^2))
    
    #and N the numerator, which is going to differ by the partial:
    for (a in seq(from=1,to=36,by=1)){
    
    #find out which 'j' we're talking about
    j<-ceiling(a/3)
    #and which s we're talking about
    s<-a-(j-1)*3+1
    #need a forumla to select the right alpha
    alpha<-coef[1+(j-1)*5+s]
    
    #if the weight is zero, we can ignore this
    if(norm[val.1,'weights']!=0){
    
    #if we have to care on one side
      
    if(pers.data[j]==s-1){
      #I'm going to take logs to (hopefully) avoid this going to infinity
      diff.alphas.it[a,1]<-diff.alphas.it[a,1]+
        exp(log(norm[val.1,'weights']*(1/(sqrt(2*pi*coef[1]^2)))*exp(-(1/2)*(m/coef[1])^2))+
        ((alpha-m)^2)/2+
        log(C)-(log(max(pnorm(coef[pers.data[j]+2+5*(j-1)]-m)-pnorm(coef[pers.data[j]+1+5*(j-1)]-m),eps))))
    }  
      
    if(pers.data[j]==s){  
      #it's the same but minus in this case
      diff.alphas.it[a,1]<-diff.alphas.it[a,1]-
        exp(log(norm[val.1,'weights']*(1/(sqrt(2*pi*coef[1]^2)))*exp(-(1/2)*(m/coef[1])^2))+
              ((alpha-m)^2)/2+
              log(C)-(log(max(pnorm(coef[pers.data[j]+2+5*(j-1)]-m)-pnorm(coef[pers.data[j]+1+5*(j-1)]-m),eps))))
      
    
    
    }
      #This was the original
      #diff.alphas.it[a,1]<-diff.alphas.it[a,1]+norm[val.1,'weights']*(1/(sqrt(2*pi*coef[1]^2)))*exp(-(1/2)*(m/coef[1])^2)*
       # (as.numeric((pers.data[j]==s-1))*exp(((alpha-m)^2)/2)-as.numeric((pers.data[j]==s))*exp(((alpha-m)^2)/2))*
        #exp(log(C)-(log(max(pnorm(coef[pers.data[j]+2+5*(j-1)]-m)-pnorm(coef[pers.data[j]+1+5*(j-1)]-m),eps))))
    }
  }
  }
  #results[val,]<-t(diff.alphas.it[,1]/B)
  #B is the same for each person, so we can now right
  diff.alphas.it[,1]<-diff.alphas.it[,1]/B
  diff.alphas[,1]<-diff.alphas[,1]+diff.alphas.it[,1]
  
  }
  return(diff.alphas)
}

dif_alphas(coef.guess)

#now we need a Hessian function to be evaluated at the coefficients
dif_Hessian<-function(coef){
 #our Hessian is 37x37
  #coef<-coef.guess
  Hessian<-matrix(nrow=37,ncol=37)
  #do the first row because it's easy
    zeros<-matrix(ncol=1,nrow=length(coef),data=0)
    zeros[1,1]<-10*eps
    coef.plus.delta<-coef+zeros
    Hessian[1,1]<-(dif_sigma(coef.plus.delta)-dif_sigma(coef))/(10*eps)
    #do the rest of the top row while we're there
    Hessian[1,2:37]<-t((dif_alphas(coef.plus.delta)-dif_alphas(coef))/(10*eps))
  
  #now do all subsequent ones
    for (val in seq(from=2,to=37)){
    #get the coefficients as before
      j<-ceiling((val-1)/3)
      s<-val-(j-1)*3
      coef.no<-1+(j-1)*5+s
      #okay, now we can do all subsequent rows:
      zeros<-matrix(ncol=1,nrow=length(coef),data=0)
      zeros[coef.no,1]<-1
      coef.plus.delta<-coef+zeros
      Hessian[val,1]<-(dif_sigma(coef.plus.delta)-dif_sigma(coef))
      #do the rest of the top row while we're there: problem, the delta one returns an NAN!
      Hessian[val,2:37]<-t((dif_alphas(coef.plus.delta)-dif_alphas(coef)))
      
      
    }
return(Hessian)
}  

solve(dif_Hessian(coef.guess))

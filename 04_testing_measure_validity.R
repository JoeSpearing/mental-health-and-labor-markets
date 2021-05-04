rm(list=ls())
count.to.nine<-seq(from=1,to=9,by=1)
letter.list<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')
#for each number
for (val in count.to.nine){
  #set directory
  directory<-paste('C:\\Users\\Joe Spearing\\Documents\\measuring morbidity\\UKDA-6614-spss\\spss\\spss24\\ukhls_w',val,sep='')
  setwd(directory)
  #upload data: indresp
  file.name<-paste(letter.list[val],'_indresp.SAV',sep='')
  df<-read_sav(file.name)
  df_name<-paste('indresp_',val+18,sep='')
  assign(df_name,df)
  #hhresp
  file.name<-paste(letter.list[val],'_hhresp.SAV',sep='')
  df<-read_sav(file.name)
  df_name<-paste('hhresp_',val+18,sep='')
  assign(df_name,df)
}


indresp.list<-list(indresp_19,indresp_20,indresp_21,indresp_22,indresp_23,indresp_24,indresp_25,indresp_26,indresp_27)

#need to do waves 1 and 2 separately
val<-1
  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  age<-paste(letter.list[val],'_age_dv',sep='')
  sex<-paste(letter.list[val],'_sex',sep='')
  GHQ1<-paste(letter.list[val],'_scghqa',sep='')
  GHQ2<-paste(letter.list[val],'_scghqb',sep='')
  GHQ3<-paste(letter.list[val],'_scghqc',sep='')
  GHQ4<-paste(letter.list[val],'_scghqd',sep='')
  GHQ5<-paste(letter.list[val],'_scghqe',sep='')
  GHQ6<-paste(letter.list[val],'_scghqf',sep='')
  GHQ7<-paste(letter.list[val],'_scghqg',sep='')
  GHQ8<-paste(letter.list[val],'_scghqh',sep='')
  GHQ9<-paste(letter.list[val],'_scghqi',sep='')
  GHQ10<-paste(letter.list[val],'_scghqj',sep='')
  GHQ11<-paste(letter.list[val],'_scghqk',sep='')
  GHQ12<-paste(letter.list[val],'_scghql',sep='')
  dep<-paste(letter.list[val],'_hcond17',sep='')
  dep.s<-paste(letter.list[val],'_hconds17',sep='')
  dep.n<-paste(letter.list[val],'_hcondn17',sep='')
  dep.a<-paste(letter.list[val],'_hconda17',sep='')
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val])
  
  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],indresp_n[,age],indresp_n[,sex],indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],
                          indresp_n[,dep],indresp_n[,dep.s],-9,indresp_n[,dep.a],val))
  #name columns
  colnames(df)<-c('pid','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','dep', 'dep_still','dep_new','dep_age','wave_number')
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
val<-2

  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  age<-paste(letter.list[val],'_age_dv',sep='')
  sex<-paste(letter.list[val],'_sex',sep='')
  GHQ1<-paste(letter.list[val],'_scghqa',sep='')
  GHQ2<-paste(letter.list[val],'_scghqb',sep='')
  GHQ3<-paste(letter.list[val],'_scghqc',sep='')
  GHQ4<-paste(letter.list[val],'_scghqd',sep='')
  GHQ5<-paste(letter.list[val],'_scghqe',sep='')
  GHQ6<-paste(letter.list[val],'_scghqf',sep='')
  GHQ7<-paste(letter.list[val],'_scghqg',sep='')
  GHQ8<-paste(letter.list[val],'_scghqh',sep='')
  GHQ9<-paste(letter.list[val],'_scghqi',sep='')
  GHQ10<-paste(letter.list[val],'_scghqj',sep='')
  GHQ11<-paste(letter.list[val],'_scghqk',sep='')
  GHQ12<-paste(letter.list[val],'_scghql',sep='')
  dep<-paste(letter.list[val],'_hcond17',sep='')
  dep.s<-paste(letter.list[val],'_hconds17',sep='')
  dep.n<-paste(letter.list[val],'_hcondn17',sep='')
  dep.a<-paste(letter.list[val],'_hconda17',sep='')
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val])
  
  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],indresp_n[,age],indresp_n[,sex],indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],
                          -9,-9,indresp_n[,dep.n],-9,val))
  #name columns
  colnames(df)<-c('pid','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','dep', 'dep_still','dep_new','dep_age','wave_number')
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  


for (val in seq(from=3,to=9,by=1)){
  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  age<-paste(letter.list[val],'_age_dv',sep='')
  sex<-paste(letter.list[val],'_sex',sep='')
  GHQ1<-paste(letter.list[val],'_scghqa',sep='')
  GHQ2<-paste(letter.list[val],'_scghqb',sep='')
  GHQ3<-paste(letter.list[val],'_scghqc',sep='')
  GHQ4<-paste(letter.list[val],'_scghqd',sep='')
  GHQ5<-paste(letter.list[val],'_scghqe',sep='')
  GHQ6<-paste(letter.list[val],'_scghqf',sep='')
  GHQ7<-paste(letter.list[val],'_scghqg',sep='')
  GHQ8<-paste(letter.list[val],'_scghqh',sep='')
  GHQ9<-paste(letter.list[val],'_scghqi',sep='')
  GHQ10<-paste(letter.list[val],'_scghqj',sep='')
  GHQ11<-paste(letter.list[val],'_scghqk',sep='')
  GHQ12<-paste(letter.list[val],'_scghql',sep='')
  dep<-paste(letter.list[val],'_hcond17',sep='')
  dep.s<-paste(letter.list[val],'_hconds17',sep='')
  dep.n<-paste(letter.list[val],'_hcondn17',sep='')
  dep.a<-paste(letter.list[val],'_hconda17',sep='')
  JPY.measure<-
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val])
  
  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],indresp_n[,age],indresp_n[,sex],indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],
                          indresp_n[,dep],indresp_n[,dep.s],indresp_n[,dep.n],indresp_n[,dep.a],val))
  #name columns
  colnames(df)<-c('pid','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','dep', 'dep_still','dep_new','dep_age','wave_number')
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
}
  

full.data.set<-rbind(wave19data,wave20data,wave21data,wave22data,wave23data,wave24data,wave25data,wave26data,wave27data)

#create our measure of mental health
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

mental.health.measures<-as.data.frame(matrix(data=0,ncol=3,nrow=nrow(full.data.set)))
colnames(mental.health.measures)<-c('measure_1','measure_2','measure_3')

#measure 1
mental.health.measures[,'measure_1']<-round(full.data.set[,'GHQ1']/4)+round(full.data.set[,'GHQ2']/4)+round(full.data.set[,'GHQ3']/4)+
  round(full.data.set[,'GHQ4']/4)+round(full.data.set[,'GHQ5']/4)+round(full.data.set[,'GHQ6']/4)+round(full.data.set[,'GHQ7']/4)+
  round(full.data.set[,'GHQ8']/4)+round(full.data.set[,'GHQ9']/4)+round(full.data.set[,'GHQ10']/4)+round(full.data.set[,'GHQ11']/4)+
  round(full.data.set[,'GHQ12']/4)

#measure 2
mental.health.measures[,'measure_2']<-(full.data.set[,'GHQ1']/4)+(full.data.set[,'GHQ2']/4)+(full.data.set[,'GHQ3']/4)+
(full.data.set[,'GHQ4']/4)+(full.data.set[,'GHQ5']/4)+(full.data.set[,'GHQ6']/4)+(full.data.set[,'GHQ7']/4)+
  (full.data.set[,'GHQ8']/4)+(full.data.set[,'GHQ9']/4)+(full.data.set[,'GHQ10']/4)+(full.data.set[,'GHQ11']/4)+
  (full.data.set[,'GHQ12']/4)

#measure 3
mental.health.measures[,'measure_3']<-floor((full.data.set[,'GHQ1']-1)/3)+floor((full.data.set[,'GHQ2']-1)/3)+floor((full.data.set[,'GHQ3']-1)/3)+
  floor((full.data.set[,'GHQ4']-1)/3)+floor((full.data.set[,'GHQ5']-1)/3)+floor((full.data.set[,'GHQ6']-1)/3)+floor((full.data.set[,'GHQ7']-1)/3)+
  floor((full.data.set[,'GHQ8']-1)/3)+floor((full.data.set[,'GHQ9']-1)/3)+floor((full.data.set[,'GHQ10']-1)/3)+floor((full.data.set[,'GHQ11']-1)/3)+
  floor((full.data.set[,'GHQ12']-1)/3)

full.data.set<-cbind(full.data.set,mental.health.measures)
#what is the average score amongst someone with depression?
df<-subset(full.data.set,dep_still==1|dep_new==1)
dep.m1<-mean(df[,'measure_1'])
dep.m2<-mean(df[,'measure_2'])
dep.m3<-mean(df[,'measure_3'])
#and without depression?
df<-subset(full.data.set,dep_still==0|dep_new==0)
n.dep.m1<-mean(df[,'measure_1'])
n.dep.m2<-mean(df[,'measure_2'])
n.dep.m3<-mean(df[,'measure_3'])

#################################
# probability of new depression #
#################################

df<-subset(full.data.set,dep_new>=0)
prob.dep<-glm(dep_new~measure_1,data=df,family=binomial(probit))
exp(0.11979)

##########################
#post-depression profile #
##########################

df<-subset(full.data.set,dep_age>0)
age.minus.diag<-as.data.frame(df[,'age']-df[,'dep_age'])
colnames(age.minus.diag)<-'age_minus_diag'
df<-cbind(df,age.minus.diag)
df<-subset(df,age_minus_diag>=0)
df<-subset(df,age_minus_diag<=15)
df<-subset(df,dep_still>0)
df[,'age_minus_diag']<-as.factor(df[,'age_minus_diag'])
age.profile.reg<-lm(measure_1~factor(age_minus_diag),data=df)
coefs<-age.profile.reg$coefficients

profile<-matrix(nrow=15,ncol=3)
profile[,1]<-coefs[2:16]+coefs[1]
plot(profile[,1],type='l',ylim=c(1,10),xlab='years after diagnosis',ylab='mental heath score',main='Time effect on score',col='red')

age.profile.reg<-lm(measure_1~factor(age_minus_diag)+factor(dep_still)*factor(age_minus_diag),data=df)
coefs<-age.profile.reg$coefficients
profile[,2]<-coefs[2:16]+coefs[1]
profile[,3]<-profile[,2]+coefs[17]+coefs[18:32]

lines(profile[,2],col='blue')
lines(profile[,3],col='green')

legend('topright',legend=c('total','still have depression','no longer have depression'),fill=c('red','blue','green'))

rm(list=ls())
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\raw_data')
CPI.data<-read.csv('CPI_ONS.CSV')
CPI.data<-CPI.data[3:31,]
library(foreign)
library(haven)
###
#upload this data: BHPS
count.to.seventeen<-seq(from=1,to=18,by=1)
letter.list<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')
#for each number
for (val in count.to.seventeen){
  #set directory
  directory<-paste('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\raw_data\\bhps_w',val,sep='')
  setwd(directory)
  #upload data: indresp
  file.name<-paste('b',letter.list[val],'_indresp.SAV',sep='')
  df<-read_sav(file.name)
  df_name<-paste('indresp_',val,sep='')
  assign(df_name,df)
  #hhresp
  file.name<-paste('b',letter.list[val],'_hhresp.SAV',sep='')
  df<-read_sav(file.name)
  df_name<-paste('hhresp_',val,sep='')
  assign(df_name,df)
}

#and now the understanding society data
count.to.ten<-seq(from=1,to=10,by=1)
#for each number
for (val in count.to.ten){
  #set directory
  directory<-paste('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\raw_data\\ukhls_w',val,sep='')
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


#we also need cross-wave data
setwd('C:\\Users\\Joe Spearing\\Documents\\measuring morbidity\\UKDA-6614-spss\\spss\\spss24\\ukhls_wx')
cross.wave.data<-read_sav('xwavedat.SAV')

#create some df lists

indresp.list<-list(indresp_1,indresp_2,indresp_3,indresp_4,indresp_5,indresp_6,indresp_7,indresp_8,indresp_9,indresp_10,
                   indresp_11,indresp_12,indresp_13,indresp_14,indresp_15,indresp_16,indresp_17,indresp_18,indresp_19,
                   indresp_20,indresp_21,indresp_22,indresp_23,indresp_24,indresp_25,indresp_26,indresp_27,indresp_28)

hhresp.list<-list(hhresp_1,hhresp_2,hhresp_3,hhresp_4,hhresp_5,hhresp_6,hhresp_7,hhresp_8,hhresp_9,hhresp_10,
                  hhresp_11,hhresp_12,hhresp_13,hhresp_14,hhresp_15,hhresp_16,hhresp_17,hhresp_18,hhresp_19,
                  hhresp_20,hhresp_21,hhresp_22,hhresp_23,hhresp_24,hhresp_25,hhresp_26,hhresp_27,hhresp_28)

#for the BHPS waves
#given we're in wave val
for (val in count.to.seventeen){
#get a list of variables (and names) I want to extract
person.ID<-'pid'
employment.stat<-paste('b',letter.list[val],'_jbstat',sep='')
age<-paste('b',letter.list[val],'_age',sep='')
sex<-paste('b',letter.list[val],'_sex',sep='')
GHQ1<-paste('b',letter.list[val],'_scghqa',sep='')
GHQ2<-paste('b',letter.list[val],'_scghqb',sep='')
GHQ3<-paste('b',letter.list[val],'_scghqc',sep='')
GHQ4<-paste('b',letter.list[val],'_scghqd',sep='')
GHQ5<-paste('b',letter.list[val],'_scghqe',sep='')
GHQ6<-paste('b',letter.list[val],'_scghqf',sep='')
GHQ7<-paste('b',letter.list[val],'_scghqg',sep='')
GHQ8<-paste('b',letter.list[val],'_scghqh',sep='')
GHQ9<-paste('b',letter.list[val],'_scghqi',sep='')
GHQ10<-paste('b',letter.list[val],'_scghqj',sep='')
GHQ11<-paste('b',letter.list[val],'_scghqk',sep='')
GHQ12<-paste('b',letter.list[val],'_scghql',sep='')
occ<-paste('b',letter.list[val],'_jbisco88_cc',sep='')
soc<-paste('b',letter.list[val],'_jbisco_cc',sep='')
mar.stat<-paste('b',letter.list[val],'_mastat',sep='')
educ<-paste('b',letter.list[val],'_qfedhi',sep='')
monthly.labor.income<-paste('b',letter.list[val],'_fimnlabgrs_dv',sep='')
job.level<-paste('b',letter.list[val],'_jbnssec3_dv',sep='')
sickness.benefit<-paste('b',letter.list[val],'_f158',sep='')
full.time<-paste('b',letter.list[val],'_jbft_dv',sep='')
aids.disables<-paste('b',letter.list[val],'_aidhh',sep='')
region<-paste('b',letter.list[val],'_gor_dv',sep='')
jbhrs<-paste('b',letter.list[val],'_jbhrs',sep='')
hh.kids<-paste('b',letter.list[val],'_nkids_dv',sep='')
hh.income<-paste('b',letter.list[val],'_fihhmngrs_dv',sep='')
hh.conversion.factor<-paste('b',letter.list[val],'_hhnetde2',sep='')
hh.identifier<-paste('b',letter.list[val],'_hid',sep='')
hh.ownership<-paste('b',letter.list[val],'_hsownd_bh',sep='')

#find the indresp df we want
indresp_n<-as.data.frame(indresp.list[val])
#and the hhresp
hhresp_n<-as.data.frame(hhresp.list[val])
#construct the race var from crosswave data
race<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'race_bh'])

#annoyingly, we also need to pick out the right personal identifier (we want the cross wave one)
pidp<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'pidp'])


#stick this all in a df
df<-as.data.frame(cbind(pidp,indresp_n[,employment.stat],indresp_n[,age],indresp_n[,sex],indresp_n[,GHQ1],indresp_n[,GHQ2],
                        indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                        indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],indresp_n[,occ],indresp_n[,mar.stat],indresp_n[,educ],
                        indresp_n[,monthly.labor.income]*(CPI.data[29,2]/CPI.data[val+1,2]),indresp_n[,full.time],race,
                        indresp_n[,aids.disables],indresp_n[,sickness.benefit],indresp_n[,region],indresp_n[,jbhrs],indresp_n[,soc],
                        NA,val,
                        hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.kids],
                        hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.income]*(CPI.data[29,2]/CPI.data[val+1,2]),
                        hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.conversion.factor],
                        hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.ownership],
                        NA))
#name columns
colnames(df)<-c('pid','employment_stat','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                'GHQ11','GHQ12','occupation', 'marital_stat','education','monthly_labor_income',
                'full_time','race','helps_disables','receives_sickness_benefit','region','weekly_hours','SOC','job_level',
                'wave_number','no_kids','hh_income','conversion_factor',
                'household_ownerships','JPY_measure')

#name df
df.name<-paste('wave',val,'data',sep='')

assign(df.name,df)

}

#now for the US waves:
for (val in count.to.ten){
  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  employment.stat<-paste(letter.list[val],'_jbstat',sep='')
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
  occ<-paste(letter.list[val],'_jbisco88_cc',sep='')
  mar.stat<-paste(letter.list[val],'_mastat_dv',sep='')
  educ<-paste(letter.list[val],'_qfhigh_dv',sep='')
  soc2<-paste(letter.list[val],'_jbsoc00_cc',sep='')
  monthly.labor.income<-paste(letter.list[val],'_fimnlabgrs_dv',sep='')
  sickness.benefit<-paste(letter.list[val],'_pbnft9',sep='')
  ethnicity.var<-paste(letter.list[val],'_ethn_dv',sep='')
  full.time<-paste(letter.list[val],'_jbft_dv',sep='')
  aids.disables<-paste(letter.list[val],'_aidhh',sep='')
  job.level<-paste(letter.list[val],'_jbnssec3_dv',sep='')
  region<-paste(letter.list[val],'_gor_dv',sep='')
  jbhrs<-paste(letter.list[val],'_jbhrs',sep='')
  top.code.income<-paste(letter.list[val],'_fimnlabgrs_tc',sep='')
  hh.kids<-paste(letter.list[val],'_nkids_dv',sep='')
  hh.income<-paste(letter.list[val],'_fihhmngrs_dv',sep='')
  hh.conversion.factor<-paste(letter.list[val],'_ieqmoecd_dv',sep='')
  hh.identifier<-paste(letter.list[val],'_hidp',sep='')
  hh.ownership<-paste(letter.list[val],'_hsownd',sep='')
  top.code.hhincome<-paste(letter.list[val],'_fihhmngrs_tc',sep='')
  jpy.measure<-paste(letter.list[val],'_sf12mcs_dv',sep='')
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val+18])
  #and the hhresp
  hhresp_n<-as.data.frame(hhresp.list[val+18])
  #construct the ethnicity variable, which I'm going to treat as a race variable
  race<-indresp_n[,ethnicity.var]

  #annoyingly, we also need to pick out the right personal identifier (we want the cross wave one)
  pidp<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'pidp'])
  
  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],indresp_n[,employment.stat],indresp_n[,age],indresp_n[,sex],indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],indresp_n[,occ],indresp_n[,mar.stat],indresp_n[,educ],
                          indresp_n[,monthly.labor.income]*(CPI.data[29,2]/CPI.data[val+1,2]),indresp_n[,full.time],race,
                          indresp_n[,aids.disables],indresp_n[,sickness.benefit],indresp_n[,region],indresp_n[,jbhrs],indresp_n[,soc2],
                          indresp_n[,job.level],val+18,
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.kids],
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.income]*(CPI.data[29,2]/CPI.data[val+1,2]),
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.conversion.factor],
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.ownership],
                          indresp_n[,jpy.measure],indresp_n[,top.code.income],
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),top.code.hhincome]))
  #name columns
  colnames(df)<-c('pid','employment_stat','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','occupation', 'marital_stat','education','monthly_labor_income',
                  'full_time','race','helps_disables','receives_sickness_benefit','region','weekly_hours',
                  'SOC','job_level','wave_number','no_kids','hh_income','conversion_factor',
                  'household_ownerships','JPY_measure','topcode_income','topcode_hhincome')
  
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
}

#concert to factors
conversion.list<-c('employment_stat','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                   'GHQ11','GHQ12','occupation', 'marital_stat','education',
                   'full_time','race','no_kids','JPY_measure','helps_disables','receives_sickness_benefit','household_ownerships')
for (var in conversion.list){
  wave1data[,var]<-as.factor(wave1data[,var])
  wave2data[,var]<-as.factor(wave2data[,var])
  wave3data[,var]<-as.factor(wave3data[,var])
  wave4data[,var]<-as.factor(wave4data[,var])
  wave5data[,var]<-as.factor(wave5data[,var])
  wave6data[,var]<-as.factor(wave6data[,var])
  wave7data[,var]<-as.factor(wave7data[,var])
  wave8data[,var]<-as.factor(wave8data[,var])
  wave9data[,var]<-as.factor(wave9data[,var])
  wave10data[,var]<-as.factor(wave10data[,var])
  wave11data[,var]<-as.factor(wave11data[,var])
  wave12data[,var]<-as.factor(wave12data[,var])
  wave13data[,var]<-as.factor(wave13data[,var])
  wave14data[,var]<-as.factor(wave14data[,var])
  wave15data[,var]<-as.factor(wave15data[,var])
  wave16data[,var]<-as.factor(wave16data[,var])
  wave17data[,var]<-as.factor(wave17data[,var])
  wave18data[,var]<-as.factor(wave18data[,var])
  wave19data[,var]<-as.factor(wave19data[,var])
  wave20data[,var]<-as.factor(wave20data[,var])
  wave21data[,var]<-as.factor(wave21data[,var])
  wave22data[,var]<-as.factor(wave22data[,var])
  wave23data[,var]<-as.factor(wave23data[,var])
  wave24data[,var]<-as.factor(wave24data[,var])
  wave25data[,var]<-as.factor(wave25data[,var])
  wave26data[,var]<-as.factor(wave26data[,var])
  wave27data[,var]<-as.factor(wave27data[,var])
  wave28data[,var]<-as.factor(wave28data[,var])
  
}

#get rid of top-coded income variables. Then get rid of the last two variables
wave19data<-subset(wave19data,topcode_income==0)
wave20data<-subset(wave20data,topcode_income==0)
wave21data<-subset(wave21data,topcode_income==0)
wave22data<-subset(wave22data,topcode_income==0)
wave23data<-subset(wave23data,topcode_income==0)
wave24data<-subset(wave24data,topcode_income==0)
wave25data<-subset(wave25data,topcode_income==0)
wave26data<-subset(wave26data,topcode_income==0)
wave27data<-subset(wave27data,topcode_income==0)
wave28data<-subset(wave28data,topcode_income==0)

wave19data<-subset(wave19data,topcode_hhincome==0)
wave20data<-subset(wave20data,topcode_hhincome==0)
wave21data<-subset(wave21data,topcode_hhincome==0)
wave22data<-subset(wave22data,topcode_hhincome==0)
wave23data<-subset(wave23data,topcode_hhincome==0)
wave24data<-subset(wave24data,topcode_hhincome==0)
wave25data<-subset(wave25data,topcode_hhincome==0)
wave26data<-subset(wave26data,topcode_hhincome==0)
wave27data<-subset(wave27data,topcode_hhincome==0)
wave28data<-subset(wave28data,topcode_hhincome==0)

wave19data<-wave19data[,1:34]
wave20data<-wave20data[,1:34]
wave21data<-wave21data[,1:34]
wave22data<-wave22data[,1:34]
wave23data<-wave23data[,1:34]
wave24data<-wave24data[,1:34]
wave25data<-wave25data[,1:34]
wave26data<-wave26data[,1:34]
wave27data<-wave27data[,1:34]
wave28data<-wave28data[,1:34]

#create a 'full data set'
full.data.set<-rbind(wave1data,wave2data,wave3data,wave4data,wave5data,wave6data,wave7data,wave8data,wave9data,wave10data,wave11data,wave12data,
                     wave13data,wave14data,wave15data,wave16data,wave17data,wave18data,wave19data,wave20data,wave21data,wave22data,wave23data,
                     wave24data,wave25data,wave26data,wave27data,wave28data)

#convert factors into numbers for the MH measures
full.data.set[,'GHQ1']<-as.numeric(as.character(full.data.set[,'GHQ1']))
full.data.set[,'GHQ2']<-as.numeric(as.character(full.data.set[,'GHQ2']))
full.data.set[,'GHQ3']<-as.numeric(as.character(full.data.set[,'GHQ3']))
full.data.set[,'GHQ4']<-as.numeric(as.character(full.data.set[,'GHQ4']))
full.data.set[,'GHQ5']<-as.numeric(as.character(full.data.set[,'GHQ5']))
full.data.set[,'GHQ6']<-as.numeric(as.character(full.data.set[,'GHQ6']))
full.data.set[,'GHQ7']<-as.numeric(as.character(full.data.set[,'GHQ7']))
full.data.set[,'GHQ8']<-as.numeric(as.character(full.data.set[,'GHQ8']))
full.data.set[,'GHQ9']<-as.numeric(as.character(full.data.set[,'GHQ9']))
full.data.set[,'GHQ10']<-as.numeric(as.character(full.data.set[,'GHQ10']))
full.data.set[,'GHQ11']<-as.numeric(as.character(full.data.set[,'GHQ11']))
full.data.set[,'GHQ12']<-as.numeric(as.character(full.data.set[,'GHQ12']))

#save this new data
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\processed_data')
write.csv(full.data.set,'mental_health_labor_mkt.CSV')

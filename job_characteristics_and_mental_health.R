rm(list=ls())
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\raw_data')
CPI.data<-read.csv('CPI_ONS.CSV')
CPI.data<-CPI.data[3:31,]
library(foreign)
library(haven)
library(hdm)
###
letter.list<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')

#and now the understanding society data
count.to.ten<-seq(from=2,to=10,by=2)
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

indresp.list<-list(indresp_20,indresp_22,indresp_24,indresp_26,indresp_28)
hhresp.list<-list(hhresp_20,hhresp_22,hhresp_24,hhresp_26,hhresp_28)

#we also need cross-wave data
setwd('C:\\Users\\Joe Spearing\\Documents\\measuring morbidity\\UKDA-6614-spss\\spss\\spss24\\ukhls_wx')
cross.wave.data<-read_sav('xwavedat.SAV')

#now for the US waves 8 and 10:
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
  three.class<-paste(letter.list[val],'_jbnssec3_dv',sep='')
  mar.stat<-paste(letter.list[val],'_mastat_dv',sep='')
  educ<-paste(letter.list[val],'_qfhigh_dv',sep='')
  soc2<-paste(letter.list[val],'_jbsoc00_cc',sep='')
  monthly.labor.income<-paste(letter.list[val],'_fimnlabgrs_dv',sep='')
  sickness.benefit<-paste(letter.list[val],'_pbnft9',sep='')
  ethnicity.var<-paste(letter.list[val],'_ethn_dv',sep='')
  full.time<-paste(letter.list[val],'_jbft_dv',sep='')
  mang.resp<-paste(letter.list[val],'_jbmngr',sep='')
  aids.disables<-paste(letter.list[val],'_aidhh',sep='')
  region<-paste(letter.list[val],'_gor_dv',sep='')
  top.code.income<-paste(letter.list[val],'_fimnlabgrs_tc',sep='')
  hh.kids<-paste(letter.list[val],'_nkids_dv',sep='')
  hh.income<-paste(letter.list[val],'_fihhmngrs_dv',sep='')
  hh.conversion.factor<-paste(letter.list[val],'_ieqmoecd_dv',sep='')
  hh.identifier<-paste(letter.list[val],'_hidp',sep='')
  hh.ownership<-paste(letter.list[val],'_hsownd',sep='')
  top.code.hhincome<-paste(letter.list[val],'_fihhmngrs_tc',sep='')
  jpy.measure<-paste(letter.list[val],'_sf12mcs_dv',sep='')
  
  #job characteristics
  job.location<-paste(letter.list[val],'_jbpl',sep='')
  part.time.available<-paste(letter.list[val],'_jbflex1',sep='')#2,4,6,8,10
  #on.call.available<-paste(letter.list[val],'_jbflex10',sep='') #8,10
  ttonlys.available<-paste(letter.list[val],'_jbflex2',sep='') #2,4,6,8,10
  compressed.week.available<-paste(letter.list[val],'_jbflex5',sep='') #2,4,6,8,10
  annualisedhrs.available<-paste(letter.list[val],'_jbflex6',sep='') #2,4,6,8,10
  other.flex.available<-paste(letter.list[val],'_jbflex8',sep='') #2,4,6,8,10
  inf.flex.available<-paste(letter.list[val],'_jbfxinf',sep='') #2,4,6,8,10
  wkfrhome.available<-paste(letter.list[val],'_jbflex7',sep='') #2,4,6,8,10
  part.time.use<-paste(letter.list[val],'_jbfxuse1',sep='') #2,4,6,8,10
  #on.call.use<-paste(letter.list[val],'_jbfxuse10',sep='') #8,10
  wkfrhome.use<-paste(letter.list[val],'_jbfxuse7',sep='') #2,4,6,8,10
  term.time.use<-paste(letter.list[val],'_jbfxuse2',sep='') #2,4,6,8,10
  compressed.week.use<-paste(letter.list[val],'_jbfxuse5',sep='') #2,4,6,8,10
  annualisedhrs.use<-paste(letter.list[val],'_jbfxuse6',sep='') #2,4,6,8,10
  wkaut1<-paste(letter.list[val],'_wkaut1',sep='') #2,4,6,8,10
  wkaut2<-paste(letter.list[val],'_wkaut2',sep='') #2,4,6,8,10
  wkaut3<-paste(letter.list[val],'_wkaut3',sep='') #2,4,6,8,10
  wkaut4<-paste(letter.list[val],'_wkaut4',sep='') #2,4,6,8,10
  wkaut5<-paste(letter.list[val],'_wkaut5',sep='') #2,4,6,8,10
  hrs.per.week<-paste(letter.list[val],'_jbhrs',sep='')
  ot.hrs.per.week<-paste(letter.list[val],'_jbotpd',sep='')
  depenth1<-paste(letter.list[val],'_depenth1',sep='') #2,4,6,8,10
  depenth2<-paste(letter.list[val],'_depenth2',sep='') #2,4,6,8,10
  depenth3<-paste(letter.list[val],'_depenth3',sep='') #2,4,6,8,10
  depenth4<-paste(letter.list[val],'_depenth4',sep='') #2,4,6,8,10
  depenth5<-paste(letter.list[val],'_depenth5',sep='') #2,4,6,8,10
  depenth6<-paste(letter.list[val],'_depenth6',sep='') #2,4,6,8,10
  job.satisfaction<-paste(letter.list[val],'_jbsat',sep='')
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val/2])
  #and the hhresp
  hhresp_n<-as.data.frame(hhresp.list[val/2])
  #construct the ethnicity variable, which I'm going to treat as a race variable
  race<-indresp_n[,ethnicity.var]

  #annoyingly, we also need to pick out the right personal identifier (we want the cross wave one)
  pidp<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,person.ID]),as.numeric(as.character(unlist(cross.wave.data[,2])))),'pidp'])
  
  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],indresp_n[,employment.stat],indresp_n[,age],indresp_n[,sex],indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12],indresp_n[,occ],indresp_n[,three.class],indresp_n[,mar.stat],indresp_n[,educ],
                          indresp_n[,monthly.labor.income]*(CPI.data[29,2]/CPI.data[val+1,2]),indresp_n[,full.time],race,
                          indresp_n[,aids.disables],indresp_n[,sickness.benefit],indresp_n[,region],indresp_n[,soc2],val+18,
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.kids],
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.income]*(CPI.data[29,2]/CPI.data[val+1,2]),
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.conversion.factor],
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),hh.ownership],
                          indresp_n[,jpy.measure],indresp_n[,top.code.income],
                          hhresp_n[match(indresp_n[,hh.identifier],hhresp_n[,hh.identifier]),top.code.hhincome],
                          indresp_n[,job.location],indresp_n[,part.time.available],
                          #indresp_n[,on.call.available],
                          indresp_n[,ttonlys.available],indresp_n[,compressed.week.available],indresp_n[,annualisedhrs.available],indresp_n[,other.flex.available],
                          indresp_n[,inf.flex.available],indresp_n[,wkfrhome.available],indresp_n[,part.time.use],
                          #indresp_n[,on.call.use],
                          indresp_n[,wkfrhome.use],indresp_n[,term.time.use],
                          indresp_n[,compressed.week.use],indresp_n[,annualisedhrs.use],indresp_n[,wkaut1],
                          indresp_n[,wkaut2],indresp_n[,wkaut3],indresp_n[,wkaut4],indresp_n[,wkaut5],
                          indresp_n[,hrs.per.week],indresp_n[,ot.hrs.per.week],indresp_n[,depenth1],
                          indresp_n[,depenth2],indresp_n[,depenth3],indresp_n[,depenth4],indresp_n[,depenth5],
                          indresp_n[,depenth6],indresp_n[,job.satisfaction],indresp_n[,mang.resp]))
  #name columns
  colnames(df)<-c('pid','employment_stat','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                  'GHQ11','GHQ12','occupation','three_class', 'marital_stat','education','monthly_labor_income',
                  'full_time','race','helps_disables','receives_sickness_benefit','region','SOC','wave_number','no_kids','hh_income','conversion_factor',
                  'household_ownerships','JPY_measure','topcode_income','topcode_hhincome',
                 
                   'job_location', 'part_time_available',
                  #'on_call_available',
                  'ttonlys_available','compressed_week_available','annualisedhrs_available',
                  'other_flex_available','inf_flex_available','wkfrhome_available',
                  'part_time_use',
                  #'on_call_use',
                   'wkfrhome_use','term_time_use',
                  'compressed_week_use','annualisedhrs_use','wkaut1',
                  'wkaut2','wkaut3','wkaut4','wkaut5','hrs_per_week',
                  'ot_hrs_per_week','depenth1','depenth2','depenth3',
                  'depenth4','depenth5','depenth6','job_satisfaction','managerial_responsibility')
  
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
}

#concert to factors
conversion.list<-c('employment_stat','age','sex', 'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10',
                   'GHQ11','GHQ12','occupation', 'three_class','marital_stat','education',
                   'full_time','race','no_kids','JPY_measure','helps_disables','receives_sickness_benefit','household_ownerships',
                   'job_location', 'part_time_available',
                   'ttonlys_available','compressed_week_available','annualisedhrs_available',
                   'other_flex_available','inf_flex_available','wkfrhome_available',
                   'part_time_use',
                   'wkfrhome_use','term_time_use',
                   'compressed_week_use','annualisedhrs_use','wkaut1',
                   'wkaut2','wkaut3','wkaut4','wkaut5','hrs_per_week',
                   'ot_hrs_per_week','depenth1','depenth2','depenth3',
                   'depenth4','depenth5','depenth6','job_satisfaction')

for (var in conversion.list){
  wave20data[,var]<-as.factor(wave20data[,var])
  
  wave22data[,var]<-as.factor(wave22data[,var])
  
  wave24data[,var]<-as.factor(wave24data[,var])
  
  wave26data[,var]<-as.factor(wave26data[,var])
  
  wave28data[,var]<-as.factor(wave28data[,var])
  
}

#get rid of top-coded income variables. Then get rid of the last two variables

wave20data<-subset(wave20data,topcode_income==0)

wave22data<-subset(wave22data,topcode_income==0)

wave24data<-subset(wave24data,topcode_income==0)

wave26data<-subset(wave26data,topcode_income==0)

wave28data<-subset(wave28data,topcode_income==0)


wave20data<-subset(wave20data,topcode_hhincome==0)

wave22data<-subset(wave22data,topcode_hhincome==0)

wave24data<-subset(wave24data,topcode_hhincome==0)

wave26data<-subset(wave26data,topcode_hhincome==0)

wave28data<-subset(wave28data,topcode_hhincome==0)


#create a 'full data set'
full.data.set<-rbind(wave20data,wave22data,wave24data,wave26data,wave28data)

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

setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\processed_data')
write.csv(full.data.set,'job_characteristics_and_mental_health.CSV')


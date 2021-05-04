rm(list=ls())
library(haven)
###
#upload this data: BHPS
letter.list<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')
#for each number
#and now the understanding society data
counter<-seq(from=2,to=8,by=2)
#for each number
for (val in counter){
  #set directory
  directory<-paste('C:\\Users\\Joe Spearing\\Documents\\measuring morbidity\\UKDA-6614-spss\\spss\\spss24\\ukhls_w',val,sep='')
  setwd(directory)
  #upload data: indresp
  file.name<-paste(letter.list[val],'_indresp.SAV',sep='')
  df<-read_sav(file.name)
  df_name<-paste('indresp_',val+18,sep='')
  assign(df_name,df)
}

#we also need cross-wave data
setwd('C:\\Users\\Joe Spearing\\Documents\\measuring morbidity\\UKDA-6614-spss\\spss\\spss24\\ukhls_wx')
cross.wave.data<-read_sav('xwavedat.SAV')

#create some df lists

indresp.list<-list(indresp_20,indresp_22,indresp_24,indresp_26)


US.list<-seq(from=1,to=4,by=1)
for (val.1 in US.list){
  val<-val.1*2
  #get a list of variables (and names) I want to extract
  person.ID<-'pidp'
  occ<-paste(letter.list[val],'_jbisco88_cc',sep='')
  wkaut1<-paste(letter.list[val],'_wkaut1',sep='')
  wkaut2<-paste(letter.list[val],'_wkaut2',sep='')
  wkaut3<-paste(letter.list[val],'_wkaut3',sep='')
  wkaut4<-paste(letter.list[val],'_wkaut4',sep='')
  wkaut5<-paste(letter.list[val],'_wkaut5',sep='')
  
  depenth1<-paste(letter.list[val],'_depenth1',sep='')
  depenth2<-paste(letter.list[val],'_depenth2',sep='')
  depenth3<-paste(letter.list[val],'_depenth3',sep='')
  depenth4<-paste(letter.list[val],'_depenth4',sep='')
  depenth5<-paste(letter.list[val],'_depenth5',sep='')
  depenth6<-paste(letter.list[val],'_depenth6',sep='')
  
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
  
  #find the indresp df we want
  indresp_n<-as.data.frame(indresp.list[val.1])
  
  #stick this all in a df
  df<-as.data.frame(cbind(indresp_n[,person.ID],indresp_n[,occ],indresp_n[,wkaut1],indresp_n[,wkaut2],indresp_n[,wkaut3],indresp_n[,wkaut4],
                          indresp_n[,wkaut5],indresp_n[,depenth1],indresp_n[,depenth2],indresp_n[,depenth3],indresp_n[,depenth4],
                          indresp_n[,depenth5],indresp_n[,depenth6],indresp_n[,GHQ1],indresp_n[,GHQ2],
                          indresp_n[,GHQ3],indresp_n[,GHQ4],indresp_n[,GHQ5],indresp_n[,GHQ6],indresp_n[,GHQ7],indresp_n[,GHQ8],indresp_n[,GHQ9],
                          indresp_n[,GHQ10],indresp_n[,GHQ11],indresp_n[,GHQ12]))
  #name columns
  colnames(df)<-c('pid','occupation','wkaut1','wkaut2','wkaut3','wkaut4','wkaut5','depenth1','depenth2','depenth3','depenth4','depenth5','depenth6',
                  'GHQ1','GHQ2','GHQ3','GHQ4','GHQ5','GHQ6','GHQ7','GHQ8','GHQ9','GHQ10','GHQ11','GHQ12')
  
  #name df
  df.name<-paste('wave',val+18,'data',sep='')
  
  assign(df.name,df)
  
}

occ.class.data<-rbind(wave20data,wave22data,wave24data,wave26data)

occ.list<-unique(occ.class.data[,'occupation'])

#get rid of nulls
occ.class.data<-subset(occ.class.data,occupation>0)
occ.class.data<-subset(occ.class.data,wkaut1>0)
occ.class.data<-subset(occ.class.data,wkaut2>0)
occ.class.data<-subset(occ.class.data,wkaut3>0)
occ.class.data<-subset(occ.class.data,wkaut4>0)
occ.class.data<-subset(occ.class.data,wkaut5>0)
occ.class.data<-subset(occ.class.data,depenth1>0)
occ.class.data<-subset(occ.class.data,depenth2>0)
occ.class.data<-subset(occ.class.data,depenth3>0)
occ.class.data<-subset(occ.class.data,depenth4>0)
occ.class.data<-subset(occ.class.data,depenth5>0)
occ.class.data<-subset(occ.class.data,depenth6>0)
occ.class.data<-subset(occ.class.data,GHQ1>0)
occ.class.data<-subset(occ.class.data,GHQ2>0)
occ.class.data<-subset(occ.class.data,GHQ3>0)
occ.class.data<-subset(occ.class.data,GHQ4>0)
occ.class.data<-subset(occ.class.data,GHQ5>0)
occ.class.data<-subset(occ.class.data,GHQ6>0)
occ.class.data<-subset(occ.class.data,GHQ7>0)
occ.class.data<-subset(occ.class.data,GHQ8>0)
occ.class.data<-subset(occ.class.data,GHQ9>0)
occ.class.data<-subset(occ.class.data,GHQ10>0)
occ.class.data<-subset(occ.class.data,GHQ11>0)
occ.class.data<-subset(occ.class.data,GHQ12>0)

#load occupation key
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\processed_data')
occ.key<-read.csv('occupation_classifications.CSV')

#link these
occupation.name<-occ.key[match(occ.class.data[,'occupation'],occ.key[,1]),2]

occ.class.data<-cbind(occ.class.data,occupation.name)

write.csv(occ.class.data,'occ_class_data.CSV')

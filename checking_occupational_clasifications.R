rm(list=ls())
library(foreign)
library(haven)

directory<-paste('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\raw_data\\bhps_w18')
setwd(directory)
#upload data: indresp18. I want to compare occupational measures
file.name<-'br_indresp.SAV'
df_raw<-read_sav(file.name)
df<-df_raw
#the occupation variables I want are: 'br_jbisco88_cc', 'br_jbisco_cc', 'br_jbsoc00_cc'

df<-cbind(df[,'br_jbisco88_cc'],df[,'br_jbsoc00_cc'])

#subset to kill NAs
df<-subset(df,is.na(br_jbisco88_cc)==FALSE)
df<-subset(df,is.na(br_jbsoc00_cc)==FALSE)
#and to kill nulls
df<-subset(df,br_jbisco88_cc>=0)
df<-subset(df,br_jbsoc00_cc>=0)

#upload the keys
#2000 soc code
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\raw_data\\soc_definitions')
soc.2000<-read.csv('soc-definitions-2000.CSV')
soc.2000<-soc.2000[7:827,1:2]
colnames(soc.2000)<-c('2000_SOC_code','2000_SOC_title')

#1990 soc code
soc.1990<-read.csv('soc_1990_definitions.CSV',header = FALSE)
colnames(soc.1990)<-c('1990_soc_code','1990_soc_title')

#2018 soc code
soc.2018<-read.csv('soc_2018_definitions.CSV')
soc.2018<-subset(soc.2018,SOC.Group=='Detailed')
soc.2018<-soc.2018[,2:3]

#illo codes
setwd('C:\\Users\\Joe Spearing\\Documents\\mental_health_and_labor_mkt_outcomes\\processed_data')
occ.data<-read.csv('occ_class_data.CSV')
occ.illo<-as.data.frame(cbind(as.character(unique(occ.data[,'occupation.name'])),
                occ.data[match(unique(occ.data[,'occupation.name']),occ.data[,'occupation.name']),'occupation']))

df[1:10,]

#computing professionals, Information and communication technology professionals


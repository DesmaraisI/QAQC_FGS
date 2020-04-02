library(devtools)
library(tidyverse)
library(ggplot2)
library(lubridate)

library(gridExtra)
library(hakaiApi)
library(here)

client<-hakaiApi::Client$new()

#DO13C data
DO13C_FGSendpoint <- sprintf("%s/%s", client$api_root,'eims/views/output/do13c?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_F"%2C"FGS_KN"%2C"FGS_R"%2C"FGS_WSC"%2C"FGS_Y"}&limit=-1')

DO13C_FGS<-client$get(DO13C_FGSendpoint)

DO13C_FGS_data<-DO13C_FGS %>% 
  select(event_pk,date,survey,site_id,ppm_c_doc,ppm_c_doc_flag,row_flag,quality_level)

write_csv(DO13C_FGS_data,here("data","DOC13C.csv"))

#DOC data
DOC_FGSendpoint <- sprintf("%s/%s", client$api_root,'eims/views/output/doc?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_R"%2C"FGS_Y"}&limit=-1')

DOC_FGS<-client$get(DOC_FGSendpoint)

DOC_FGS_data<-DOC_FGS %>% 
  select(event_pk,date,survey,site_id,doc,doc_flag,row_flag,quality_level)

write_csv(DOC_FGS_data,here("data","DOC.csv"))

#O18 data
O18_FGSendpoint <- sprintf("%s/%s", client$api_root,'eims/views/output/o18?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_R"%2C"FGS_Y"%2C"FGS_KN"}&limit=-1')

O18_FGS<-client$get(O18_FGSendpoint)

O18_FGS_data<-O18_FGS %>% 
  select(event_pk,date,survey,site_id,delta_o18,delta_o18_flag,delta_h2, delta_h2_flag,row_flag,quality_level)

write_csv(O18_FGS_data,here("data","O18.csv"))

#Nutrients data
Nut_FGSendpoint<- sprintf("%s/%s", client$api_root,'eims/views/output/nutrients?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_R"%2C"FGS_Y"%2C"FGS_KN"}&limit=-1')

Nut_FGS<-client$get(Nut_FGSendpoint)

Nut_FGS_data<-Nut_FGS %>% 
  select(event_pk,date,survey,site_id,nh4_,tp,tdp,tn,tdn,no2_no3_um,po4,sio2,nh4__flag,tp_flag,tdp_flag,tn_flag,tdn_flag,no2_no3_flag,po4_flag,sio2_flag,row_flag,quality_level)

write_csv(Nut_FGS_data,here("data","Nut.csv"))

#TSS data
TSS_FGSendpoint<- sprintf("%s/%s", client$api_root,'eims/views/output/ts?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_KN"%2C"FGS_R"%2C"FGS_Y"}&limit=-1')

TSS_FGS<-client$get(TSS_FGSendpoint)

TSS_FGS_data<-TSS_FGS %>% 
  select(event_pk,date,survey,site_id,tss_mg_l,tss_flag,row_flag,quality_level)

write_csv(TSS_FGS_data,here("data","TSS.csv"))


#POM data
POM_FGSendpoint<- sprintf("%s/%s", client$api_root,'eims/views/output/poms?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_F"%2C"FGS_KN"%2C"FGS_R"%2C"FGS_WSC"%2C"FGS_Y"}&limit=-1')

POM_FGS<-client$get(POM_FGSendpoint)

POM_FGS_data<-POM_FGS %>% 
  select(event_pk,date,survey,site_id,acidified,hakai_id,ug_n,ug_c,c_flag,n_flag,row_flag,quality_level)

write_csv(POM_FGS_data,here("data","POM.csv"))



#Cations/Alkalinity data
Akan_Cat_FGSendpoint<- sprintf("%s/%s", client$api_root,'eims/views/output/akan_cat_ph?survey%26%26{"FGS"%2C"FGS_B"%2C"FGS_F"%2C"FGS_KN"%2C"FGS_R"%2C"FGS_WSC"%2C"FGS_Y"}&limit=-1')

Akan_Cat_data<-client$get(Akan_Cat_FGSendpoint)

Akan_Cat_FGS_data<-Akan_Cat_data %>% 
  select(event_pk,date,survey,site_id,acidity,alkh,ph,s,so4,mg,zn,na,al,ca,cl,fe,b,f,k,si,cu,sr,acidity_flag,alkh_flag,ph_flag,s_flag,so4_flag,mg_flag,zn_flag,na_flag,al_flag,ca_flag,cl_flag,fe_flag,b_flag,f_flag,k_flag,si_flag,cu_flag,sr_flag,row_flag,quality_level)

write_csv(Akan_Cat_FGS_data,here("data","Akan_Cat.csv"))



#Possible useful older scripts
ggplot(data)+geom_point(aes(Date,TN))
ggplot(data)+geom_point(aes(x=Date,y=TN,color=Survey))
ggplot(data)+geom_point(aes(x=Date,y=TN,shape=Survey))
ggplot(data)+geom_point(aes(x=Date,y=TN,alpha=Survey))
ggplot(data)+geom_point(aes(Date,TN),color="red")
ggplot(data)+geom_point(aes(Date,TN))+facet_wrap(~Survey)
ggplot(data)+geom_point(aes(Date,TDN))+facet_grid(Survey~Site_ID)
ggplot(data)+geom_point(aes(Date,TDN))+facet_grid(Survey~.)
ggplot(data)+geom_bar(aes(Survey,fill=Survey))
ggplot(data)+geom_bar(aes(Survey,fill=Site_ID))
ggplot(data)+geom_bar(aes(Survey,fill=Site_ID),position="dodge")
ggplot(data)+geom_point(aes(x=Date,y=TN,color=Survey),position="jitter")
ggplot(data)+geom_bar(aes(Survey,fill=Site_ID))+coord_flip()

FGS <- filter(data,Survey=='FGS')
UA_MV_Flags <- filter(data, TP_FLag=='MV'| TN_Flag=='MV'| NH4_Flag=='MV')

UofA <- filter(data,Analyzing_Lab=='UA')
ggplot(UofA)+geom_point(aes(Date,TDN))+facet_wrap(~Survey)



s_2018_qc<-ggplot(data, aes(x=date, y=s)) + theme_linedraw() + geom_point(aes(shape=quality_level,color=s_flag)) + facet_grid(survey~.)
ggsave('fig_output/s_2018_qc.png',s_2018_qc,width = 10, dpi = 300)

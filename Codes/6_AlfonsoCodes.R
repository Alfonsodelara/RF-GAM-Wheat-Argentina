setwd("D:/GitHub/RF-GAM-Wheat-Argentina")
library(here)
library(tidyverse)
library(sf)
library(skimr)
library(writexl)

# Spatial yield variability

difm_geo= readRDS("Data/all_data.rds")
df= select(difm_geo, id_field, data) %>% unnest(cols= c(data)) %>% select(id_field, nrate, yield) 
difmbox= filter(df, nrate<26 | nrate>161)%>% mutate(Fertilizer= ifelse(nrate<50, "N0","Nmax"))

yields= table%>% group_by(ID_FIELD) %>% summarize(Mean= mean(Yield), Min=min(Yield), Max=max(Yield), SD= sd(Yield), CV= sd(Yield)/mean(Yield)*100); yields

a= difmbox%>% group_by(id_field, Fertilizer) %>% summarize(Mean= mean(yield), Min=min(yield), Max=max(yield), SD= sd(yield), CV= sd(yield)/mean(yield)*100);a
b= difmbox%>% group_by(Fertilizer) %>% summarize(Mean= mean(yield), Min=min(yield), Max=max(yield), SD= sd(yield), CV= sd(yield)/mean(yield)*100);b
write_xlsx(a, "D:\\YieldResponse_byField.xlsx")
write_xlsx(b, "D:\\YieldResponse_Overall.xlsx")

library(ggpubr)
ggplot(difmbox, aes(x= Fertilizer, y=yield,fill=Fertilizer))+geom_boxplot(width=0.5)+facet_wrap(~id_field, nrow= 3)+ 
  stat_compare_means( aes(label = ..p.signif..),label.x = 1.5, label.y=8)+ geom_jitter(alpha = 0.1, width = 0.1)



#EONR
rf_eonr_results= readRDS("Results/rf_eonr_results.rds")
gam_eonr_results= readRDS("Results/gam_eonr_results.rds")
# head(rf_eonr_results)
# head(rf_eonr_results$eonr_data[[1]])
# rf_eonr_results[[3]]%>% head()


df1= select(rf_eonr_results, id_field, eonr_data) %>% unnest(cols= c(eonr_data)) %>% select(id_field,nrate) %>% mutate(Model="RF")
df2= select(gam_eonr_results, id_field, eonr_data) %>% unnest(cols= c(eonr_data))%>% select(id_field,nrate) %>% mutate(Model="GAM")
df= rbind(df1, df2)


df %>% group_by(Model,id_field)%>% skim(nrate) 
df %>% group_by(Model)%>% skim(nrate) 


write_xlsx(df %>% group_by(Model, id_field)%>% skim(nrate), "D:\\Lailadf.xlsx")

#YEONR
df1= select(rf_eonr_results, id_field, eonr_data) %>% unnest(cols= c(eonr_data)) %>% select(id_field,yield) %>% mutate(Model="RF")
df2= select(gam_eonr_results, id_field, eonr_data) %>% unnest(cols= c(eonr_data))%>% select(id_field,yield) %>% mutate(Model="GAM")
df= rbind(df1, df2)

df %>% group_by(Model, id_field)%>% skim(yield) 
df %>% ungroup()%>% skim(yield)


#RMSE
rf_rmse= readRDS("Results/rf_rmse.rds")%>% mutate(Model="RF")
gam_rmse= readRDS("Results/gam_rmse.rds")%>% mutate(Model="GAM")
df= rbind(rf_rmse,gam_rmse)

df %>% group_by(Model)%>% skim()
df %>% ungroup()%>% skim()












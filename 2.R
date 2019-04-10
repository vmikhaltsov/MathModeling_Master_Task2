library(tidyverse)
library(stringr)
library(lubridate)
eddy=read_csv("eddypro.csv", skip=1, na=c(" ","NA","-9999","-9999.0"), comment=c("[")); 
eddy=eddy[-1,]
eddy=select(eddy,-(roll))
eddy=eddy %>% mutate_if(is.character, factor)

names(eddy) = names(eddy) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_slash_") %>%
  str_replace_all("[%]","_pecent_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(eddy)

eddy = mutate(eddy, month = month(date))
eddy_num=filter(eddy, daytime==TRUE)
eddy_num=filter(eddy, month>=3 & month<=5)[,sapply(eddy, is.numeric)]
eddy_non_num=eddy[,!sapply(eddy, is.numeric)]

cor_td=cor(drop_na(eddy_num)) %>% as.data.frame %>% select(h2o_flux)
cor_td=cbind(cor_td,adjusted=sqrt(cor_td$h2o_flux^2))
vars=row.names(cor_td)[cor_td$h2o_flux^2>.1] %>% na.exclude()
correlation_formula=as.formula(paste("h2o_flux~",paste(vars,collapse="+"),sep=" "))

model1=lm(correlation_formula,data=eddy_num)

names(model1)

summary(model1)

anova(model1)

#удаляем малозначимые показатели
vars_cut=c("H", "LE","qc_LE","rand_err_LE","co2_flux", "rand_err_h2o_flux", "co2_mole_fraction", "u_rot",
           "un_H", "un_LE", "LE_scf", "un_h2o_flux", "w_slash_ts_cov", "w_slash_h2o_cov", "co2_1") 
#("DOY","Tau", "rand_err_Tau","H","rand_err_H","LE","qc_LE","rand_err_LE","co2_flux",
# "qc_co2_flux","rand_err_co2_flux","rand_err_h2o_flux","co2_molar_density","h2o_time_lag",
 # "sonic_temperature", "air_temperature","air_density","air_molar_volume","es","RH","VPD",
 # "u_star_","TKE","T_star_","un_Tau","un_H", "un_LE", "un_co2_flux", "un_h2o_flux", 
 # "mean_value", "w_slash_co2_cov", "w_slash_h2o_cov", "h2o_signal_strength_7200")

correlation_formula_cut=as.formula(paste("h2o_flux~",paste(vars_cut,collapse="+"),sep=" "))

modelc=lm(correlation_formula_cut,data=eddy_num)
anova(modelc)
summary(modelc)


cor_lrm=cor(drop_na(eddy_num) %>% as.data.frame %>% select(vars_cut))
cor_lrm=sqrt(cor_lrm^2)


#избавляемся от одного показателя в коррелирующих парах
vars2=c("qc_LE", "co2_flux", "u_rot")
correlation_formula2=as.formula(paste("h2o_flux~",paste(vars2,collapse="+"),sep=" "))

model2=lm(correlation_formula2,data=eddy_num)

names(model2)

summary(model2)

anova(model2)

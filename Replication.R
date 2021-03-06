# remove environment #
rm(list=ls())

# set working directory #
setwd("C:\\Users\\byngdeuk\\Desktop\\BDW")

# import replication stata data into dataframe #
library(foreign)
replication <- read.dta("replication start.dta")

# generate number of adoptions for any crimes by country and year (for law and nap) #
replication$L_num = replication$L_Femicide + replication$L_forced_sterilization + replication$L_stalking + replication$L_property + replication$L_violence_against_women + replication$L_domestic + replication$L_sexual_violence + replication$L_new + replication$L_sexual_harassment + replication$L_FGM + replication$L_trafficking + replication$L_child_ealry_forced
replication$NAP_num = replication$NAP_Femicide + replication$NAP_forced_sterilization + replication$NAP_stalking + replication$NAP_property + replication$NAP_violence_against_women + replication$NAP_domestic + replication$NAP_sexual_violence + replication$NAP_new + replication$NAP_sexual_harassment + replication$NAP_FGM + replication$NAPL_trafficking + replication$NAPL_child_ealry_forced

# generate dummy variable if a country in a given year adopt any components by country and year (for law and nap) #
install.packages("dplyr")
install.packages("tidyverse")
library("dplyr")
library("tidyverse")
mutate
replication2 <- replication %>% mutate(L_any = ifelse(L_num > 0, 1, 0))
replication3 <- replication2 %>% mutate(NAP_any = ifelse(NAP_num > 0, 1, 0))

# generate cumulative number of adoptions for any components by country and year (for law and nap) #
replication4 <- replication3 %>%
  group_by(ccode) %>%
  mutate(L_has_num = cumsum(L_num),
         NAP_has_num = cumsum(NAP_num),
         L_has_any = ifelse(L_has_num > 0, 1, 0),
         NAP_has_any = ifelse(NAP_has_num > 0, 1, 0),
         lag.L_has_any = dplyr::lag(L_has_any, n=1, default = NA), # generate lagged variables for various purposes such as creating spatial lag and lagged independent variables #
         lag.NAP_has_any = dplyr::lag(NAP_has_any, n=1, default = NA)) 


replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.L_has_num = dplyr::lag(L_has_num, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.NAP_has_num = dplyr::lag(NAP_has_num, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.polity2 = dplyr::lag(polity2, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.cgdppc = dplyr::lag(cgdppc, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.vdem_gender = dplyr::lag(vdem_gender, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.actotal = dplyr::lag(actotal, n=1, default = NA)) %>%
  mutate(lag.Femployment15ILO = dplyr::lag(Femployment15ILO, n=1, default = NA)) %>%
  mutate(lag.Femployment15NAT = dplyr::lag(Femployment15NAT, n=1, default = NA)) %>%
  mutate(lag.Flaborforceparticipation15 = dplyr::lag(Flaborforceparticipation15, n=1, default = NA)) %>%
  mutate(lag.Flaborforceparticipation15NAT = dplyr::lag(Flaborforceparticipation15NAT, n=1, default = NA)) %>%
  mutate(lag.Flaborforceparticipation1564 = dplyr::lag(Flaborforceparticipation1564, n=1, default = NA)) %>%
  mutate(lag.Fprimarygross = dplyr::lag(Fprimarygross, n=1, default = NA)) %>%
  mutate(lag.Fprimarynet = dplyr::lag(Fprimarynet, n=1, default = NA)) %>%
  mutate(lag.Fsecondarygross = dplyr::lag(Fsecondarygross, n=1, default = NA)) %>%
  mutate(lag.Fsecondarynet = dplyr::lag(Fsecondarynet, n=1, default = NA)) %>%
  mutate(lag.FunemploymentILO = dplyr::lag(FunemploymentILO, n=1, default = NA)) %>%
  mutate(lag.FunemploymentNAT = dplyr::lag(FunemploymentNAT, n=1, default = NA)) %>%
  mutate(lag.FwageILO = dplyr::lag(FwageILO, n=1, default = NA)) %>%
  mutate(lag.Fparliaments = dplyr::lag(Fparliaments, n=1, default = NA))


#replace na of laggded to 0#
replication4$lag.L_has_any[which(is.na(replication4$lag.L_has_any))] <- 0
replication4$lag.NAP_has_any[which(is.na(replication4$lag.NAP_has_any))] <- 0
replication4$lag.L_has_num[which(is.na(replication4$lag.L_has_num))] <- 0
replication4$lag.NAP_has_num[which(is.na(replication4$lag.NAP_has_num))] <- 0


# replace NA of ht_region and ht_colonial with first observed value by ccode  because region and colonial heritage didn't change over time #
replication4 <- replication4 %>%
  group_by(ccode) %>%
  fill(ht_colonial, ht_region, lp_legor) %>% #default direction down
  fill(ht_colonial, ht_region, lp_legor, .direction = "up")

# change ht_colonial = 0 to = 11 for later calculation #
replication4 <- replication4 %>%
  mutate(ht_colonial=replace(ht_colonial, ht_colonial == 0, 11))

# create the sum of number of countires according to ht_region, ht_colonial, and lp_legor. Replace NA count to NA #
replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(count_ht_region = n())

replication4 <- replication4 %>%
  group_by(ccode, year, ht_region) %>%
  mutate(count_ht_region = factor(ifelse(ht_region == "NA", "NA", count_ht_region)))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(count_ht_colonial = n())

replication4 <- replication4 %>%
  group_by(ccode, year, ht_colonial) %>%
  mutate(count_ht_colonial = factor(ifelse(ht_colonial == "NA", "NA", count_ht_colonial)))


replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(count_lp_legor = n())

replication4 <- replication4 %>%
  group_by(ccode, year, lp_legor) %>%
  mutate(count_lp_legor = factor(ifelse(lp_legor == "NA", "NA", count_lp_legor)))

# numbers of countries with laws and NAP in a lagged year #
replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(Lnumcountries_ht_region = sum(lag.L_has_any))

replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(NAPnumcountries_ht_region = sum(lag.NAP_has_any))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(Lnumcountries_ht_colonial = sum(lag.L_has_any))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(NAPnumcountries_ht_colonial = sum(lag.NAP_has_any))

replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(Lnumcountries_lp_legor = sum(lag.L_has_any))

replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(NAPnumcountries_lp_legor = sum(lag.NAP_has_any))

# create percentage of countries with law and NAP for diffusion variables #
replication4 <- replication4 %>%
  mutate(DL_ht_region = as.numeric(Lnumcountries_ht_region)/as.numeric(count_ht_region))

replication4 <- replication4 %>%
  mutate(DNAP_ht_region = as.numeric(NAPnumcountries_ht_region)/as.numeric(count_ht_region))

replication4 <- replication4 %>%
  mutate(DL_ht_colonial = as.numeric(Lnumcountries_ht_colonial)/as.numeric(count_ht_colonial))

replication4 <- replication4 %>%
  mutate(DNAP_ht_colonial = as.numeric(NAPnumcountries_ht_colonial)/as.numeric(count_ht_colonial))

replication4 <- replication4 %>%
  mutate(DL_lp_legor = as.numeric(Lnumcountries_lp_legor)/as.numeric(count_lp_legor))

replication4 <- replication4 %>%
  mutate(DNAP_lp_legor = as.numeric(NAPnumcountries_lp_legor)/as.numeric(count_lp_legor))

# by components diffusion variables.

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_stalking_num = cumsum(L_stalking),
         L_has_stalking = ifelse(L_stalking_num > 0, 1, 0),
         lag.L_stalking_any = dplyr::lag(L_has_stalking, n=1, default = NA))

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_vaw_num = cumsum(L_violence_against_women),
         L_has_vaw = ifelse(L_vaw_num > 0, 1, 0),
         lag.L_vaw_any = dplyr::lag(L_has_vaw, n=1, default = NA))


replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_domestic_num = cumsum(L_domestic),
         L_has_domestic = ifelse(L_domestic_num > 0, 1, 0),
         lag.L_domestic_any = dplyr::lag(L_has_domestic, n=1, default = NA))

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_sv_num = cumsum(L_sexual_violence),
         L_has_sv = ifelse(L_sv_num > 0, 1, 0),
         lag.L_sv_any = dplyr::lag(L_has_sv, n=1, default = NA))

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_sh_num = cumsum(L_sexual_harassment),
         L_has_sh = ifelse(L_sh_num > 0, 1, 0),
         lag.L_sh_any = dplyr::lag(L_has_sh, n=1, default = NA))

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_FGM_num = cumsum(L_FGM),
         L_has_FGM = ifelse(L_FGM_num > 0, 1, 0),
         lag.L_FGM_any = dplyr::lag(L_has_FGM, n=1, default = NA))

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_trafficking_num = cumsum(L_trafficking),
         L_has_trafficking = ifelse(L_trafficking_num > 0, 1, 0),
         lag.L_trafficking_any = dplyr::lag(L_has_trafficking, n=1, default = NA))

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(L_CEF_num = cumsum(L_child_ealry_forced),
         L_has_CEF = ifelse(L_CEF_num > 0, 1, 0),
         lag.L_CEF_any = dplyr::lag(L_has_CEF, n=1, default = NA))

#replace na of laggded to 0#
replication4$lag.L_stalking_any[which(is.na(replication4$lag.L_stalking_any))] <- 0
replication4$lag.L_vaw_any[which(is.na(replication4$lag.L_vaw_any))] <- 0
replication4$lag.L_domestic_any[which(is.na(replication4$lag.L_domestic_any))] <- 0
replication4$lag.L_sv_any[which(is.na(replication4$lag.L_sv_any))] <- 0
replication4$lag.L_sh_any[which(is.na(replication4$lag.L_sh_any))] <- 0
replication4$lag.L_FGM_any[which(is.na(replication4$lag.L_FGM_any))] <- 0
replication4$lag.L_trafficking_any[which(is.na(replication4$lag.L_trafficking_any))] <- 0
replication4$lag.L_CEF_any[which(is.na(replication4$lag.L_CEF_any))] <- 0

# Counting number of countries by components. #
replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(Lnumberstalking_ht_region = sum(lag.L_stalking_any),
         Lnumbervaw_ht_region = sum(lag.L_vaw_any),
         Lnumberdomestic_ht_region = sum(lag.L_domestic_any),
         Lnumbersv_ht_region = sum(lag.L_sv_any),
         Lnumbersh_ht_region = sum(lag.L_sh_any),
         LnumberFGM_ht_region = sum(lag.L_FGM_any),
         Lnumbertrafficking_ht_region = sum(lag.L_trafficking_any),
         LnumberCEF_ht_region = sum(lag.L_CEF_any))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(Lnumberstalking_ht_colonial = sum(lag.L_stalking_any),
         Lnumbervaw_ht_colonial = sum(lag.L_vaw_any),
         Lnumberdomestic_ht_colonial = sum(lag.L_domestic_any),
         Lnumbersv_ht_colonial = sum(lag.L_sv_any),
         Lnumbersh_ht_colonial = sum(lag.L_sh_any),
         LnumberFGM_ht_colonial = sum(lag.L_FGM_any),
         Lnumbertrafficking_ht_colonial = sum(lag.L_trafficking_any),
         LnumberCEF_ht_colonial = sum(lag.L_CEF_any))

replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(Lnumberstalking_lp_legor = sum(lag.L_stalking_any),
         Lnumbervaw_lp_legor = sum(lag.L_vaw_any),
         Lnumberdomestic_lp_legor = sum(lag.L_domestic_any),
         Lnumbersv_lp_legor = sum(lag.L_sv_any),
         Lnumbersh_lp_legor = sum(lag.L_sh_any),
         LnumberFGM_lp_legor = sum(lag.L_FGM_any),
         Lnumbertrafficking_lp_legor = sum(lag.L_trafficking_any),
         LnumberCEF_lp_legor = sum(lag.L_CEF_any))

replication4 <- replication4 %>%
  mutate(stalking_ht_region = as.numeric(Lnumberstalking_ht_region)/as.numeric(count_ht_region),
         vaw_ht_region = as.numeric(Lnumbervaw_ht_region)/as.numeric(count_ht_region),
         domestic_ht_region = as.numeric(Lnumberdomestic_ht_region)/as.numeric(count_ht_region),
         sv_ht_region = as.numeric(Lnumbersv_ht_region)/as.numeric(count_ht_region),
         sh_ht_region = as.numeric(Lnumbersh_ht_region)/as.numeric(count_ht_region),
         FGM_ht_region = as.numeric(LnumberFGM_ht_region)/as.numeric(count_ht_region),
         trafficking_ht_region = as.numeric(Lnumbertrafficking_ht_region)/as.numeric(count_ht_region),
         CEF_ht_region = as.numeric(LnumberCEF_ht_region)/as.numeric(count_ht_region))

replication4 <- replication4 %>%
  mutate(stalking_ht_colonial = as.numeric(Lnumberstalking_ht_colonial)/as.numeric(count_ht_colonial),
         vaw_ht_colonial = as.numeric(Lnumbervaw_ht_colonial)/as.numeric(count_ht_colonial),
         domestic_ht_colonial = as.numeric(Lnumberdomestic_ht_colonial)/as.numeric(count_ht_colonial),
         sv_ht_colonial = as.numeric(Lnumbersv_ht_colonial)/as.numeric(count_ht_colonial),
         sh_ht_colonial = as.numeric(Lnumbersh_ht_colonial)/as.numeric(count_ht_colonial),
         FGM_ht_colonial = as.numeric(LnumberFGM_ht_colonial)/as.numeric(count_ht_colonial),
         trafficking_ht_colonial = as.numeric(Lnumbertrafficking_ht_colonial)/as.numeric(count_ht_colonial),
         CEF_ht_colonial = as.numeric(LnumberCEF_ht_colonial)/as.numeric(count_ht_colonial))

replication4 <- replication4 %>%
  mutate(stalking_lp_legor = as.numeric(Lnumberstalking_lp_legor)/as.numeric(count_lp_legor),
         vaw_lp_legor = as.numeric(Lnumbervaw_lp_legor)/as.numeric(count_lp_legor),
         domestic_lp_legor = as.numeric(Lnumberdomestic_lp_legor)/as.numeric(count_lp_legor),
         sv_lp_legor = as.numeric(Lnumbersv_lp_legor)/as.numeric(count_lp_legor),
         sh_lp_legor = as.numeric(Lnumbersh_lp_legor)/as.numeric(count_lp_legor),
         FGM_lp_legor = as.numeric(LnumberFGM_lp_legor)/as.numeric(count_lp_legor),
         trafficking_lp_legor = as.numeric(Lnumbertrafficking_lp_legor)/as.numeric(count_lp_legor),
         CEF_lp_legor = as.numeric(LnumberCEF_lp_legor)/as.numeric(count_lp_legor))

# create time and time^2 variable by first adoption of law against crimes aginst women #
replication4 <- replication4 %>%
  mutate(time = year - 1908)
replication4 <- replication4 %>%
  mutate(time_sq = time * time)

# Variables Labeling #


# draw maps for countries having laws and NAP #
library("ggplot2")
install.packages("cshapes")
library("cshapes")
cshp(date=NA, useGW=FALSE)
cshp.1946 <- cshp(date=as.Date("1946-6-30"), useGW=FALSE)
my1946 <- fortify(cshp.1946)
id <- c(cshp.1946$FEATUREID)
ccode <- c(cshp.1946$COWCODE)
df1946 <- data.frame(id,ccode)
my1946M <- merge(my1946, df1946, by.my1946 = "id", by.df1946 = "id")
d1946 <- subset(replication4, year == 1946)
my1946M2 <- merge(my1946M, d1946, by.my1946M = "ccode", by.d1946 = "ccode")
map1946 <- ggplot(my1946M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("1946")
print(map1946)

cshp.1966 <- cshp(date=as.Date("1966-6-30"), useGW=FALSE)
my1966 <- fortify(cshp.1966)
id <- c(cshp.1966$FEATUREID)
ccode <- c(cshp.1966$COWCODE)
df1966 <- data.frame(id,ccode)
my1966M <- merge(my1966, df1966, by.my1966 = "id", by.df1966 = "id")
d1966 <- subset(replication4, year == 1966)
my1966M2 <- merge(my1966M, d1966, by.my1966M = "ccode", by.d1966 = "ccode")
map1966 <- ggplot(my1966M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("1966")
print(map1966)

cshp.1986 <- cshp(date=as.Date("1986-6-30"), useGW=FALSE)
my1986 <- fortify(cshp.1986)
id <- c(cshp.1986$FEATUREID)
ccode <- c(cshp.1986$COWCODE)
df1986 <- data.frame(id,ccode)
my1986M <- merge(my1986, df1986, by.my1986 = "id", by.df1966 = "id")
d1986 <- subset(replication4, year == 1986)
my1986M2 <- merge(my1986M, d1986, by.my1986M = "ccode", by.d1986 = "ccode")
map1986 <- ggplot(my1986M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("1986")
print(map1986)

cshp.1996 <- cshp(date=as.Date("1996-6-30"), useGW=FALSE)
my1996 <- fortify(cshp.1996)
id <- c(cshp.1996$FEATUREID)
ccode <- c(cshp.1996$COWCODE)
df1996 <- data.frame(id,ccode)
my1996M <- merge(my1996, df1996, by.my1996 = "id", by.df1996 = "id")
d1996 <- subset(replication4, year == 1996)
my1996M2 <- merge(my1996M, d1996, by.my1996M = "ccode", by.d1996 = "ccode")
map1996 <- ggplot(my1996M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("1996")
print(map1996)

cshp.2006 <- cshp(date=as.Date("2006-6-30"), useGW=FALSE)
my2006 <- fortify(cshp.2006)
id <- c(cshp.2006$FEATUREID)
ccode <- c(cshp.2006$COWCODE)
df2006 <- data.frame(id,ccode)
my2006M <- merge(my2006, df2006, by.my2006 = "id", by.df2006 = "id")
d2006 <- subset(replication4, year == 2006)
my2006M2 <- merge(my2006M, d2006, by.my2006M = "ccode", by.d2006 = "ccode")
map2006 <- ggplot(my2006M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2006")
print(map2006)

cshp.2016 <- cshp(date=as.Date("2016-6-30"), useGW=FALSE)
my2016 <- fortify(cshp.2016)
id <- c(cshp.2016$FEATUREID)
ccode <- c(cshp.2016$COWCODE)
df2016 <- data.frame(id,ccode)
my2016M <- merge(my2016, df2016, by.my2016 = "id", by.df2016 = "id")
d2016 <- subset(replication4, year == 2016)
my2016M2 <- merge(my2016M, d2016, by.my2016M = "ccode", by.d2016 = "ccode")
map2016 <- ggplot(my2016M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2016")
print(map2016)

#Combine Maps#
install.packages("ggpubr")
library(ggpubr)
map <- ggarrange(map1966, map1986, map1996, map2016,
         ncol = 2, nrow =2)
annotate_figure(map, top = text_grob("Figure 3: Countries with Laws about Violence Against Women", color = "black", face = "bold", size = 14),
             bottom = text_grob("Light Blue means a country with Laws and Dark Blue without Laws", face = "bold", size = 10))

replication4$lawnap = replication4$L_has_any + replication4$NAP_has_any


#NAP Maps#
cshp.1990 <- cshp(date=as.Date("1990-6-30"), useGW=FALSE)
my1990 <- fortify(cshp.1990)
id <- c(cshp.1990$FEATUREID)
ccode <- c(cshp.1990$COWCODE)
df1990 <- data.frame(id,ccode)
my1990M <- merge(my1990, df1990, by.my1990 = "id", by.df1990 = "id")
d1990 <- subset(replication4, year == 1990)
my1990M2 <- merge(my1990M, d1990, by.my1990M = "ccode", by.d1990 = "ccode")
map1990 <- ggplot(my1990M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = NAP_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("1990")
print(map1990)

cshp.2000 <- cshp(date=as.Date("2000-6-30"), useGW=FALSE)
my2000 <- fortify(cshp.2000)
id <- c(cshp.2000$FEATUREID)
ccode <- c(cshp.2000$COWCODE)
df2000 <- data.frame(id,ccode)
my2000M <- merge(my2000, df2000, by.my2000 = "id", by.df2000 = "id")
d2000 <- subset(replication4, year == 2000)
my2000M2 <- merge(my2000M, d2000, by.my2000M = "ccode", by.d2000 = "ccode")
map2000 <- ggplot(my2000M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = NAP_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2000")
print(map2000)

cshp.2010 <- cshp(date=as.Date("2010-6-30"), useGW=FALSE)
my2010 <- fortify(cshp.2010)
id <- c(cshp.2010$FEATUREID)
ccode <- c(cshp.2010$COWCODE)
df2010 <- data.frame(id,ccode)
my2010M <- merge(my2010, df2010, by.my2010 = "id", by.df2010 = "id")
d2010 <- subset(replication4, year == 2010)
my2010M2 <- merge(my2010M, d2010, by.my2010M = "ccode", by.d2010 = "ccode")
map2010 <- ggplot(my2010M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = NAP_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2010")
print(map2010)

cshp.2016 <- cshp(date=as.Date("2016-6-30"), useGW=FALSE)
my2016 <- fortify(cshp.2016)
id <- c(cshp.2016$FEATUREID)
ccode <- c(cshp.2016$COWCODE)
df2016 <- data.frame(id,ccode)
my2016M <- merge(my2016, df2016, by.my2016 = "id", by.df2016 = "id")
d2016 <- subset(replication4, year == 2016)
my2016M2 <- merge(my2016M, d2016, by.my2016M = "ccode", by.d2016 = "ccode")
map2016N <- ggplot(my2016M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = NAP_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2016")
print(map2016N)

#Combine Maps for NAP#
library(ggpubr)
mapN <- ggarrange(map1990, map2000, map2010, map2016N,
                 ncol = 2, nrow =2)
annotate_figure(mapN, top = text_grob("Figure 4: Countries with NAP about Violence Against Women", color = "black", face = "bold", size = 14),
                bottom = text_grob("Light Blue means a country with NAP and Dark Blue without NAP", face = "bold", size = 10))
print(mapN)

#### Maps for law and NAP
my1990 <- fortify(cshp.1990)
id <- c(cshp.1990$FEATUREID)
ccode <- c(cshp.1990$COWCODE)
df1990 <- data.frame(id,ccode)
my1990M <- merge(my1990, df1990, by.my1990 = "id", by.df1990 = "id")
d1990 <- subset(replication4, year == 1990)
my1990M2 <- merge(my1990M, d1990, by.my1990M = "ccode", by.d1990 = "ccode")
map1990 <- ggplot(my1990M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = lawnap), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("1990")
print(map1990)

cshp.2000 <- cshp(date=as.Date("2000-6-30"), useGW=FALSE)
my2000 <- fortify(cshp.2000)
id <- c(cshp.2000$FEATUREID)
ccode <- c(cshp.2000$COWCODE)
df2000 <- data.frame(id,ccode)
my2000M <- merge(my2000, df2000, by.my2000 = "id", by.df2000 = "id")
d2000 <- subset(replication4, year == 2000)
my2000M2 <- merge(my2000M, d2000, by.my2000M = "ccode", by.d2000 = "ccode")
map2000 <- ggplot(my2000M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = lawnap), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2000")
print(map2000)

cshp.2010 <- cshp(date=as.Date("2010-6-30"), useGW=FALSE)
my2010 <- fortify(cshp.2010)
id <- c(cshp.2010$FEATUREID)
ccode <- c(cshp.2010$COWCODE)
df2010 <- data.frame(id,ccode)
my2010M <- merge(my2010, df2010, by.my2010 = "id", by.df2010 = "id")
d2010 <- subset(replication4, year == 2010)
my2010M2 <- merge(my2010M, d2010, by.my2010M = "ccode", by.d2010 = "ccode")
map2010 <- ggplot(my2010M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = lawnap), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2010")
print(map2010)

cshp.2016 <- cshp(date=as.Date("2016-6-30"), useGW=FALSE)
my2016 <- fortify(cshp.2016)
id <- c(cshp.2016$FEATUREID)
ccode <- c(cshp.2016$COWCODE)
df2016 <- data.frame(id,ccode)
my2016M <- merge(my2016, df2016, by.my2016 = "id", by.df2016 = "id")
d2016 <- subset(replication4, year == 2016)
my2016M2 <- merge(my2016M, d2016, by.my2016M = "ccode", by.d2016 = "ccode")
map2016N <- ggplot(my2016M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = lawnap), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("2016")
print(map2016N)

#Combine Maps for NAP#
library(ggpubr)
mapN <- ggarrange(map1990, map2000, map2010, map2016N,
                  ncol = 2, nrow =2)
annotate_figure(mapN, top = text_grob("Figure 4: Countries with NAP about Violence Against Women", color = "black", face = "bold", size = 14),
                bottom = text_grob("Light Blue means a country with NAP, Gray Blue with Law but not NAP, and Dark Blue without Law", face = "bold", size = 10))



# save dataframe #
write.dta(replication4, file = "replication4.dta")

# Count number of countries with laws or NAP by year for cumulative graph #
library(dplyr)
cum <- replication4 %>%
  filter(L_has_any>0) %>%
  group_by(year) %>%
  tally()

cumNAP <- replication4 %>%
  filter(NAP_has_any>0) %>%
  group_by(year) %>%
  tally()

# Cumulative line graphs of Number of Countries with Laws against Crimes against Women #
cumulative <- ggplot() +
  geom_line(aes(y = n, x = year), size=1, data = cum,
            ) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1908,2016,8)) +
  labs(x="Year", y="Number of Countries") +
  ggtitle("Cumulative Number of Countries with Laws against Violance Against Women") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white"))
cumulative

cumulativeNAP <- ggplot() +
  geom_line(aes(y = n, x = year), size=1, data = cumNAP,
  ) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1988,2016,8)) +
  labs(x="Year", y="Number of Countries") +
  ggtitle("Cumulative Number of Countries with NAP against Violance Against Women") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white"))
cumulativeNAP

# Number of adoption of laws and NAP by year #
cumyear <- replication4 %>%
  group_by(year) %>%
  filter(year>1907) %>%
  summarise(numinyear = sum(L_any))

cumyearLAW <- ggplot(cumyear, aes(x = year, y = numinyear)) +
  geom_col() +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1908,2016,8)) +
  labs(x="Year", y="Number of Countries") +
  ggtitle("Number of Countries Adopting Laws against Violence Against Women by Year") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white"))
cumyearLAW

cumyear2 <- replication4 %>%
  group_by(year) %>%
  filter(year>1987) %>%
  summarise(numinyear = sum(NAP_any))

cumyearNAP <- ggplot(cumyear2, aes(x = year, y = numinyear)) +
  geom_col() +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1908,2016,8)) +
  labs(x="Year", y="Number of Countries") +
  ggtitle("Number of Countries Adopting NAP against Violence Against Women by Year") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white"))
cumyearNAP


install.packages("plotly") 
library(plotly)

cumulative <- ggplotly(cumulative)
cumulative 
savePlot(CumulativeNumber)
ggsave(filename = "Cumulative Number of Countries with Laws against Violence Against Women", plot = plot.default())

# Analysis #
library(survival)
library(MASS)
fit2 = coxph(Surv(time,L_has_any)~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.polity2 + lag.cgdppc + lag.vdem_gender + cluster(ccode), data=replication4)
summary(fit2)

# drop before first any law adoption#
replication <- subset(replication4, year > 1907)

#Labeling# 
replication$ht_region <- factor(replication$ht_region, 
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East", "Sub-Saharan Africa", "Western Europe & North America", "East Asia", "South-East Asia", "South Asia", "The Pacific", "The Caribbean"))

#Descriptive Table for Variables#
summary(replication)

# visualization #
by_year_region <- replication %>%
  group_by(year, ht_region) %>%
  summarise(NumofCon = sum(L_has_any))

ggplot(by_year_region, aes(x=year, y=NumofCon, color=ht_region)) +
  geom_path() +
  facet_wrap(~ ht_region)


#subset lag.L_has_any<1: drop after first adoption#
subset1 <- subset(replication, lag.L_has_any<1)

# test cluster#
install.packages("clusterSEs")
library(clusterSEs)

##Any law##
#first adoption#
myprobit <- glm(L_any ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subset1)
summary(myprobit)
nobs(myprobit)
clust.myprobit <- cluster.bs.glm(myprobit, subset1, ~ ccode, report = T)

mycox <- coxph(Surv(time,L_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + cluster(ccode), data=subset1)
summary(mycox)

#repeated adoption#
myprobit2 <- glm(L_any ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=replication)
summary(myprobit2)
nobs(myprobit2)
clust.myprobit2 <- cluster.bs.glm(myprobit2, replication, ~ ccode, report = T)

# I cannot use proportional hazard model because it does not pass the diagnostic test of proportional hazard #
mycox2 <- coxph(Surv(time,L_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + cluster(ccode), data=replication)
summary(mycox2)
test.ph <- cox.zph(mycox2)
test.ph

#simple hazard model#


##Diffusion of NAP without considering steps##
#subset lag.NAP_has_any<1: drop after first NAP#
replicationNAP <- subset(replication4, year > 1991)
replicationNAP <- replicationNAP %>%
  mutate(time = year - 1992)
replicationNAP <- replicationNAP %>%
  mutate(time_sq = time * time)

subset1NAP <- subset(replicationNAP, lag.NAP_has_any<1)

#first NAP#
myprobitNAP <- glm(NAP_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subset1NAP)
summary(myprobitNAP)
nobs(myprobitNAP)
clust.myprobitNAP <- cluster.bs.glm(myprobitNAP, subset1NAP, ~ ccode, report = T)


mycoxNAP <- coxph(Surv(time,NAP_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + cluster(ccode), data=subset1NAP)
summary(mycoxNAP)

write.dta(replicationNAP, file = "subset1NAP.dta")


#repeated NAP#
myprobit2NAP <- glm(NAP_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=replicationNAP)
summary(myprobit2NAP)
nobs(myprobit2NAP)
clust.myprobit2NAP <- cluster.bs.glm(myprobit2NAP, replicationNAP, ~ ccode, report = T)


mycox2NAP <- coxph(Surv(time,NAP_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender, data=replicationNAP)
summary(mycox2NAP)


##Diffusion of NAP after Adoption of Law##
#repeated NAP after adoption of law#
subsetLNAP <- subset(replicationNAP, L_has_any>0)
myprobitLNAP <- glm(NAP_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subsetLNAP)
summary(myprobitLNAP)
nobs(myprobitLNAP)
clust.myprobitLNAP <- cluster.bs.glm(myprobitLNAP, subsetLNAP, ~ ccode, report = T)


#first NAP after adoption of law#
subset1LNAP <- subset(subsetLNAP, lag.NAP_has_any<1)
myprobit1LNAP <- glm(NAP_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subset1LNAP)
summary(myprobit1LNAP)

nobs(myprobit1LNAP)
clust.myprobit1LNAP <- cluster.bs.glm(myprobit1LNAP, subset1LNAP, ~ ccode, report = T)

# getting out replication for running stata #
write.dta(replication, file = "replication.dta")

# create Number and Percentage of CEDAW countries #
replication <- replication %>%
  group_by(year) %>%
  mutate(count_country = n())

replication <- replication %>%
  group_by(year) %>%
  mutate(count_country_CEDAW = sum(CEDAW))

replication <- replication %>%
  mutate(percentage_CEDAW = as.numeric(count_country_CEDAW)/as.numeric(count_country))

# create lagged adoptions by country for multiple components analysis #
replication <- replication %>%
  group_by(ccode) %>%
  mutate(lag.L_Feminicide = dplyr::lag(L_Femicide, n=1, default = NA)) %>%
  mutate(lag.L_forced_sterilization = dplyr::lag(L_forced_sterilization, n=1, default = NA)) %>%
  mutate(lag.L_stalking = dplyr::lag(L_stalking, n=1, default = NA)) %>%
  mutate(lag.L_property = dplyr::lag(L_property, n=1, default = NA)) %>%
  mutate(lag.L_violence_against_women = dplyr::lag(L_violence_against_women, n=1, default = NA)) %>%
  mutate(lag.L_domestic = dplyr::lag(L_domestic, n=1, default = NA)) %>%
  mutate(lag.L_sexual_violence = dplyr::lag(L_sexual_violence, n=1, default = NA)) %>%
  mutate(lag.L_new = dplyr::lag(L_new, n=1, default = NA)) %>%
  mutate(lag.L_sexual_harassment = dplyr::lag(L_sexual_harassment, n=1, default = NA)) %>%
  mutate(lag.L_FGM = dplyr::lag(L_FGM, n=1, default = NA)) %>%
  mutate(lag.L_trafficking = dplyr::lag(L_trafficking, n=1, default = NA)) %>%
  mutate(lag.L_child_ealry_forced = dplyr::lag(L_child_ealry_forced, n=1, default = NA)) %>%
  mutate(lag.NAP_Femicide = dplyr::lag(NAP_Femicide, n=1, default = NA)) %>%
  mutate(lag.NAP_forced_sterilization = dplyr::lag(NAP_forced_sterilization, n=1, default = NA)) %>%
  mutate(lag.NAP_stalking = dplyr::lag(NAP_stalking, n=1, default = NA)) %>%
  mutate(lag.NAP_property = dplyr::lag(NAP_property, n=1, default = NA)) %>%
  mutate(lag.NAP_violence_against_women = dplyr::lag(NAP_violence_against_women, n=1, default = NA)) %>%
  mutate(lag.NAP_domestic = dplyr::lag(NAP_domestic, n=1, default = NA)) %>%
  mutate(lag.NAP_sexual_violence = dplyr::lag(NAP_sexual_violence, n=1, default = NA)) %>%
  mutate(lag.NAP_new = dplyr::lag(NAP_new, n=1, default = NA)) %>%
  mutate(lag.NAP_sexual_harassment = dplyr::lag(NAP_sexual_harassment, n=1, default = NA)) %>%
  mutate(lag.NAP_FGM = dplyr::lag(NAP_FGM, n=1, default = NA)) %>%
  mutate(lag.NAPL_trafficking = dplyr::lag(NAPL_trafficking, n=1, default = NA)) %>%
  mutate(lag.NAPL_child_ealry_forced = dplyr::lag(NAPL_child_ealry_forced, n=1, default = NA))

#replace na of laggded to 0#
replication$lag.L_Feminicide[which(is.na(replication$lag.L_Feminicide))] <- 0
replication$lag.L_forced_sterilization[which(is.na(replication$lag.L_forced_sterilization))] <- 0
replication$lag.L_stalking[which(is.na(replication$lag.L_stalking))] <- 0
replication$lag.L_property[which(is.na(replication$lag.L_property))] <- 0
replication$lag.L_violence_against_women[which(is.na(replication$lag.L_violence_against_women))] <- 0
replication$lag.L_domestic[which(is.na(replication$lag.L_domestic))] <- 0
replication$lag.L_sexual_violence[which(is.na(replication$lag.L_sexual_violence))] <- 0
replication$lag.L_new[which(is.na(replication$lag.L_new))] <- 0
replication$lag.L_sexual_harassment[which(is.na(replication$lag.L_sexual_harassment))] <- 0
replication$lag.L_FGM[which(is.na(replication$lag.L_FGM))] <- 0
replication$lag.L_trafficking[which(is.na(replication$lag.L_trafficking))] <- 0
replication$lag.L_child_ealry_forced[which(is.na(replication$lag.L_child_ealry_forced))] <- 0
replication$lag.NAP_Femicide[which(is.na(replication$lag.NAP_Femicide))] <- 0
replication$lag.NAP_forced_sterilization[which(is.na(replication$lag.NAP_forced_sterilization))] <- 0
replication$lag.NAP_stalking[which(is.na(replication$lag.NAP_stalking))] <- 0
replication$lag.NAP_property[which(is.na(replication$lag.NAP_property))] <- 0
replication$lag.NAP_violence_against_women[which(is.na(replication$lag.NAP_violence_against_women))] <- 0
replication$lag.NAP_domestic[which(is.na(replication$lag.NAP_domestic))] <- 0
replication$lag.NAP_sexual_violence[which(is.na(replication$lag.NAP_sexual_violence))] <- 0
replication$lag.NAP_new[which(is.na(replication$lag.NAP_new))] <- 0
replication$lag.NAP_sexual_harassment[which(is.na(replication$lag.NAP_sexual_harassment))] <- 0
replication$lag.NAP_FGM[which(is.na(replication$lag.NAP_FGM))] <- 0
replication$lag.NAPL_trafficking[which(is.na(replication$lag.NAPL_trafficking))] <- 0
replication$lag.NAPL_child_ealry_forced[which(is.na(replication$lag.NAPL_child_ealry_forced))] <- 0


        
# getting out replication for running stata #
write.dta(replication, file = "replication.dta")

##Run Stata code in R##
if (!require(RStata)) install.packages("RStata"); library(RStata) # this will install RStata if not already installed
options("C:/Program Files (x86)/Stata14/StataMP-64.exe")
options("RStata.StataVersion" = 14)
stata("Final(BD).do",
      stata.path = "C:/Program Files (x86)/Stata14/StataMP-64.exe", # yours probably differs: use the chooseStataBin() command on windows or linux machines; on Macs, right click on the Stata app, select "Show Package Contents", then see what's in the Contents/MacOS/ directory
      stata.version = 14)  # again, specify what _you_ have)

##Run Stata simulation code in R##
if (!require(RStata)) install.packages("RStata"); library(RStata) # this will install RStata if not already installed
options("C:/Program Files (x86)/Stata14/StataMP-64.exe")
options("RStata.StataVersion" = 14)
stata("Final_BD_simulation.do",
      stata.path = "C:/Program Files (x86)/Stata14/StataMP-64.exe", # yours probably differs: use the chooseStataBin() command on windows or linux machines; on Macs, right click on the Stata app, select "Show Package Contents", then see what's in the Contents/MacOS/ directory
      stata.version = 14)  # again, specify what _you_ have)

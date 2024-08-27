#' ---
#' title: "Stochastic frontier analysis of livestock production"
#' author: "Frédéric Baudron"
#' date: "August 26th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list=ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

suppressPackageStartupMessages({
  library(frontier)
  library(ggplot2)
  library(ggthemes)
  library(cowplot)
  library(tidyr)
  library(viridis)
  library(egg)
  library(grid)
  library(cowplot)
})


# SET WORKING DIRECTORY---------------------------------------------------------

setwd('D:\\Mes Donnees\\0. Publi\\3. Crop-livestock\\0. Revision Experimental Ag\\Data & code\\')


# LOADING AND MANIPULATING THE DATA---------------------------------------------

DT = read.csv("Data sfa livestock.csv")

DT$clay_silt = DT$sgrids_clay + DT$sgrids_silt

DT$Small_ruminants = DT$SMall_ruminants

DT$TLU = DT$TLU + DT$TLU_offtake + DT$TLU_mortality

DT$Cattle = DT$Cattle + DT$Number.of.improved.adult....3yr..cattle.sold.in.the.past.12.months. +
  DT$Number.of.indigenous.adult....3yr..cattle.sold.in.the.past.12.months. +
  DT$Number.of.improved.juvenile....3yr..cattle.sold.in.the.past.12.months. +
  DT$Number.of.indigenous.juvenile....3yr..cattle.sold.in.the.past.12.months. +
  DT$Number.of.oxen.sold.in.the.past.12.months. +
  DT$Number.of.improved.adult....3yr..cattle.slaughtered.in.the.past.12.months. +
  DT$Number.of.indigenous.adult....3yr..cattle.slaughtered.in.the.past.12.months. +
  DT$Number.of.improved.juvenile....3yr..cattle.slaughtered.in.the.past.12.months. +
  DT$Number.of.indigenous.juvenile....3yr..cattle.slaughtered.in.the.past.12.months. +
  DT$Number.of.oxen.slaughtered.in.the.past.12.months. +
  DT$Number.of.improved.adult....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.indigenous.adult....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.improved.juvenile....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.indigenous.juvenile....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.donkeys.dying.in.the.past.12.months.

DT$Small_ruminants = DT$Small_ruminants +
  DT$Number.of.improved.goats.sold.in.the.past.12.months. +
  DT$Number.of.indigenous.goats.sold.in.the.past.12.months. +
  DT$Number.of.sheep.sold.in.the.past.12.months. +
  DT$Number.of.improved.goats.slaughtered.in.the.past.12.months. +
  DT$Number.of.indigenous.goats.slaughtered.in.the.past.12.months. +
  DT$Number.of.sheep.slaughtered.in.the.past.12.months. +
  DT$Number.of.improved.goats.dying.in.the.past.12.months. +
  DT$Number.of.indigenous.goats.dying.in.the.past.12.months. +
  DT$Number.of.sheep.dying.in.the.past.12.months.

DT$Poultry = DT$Poultry +
  DT$Number.of.chicken.sold.in.the.past.12.months. +
  DT$Number.of.turkey.sold.in.the.past.12.months. +
  DT$Number.of.guinea.fowls.sold.in.the.past.12.months. +
  DT$Number.of.chicken.slaughtered.in.the.past.12.months. +
  DT$Number.of.turkey.slaughtered.in.the.past.12.months. +
  DT$Number.of.guinea.fowls.slaughtered.in.the.past.12.months. +
  DT$Number.of.chicken.dying.in.the.past.12.months. +
  DT$Number.of.turkey.dying.in.the.past.12.months. +
  DT$Number.of.guinea.fowls.dying.in.the.past.12.months.


DT$Cattle_mortality = DT$Number.of.improved.adult....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.indigenous.adult....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.improved.juvenile....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.indigenous.juvenile....3yr..cattle.dying.in.the.past.12.months. +
  DT$Number.of.donkeys.dying.in.the.past.12.months.

DT$Small_ruminants_mortality = DT$Number.of.improved.goats.dying.in.the.past.12.months. +
  DT$Number.of.indigenous.goats.dying.in.the.past.12.months. +
  DT$Number.of.sheep.dying.in.the.past.12.months.

DT$Poultry_mortality = DT$Number.of.chicken.dying.in.the.past.12.months. +
  DT$Number.of.turkey.dying.in.the.past.12.months. +
  DT$Number.of.guinea.fowls.dying.in.the.past.12.months.


DT$Cropped_area = ifelse(DT$Cropped_area == 0, 0.0001, DT$Cropped_area)
DT$Age_HHH = ifelse(DT$Age_HHH == 0, 0.0001, DT$Age_HHH)
DT$Family_size = ifelse(DT$Family_size == 0, 0.0001, DT$Family_size)
DT$Equipment_value = ifelse(DT$Equipment_value == 0, 0.0001, DT$Equipment_value)
DT$Grain = ifelse(DT$Grain == 0, 0.0001, DT$Grain)
DT$Fertilizers = ifelse(DT$Fertilizers == 0, 0.0001, DT$Fertilizers)
DT$Organic_amendments = ifelse(DT$Organic_amendments == 0, 0.0001, DT$Organic_amendments)
DT$Cattle = ifelse(DT$Cattle == 0, 0.01, DT$Cattle)
DT$Small_ruminants = ifelse(DT$Small_ruminants == 0, 0.01, DT$Small_ruminants)
DT$Poultry = ifelse(DT$Poultry == 0, 0.01, DT$Poultry)
DT$TLU_mortality = ifelse(DT$TLU_mortality == 0, 0.0001, DT$TLU_mortality)
DT$Cattle_mortality = ifelse(DT$Cattle_mortality == 0, 0.0001, DT$Cattle_mortality)
DT$Small_ruminants_mortality = ifelse(DT$Small_ruminants_mortality == 0, 0.0001, DT$Small_ruminants_mortality)
DT$Poultry_mortality = ifelse(DT$Poultry_mortality == 0, 0.0001, DT$Poultry_mortality)

DT$TLU_mort_rate = DT$TLU_mortality / DT$TLU
DT$Cattle_mort_rate = DT$Cattle_mortality / DT$Cattle
DT$Small_ruminants_mort_rate = DT$Small_ruminants_mortality / DT$Small_ruminants
DT$Poultry_mort_rate = DT$Poultry_mortality / DT$Poultry


DT1 = DT[ which(DT$Type == "Type 1"), ]
DT2 = DT[ which(DT$Type == "Type 2"), ]
DT3 = DT[ which(DT$Type == "Type 3"), ]


# 1. LIVESTOCK OFFTAKE TOTAL----------------------------------------------------

DTTLU = subset(DT, TLU > 0)

DTTLU = subset(DTTLU, TLU_offtake > 0)

# Outliers

# boxplot(DTTLU$TLU_offtake, ylab='Livestock offtake (TLU)')
DTTLU = subset(DTTLU, TLU_offtake < 12)
# boxplot(DTTLU$TLU_offtake, ylab='Livestock offtake (TLU)')

# boxplot(DTTLU$TLU, ylab='Livestock ownership (TLU)')
DTTLU = subset(DTTLU, TLU < 60)
# boxplot(DTTLU$TLU, ylab='Livestock ownership (TLU)')

# boxplot(DTTLU$Cropped_area, ylab='Cropped area (ha)')
DTTLU = subset(DTTLU, Cropped_area < 25)
# boxplot(DTTLU$Cropped_area, ylab='Cropped area (ha)')

# boxplot(DTTLU$TLU_mort_rate, ylab='Livestock mortality rate')


# plot(DTTLU$TLU, DTTLU$TLU_offtake, ylab ='Livestock offtake (TLU)', xlab ='Livestock ownership (TLU)')
# plot(DTTLU$Cropped_area, DTTLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Cropped area (ha)')


# Data transformations

DTTLU$Log_TLU = log(DTTLU$TLU/mean(DTTLU$TLU,na.rm=TRUE))
DTTLU$Log_TLUprod = log(DTTLU$TLU_offtake/mean(DTTLU$TLU_offtake,na.rm=TRUE))
DTTLU$Log_cattle = log(DTTLU$Cattle/mean(DTTLU$Cattle,na.rm=TRUE))
DTTLU$Log_rum = log(DTTLU$Small_ruminants/mean(DTTLU$Small_ruminants,na.rm=TRUE))
DTTLU$Log_poultry = log(DTTLU$Poultry/mean(DTTLU$Poultry,na.rm=TRUE))
DTTLU$Log_Area = log(DTTLU$Cropped_area/mean(DTTLU$Cropped_area,na.rm=TRUE))
DTTLU$Log_Age = log(DTTLU$Age_HHH/mean(DTTLU$Age_HHH,na.rm=TRUE))
DTTLU$Log_Fam= log(DTTLU$Family_size/mean(DTTLU$Family_size,na.rm=TRUE))
DTTLU$Log_Equip = log(DTTLU$Equipment_value/mean(DTTLU$Equipment_value,na.rm=TRUE))
DTTLU$Log_Grain = log(DTTLU$Grain/mean(DTTLU$Grain,na.rm=TRUE))


DTTLU$Log_TLU_mortality = log(DTTLU$TLU_mortality/mean(DTTLU$TLU_mortality,na.rm=TRUE))
 
# hist(DTTLU$TLU_mort_rate)
DTTLU$Log_TLU_mort_rate = log(DTTLU$TLU_mort_rate/mean(DTTLU$TLU_mort_rate,na.rm=TRUE))
# hist(DTTLU$Log_TLU_mort_rate)

DTTLU$Log_cattle_mortality = log(DTTLU$Cattle_mortality/mean(DTTLU$Cattle_mortality,na.rm=TRUE))
DTTLU$Log_small_ruminants_mortality = log(DTTLU$Small_ruminants_mortality/mean(DTTLU$Small_ruminants_mortality,na.rm=TRUE))
DTTLU$Log_poultry_mortality = log(DTTLU$Poultry_mortality/mean(DTTLU$Poultry_mortality,na.rm=TRUE))

DTTLU$Log_cattle_mort_rate = log(DTTLU$Cattle_mort_rate/mean(DTTLU$Cattle_mort_rate,na.rm=TRUE))
DTTLU$Log_small_ruminants_mort_rate = log(DTTLU$Small_ruminants_mort_rate/mean(DTTLU$Small_ruminants_mort_rate,na.rm=TRUE))
DTTLU$Log_poultry_mort_rate = log(DTTLU$Poultry_mort_rate/mean(DTTLU$Poultry_mort_rate,na.rm=TRUE))


DTTLU$chirps_avg = log(DTTLU$chirps_avg/mean(DTTLU$chirps_avg,na.rm=TRUE))
DTTLU$chirps_cv = log(DTTLU$chirps_cv/mean(DTTLU$chirps_cv,na.rm=TRUE))
DTTLU$gyga_tseas = log(DTTLU$gyga_tseas/mean(DTTLU$gyga_tseas,na.rm=TRUE))
DTTLU$gyga_gdd = log(DTTLU$gyga_gdd/mean(DTTLU$gyga_gdd,na.rm=TRUE))
DTTLU$gyga_ai = log(DTTLU$gyga_ai/mean(DTTLU$gyga_ai,na.rm=TRUE))
DTTLU$clay_silt = log(DTTLU$clay_silt/mean(DTTLU$clay_silt,na.rm=TRUE))
DTTLU$alt = log(DTTLU$alt/mean(DTTLU$alt,na.rm=TRUE))


# Explaining (in)efficiencies


SFA_DTTLU_1 = sfa(Log_TLUprod ~ 
                    # Log_cattle + Log_rum + Log_poultry + 
                    Log_TLU +
                    # Log_cattle_mort_rate + Log_small_ruminants_mort_rate + Log_poultry_mort_rate +
                    Log_cattle_mortality + Log_small_ruminants_mortality + Log_poultry_mortality +
                    # Log_TLU_mortality +
                    Log_Area + 
                    # alt +
                    chirps_avg + 
                    chirps_cv +
                    # gyga_tseas +
                    # gyga_ai + 
                    gyga_gdd + 
                    clay_silt + 
                    # Log_Grain + 
                    Log_Equip +
                    as.factor(Shelters) + as.factor(Routine_vaccination) + 
                    as.factor(Home_vaccination) + as.factor(Castration) +
                    as.factor(Deworming) + as.factor(Dipping) + as.factor(Home_spraying) + 
                    as.factor(Paravet) + as.factor(Home_feed),
                  data = DTTLU)
summary(SFA_DTTLU_1)


DTTLU$efficiency_SFA_DTTLU_1 = efficiencies(SFA_DTTLU_1, asInData=TRUE)
# DTTLU$efficiency_SFA_DTTLU_1

DTTLU$techeff_offtake_SFA_DTTLU_1 = DTTLU$TLU_offtake / DTTLU$efficiency_SFA_DTTLU_1
# DTTLU$techeff_offtake_SFA_DTTLU_1

# plot(DTTLU$efficiency_SFA_DTTLU_1, DTTLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Efficiency')

mean(na.omit(DTTLU$TLU_offtake))
mean(na.omit(DTTLU$techeff_offtake_SFA_DTTLU_1))
mean(na.omit(DTTLU$efficiency_SFA_DTTLU_1))


plot1 = ggplot(DTTLU, aes(x = efficiency_SFA_DTTLU_1, y = TLU_offtake, fill = District.x)) +
  geom_point(size=3, shape=21, alpha = 0.5) + 
  geom_hline(yintercept = mean(na.omit(DTTLU$TLU_offtake)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DTTLU$efficiency_SFA_DTTLU_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("A. Livestock - All types") +
  xlab("Efficiency") + ylab("Livestock production (TLU)") +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  theme(plot.title = element_text(size=16, face = 'bold'),
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=10),
        legend.title = element_blank(),
        legend.text = element_text(size=8, face = "bold"),
        legend.background = element_rect(size = 0.5, colour = "black"),
        legend.justification = c(0.1, 0.9), legend.position = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 8.7)

plot1

SFA_DTTLU_1df = as.data.frame(coef(summary(SFA_DTTLU_1)))

# write.csv(SFA_DTTLU_1df, "TLU Total.csv")


# 2. LIVESTOCK OFFTAKE TYPE1----------------------------------------------------

DT1TLU = subset(DT1, TLU > 0)

DT1TLU = subset(DT1TLU, TLU_offtake > 0)


# Outliers

# boxplot(DT1TLU$TLU_offtake, ylab='Livestock offtake (TLU)')

# boxplot(DT1TLU$TLU, ylab='Livestock (TLU)')

# boxplot(DT1TLU$Cropped_area, ylab='Cropped area (ha)')
DT1TLU = subset(DT1TLU, Cropped_area < 15)
# boxplot(DT1TLU$Cropped_area, ylab='Cropped area (ha)')

DT1TLU$TLU_mort_rate = DT1TLU$TLU_mortality / DT1TLU$TLU
# boxplot(DT1TLU$TLU_mort_rate, ylab='Livestock mortality rate')

# plot(DT1TLU$TLU, DT1TLU$TLU_offtake, ylab ='Livestock offtake (TLU)', xlab ='Livestock ownership (TLU)')
# plot(DT1TLU$Cropped_area, DT1TLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Cropped area (ha)')


# Data transformations

DT1TLU$Log_TLU = log(DT1TLU$TLU/mean(DT1TLU$TLU,na.rm=TRUE))
DT1TLU$Log_TLUprod = log(DT1TLU$TLU_offtake/mean(DT1TLU$TLU_offtake,na.rm=TRUE))
DT1TLU$Log_cattle = log(DT1TLU$Cattle/mean(DT1TLU$Cattle,na.rm=TRUE))
DT1TLU$Log_rum = log(DT1TLU$Small_ruminants/mean(DT1TLU$Small_ruminants,na.rm=TRUE))
DT1TLU$Log_poultry = log(DT1TLU$Poultry/mean(DT1TLU$Poultry,na.rm=TRUE))
DT1TLU$Log_Area = log(DT1TLU$Cropped_area/mean(DT1TLU$Cropped_area,na.rm=TRUE))
DT1TLU$Log_Age = log(DT1TLU$Age_HHH/mean(DT1TLU$Age_HHH,na.rm=TRUE))
DT1TLU$Log_Fam= log(DT1TLU$Family_size/mean(DT1TLU$Family_size,na.rm=TRUE))
DT1TLU$Log_Equip = log(DT1TLU$Equipment_value/mean(DT1TLU$Equipment_value,na.rm=TRUE))
DT1TLU$Log_Grain = log(DT1TLU$Grain/mean(DT1TLU$Grain,na.rm=TRUE))

DT1TLU$Log_TLU_mortality = log(DT1TLU$TLU_mortality/mean(DT1TLU$TLU_mortality,na.rm=TRUE))

# hist(DT1TLU$TLU_mort_rate)
DT1TLU$Log_TLU_mort_rate = log(DT1TLU$TLU_mort_rate/mean(DT1TLU$TLU_mort_rate,na.rm=TRUE))
# hist(DT1TLU$Log_TLU_mort_rate)

DT1TLU$Log_cattle_mortality = log(DT1TLU$Cattle_mortality/mean(DT1TLU$Cattle_mortality,na.rm=TRUE))
DT1TLU$Log_small_ruminants_mortality = log(DT1TLU$Small_ruminants_mortality/mean(DT1TLU$Small_ruminants_mortality,na.rm=TRUE))
DT1TLU$Log_poultry_mortality = log(DT1TLU$Poultry_mortality/mean(DT1TLU$Poultry_mortality,na.rm=TRUE))

DT1TLU$Log_cattle_mort_rate = log(DT1TLU$Cattle_mort_rate/mean(DT1TLU$Cattle_mort_rate,na.rm=TRUE))
DT1TLU$Log_small_ruminants_mort_rate = log(DT1TLU$Small_ruminants_mort_rate/mean(DT1TLU$Small_ruminants_mort_rate,na.rm=TRUE))
DT1TLU$Log_poultry_mort_rate = log(DT1TLU$Poultry_mort_rate/mean(DT1TLU$Poultry_mort_rate,na.rm=TRUE))

DT1TLU$chirps_avg = log(DT1TLU$chirps_avg/mean(DT1TLU$chirps_avg,na.rm=TRUE))
DT1TLU$chirps_cv = log(DT1TLU$chirps_cv/mean(DT1TLU$chirps_cv,na.rm=TRUE))
DT1TLU$gyga_tseas = log(DT1TLU$gyga_tseas/mean(DT1TLU$gyga_tseas,na.rm=TRUE))
DT1TLU$gyga_gdd = log(DT1TLU$gyga_gdd/mean(DT1TLU$gyga_gdd,na.rm=TRUE))
DT1TLU$gyga_ai = log(DT1TLU$gyga_ai/mean(DT1TLU$gyga_ai,na.rm=TRUE))
DT1TLU$clay_silt = log(DT1TLU$clay_silt/mean(DT1TLU$clay_silt,na.rm=TRUE))
DT1TLU$alt = log(DT1TLU$alt/mean(DT1TLU$alt,na.rm=TRUE))


# Explaining (in)efficiencies

SFA_DT1TLU_1 = sfa(Log_TLUprod ~ 
                     # Log_cattle + Log_rum + Log_poultry + 
                     Log_TLU +
                     # Log_cattle_mort_rate + Log_small_ruminants_mort_rate + Log_poultry_mort_rate +
                     Log_cattle_mortality + Log_small_ruminants_mortality + Log_poultry_mortality +
                     # Log_TLU_mortality +
                     Log_Area + 
                     # alt +
                     chirps_avg + 
                     chirps_cv +
                     # gyga_tseas +
                     # gyga_ai + 
                     gyga_gdd + 
                     clay_silt + 
                     # Log_Grain + 
                     Log_Equip +
                     as.factor(Shelters) + as.factor(Routine_vaccination) + 
                     as.factor(Home_vaccination) + as.factor(Castration) +
                     as.factor(Deworming) + as.factor(Dipping) + as.factor(Home_spraying) + 
                     as.factor(Paravet) + as.factor(Home_feed),
                  data = DT1TLU)
summary(SFA_DT1TLU_1)


DT1TLU$efficiency_SFA_DT1TLU_1 = round(efficiencies(SFA_DT1TLU_1, asInData=TRUE),3)
# DT1TLU$efficiency_SFA_DT1TLU_1

DT1TLU$techeff_offtake_SFA_DT1TLU_1 = DT1TLU$TLU_offtake / DT1TLU$efficiency_SFA_DT1TLU_1
# DT1TLU$techeff_offtake_SFA_DT1TLU_1

# plot(DT1TLU$efficiency_SFA_DT1TLU_1, DT1TLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Efficiency')

mean(na.omit(DT1TLU$TLU_offtake))
mean(na.omit(DT1TLU$techeff_offtake_SFA_DT1TLU_1))
mean(na.omit(DT1TLU$efficiency_SFA_DT1TLU_1))


plot2 = ggplot(DT1TLU, aes(x = efficiency_SFA_DT1TLU_1, y = TLU_offtake)) +
  geom_point(size=3, shape=21, alpha = 0.6, fill = "#BB3754FF") + 
  geom_hline(yintercept = mean(na.omit(DT1TLU$TLU_offtake)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DT1TLU$efficiency_SFA_DT1TLU_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("B. Livestock - Type 1") +
  xlab("Efficiency") + ylab("Livestock production (TLU)") +
  theme(plot.title = element_text(size=16, face = 'bold'),
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=10),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size=8),
        legend.background = element_rect(size = 0.5, colour = "black"),
        legend.justification = c(0.1, 0.9), legend.position = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 8.7)

plot2


SFA_DT1TLU_1df = as.data.frame(coef(summary(SFA_DT1TLU_1)))

# write.csv(SFA_DT1TLU_1df, "TLU Type 1.csv")


# 3. LIVESTOCK OFFTAKE TYPE2----------------------------------------------------

DT2TLU = subset(DT2, TLU > 0)

DT2TLU = subset(DT2TLU, TLU_offtake > 0)


# Outliers

# boxplot(DT2TLU$TLU_offtake, ylab='Livestock offtake (TLU)')
DT2TLU = subset(DT2TLU, TLU_offtake < 5)
# boxplot(DT2TLU$TLU_offtake, ylab='Livestock offtake (TLU)')

# boxplot(DT2TLU$TLU, ylab='Livestock ownership (TLU)')

# boxplot(DT2TLU$Cropped_area, ylab='Cropped area (ha)')

DT2TLU$TLU_mort_rate = DT2TLU$TLU_mortality / DT2TLU$TLU
# boxplot(DT2TLU$TLU_mort_rate, ylab='Livestock mortality rate')

# plot(DT2TLU$TLU, DT2TLU$TLU_offtake, ylab ='Livestock offtake (TLU)', xlab ='Livestock ownership (TLU)')
# plot(DT2TLU$Cropped_area, DT2TLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Cropped area (ha)')


# Data transformations
 
DT2TLU$Log_TLU = log(DT2TLU$TLU/mean(DT2TLU$TLU,na.rm=TRUE))
DT2TLU$Log_TLUprod = log(DT2TLU$TLU_offtake/mean(DT2TLU$TLU_offtake,na.rm=TRUE))
DT2TLU$Log_cattle = log(DT2TLU$Cattle/mean(DT2TLU$Cattle,na.rm=TRUE))
DT2TLU$Log_rum = log(DT2TLU$Small_ruminants/mean(DT2TLU$Small_ruminants,na.rm=TRUE))
DT2TLU$Log_poultry = log(DT2TLU$Poultry/mean(DT2TLU$Poultry,na.rm=TRUE))
DT2TLU$Log_Area = log(DT2TLU$Cropped_area/mean(DT2TLU$Cropped_area,na.rm=TRUE))
DT2TLU$Log_Age = log(DT2TLU$Age_HHH/mean(DT2TLU$Age_HHH,na.rm=TRUE))
DT2TLU$Log_Fam= log(DT2TLU$Family_size/mean(DT2TLU$Family_size,na.rm=TRUE))
DT2TLU$Log_Equip = log(DT2TLU$Equipment_value/mean(DT2TLU$Equipment_value,na.rm=TRUE))
DT2TLU$Log_Grain = log(DT2TLU$Grain/mean(DT2TLU$Grain,na.rm=TRUE))

DT2TLU$Log_TLU_mortality = log(DT2TLU$TLU_mortality/mean(DT2TLU$TLU_mortality,na.rm=TRUE))

# hist(DT2TLU$TLU_mort_rate)
DT2TLU$Log_TLU_mort_rate = log(DT2TLU$TLU_mort_rate/mean(DT2TLU$TLU_mort_rate,na.rm=TRUE))
# hist(DT2TLU$Log_TLU_mort_rate)

DT2TLU$Log_cattle_mortality = log(DT2TLU$Cattle_mortality/mean(DT2TLU$Cattle_mortality,na.rm=TRUE))
DT2TLU$Log_small_ruminants_mortality = log(DT2TLU$Small_ruminants_mortality/mean(DT2TLU$Small_ruminants_mortality,na.rm=TRUE))
DT2TLU$Log_poultry_mortality = log(DT2TLU$Poultry_mortality/mean(DT2TLU$Poultry_mortality,na.rm=TRUE))

DT2TLU$Log_cattle_mort_rate = log(DT2TLU$Cattle_mort_rate/mean(DT2TLU$Cattle_mort_rate,na.rm=TRUE))
DT2TLU$Log_small_ruminants_mort_rate = log(DT2TLU$Small_ruminants_mort_rate/mean(DT2TLU$Small_ruminants_mort_rate,na.rm=TRUE))
DT2TLU$Log_poultry_mort_rate = log(DT2TLU$Poultry_mort_rate/mean(DT2TLU$Poultry_mort_rate,na.rm=TRUE))

DT2TLU$chirps_avg = log(DT2TLU$chirps_avg/mean(DT2TLU$chirps_avg,na.rm=TRUE))
DT2TLU$chirps_cv = log(DT2TLU$chirps_cv/mean(DT2TLU$chirps_cv,na.rm=TRUE))
DT2TLU$gyga_tseas = log(DT2TLU$gyga_tseas/mean(DT2TLU$gyga_tseas,na.rm=TRUE))
DT2TLU$gyga_gdd = log(DT2TLU$gyga_gdd/mean(DT2TLU$gyga_gdd,na.rm=TRUE))
DT2TLU$gyga_ai = log(DT2TLU$gyga_ai/mean(DT2TLU$gyga_ai,na.rm=TRUE))
DT2TLU$clay_silt = log(DT2TLU$clay_silt/mean(DT2TLU$clay_silt,na.rm=TRUE))
DT2TLU$alt = log(DT2TLU$alt/mean(DT2TLU$alt,na.rm=TRUE))

 
# Explaining (in)efficiencies
 
SFA_DT2TLU_1 = sfa(Log_TLUprod ~ 
                     # Log_cattle + Log_rum + Log_poultry + 
                     Log_TLU +
                     # Log_cattle_mort_rate + Log_small_ruminants_mort_rate + Log_poultry_mort_rate +
                     Log_cattle_mortality + Log_small_ruminants_mortality + Log_poultry_mortality +
                     # Log_TLU_mortality +
                     Log_Area + 
                     # alt +
                     chirps_avg + 
                     chirps_cv +
                     # gyga_tseas +
                     # gyga_ai + 
                     gyga_gdd + 
                     clay_silt + 
                     # Log_Grain + 
                     Log_Equip +
                     as.factor(Shelters) + as.factor(Routine_vaccination) + 
                     as.factor(Home_vaccination) + as.factor(Castration) +
                     as.factor(Deworming) + as.factor(Dipping) + as.factor(Home_spraying) + 
                     as.factor(Paravet) + as.factor(Home_feed),
                    data = DT2TLU)
summary(SFA_DT2TLU_1)
 
DT2TLU$efficiency_SFA_DT2TLU_1 = efficiencies(SFA_DT2TLU_1, asInData=TRUE)
# DT2TLU$efficiency_SFA_DT2TLU_1
 
DT2TLU$techeff_offtake_SFA_DT2TLU_1 = DT2TLU$TLU_offtake / DT2TLU$efficiency_SFA_DT2TLU_1
# DT2TLU$techeff_offtake_SFA_DT2TLU_1
 
# plot(DT2TLU$efficiency_SFA_DT2TLU_1, DT2TLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Efficiency')

mean(na.omit(DT2TLU$TLU_offtake))
mean(na.omit(DT2TLU$techeff_offtake_SFA_DT2TLU_1))
mean(na.omit(DT2TLU$efficiency_SFA_DT2TLU_1))


plot3 = ggplot(DT2TLU, aes(x = efficiency_SFA_DT2TLU_1, y = TLU_offtake)) +
   geom_point(size=3, shape=21, alpha = 0.6, fill = "#F98C0AFF") + 
  geom_hline(yintercept = mean(na.omit(DT2TLU$TLU_offtake)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DT2TLU$efficiency_SFA_DT2TLU_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("C. Livestock - Type 2") +
   xlab("Efficiency") + ylab("Livestock production (TLU)") +
   theme(plot.title = element_text(size=16, face = 'bold'),
         axis.title = element_text(size=14, face="bold"), 
         axis.text = element_text(size=10),
         legend.title = element_text(size=10, face="bold"),
         legend.text = element_text(size=8),
         legend.background = element_rect(size = 0.5, colour = "black"),
         legend.justification = c(0.1, 0.9), legend.position = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 8.7)
 
plot3
 
SFA_DT2TLU_1df = as.data.frame(coef(summary(SFA_DT2TLU_1)))
 
# write.csv(SFA_DT2TLU_1df, "TLU Type 2.csv")
 

# 4. LIVESTOCK OFFTAKE TYPE3----------------------------------------------------

DT3TLU = subset(DT3, TLU > 0)
 
DT3TLU = subset(DT3TLU, TLU_offtake > 0)


# Outliers

# boxplot(DT3TLU$TLU_offtake, ylab='Livestock offtake (TLU)')
DT3TLU = subset(DT3TLU, TLU_offtake < 15)
# boxplot(DT3TLU$TLU_offtake, ylab='Livestock offtake (TLU)')

# boxplot(DT3TLU$TLU, ylab='Livestock ownership (TLU)')
DT3TLU = subset(DT3TLU, TLU < 60)
# boxplot(DT3TLU$TLU, ylab='Livestock ownership (TLU)')

# boxplot(DT3TLU$Cropped_area, ylab='Cropped area (ha)')
DT3TLU = subset(DT3TLU, Cropped_area < 18)
# boxplot(DT3TLU$Cropped_area, ylab='Cropped area (ha)')

DT3TLU$TLU_mort_rate = DT3TLU$TLU_mortality / DT3TLU$TLU
# boxplot(DT3TLU$TLU_mort_rate, ylab='Livestock mortality rate')

# plot(DT3TLU$TLU, DT3TLU$TLU_offtake, ylab ='Livestock offtake (TLU)', xlab ='Livestock ownership (TLU)')
# plot(DT3TLU$Cropped_area, DT3TLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Cropped area (ha)')


# Data transformations

DT3TLU$Log_TLU = log(DT3TLU$TLU/mean(DT3TLU$TLU,na.rm=TRUE))
DT3TLU$Log_TLUprod = log(DT3TLU$TLU_offtake/mean(DT3TLU$TLU_offtake,na.rm=TRUE))
DT3TLU$Log_cattle = log(DT3TLU$Cattle/mean(DT3TLU$Cattle,na.rm=TRUE))
DT3TLU$Log_rum = log(DT3TLU$Small_ruminants/mean(DT3TLU$Small_ruminants,na.rm=TRUE))
DT3TLU$Log_poultry = log(DT3TLU$Poultry/mean(DT3TLU$Poultry,na.rm=TRUE))
DT3TLU$Log_Area = log(DT3TLU$Cropped_area/mean(DT3TLU$Cropped_area,na.rm=TRUE))
DT3TLU$Log_Age = log(DT3TLU$Age_HHH/mean(DT3TLU$Age_HHH,na.rm=TRUE))
DT3TLU$Log_Fam= log(DT3TLU$Family_size/mean(DT3TLU$Family_size,na.rm=TRUE))
DT3TLU$Log_Equip = log(DT3TLU$Equipment_value/mean(DT3TLU$Equipment_value,na.rm=TRUE))
DT3TLU$Log_Grain = log(DT3TLU$Grain/mean(DT3TLU$Grain,na.rm=TRUE))

DT3TLU$Log_TLU_mortality = log(DT3TLU$TLU_mortality/mean(DT3TLU$TLU_mortality,na.rm=TRUE))

# hist(DT3TLU$TLU_mort_rate)
DT3TLU$Log_TLU_mort_rate = log(DT3TLU$TLU_mort_rate/mean(DT3TLU$TLU_mort_rate,na.rm=TRUE))
# hist(DT3TLU$Log_TLU_mort_rate)

DT3TLU$Log_cattle_mortality = log(DT3TLU$Cattle_mortality/mean(DT3TLU$Cattle_mortality,na.rm=TRUE))
DT3TLU$Log_small_ruminants_mortality = log(DT3TLU$Small_ruminants_mortality/mean(DT3TLU$Small_ruminants_mortality,na.rm=TRUE))
DT3TLU$Log_poultry_mortality = log(DT3TLU$Poultry_mortality/mean(DT3TLU$Poultry_mortality,na.rm=TRUE))

DT3TLU$Log_cattle_mort_rate = log(DT3TLU$Cattle_mort_rate/mean(DT3TLU$Cattle_mort_rate,na.rm=TRUE))
DT3TLU$Log_small_ruminants_mort_rate = log(DT3TLU$Small_ruminants_mort_rate/mean(DT3TLU$Small_ruminants_mort_rate,na.rm=TRUE))
DT3TLU$Log_poultry_mort_rate = log(DT3TLU$Poultry_mort_rate/mean(DT3TLU$Poultry_mort_rate,na.rm=TRUE))

DT3TLU$chirps_avg = log(DT3TLU$chirps_avg/mean(DT3TLU$chirps_avg,na.rm=TRUE))
DT3TLU$chirps_cv = log(DT3TLU$chirps_cv/mean(DT3TLU$chirps_cv,na.rm=TRUE))
DT3TLU$gyga_tseas = log(DT3TLU$gyga_tseas/mean(DT3TLU$gyga_tseas,na.rm=TRUE))
DT3TLU$gyga_gdd = log(DT3TLU$gyga_gdd/mean(DT3TLU$gyga_gdd,na.rm=TRUE))
DT3TLU$gyga_ai = log(DT3TLU$gyga_ai/mean(DT3TLU$gyga_ai,na.rm=TRUE))
DT3TLU$clay_silt = log(DT3TLU$clay_silt/mean(DT3TLU$clay_silt,na.rm=TRUE))
DT3TLU$alt = log(DT3TLU$alt/mean(DT3TLU$alt,na.rm=TRUE))

 
# Explaining (in)efficiencies
 
SFA_DT3TLU_1 = sfa(Log_TLUprod ~ 
                     # Log_cattle + Log_rum + Log_poultry + 
                     Log_TLU +
                     # Log_cattle_mort_rate + Log_small_ruminants_mort_rate + Log_poultry_mort_rate +
                     Log_cattle_mortality + Log_small_ruminants_mortality + Log_poultry_mortality +
                     # Log_TLU_mortality +
                     Log_Area + 
                     # alt +
                     chirps_avg + 
                     chirps_cv +
                     # gyga_tseas +
                     # gyga_ai + 
                     gyga_gdd + 
                     clay_silt + 
                     # Log_Grain + 
                     Log_Equip +
                     as.factor(Shelters) + as.factor(Routine_vaccination) + 
                     as.factor(Home_vaccination) + as.factor(Castration) +
                     as.factor(Deworming) + as.factor(Dipping) + as.factor(Home_spraying) + 
                     as.factor(Paravet) + as.factor(Home_feed),
                    data = DT3TLU)
summary(SFA_DT3TLU_1)
 
DT3TLU$efficiency_SFA_DT3TLU_1 = efficiencies(SFA_DT3TLU_1, asInData=TRUE)
# DT3TLU$efficiency_SFA_DT3TLU_1
 
DT3TLU$techeff_offtake_SFA_DT3TLU_1 = DT3TLU$TLU_offtake / DT3TLU$efficiency_SFA_DT3TLU_1
# DT3TLU$techeff_offtake_SFA_DT3TLU_1
 
# plot(DT3TLU$efficiency_SFA_DT3TLU_1, DT3TLU$TLU_offtake, ylab = 'Livestock production (TLU)', xlab ='Efficiency')

mean(na.omit(DT3TLU$TLU_offtake))
mean(na.omit(DT3TLU$techeff_offtake_SFA_DT3TLU_1))
mean(na.omit(DT3TLU$efficiency_SFA_DT3TLU_1))

plot4 = ggplot(DT3TLU, aes(x = efficiency_SFA_DT3TLU_1, y = TLU_offtake)) +
   geom_point(size=3, shape=21, alpha = 0.6, fill = "#56106EFF") + 
  geom_hline(yintercept = mean(na.omit(DT3TLU$TLU_offtake)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DT3TLU$efficiency_SFA_DT3TLU_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("D. Livestock - Type 3") +
   xlab("Efficiency") + ylab("Livestock production (TLU)") +
   theme(plot.title = element_text(size=16, face = 'bold'),
         axis.title = element_text(size=14, face="bold"), 
         axis.text = element_text(size=10),
         legend.title = element_text(size=10, face="bold"),
         legend.text = element_text(size=8),
         legend.background = element_rect(size = 0.5, colour = "black"),
         legend.justification = c(0.1, 0.9), legend.position = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 8.7)
 
plot4
 
SFA_DT3TLU_1df = as.data.frame(coef(summary(SFA_DT3TLU_1)))
 
# write.csv(SFA_DT3TLU_1df, "TLU Type 3.csv")
 
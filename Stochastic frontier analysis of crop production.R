#' ---
#' title: "Stochastic frontier analysis of crop production"
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

DT = read.csv("Data sfa crop.csv")

DT$clay_silt = DT$sgrids_clay + DT$sgrids_silt

DT1 = DT[ which(DT$Type == "Type 1"), ]
DT2 = DT[ which(DT$Type == "Type 2"), ]
DT3 = DT[ which(DT$Type == "Type 3"), ]


# 1. GRAIN PRODUCTION TOTAL-----------------------------------------------------

DTCRP = subset(DT, Cropped_area > 0)
DTCRP = subset(DTCRP, Grain > 0)

DTCRP$TLU = ifelse(DTCRP$TLU == 0, 0.0001, DTCRP$TLU)
DTCRP$Age_HHH = ifelse(DTCRP$Age_HHH == 0, 0.0001, DTCRP$Age_HHH)
DTCRP$Family_size = ifelse(DTCRP$Family_size == 0, 0.0001, DTCRP$Family_size)
DTCRP$Equipment_value = ifelse(DTCRP$Equipment_value == 0, 0.0001, DTCRP$Equipment_value)
DTCRP$Fertilizers = ifelse(DTCRP$Fertilizers == 0, 0.0001, DTCRP$Fertilizers)
DTCRP$Organic_amendments = ifelse(DTCRP$Organic_amendments == 0, 0.0001, DTCRP$Organic_amendments)
DTCRP$maize_area = ifelse(DTCRP$maize_area == 0, 0.0001, DTCRP$maize_area)
DTCRP$sorghum_area = ifelse(DTCRP$sorghum_area == 0, 0.0001, DTCRP$sorghum_area)
DTCRP$pearlmillet_area = ifelse(DTCRP$pearlmillet_area == 0, 0.0001, DTCRP$pearlmillet_area)
DTCRP$fingermillet_area = ifelse(DTCRP$fingermillet_area == 0, 0.0001, DTCRP$fingermillet_area)

# Outliers

# boxplot(DTCRP$Grain, ylab='Cereal production (kg)')
DTCRP = subset(DTCRP, Grain < 8000)
# boxplot(DTCRP$Grain, ylab='Cereal production (kg)')

# boxplot(DTCRP$Cropped_area, ylab='Cropped area (ha)')
DTCRP = subset(DTCRP, Cropped_area < 18)
# boxplot(DTCRP$Cropped_area, ylab='Cropped area (ha)')

# boxplot(DTCRP$TLU, ylab='Livestock ownership (TLU)')
DTCRP = subset(DTCRP, TLU < 32)
# boxplot(DTCRP$TLU, ylab='Livestock ownership (TLU)')

# plot(DTCRP$Cropped_area, DTCRP$Grain, ylab = 'Cereal production (kg)', xlab ='Cropped area (ha)')
DTCRP$Grain = ifelse(DTCRP$Cropped_area < 2 & DTCRP$Grain > 4000, NA, DTCRP$Grain)
# plot(DTCRP$Cropped_area, DTCRP$Grain, ylab = 'Cereal production (kg)', xlab ='Cropped area (ha)')


# Data transformations

DTCRP$Log_Grain = log(DTCRP$Grain/mean(DTCRP$Grain,na.rm=TRUE))
DTCRP$Log_TLU = log(DTCRP$TLU/mean(DTCRP$TLU,na.rm=TRUE))
DTCRP$Log_Area = log(DTCRP$Cropped_area/mean(DTCRP$Cropped_area,na.rm=TRUE))
DTCRP$Log_Age = log(DTCRP$Age_HHH/mean(DTCRP$Age_HHH,na.rm=TRUE))
DTCRP$Log_Fam= log(DTCRP$Family_size/mean(DTCRP$Family_size,na.rm=TRUE))
DTCRP$Log_Equip = log(DTCRP$Equipment_value/mean(DTCRP$Equipment_value,na.rm=TRUE))
DTCRP$Log_Fert= log(DTCRP$Fertilizers/mean(DTCRP$Fertilizers,na.rm=TRUE))
DTCRP$Log_Org = log(DTCRP$Organic_amendments/mean(DTCRP$Organic_amendments,na.rm=TRUE))
DTCRP$Log_Maize = log(DTCRP$maize_area/mean(DTCRP$maize_area,na.rm=TRUE))
DTCRP$Log_Sorghum = log(DTCRP$sorghum_area/mean(DTCRP$sorghum_area,na.rm=TRUE))
DTCRP$Log_Pearl = log(DTCRP$pearlmillet_area/mean(DTCRP$pearlmillet_area,na.rm=TRUE))
DTCRP$Log_Finger = log(DTCRP$fingermillet_area/mean(DTCRP$fingermillet_area,na.rm=TRUE))

DTCRP$chirps_avg = log(DTCRP$chirps_avg/mean(DTCRP$chirps_avg,na.rm=TRUE))
DTCRP$chirps_cv = log(DTCRP$chirps_cv/mean(DTCRP$chirps_cv,na.rm=TRUE))
DTCRP$gyga_tseas = log(DTCRP$gyga_tseas/mean(DTCRP$gyga_tseas,na.rm=TRUE))
DTCRP$gyga_gdd = log(DTCRP$gyga_gdd/mean(DTCRP$gyga_gdd,na.rm=TRUE))
DTCRP$gyga_ai = log(DTCRP$gyga_ai/mean(DTCRP$gyga_ai,na.rm=TRUE))
DTCRP$clay_silt = log(DTCRP$clay_silt/mean(DTCRP$clay_silt,na.rm=TRUE))
DTCRP$alt = log(DTCRP$alt/mean(DTCRP$alt,na.rm=TRUE))


# Explaining (in)efficiencies

SFA_DTCRP_1 = sfa(Log_Grain ~ Log_Maize + Log_Sorghum + Log_Pearl + Log_Finger +
                    # alt +
                    chirps_avg + 
                    chirps_cv +
                    # gyga_tseas +
                    # gyga_ai + 
                    gyga_gdd + 
                    clay_silt + 
                    Log_Fert + Log_Org + Log_Equip + Log_TLU +
                    as.factor(Certified_seeds) + 
                    # as.factor(Seed_bank) + 
                    as.factor(DT_varieties) +
                    as.factor(Small_grains) + as.factor(Crop_rotation) + as.factor(Intercropping) +
                    as.factor(Cover_crops) + as.factor(Mulching) + as.factor(IPM) + 
                    as.factor(Compost_manure) + as.factor(Plant_density),
                  data = DTCRP)
summary(SFA_DTCRP_1)


DTCRP$efficiency_SFA_DTCRP_1 = efficiencies(SFA_DTCRP_1, asInData=TRUE)
# hist(DTCRP$efficiency_SFA_DTCRP_1)  

DTCRP$techeff_grain_SFA_DTCRP_1 = DTCRP$Grain / DTCRP$efficiency_SFA_DTCRP_1

mean(na.omit(DTCRP$Grain))
mean(na.omit(DTCRP$techeff_grain_SFA_DTCRP_1))
mean(na.omit(DTCRP$efficiency_SFA_DTCRP_1))

# plot(DTCRP$efficiency_SFA_DTCRP_1, DTCRP$Grain, ylab = 'Cereal production (kg)', xlab ='Efficiency')

plot1 = ggplot(DTCRP, aes(x = efficiency_SFA_DTCRP_1, y =  Grain, fill = District.x)) +
  geom_point(size=3, shape=21, alpha = 0.6) + 
  geom_hline(yintercept = mean(na.omit(DTCRP$Grain)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DTCRP$efficiency_SFA_DTCRP_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("A. Crop - All types") +
  xlab("Efficiency") + ylab("Cereal production (kg)") +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  theme(plot.title = element_text(size=16, face = 'bold'),
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=10),
        legend.title = element_blank(),
        legend.text = element_text(size = 8, face = "bold"),
        legend.background = element_rect(size = 0.5, colour = "black"),
        legend.position = c(0.1, 0.9), legend.justification = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 6500)
# +
#   scale_y_continuous(trans='log10')

plot1


SFA_DTCRP_1df = as.data.frame(coef(summary(SFA_DTCRP_1)))

# write.csv(SFA_DTCRP_1df, "Grain Total.csv")


# 2. GRAIN PRODUCTION TYPE1-----------------------------------------------------

DT1CRP = subset(DT1, Cropped_area > 0)

DT1CRP = subset(DT1CRP, Grain > 0)

DT1CRP$TLU = ifelse(DT1CRP$TLU == 0, 0.0001, DT1CRP$TLU)
DT1CRP$Age_HHH = ifelse(DT1CRP$Age_HHH == 0, 0.0001, DT1CRP$Age_HHH)
DT1CRP$Family_size = ifelse(DT1CRP$Family_size == 0, 0.0001, DT1CRP$Family_size)
DT1CRP$Equipment_value = ifelse(DT1CRP$Equipment_value == 0, 0.0001, DT1CRP$Equipment_value)
DT1CRP$Fertilizers = ifelse(DT1CRP$Fertilizers == 0, 0.0001, DT1CRP$Fertilizers)
DT1CRP$Organic_amendments = ifelse(DT1CRP$Organic_amendments == 0, 0.0001, DT1CRP$Organic_amendments)
DT1CRP$maize_area = ifelse(DT1CRP$maize_area == 0, 0.0001, DT1CRP$maize_area)
DT1CRP$sorghum_area = ifelse(DT1CRP$sorghum_area == 0, 0.0001, DT1CRP$sorghum_area)
DT1CRP$pearlmillet_area = ifelse(DT1CRP$pearlmillet_area == 0, 0.0001, DT1CRP$pearlmillet_area)
DT1CRP$fingermillet_area = ifelse(DT1CRP$fingermillet_area == 0, 0.0001, DT1CRP$fingermillet_area)


# Outliers

# boxplot(DT1CRP$Grain, ylab='Cereal production (kg)')

# boxplot(DT1CRP$Cropped_area, ylab='Cropped area (ha)')
DT1CRP = subset(DT1CRP, Cropped_area < 7)
# boxplot(DT1CRP$Cropped_area, ylab='Cropped area (ha)')

# boxplot(DT1CRP$TLU, ylab='Livestock ownership (TLU)')
DT1CRP = subset(DT1CRP, TLU < 20)
# boxplot(DT1CRP$TLU, ylab='Livestock ownership (TLU)')

# plot(DT1CRP$Cropped_area, DT1CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Cropped area (ha)')


# Data transformations

DT1CRP$Log_Grain = log(DT1CRP$Grain/mean(DT1CRP$Grain,na.rm=TRUE))
DT1CRP$Log_TLU = log(DT1CRP$TLU/mean(DT1CRP$TLU,na.rm=TRUE))
DT1CRP$Log_Area = log(DT1CRP$Cropped_area/mean(DT1CRP$Cropped_area,na.rm=TRUE))
DT1CRP$Log_Age = log(DT1CRP$Age_HHH/mean(DT1CRP$Age_HHH,na.rm=TRUE))
DT1CRP$Log_Fam= log(DT1CRP$Family_size/mean(DT1CRP$Family_size,na.rm=TRUE))
DT1CRP$Log_Equip = log(DT1CRP$Equipment_value/mean(DT1CRP$Equipment_value,na.rm=TRUE))
DT1CRP$Log_Fert= log(DT1CRP$Fertilizers/mean(DT1CRP$Fertilizers,na.rm=TRUE))
DT1CRP$Log_Org = log(DT1CRP$Organic_amendments/mean(DT1CRP$Organic_amendments,na.rm=TRUE))
DT1CRP$Log_Maize = log(DT1CRP$maize_area/mean(DT1CRP$maize_area,na.rm=TRUE))
DT1CRP$Log_Sorghum = log(DT1CRP$sorghum_area/mean(DT1CRP$sorghum_area,na.rm=TRUE))
DT1CRP$Log_Pearl = log(DT1CRP$pearlmillet_area/mean(DT1CRP$pearlmillet_area,na.rm=TRUE))
DT1CRP$Log_Finger = log(DT1CRP$fingermillet_area/mean(DT1CRP$fingermillet_area,na.rm=TRUE))

DT1CRP$chirps_avg = log(DT1CRP$chirps_avg/mean(DT1CRP$chirps_avg,na.rm=TRUE))
DT1CRP$chirps_cv = log(DT1CRP$chirps_cv/mean(DT1CRP$chirps_cv,na.rm=TRUE))
DT1CRP$gyga_tseas = log(DT1CRP$gyga_tseas/mean(DT1CRP$gyga_tseas,na.rm=TRUE))
DT1CRP$gyga_gdd = log(DT1CRP$gyga_gdd/mean(DT1CRP$gyga_gdd,na.rm=TRUE))
DT1CRP$gyga_ai = log(DT1CRP$gyga_ai/mean(DT1CRP$gyga_ai,na.rm=TRUE))
DT1CRP$clay_silt = log(DT1CRP$clay_silt/mean(DT1CRP$clay_silt,na.rm=TRUE))
DT1CRP$alt = log(DT1CRP$alt/mean(DT1CRP$alt,na.rm=TRUE))


# Explaining (in)efficiencies

SFA_DT1CRP_1 = sfa(Log_Grain ~ Log_Maize + Log_Sorghum + Log_Pearl + Log_Finger +
                    # alt +
                    chirps_avg + 
                    chirps_cv +
                    # gyga_tseas +
                    # gyga_ai + 
                    gyga_gdd + 
                    clay_silt + 
                    Log_Fert + Log_Org + Log_Equip + Log_TLU +
                    as.factor(Certified_seeds) + 
                     # as.factor(Seed_bank) + 
                     as.factor(DT_varieties) +
                    as.factor(Small_grains) + as.factor(Crop_rotation) + as.factor(Intercropping) +
                    as.factor(Cover_crops) + as.factor(Mulching) + as.factor(IPM) + 
                    as.factor(Compost_manure) + as.factor(Plant_density),
                  data = DT1CRP)
summary(SFA_DT1CRP_1)


DT1CRP$efficiency_SFA_DT1CRP_1 = efficiencies(SFA_DT1CRP_1, asInData=TRUE)
# hist(DT1CRP$efficiency_SFA_DT1CRP_1)  

DT1CRP$techeff_grain_SFA_DT1CRP_1 = DT1CRP$Grain / DT1CRP$efficiency_SFA_DT1CRP_1
# DT1CRP$techeff_grain_SFA_DT1CRP_1

DT1CRP$techeff_grain_SFA_DT1CRP_1


mean(na.omit(DT1CRP$Grain))
mean(na.omit(DT1CRP$techeff_grain_SFA_DT1CRP_1))
mean(na.omit(DT1CRP$efficiency_SFA_DT1CRP_1))

# plot(DT1CRP$efficiency_SFA_DT1CRP_1, DT1CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Efficiency')

plot2 = ggplot(DT1CRP, aes(x = efficiency_SFA_DT1CRP_1, y = Grain)) +
  geom_point(size=3, shape=21, alpha = 0.6, fill = "#BB3754FF") + 
  geom_hline(yintercept = mean(na.omit(DT1CRP$Grain)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DT1CRP$efficiency_SFA_DT1CRP_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("B. Crop - Type 1") +
  xlab("Efficiency") + ylab("Cereal production (kg)") +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  theme(plot.title = element_text(size=16, face = 'bold'),
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=10),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size=8),
        legend.background = element_rect(size = 0.5, colour = "black"),
        legend.position = c(0.1, 0.9), legend.justification = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 6500)

plot2


SFA_DT1CRP_1df = as.data.frame(coef(summary(SFA_DT1CRP_1)))

# write.csv(SFA_DT1CRP_1df, "Grain Type 1.csv")


# 3. GRAIN PRODUCTION TYPE2-----------------------------------------------------

DT2CRP = subset(DT2, Cropped_area > 0)

DT2CRP = subset(DT2CRP, Grain > 0)

DT2CRP$TLU = ifelse(DT2CRP$TLU == 0, 0.0001, DT2CRP$TLU)
DT2CRP$Age_HHH = ifelse(DT2CRP$Age_HHH == 0, 0.0001, DT2CRP$Age_HHH)
DT2CRP$Family_size = ifelse(DT2CRP$Family_size == 0, 0.0001, DT2CRP$Family_size)
DT2CRP$Equipment_value = ifelse(DT2CRP$Equipment_value == 0, 0.0001, DT2CRP$Equipment_value)
DT2CRP$Fertilizers = ifelse(DT2CRP$Fertilizers == 0, 0.0001, DT2CRP$Fertilizers)
DT2CRP$Organic_amendments = ifelse(DT2CRP$Organic_amendments == 0, 0.0001, DT2CRP$Organic_amendments)
DT2CRP$maize_area = ifelse(DT2CRP$maize_area == 0, 0.0001, DT2CRP$maize_area)
DT2CRP$sorghum_area = ifelse(DT2CRP$sorghum_area == 0, 0.0001, DT2CRP$sorghum_area)
DT2CRP$pearlmillet_area = ifelse(DT2CRP$pearlmillet_area == 0, 0.0001, DT2CRP$pearlmillet_area)
DT2CRP$fingermillet_area = ifelse(DT2CRP$fingermillet_area == 0, 0.0001, DT2CRP$fingermillet_area)


# Outliers

# boxplot(DT2CRP$Grain, ylab='Cereal production (kg)')
DT2CRP = subset(DT2CRP, Grain < 7000)
# boxplot(DT2CRP$Grain, ylab='Cereal production (kg)')

# boxplot(DT2CRP$Cropped_area, ylab='Cropped area (ha)')

# boxplot(DT2CRP$TLU, ylab='Livestock ownership (TLU)')
DT2CRP = subset(DT2CRP, TLU < 30)
# boxplot(DT2CRP$TLU, ylab='Livestock ownership (TLU)')

# plot(DT2CRP$Cropped_area, DT2CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Cropped area (ha)')


# Data transformations

DT2CRP$Log_Grain = log(DT2CRP$Grain/mean(DT2CRP$Grain,na.rm=TRUE))
DT2CRP$Log_TLU = log(DT2CRP$TLU/mean(DT2CRP$TLU,na.rm=TRUE))
DT2CRP$Log_Area = log(DT2CRP$Cropped_area/mean(DT2CRP$Cropped_area,na.rm=TRUE))
DT2CRP$Log_Age = log(DT2CRP$Age_HHH/mean(DT2CRP$Age_HHH,na.rm=TRUE))
DT2CRP$Log_Fam= log(DT2CRP$Family_size/mean(DT2CRP$Family_size,na.rm=TRUE))
DT2CRP$Log_Equip = log(DT2CRP$Equipment_value/mean(DT2CRP$Equipment_value,na.rm=TRUE))
DT2CRP$Log_Fert= log(DT2CRP$Fertilizers/mean(DT2CRP$Fertilizers,na.rm=TRUE))
DT2CRP$Log_Org = log(DT2CRP$Organic_amendments/mean(DT2CRP$Organic_amendments,na.rm=TRUE))
DT2CRP$Log_Maize = log(DT2CRP$maize_area/mean(DT2CRP$maize_area,na.rm=TRUE))
DT2CRP$Log_Sorghum = log(DT2CRP$sorghum_area/mean(DT2CRP$sorghum_area,na.rm=TRUE))
DT2CRP$Log_Pearl = log(DT2CRP$pearlmillet_area/mean(DT2CRP$pearlmillet_area,na.rm=TRUE))
DT2CRP$Log_Finger = log(DT2CRP$fingermillet_area/mean(DT2CRP$fingermillet_area,na.rm=TRUE))

DT2CRP$chirps_avg = log(DT2CRP$chirps_avg/mean(DT2CRP$chirps_avg,na.rm=TRUE))
DT2CRP$chirps_cv = log(DT2CRP$chirps_cv/mean(DT2CRP$chirps_cv,na.rm=TRUE))
DT2CRP$gyga_tseas = log(DT2CRP$gyga_tseas/mean(DT2CRP$gyga_tseas,na.rm=TRUE))
DT2CRP$gyga_gdd = log(DT2CRP$gyga_gdd/mean(DT2CRP$gyga_gdd,na.rm=TRUE))
DT2CRP$gyga_ai = log(DT2CRP$gyga_ai/mean(DT2CRP$gyga_ai,na.rm=TRUE))
DT2CRP$clay_silt = log(DT2CRP$clay_silt/mean(DT2CRP$clay_silt,na.rm=TRUE))
DT2CRP$alt = log(DT2CRP$alt/mean(DT2CRP$alt,na.rm=TRUE))


# Explaining (in)efficiencies

SFA_DT2CRP_1 = sfa(Log_Grain ~ Log_Maize + Log_Sorghum + Log_Pearl + Log_Finger +
                     # alt +
                     chirps_avg + 
                     chirps_cv +
                     # gyga_tseas +
                     # gyga_ai + 
                     gyga_gdd + 
                     clay_silt + 
                     Log_Fert + Log_Org + Log_Equip + Log_TLU +
                     as.factor(Certified_seeds) + 
                     # as.factor(Seed_bank) + 
                     as.factor(DT_varieties) +
                     as.factor(Small_grains) + as.factor(Crop_rotation) + as.factor(Intercropping) +
                     as.factor(Cover_crops) + as.factor(Mulching) + as.factor(IPM) + 
                     as.factor(Compost_manure) + as.factor(Plant_density),
                   data = DT2CRP)
summary(SFA_DT2CRP_1)



DT2CRP$efficiency_SFA_DT2CRP_1 = efficiencies(SFA_DT2CRP_1, asInData=TRUE)
# hist(DT2CRP$efficiency_SFA_DT2CRP_1)  

DT2CRP$techeff_grain_SFA_DT2CRP_1 = DT2CRP$Grain / DT2CRP$efficiency_SFA_DT2CRP_1
# DT2CRP$techeff_grain_SFA_DT2CRP_1

# plot(DT2CRP$efficiency_SFA_DT2CRP_1, DT2CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Efficiency')

mean(na.omit(DT2CRP$Grain))
mean(na.omit(DT2CRP$techeff_grain_SFA_DT2CRP_1))
mean(na.omit(DT2CRP$efficiency_SFA_DT2CRP_1))

plot3 = ggplot(DT2CRP, aes(x = efficiency_SFA_DT2CRP_1, y = Grain)) +
  geom_point(size=3, shape=21, alpha = 0.6, fill = "#F98C0AFF") + 
  geom_hline(yintercept = mean(na.omit(DT2CRP$Grain)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DT2CRP$efficiency_SFA_DT2CRP_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("C. Crop - Type 2") +
  xlab("Efficiency") + ylab("Cereal production (kg)") +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  theme(plot.title = element_text(size=16, face = 'bold'),
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=10),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size=8),
        legend.background = element_rect(size = 0.5, colour = "black"),
        legend.position = c(0.1, 0.9), legend.justification = c(0.1, 0.9)) +
  xlim(0, 0.83) + ylim(0, 6500)

plot3


SFA_DT2CRP_1df = as.data.frame(coef(summary(SFA_DT2CRP_1)))

# write.csv(SFA_DT2CRP_1df, "Grain Type 2.csv")


# 4. GRAIN PRODUCTION TYPE3-----------------------------------------------------

DT3CRP = subset(DT3, Cropped_area > 0)

DT3CRP = subset(DT3CRP, Grain > 0)

DT3CRP$TLU = ifelse(DT3CRP$TLU == 0, 0.0001, DT3CRP$TLU)
DT3CRP$Age_HHH = ifelse(DT3CRP$Age_HHH == 0, 0.0001, DT3CRP$Age_HHH)
DT3CRP$Family_size = ifelse(DT3CRP$Family_size == 0, 0.0001, DT3CRP$Family_size)
DT3CRP$Equipment_value = ifelse(DT3CRP$Equipment_value == 0, 0.0001, DT3CRP$Equipment_value)
DT3CRP$Fertilizers = ifelse(DT3CRP$Fertilizers == 0, 0.0001, DT3CRP$Fertilizers)
DT3CRP$Organic_amendments = ifelse(DT3CRP$Organic_amendments == 0, 0.0001, DT3CRP$Organic_amendments)
DT3CRP$maize_area = ifelse(DT3CRP$maize_area == 0, 0.0001, DT3CRP$maize_area)
DT3CRP$sorghum_area = ifelse(DT3CRP$sorghum_area == 0, 0.0001, DT3CRP$sorghum_area)
DT3CRP$pearlmillet_area = ifelse(DT3CRP$pearlmillet_area == 0, 0.0001, DT3CRP$pearlmillet_area)
DT3CRP$fingermillet_area = ifelse(DT3CRP$fingermillet_area == 0, 0.0001, DT3CRP$fingermillet_area)


# Outliers

# boxplot(DT3CRP$Grain, ylab = 'Cereal production (kg)')
DT3CRP = subset(DT3CRP, Grain < 8000)
# boxplot(DT3CRP$Grain, ylab='Cereal production (kg)')

# boxplot(DT3CRP$Cropped_area, ylab='Cropped area (ha)')
DT3CRP = subset(DT3CRP, Cropped_area < 18)
# boxplot(DT3CRP$Cropped_area, ylab='Cropped area (ha)')

# boxplot(DT3CRP$TLU, ylab='Livestock ownership (TLU)')
DT3CRP = subset(DT3CRP, TLU < 32)
# boxplot(DT3CRP$TLU, ylab='Livestock ownership (TLU)')

# plot(DT3CRP$Cropped_area, DT3CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Cropped area (ha)')
DT3CRP$Grain = ifelse(DT3CRP$Cropped_area < 2 & DT3CRP$Grain > 4000, NA, DT3CRP$Grain)
# plot(DT3CRP$Cropped_area, DT3CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Cropped area (ha)')


# Data transformations

DT3CRP$Log_Grain = log(DT3CRP$Grain/mean(DT3CRP$Grain,na.rm=TRUE))
DT3CRP$Log_TLU = log(DT3CRP$TLU/mean(DT3CRP$TLU,na.rm=TRUE))
DT3CRP$Log_Area = log(DT3CRP$Cropped_area/mean(DT3CRP$Cropped_area,na.rm=TRUE))
DT3CRP$Log_Age = log(DT3CRP$Age_HHH/mean(DT3CRP$Age_HHH,na.rm=TRUE))
DT3CRP$Log_Fam= log(DT3CRP$Family_size/mean(DT3CRP$Family_size,na.rm=TRUE))
DT3CRP$Log_Equip = log(DT3CRP$Equipment_value/mean(DT3CRP$Equipment_value,na.rm=TRUE))
DT3CRP$Log_Fert= log(DT3CRP$Fertilizers/mean(DT3CRP$Fertilizers,na.rm=TRUE))
DT3CRP$Log_Org = log(DT3CRP$Organic_amendments/mean(DT3CRP$Organic_amendments,na.rm=TRUE))
DT3CRP$Log_Maize = log(DT3CRP$maize_area/mean(DT3CRP$maize_area,na.rm=TRUE))
DT3CRP$Log_Sorghum = log(DT3CRP$sorghum_area/mean(DT3CRP$sorghum_area,na.rm=TRUE))
DT3CRP$Log_Pearl = log(DT3CRP$pearlmillet_area/mean(DT3CRP$pearlmillet_area,na.rm=TRUE))
DT3CRP$Log_Finger = log(DT3CRP$fingermillet_area/mean(DT3CRP$fingermillet_area,na.rm=TRUE))

DT3CRP$chirps_avg = log(DT3CRP$chirps_avg/mean(DT3CRP$chirps_avg,na.rm=TRUE))
DT3CRP$chirps_cv = log(DT3CRP$chirps_cv/mean(DT3CRP$chirps_cv,na.rm=TRUE))
DT3CRP$gyga_tseas = log(DT3CRP$gyga_tseas/mean(DT3CRP$gyga_tseas,na.rm=TRUE))
DT3CRP$gyga_gdd = log(DT3CRP$gyga_gdd/mean(DT3CRP$gyga_gdd,na.rm=TRUE))
DT3CRP$gyga_ai = log(DT3CRP$gyga_ai/mean(DT3CRP$gyga_ai,na.rm=TRUE))
DT3CRP$clay_silt = log(DT3CRP$clay_silt/mean(DT3CRP$clay_silt,na.rm=TRUE))
DT3CRP$alt = log(DT3CRP$alt/mean(DT3CRP$alt,na.rm=TRUE))


# Explaining (in)efficiencies

SFA_DT3CRP_1 = sfa(Log_Grain ~ Log_Maize + Log_Sorghum + Log_Pearl + Log_Finger +
                     # alt +
                     chirps_avg + 
                     chirps_cv +
                     # gyga_tseas +
                     # gyga_ai + 
                     gyga_gdd + 
                     clay_silt + 
                     Log_Fert + Log_Org + Log_Equip + Log_TLU +
                     as.factor(Certified_seeds) + 
                     # as.factor(Seed_bank) + 
                     as.factor(DT_varieties) +
                     as.factor(Small_grains) + as.factor(Crop_rotation) + as.factor(Intercropping) +
                     as.factor(Cover_crops) + as.factor(Mulching) + as.factor(IPM) + 
                     as.factor(Compost_manure) + as.factor(Plant_density),
                   data = DT3CRP)
summary(SFA_DT3CRP_1)


DT3CRP$efficiency_SFA_DT3CRP_1 = efficiencies(SFA_DT3CRP_1, asInData=TRUE)

DT3CRP$techeff_grain_SFA_DT3CRP_1 = DT3CRP$Grain / DT3CRP$efficiency_SFA_DT3CRP_1


# plot(DT3CRP$efficiency_SFA_DT3CRP_1, DT3CRP$Grain, ylab = 'Cereal production (kg)', xlab ='Efficiency')

mean(na.omit(DT3CRP$Grain))
mean(na.omit(DT3CRP$techeff_grain_SFA_DT3CRP_1))
mean(na.omit(DT3CRP$efficiency_SFA_DT3CRP_1))


plot4 = ggplot(DT3CRP, aes(x = efficiency_SFA_DT3CRP_1, y = Grain)) +
  geom_point(size=3, shape=21, alpha = 0.6, fill = "#56106EFF") + 
  geom_hline(yintercept = mean(na.omit(DT3CRP$Grain)), color = "grey20", linetype = 2, size = 0.8) +
  geom_vline(xintercept = mean(na.omit(DT3CRP$efficiency_SFA_DT3CRP_1)), color = "grey20", linetype = 2, size = 0.8) +
  theme_few() + ggtitle("D. Crop - Type 3") +
  xlab("Efficiency") + ylab("Cereal production (kg)") +
  theme(plot.title = element_text(size=16, face = 'bold'),
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=10),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size=8),
        legend.background = element_rect(size = 0.5, colour = "black"),
        legend.position = c(0.1, 0.9), legend.justification = c(0.1, 0.9))

plot4


SFA_DT3CRP_1df = as.data.frame(coef(summary(SFA_DT3CRP_1)))

# write.csv(SFA_DT3CRP_1df, "Grain Type 3.csv")

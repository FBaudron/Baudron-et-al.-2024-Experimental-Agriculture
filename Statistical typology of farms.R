#' ---
#' title: "Statistical typology of farms"
#' author: "Frédéric Baudron"
#' date: "August 26th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list=ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

suppressPackageStartupMessages({
  library(vegan)
  library(ade4)
  library(dendextend)
  library(ggplot2)
  library(ggthemes)
  library(plyr)
  library(grid)
  library(gridExtra)
  library(egg)
  library(tidyr)
  library(dplyr)
  library(tidyverse)
  library(gtsummary)
  library(cowplot)
  library(ggdendro)
  library(scales)
  library(randomForest)
})


# SET WORKING DIRECTORY---------------------------------------------------------

setwd('D:\\Mes Donnees\\0. Publi\\3. Crop-livestock\\0. Revision Experimental Ag\\Data & code\\')


# LOADING AND MANIPULATING THE DATA---------------------------------------------

TP = read.csv("Data raw.csv")

TP = TP[, -c(1:3, 7:10, 13:14, 17, 74:78, 131:180, 201, 210, 220:252, 254:256, 258:274, 280:298)]

TP1 = TP

TP1$What.is.the.sex.of.the.head.of.the.household. = as.numeric(TP1$What.is.the.sex.of.the.head.of.the.household. != "Male")
# table(TP1$What.is.the.sex.of.the.head.of.the.household.)

TP1$What.is.the.education.level.of.the.head.of.the.household. = as.numeric(TP1$What.is.the.education.level.of.the.head.of.the.household. != "Primary level")
# table(TP1$What.is.the.education.level.of.the.head.of.the.household.)

TP1$Family_size = TP1$Total.number.of.adult.males..of.age.18.and.above. + TP1$Total.number.of.adult.females..of.age.18.and.above. +
  TP1$Total.number.of.teens.of.age.12.to.17 + TP1$Total.number.of.children.of.age.3.to.11 + TP1$Total.number.of.infant.of.age.0.to.2

TP1$Are.there.relatives.outside.the.household.who.help.financially. = as.numeric(TP1$Are.there.relatives.outside.the.household.who.help.financially. != "No")
# table(TP1$Are.there.relatives.outside.the.household.who.help.financially.)

TP1$Are.there.relatives.outside.the.household.who.depend.on.it.financially..e.g...elderly..sick.. = as.numeric(TP1$Are.there.relatives.outside.the.household.who.depend.on.it.financially..e.g...elderly..sick.. != "No")
# table(TP1$Are.there.relatives.outside.the.household.who.depend.on.it.financially..e.g...elderly..sick..)

TP1$Did.your.household.hire.labour.in.during.the.past.12.months. = as.numeric(TP1$Did.your.household.hire.labour.in.during.the.past.12.months. != "No")
# table(TP1$Did.your.household.hire.labour.in.during.the.past.12.months.)

TP1$Did.your.household.sell.labour.out.during.the.past.12.months. = as.numeric(TP1$Did.your.household.sell.labour.out.during.the.past.12.months. != "No")
# table(TP1$Did.your.household.sell.labour.out.during.the.past.12.months.)

TP1$Garden = ifelse(TP1$Garden.area..ha. > 0, 1, 0)
# table(TP1$Garden)

TP1$Area_cereal = TP1$Area.cropped.in.maize.this.season.2020.21..ha.. + TP1$Area.cropped.in.sorghum.this.season.2020.21..ha.. +
  TP1$Area.cropped.in.pearl.millet.this.season.2020.21..ha.. + TP1$Area.cropped.in.finger.millet.this.season.2020.21..ha..

TP1$Area_other_crop = TP1$Area.cropped.in.sugar.bean.this.season.2020.21..ha.. + TP1$Area.cropped.in.groundnut.this.season.2020.21..ha.. +
  TP1$Area.cropped.in.cowpea.this.season.2020.21..ha.. + TP1$Area.cropped.in.sesame.this.season.2020.21..ha.. + TP1$Area.cropped.in.cotton.this.season.2020.21..ha.. +
  TP1$Area.cropped.in.tobacco.this.season.2020.21..ha.. + TP1$Area.cropped.in.forage.this.season.2020.21..ha.. + TP1$Area.cropped.in.other.crops.this.season.2020.21..ha..

TP1$Cropped_area = TP1$Area_cereal + TP1$Area_other_crop

TP1$Proportion_other_crop = TP1$Area_other_crop / TP1$Cropped_area 

TP1$Total.production.of.maize.during.the.last.2019.20.season..kg..[is.na(TP1$Total.production.of.maize.during.the.last.2019.20.season..kg..)] = 0
TP1$Total.production.of.sorghum.during.the.last.2019.20.season..kg..[is.na(TP1$Total.production.of.sorghum.during.the.last.2019.20.season..kg..)] = 0
TP1$Total.production.of.pearl.millet.during.the.last.2019.20.season..kg..[is.na(TP1$Total.production.of.pearl.millet.during.the.last.2019.20.season..kg..)] = 0
TP1$Total.production.of.finger.millet.during.the.last.2019.20.season..kg..[is.na(TP1$Total.production.of.finger.millet.during.the.last.2019.20.season..kg..)] = 0

TP1$Grain = TP1$Total.production.of.maize.during.the.last.2019.20.season..kg.. + TP1$Total.production.of.sorghum.during.the.last.2019.20.season..kg.. +
  TP1$Total.production.of.pearl.millet.during.the.last.2019.20.season..kg.. + TP1$Total.production.of.finger.millet.during.the.last.2019.20.season..kg..

TP1$Yield_cereal = TP1$Grain/(TP1$Area.cropped.in.maize.during.the.last.2019.20.season..ha.. + TP1$Area.cropped.in.sorghum.during.the.last..2019.20.season..ha.. +
  TP1$Total.production.of.pearl.millet.during.the.last.2019.20.season..kg.. + TP1$Total.production.of.finger.millet.during.the.last.2019.20.season..kg..)

TP1$Cattle = TP1$Number.of.improved.adult....3yr..cattle. + TP1$Number.of.indigenous.adult....3yr..cattle. +
  TP1$Number.of.improved.juvenile....3yr..cattle. + TP1$Number.of.indigenous.juvenile....3yr..cattle.

TP1$SMall_ruminants = TP1$Number.of.improved.goats. + TP1$Number.of.indigenous.goats. +
  TP1$Number.of.sheep.

TP1$Poultry = TP1$Number.of.chicken. + TP1$Number.of.turkey. + TP1$Number.of.guinea.fowls.

TP1$TLU = 0.7 * TP1$Cattle + 0.1 * TP1$SMall_ruminants + 0.01 * TP1$Poultry + 0.2 * TP1$Number.of.pigs. + 0.5 * TP1$Number.of.donkeys.

TP1$TLU_sold = 0.7 * (TP1$Number.of.improved.adult....3yr..cattle.sold.in.the.past.12.months. + TP1$Number.of.indigenous.adult....3yr..cattle.sold.in.the.past.12.months. +
  TP1$Number.of.improved.juvenile....3yr..cattle.sold.in.the.past.12.months. + TP1$Number.of.indigenous.juvenile....3yr..cattle.sold.in.the.past.12.months.) +
  0.1 * (TP1$Number.of.improved.goats.sold.in.the.past.12.months. + TP1$Number.of.indigenous.goats.sold.in.the.past.12.months. +
  TP1$Number.of.sheep.sold.in.the.past.12.months.) + 0.5 * TP1$Number.of.donkeys.sold.in.the.past.12.months. +
  0.2 * (TP1$Number.of.pigs.sold.in.the.past.12.months.) + 0.01 * (TP1$Number.of.chicken.sold.in.the.past.12.months. +
  TP1$Number.of.turkey.sold.in.the.past.12.months. + TP1$Number.of.guinea.fowls.sold.in.the.past.12.months.)

TP1$TLU_slaughtered = 0.7 * (TP1$Number.of.improved.adult....3yr..cattle.slaughtered.in.the.past.12.months. + TP1$Number.of.indigenous.adult....3yr..cattle.slaughtered.in.the.past.12.months. +
  TP1$Number.of.improved.juvenile....3yr..cattle.slaughtered.in.the.past.12.months. + TP1$Number.of.indigenous.juvenile....3yr..cattle.slaughtered.in.the.past.12.months.) +
  0.1 * (TP1$Number.of.improved.goats.slaughtered.in.the.past.12.months. + TP1$Number.of.indigenous.goats.slaughtered.in.the.past.12.months. +
  TP1$Number.of.sheep.slaughtered.in.the.past.12.months.) + 0.2 * (TP1$Number.of.pigs.slaughtered.in.the.past.12.months.) + 0.01 * (TP1$Number.of.chicken.slaughtered.in.the.past.12.months. +
  TP1$Number.of.turkey.slaughtered.in.the.past.12.months. + TP1$Number.of.guinea.fowls.slaughtered.in.the.past.12.months.)

TP1$TLU_mortality = 0.7 * (TP1$Number.of.improved.adult....3yr..cattle.dying.in.the.past.12.months. + TP1$Number.of.indigenous.adult....3yr..cattle.dying.in.the.past.12.months. +
  TP1$Number.of.improved.juvenile....3yr..cattle.dying.in.the.past.12.months. + TP1$Number.of.indigenous.juvenile....3yr..cattle.dying.in.the.past.12.months.) +
  0.1 * (TP1$Number.of.improved.goats.dying.in.the.past.12.months. + TP1$Number.of.indigenous.goats.dying.in.the.past.12.months. +
  TP1$Number.of.sheep.dying.in.the.past.12.months.) + 0.2 * (TP1$Number.of.pigs.dying.in.the.past.12.months.) + 0.01 * (TP1$Number.of.chicken.dying.in.the.past.12.months. +
  TP1$Number.of.turkey.dying.in.the.past.12.months. + TP1$Number.of.guinea.fowls.dying.in.the.past.12.months.) + 0.5 * TP1$Number.of.donkeys.dying.in.the.past.12.months.


TP1$Quality.certified.seeds..i.e...trusted.source..guaranted.minimum.germination.rate. = as.numeric(TP1$Quality.certified.seeds..i.e...trusted.source..guaranted.minimum.germination.rate. != "No")
# table(TP1$Quality.certified.seeds..i.e...trusted.source..guaranted.minimum.germination.rate.)

TP1$Community.seed.banks = as.numeric(TP1$Community.seed.banks != "No")
# table(TP1$Community.seed.banks)

TP1$Drought.tolerant.varieties = as.numeric(TP1$Drought.tolerant.varieties != "No")
# table(TP1$Drought.tolerant.varieties)

TP1$Small.grains = as.numeric(TP1$Small.grains != "No")
# table(TP1$Small.grains)

TP1$Crop.rotation = as.numeric(TP1$Crop.rotation != "No")
# table(TP1$Crop.rotation)

TP1$Intercropping = as.numeric(TP1$Intercropping != "No")
# table(TP1$Intercropping)

TP1$Cover.crops..i.e...crops.planted.specifically.to.control.erosion.and.or.increase.soil.fertility. = as.numeric(TP1$Cover.crops..i.e...crops.planted.specifically.to.control.erosion.and.or.increase.soil.fertility. != "No")
# table(TP1$Cover.crops..i.e...crops.planted.specifically.to.control.erosion.and.or.increase.soil.fertility.)

TP1$Mulching = as.numeric(TP1$Mulching != "No")
# table(TP1$Mulching)

TP1$Integrated.pest.management..i.e...scouting.and.use.of.several.control.methods.in.addition.to.pesticides. = as.numeric(TP1$Integrated.pest.management..i.e...scouting.and.use.of.several.control.methods.in.addition.to.pesticides. != "No")
# table(TP1$Integrated.pest.management..i.e...scouting.and.use.of.several.control.methods.in.addition.to.pesticides.)

TP1$Compost.Manure = as.numeric(TP1$Compost.Manure != "No")
# table(TP1$Compost.Manure)

TP1$Drip.Micro.irrigation = as.numeric(TP1$Drip.Micro.irrigation != "No")
# table(TP1$Drip.Micro.irrigation)

TP1$Optimum.plant.density..e.g...gap.filling..planting.at.the.right.density..including.through.the.use.of.mechanical.planters. = as.numeric(TP1$Optimum.plant.density..e.g...gap.filling..planting.at.the.right.density..including.through.the.use.of.mechanical.planters. != "No")
# table(TP1$Optimum.plant.density..e.g...gap.filling..planting.at.the.right.density..including.through.the.use.of.mechanical.planters.)

TP1$Improved.livestock.breeds = as.numeric(TP1$Improved.livestock.breeds != "No")
# table(TP1$Improved.livestock.breeds)

TP1$Improved.animal.shelters..for.goats..poultry.or.cattle..enough.space..good.ventilation..protecting.from.the.sun..dry.floor. = as.numeric(TP1$Improved.animal.shelters..for.goats..poultry.or.cattle..enough.space..good.ventilation..protecting.from.the.sun..dry.floor. != "No")
# table(TP1$Improved.animal.shelters..for.goats..poultry.or.cattle..enough.space..good.ventilation..protecting.from.the.sun..dry.floor.)

TP1$Water.infrastructure.for.livestock.at.homestead...e.g..water.trough. = as.numeric(TP1$Water.infrastructure.for.livestock.at.homestead...e.g..water.trough. != "No")
# table(TP1$Water.infrastructure.for.livestock.at.homestead...e.g..water.trough.)

TP1$Routine.vaccinations.by.Veterinary.Officer.or.Paravet = as.numeric(TP1$Routine.vaccinations.by.Veterinary.Officer.or.Paravet != "No")
# table(TP1$Routine.vaccinations.by.Veterinary.Officer.or.Paravet)

TP1$Home.vaccinations..farmer.administered.vaccinations. = as.numeric(TP1$Home.vaccinations..farmer.administered.vaccinations. != "No")
# table(TP1$Home.vaccinations..farmer.administered.vaccinations.)

TP1$Castration = as.numeric(TP1$Castration != "No")
# table(TP1$Castration)

TP1$Deworming = as.numeric(TP1$Deworming != "No")
# table(TP1$Deworming)

TP1$Dipping = as.numeric(TP1$Dipping != "No")
# table(TP1$Dipping)

TP1$Spraying.livestock.at.home = as.numeric(TP1$Spraying.livestock.at.home != "No")
# table(TP1$Spraying.livestock.at.home)

TP1$Use.of.services.of.community.animal.health.worker...Paravet. = as.numeric(TP1$Use.of.services.of.community.animal.health.worker...Paravet. != "No")
# table(TP1$Use.of.services.of.community.animal.health.worker...Paravet.)

TP1$Homemade.animal.feeds.made.with.locally.available.ingredients...e.g..for.poultry. = as.numeric(TP1$Homemade.animal.feeds.made.with.locally.available.ingredients...e.g..for.poultry. != "No")
# table(TP1$Homemade.animal.feeds.made.with.locally.available.ingredients...e.g..for.poultry.)

TP1$Fodder.production.for.ruminants..e.g..velvet.bean..lablab. = as.numeric(TP1$Fodder.production.for.ruminants..e.g..velvet.bean..lablab. != "No")
# table(TP1$Fodder.production.for.ruminants..e.g..velvet.bean..lablab.)

TP1$Fodder.preservation.for.ruminants..e.g..Silage.making.. = as.numeric(TP1$Fodder.preservation.for.ruminants..e.g..Silage.making.. != "No")
# table(TP1$Fodder.preservation.for.ruminants..e.g..Silage.making..)

TP1$Survival.feeding..feeding.of.productive.livestock.in.lean.season. = as.numeric(TP1$Survival.feeding..feeding.of.productive.livestock.in.lean.season. != "No")
# table(TP1$Survival.feeding..feeding.of.productive.livestock.in.lean.season.)

TP1$Animal.feed.supplied.by.feed.companies = as.numeric(TP1$Animal.feed.supplied.by.feed.companies != "No")
# table(TP1$Animal.feed.supplied.by.feed.companies)

TP1$Artificial.insemination = as.numeric(TP1$Artificial.insemination != "No")
# table(TP1$Artificial.insemination)

TP1$Pen.fattening = as.numeric(TP1$Pen.fattening != "No")
# table(TP1$Pen.fattening)

TP1$Fertilizers = TP1$Basal.fertilizer..kg. + TP1$Top.dressing.fertilizer..kg.

TP1$Organic_amendments = TP1$Manure..kg. + TP1$Compost..kg.

TP1$X1st.choice = as.numeric(TP1$X1st.choice == "Own production")
#table(TP1$X1st.choice)

TP1$X1st.choice.1 = as.factor(ifelse(TP1$X1st.choice.1 == "Crop sales", "Crop sales", ifelse(TP1$X1st.choice.1 == "Livestock sales", "Livestock sales", ifelse(TP1$X1st.choice.1 == "Casual labour", "Casual labour", "Other"))))
#table(TP1$X1st.choice.1)

# table(TP1$Organ.meat)
# table(TP1$Flesh.meat)
# table(TP1$Eggs)
# table(TP1$Fish.and.seafood)
# table(TP1$Milk.and.milk.products)

TP1$Organ.meat = as.numeric(TP1$Organ.meat != "No")
TP1$Flesh.meat = as.numeric(TP1$Flesh.meat != "No")
TP1$Eggs = as.numeric(TP1$Eggs != "No")
TP1$Fish.and.seafood = as.numeric(TP1$Fish.and.seafood != "No")
TP1$Milk.and.milk.products = as.numeric(TP1$Milk.and.milk.products != "No")

# Grouping animal-based vitamin A rich foods and animal-based iron rich foods
TP1$Animal_food = ifelse((TP1$Organ.meat + TP1$Eggs + TP1$Milk.and.milk.products 
                           + TP1$Flesh.meat + TP1$Fish.and.seafood)>0, 1, 0)


TP1$TLU_offtake = TP1$TLU_sold + TP1$TLU_slaughtered


# summary(TP1$Number.of.tractors.)
# summary(TP1$Number.of.ploughs.)
# summary(TP1$Number.of.cultivators.)
# summary(TP1$Number.of.scotchcarts.)
# summary(TP1$Number.of.wheelbarrows.)
# summary(TP1$Number.of.knapsack.sprayers.)

TP1$Equipment_value = 95 * TP1$Number.of.ploughs. + 130 * TP1$Number.of.cultivators. + 500 * TP1$Number.of.scotchcarts. +
  55 * TP1$Number.of.wheelbarrows. + 30 * TP1$Number.of.knapsack.sprayers.


TP1 = TP1[, c(1:8, 14:17, 116:127, 136:154, 160:180)]

names(TP1) = c("District", "Ward", "Village", "Latitude", "Longitude", "Age_HHH", "Female_headed", "Education_HHH",
                "Helper", "Dependents", "Hire_labour", "Sell_labour", 
                "Certified_seeds", "Seed_bank", "DT_varieties", "Small_grains", "Crop_rotation", 
                "Intercropping", "Cover_crops", "Mulching", "IPM", "Compost_manure", "Irrigation", "Plant_density", 
                "Improved_livestock", "Shelters", "Water_infrastructure", "Routine_vaccination", "Home_vaccination", 
                "Castration", "Deworming", "Dipping", "Home_spraying", "Paravet", "Home_feed", "Fodder_production",
                "Fodder_preservation", "Survival_feeding", "Commercial_feed", "Artifitial_insemination",
                "Pen_fattening", "Own_production_food", "Source_income", "Farm_ID", "Family_size",
                "Garden", "Area_cereal", "Area_other_crop", "Cropped_area", "Proportion_other_crop", 
                "Grain", "Yield_cereal", "Cattle", "Small_ruminants", "Poultry", "TLU", "TLU_sold", 
               "TLU_slaughtered", "TLU_mortality", "Fertilizers", "Organic_amendments", "Animal_food",
               "TLU_offtake", "Equipment_value")


# TRANSFORMING THE DATA---------------------------------------------------------

# Creating another dataset for data transformation

TP2 = TP1

TP2 = TP2[!is.na(TP2$Age_HHH),]


# Checking distribution of continuous variables and normalizing

# hist(TP2$Age_HHH)
# hist(TP2$Family_size)
# hist(TP2$Area_cereal)
# hist(TP2$Area_other_crop)
# hist(TP2$Cropped_area)
# hist(TP2$Proportion_other_crop)
# hist(TP2$Grain)
# hist(TP2$Cattle)
# hist(TP2$Small_ruminants)
# hist(TP2$Poultry)
# hist(TP2$Fertilizers)
# hist(TP2$Organic_amendments)
# hist(TP2$TLU_sold)
# hist(TP2$TLU_slaughtered)
# hist(TP2$TLU_mortality)
# hist(TP2$Equipment_value)

TP2$Family_size = log10(TP2$Family_size+(0.5*min(TP2$Family_size[TP2$Family_size > 0])))
TP2$Area_cereal = log10(TP2$Area_cereal+(0.5*min(TP2$Area_cereal[TP2$Area_cereal > 0])))
TP2$Area_other_crop = log10(TP2$Area_other_crop+(0.5*min(TP2$Area_other_crop[TP2$Area_other_crop > 0])))
TP2$Cropped_area = log10(TP2$Cropped_area+(0.5*min(TP2$Cropped_area[TP2$Cropped_area > 0])))
# TP2$Proportion_other_crop = log10(TP2$Proportion_other_crop+(0.5*min(TP2$Proportion_other_crop[TP2$Proportion_other_crop > 0])))
TP2$Grain = log10(TP2$Grain+(0.5*min(TP2$Grain[TP2$Grain > 0])))
TP2$Cattle = log10(TP2$Cattle+(0.5*min(TP2$Cattle[TP2$Cattle > 0])))
TP2$Small_ruminants = log10(TP2$Small_ruminants+(0.5*min(TP2$Small_ruminants[TP2$Small_ruminants > 0])))
# TP2$Poultry = log10(TP2$Poultry+(0.5*min(TP2$Poultry[TP2$Poultry > 0])))
TP2$Fertilizers = log10(TP2$Fertilizers+(0.5*min(TP2$Fertilizers[TP2$Fertilizers > 0])))
TP2$Organic_amendments = log10(TP2$Organic_amendments+(0.5*min(TP2$Organic_amendments[TP2$Organic_amendments > 0])))
TP2$TLU_offtake = log10(TP2$TLU_offtake+(0.5*min(TP2$TLU_offtake[TP2$TLU_offtake > 0])))
#TP2$TLU_mortality = log10(TP2$TLU_mortality+(0.5*min(TP2$TLU_mortality[TP2$TLU_mortality > 0])))
TP2$Equipment_value = log10(TP2$Equipment_value+(0.5*min(TP2$Equipment_value[TP2$Equipment_value > 0])))


# RUNNING THE TYPOLOGY----------------------------------------------------------

# Scaled Euclidean dissimilarity between continuous structural variables
dEuc_structural = vegdist(TP2[,c("Age_HHH", "Family_size","Cropped_area", "Cattle", 
                                  "Small_ruminants", "Equipment_value")], 
                           method="gower") 

# Scaled Euclidean dissimilarity between continuous functional variables (performance)
dEuc_functional = vegdist(TP2[,c("Grain","Fertilizers", "Organic_amendments", 
                                  "TLU_offtake")], method="gower", na.rm = TRUE) 

# dissimilarity between binary structural variables
dBin_ppl_structural = dist.binary(TP2[,c("Female_headed", "Education_HHH", 
                                         "Helper", "Dependents", 
                                         "Hire_labour", 
                                          "Sell_labour", "Garden")], method=2)


# dissimilarity between binary/factorial functional variables

# need to create a dissimilairty matrix For Source_income, which as 3 categories, not 2
nobs = length(TP2$Source_income)
dInc = matrix(NA, nobs, nobs)
for (i in 1:nobs){
  for (j in 1:nobs){
    dInc[i,j] = 1 - (TP2$Source_income[i] == TP2$Source_income[j])
  }
}
dInc = as.dist(dInc)

# need to calculate dissimilairty separately for each other (binary) functional variable
dProd = dist.binary(as.matrix(TP2[,c("Own_production_food")]), method=2)
dAnim = dist.binary(as.matrix(TP2[,c("Animal_food")]), method=2)

# calculate the average
dBin_ppl_functional = (dInc + dProd + dAnim)/3


# dissimilarity between binary variables of crop performance
dBin_ppl_crop = dist.binary(TP2[,c("Certified_seeds", "Seed_bank", "DT_varieties", "Small_grains",
                                "Crop_rotation", "Intercropping", "Cover_crops", "Mulching",
                                "IPM", "Compost_manure", "Irrigation", "Plant_density")], method=2)

# dissimilarity between binary variables of livestock practices
dBin_ppl_livestock = dist.binary(TP2[,c("Improved_livestock", "Shelters", "Water_infrastructure", 
                                "Routine_vaccination", "Home_vaccination", "Castration", "Deworming", 
                                "Dipping", "Home_spraying", "Paravet", "Home_feed", "Fodder_production",
                                "Fodder_preservation", "Survival_feeding", "Commercial_feed", 
                                "Artifitial_insemination", "Pen_fattening")], method=2)


# Combine the 2 dissimilarities of stuctural variables, weighted by number of variables in each dissimilarity
d_structural = (6 * dEuc_structural + 7 * dBin_ppl_structural)/13

# Combine the 2 dissimilarities of functional variables, weighted by number of variables in each dissimilarity
d_functional = (4 * dEuc_functional + 3 * dBin_ppl_functional)/7

# Combine the 2 dissimilarities of crop and livestock practices, weighted by number of variables in each dissimilarity
d_practices = (12 * dBin_ppl_crop + 17 * dBin_ppl_livestock)/29

# Combine the 3 categories of dissimilarities
dAll = (d_structural + d_functional + d_practices)/3


# principal coordinates analysis == classical multi-dimensional scaling
 
pco = cmdscale(dAll, eig = TRUE, k = 10) 
barplot(pco$eig[1:20])
# cumsum(pco$eig) / sum(pco$eig)

pco$eig[1:20]

# choosing 6 dimensions
pco_var = pco$points[,1:6]

hc_pco = hclust(dist(pco_var), method = "complete")
plot(hc_pco, hang = -1)


grpPCO = cutree(hc_pco, k = 3)
hdend = as.dendrogram(hc_pco)
hdend = color_branches(hdend, k = 3)
hdend = color_labels(hdend, k = 3)
plot(hdend)


# PLOTS-------------------------------------------------------------------------

ggd1 = as.ggdend(hdend) 

p1 = ggplot(ggd1, labels = FALSE) +
  theme_classic() + ggtitle("A") +
  scale_color_manual(values = c("#F98C0AFF", "#BB3754FF", "#56106EFF", "black")) +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"))

p1


pt = as.data.frame(pco$points)
pt$Type = grpPCO

pt$Type = as.factor(pt$Type)


p2 = ggplot(data = pt) +
  geom_point(aes(x = V1, y = V2, color = Type), size = 3, alpha = 0.6) + 
  scale_color_manual(values = c("#56106EFF", "#BB3754FF", "#F98C0AFF")) +
  xlab("PCO1") + ylab("PCO2") +
  theme_few() + ggtitle("B") +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))
  
p2

FIGTYPE = grid.arrange(p1, p2, nrow = 1, ncol = 2, widths = c(1.3, 1))

FIGTYPE

FIGTYPE = ggdraw(FIGTYPE) + 
  theme(plot.background = element_rect(fill = "white"))

# ggsave("Figure typo.jpeg", units="cm", width = 35, height = 15, dpi = 320)



TP2$Type = grpPCO

TP2 = TP2[, -c(1:43, 45:62)]

TP1 = merge(TP1, TP2, by = "Farm_ID", all.y = TRUE)

TP1$Type = as.factor(TP1$Type)

SUM = as.data.frame(TP1 %>% group_by(Type) %>% tally())
SUM$Type = c("Type 1", "Type 2", "Type 3")

FIGDISTRIB = ggplot(SUM, aes(x="", y=n, fill=Type)) + geom_bar(width = 1, stat = "identity", color = "black")+
  coord_polar("y", start=0) + theme_minimal() +
  scale_fill_viridis_d(option = "B") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.border = element_blank(), panel.grid=element_blank(),  
        axis.ticks = element_blank(),axis.text = element_blank(),
        legend.text = element_text(size = 12), legend.key.size = unit(1, 'lines'), 
        legend.title = element_blank(), legend.position="bottom")  + 
  guides(fill = guide_legend(nrow = 2, byrow=TRUE))

FIGDISTRIB


# RANDOM FOREST-----------------------------------------------------------------

tp_rf = TP1[, c(67, 7:46, 51, 53:54, 60:64)]

names(tp_rf)[48] = "TLU_offtake"
names(tp_rf)[49] = "Equipment_value"

rf_type = randomForest(Type ~ ., data = tp_rf, ntree = 1500)
print(rf_type)

importance(rf_type)
varImpPlot(rf_type)


# SUMMARY TABLE-----------------------------------------------------------------

TP1$Livestock_main_income = ifelse(TP1$Source_income == "Livestock sales", 1, 0)
TP1$Crop_main_income = ifelse(TP1$Source_income == "Crop sales", 1, 0)
TP1$Other_main_income = ifelse(TP1$Source_income == "Other", 1, 0)


TBL = TP1[, c(67, 7:9, 45, 10:13, 64, 62, 68:70, 46, 49:51, 53:55, 57, 60:61)]

TBL$Type = ifelse(TBL$Type == "1", "Type 1", TBL$Type)
TBL$Type = ifelse(TBL$Type == "2", "Type 2", TBL$Type)
TBL$Type = ifelse(TBL$Type == "3", "Type 3", TBL$Type)


table = TBL %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    digits = all_continuous() ~ 2,
    label = list(Age_HHH ~ "Age of head of the household (years)",
    Female_headed ~  "Female-headed households",
    Education_HHH ~  "Education (higher than primary)",
    Family_size ~  "Family size (n)",
    Helper ~  "Helping others",
    Dependents ~  "Being helped",
    Hire_labour ~  "Hiring labor",
    Sell_labour ~  "Selling labor ",
    Equipment_value.x ~  "Equipment value (USD)",
    Animal_food ~  "Consumed animal products in past 24 hours",
    Livestock_main_income ~  "Livestock as main source of income",
    Crop_main_income ~  "Crop as main source of income",
    Other_main_income ~  "Off-farm activities as main source of income",
    Garden ~  "Owning a garden",
    Cropped_area ~  "Total cropped area (ha)",
    Proportion_other_crop ~  "Proportion of non-cereal crop (%)",
    Grain ~  "Cereal produced in 2019/20 (kg)",
    Cattle ~ "Cattle (n)",
    Small_ruminants ~  "Goats and sheep (n)",
    Poultry ~  "Poultry (n)",
    TLU_sold ~  "Livestock sold (TLU/year)",
    Fertilizers ~  "Fertilizer applied (kg)",
    Organic_amendments ~  "Organic amendment (kg)"
    ))

table


# table %>%
#   as_gt() %>%             # convert to gt table
#   gt::gtsave(             # save table as image
#     filename = "summary_table.png")



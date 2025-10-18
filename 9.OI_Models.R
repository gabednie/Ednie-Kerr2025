library(raster)
library(terra)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(plyr)
library(rgdal)
library(parallel)
library(data.table)
library(matlib)
library(stringi)
library(ggplot2)
library(lme4)
library(lmtest)
library(pscl)
library(AER)
library(ggpubr)
library(MuMIn)
library(MASS)

#### Before running, change TPI#, OI#, AT# and manually adjust desired resolution in line 31 #########

#Calculate OI each site and write csv with values each species
#Convert all to dataframe first#
OI <- function(x) {length(which(x>=1))/(length(x)-length(which(is.na(x))))}

# loop through each site folder - get OI estimates for every species in every site 
# one file per site
folder_path <- list.dirs("E:/PhD/Rasters/TPI/SITE",         #works best if TPI rasters are grouped by site ID
                         full.names = T,recursive = FALSE)

for(fp in folder_path){ 
  raster_list = list.files(fp, pattern = "*.tif", full.names = TRUE) 
  dts11 <- as.data.frame(stack(raster_list))
  outs11 <- as.data.frame(lapply(dts11, OI))
  test <- as.data.frame(t(outs11))
  setDT(test, keep.rownames = TRUE)[]
  test <- test[!grepl("ART", test$rn)]
  test <- test[!grepl("HAR", test$rn)]
  outs11 <- test[!grepl("LOG", test$rn)]
  outs11$rn <- gsub('LUC_', 'LUCILIUS_', outs11$rn)
  ordered <- outs11[order(outs11$rn),]
  ordered$Species <- c("antiopa","aphrodite","archippus","atalanta","augustinus","baptisiae",      # add column with all species names in right order
                       "bellona","bimacula","cocyta","comyntas","cybele","cymela","dion",
                       "egeremet","eurytheme","henrici","lineola","lucia","lucilius","lygdamus",
                       "origenes","pegala","philodice","plexippus","polios","progne","rapae",
                       "sassacus","selene","tharos","themistocles","tullia","vestris")
  nb <- stri_extract_last(basename(fp), regex = "(\\d+)") # get survey identifier from folder name
  ordered$Code <- nb # add column with survey identifier
  ordered$lookup <- paste0(ordered$Species, ordered$Code) # add lookup column to easily combine to master spreadsheet
  ordered <- subset(ordered, select = -rn) # remove species+location+time identifier
  names(ordered)[names(ordered) == "V1"] <- "OI" # change column name OI
  ordered$OI <- 100*(ordered$OI) # multiply OI by 100
  file_name <- basename(fp)
  output_name <- paste0("E:/PhD/Data Files/MSc/OI/",file_name,"_OI.csv")
  write.csv(x=ordered, file=output_name)
} 

# merge ###########################################
master <- list.files("E:/PhD/Data Files/MSc/OI/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows   
write.csv(master, "E:/PhD/Data Files/MSc/OI/Master_OI.csv")

# add to master sheet
tpioi <- read.csv("E:/PhD/Data Files/MSc/Resample_TPI_OI.csv")
OI <- read.csv("E:/PhD/Data Files/MSc/OI/Master_OI.csv")

look <- subset(OI, select = -c(X, ...1))
master <- tpioi
new <- merge(master, look, by = c("lookup", "Species", "Code"))
write.csv(new, "E:/PhD/Data Files/MSc/TPI_OI.csv")



############## MODELS ##########################################################
################################################################################
#Cleaned Models

library(glmmTMB)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(lmtest)
library(readr)
library(tidyverse)
library(plyr)
library(pscl)
library(AER)
library(ggpubr)
library(MuMIn)
library(MASS)
library(lmerTest)
library(performance)
library(partR2)
library(car)

tpioi <- read.csv("D:/Gabby/MSc/Data/TPI_OI_Limestone.csv") #merged transects
hill <- read.csv("E:/PhD/Data Files/MSc/MASTER.csv")

abundance <- glmmTMB(Abundance ~ OI_Site + FHD_Simp + Floral_Richness + Size + (1|Site) + (1|Species), family = nbinom2, data = tpioi)
summary(abundance)
r2(abundance)

occurrence <- glmer(Occurrence ~ OI_Site + FHD_Simp + Floral_Richness + Size + (1|Species) + (1|Site), family=binomial(link="logit"), data=tpioi)
summary(occurrence)
r2(occurrence)

richness <- lmer(Richness ~ TPI_S_New + FHD_Simp + Floral_Richness + Size + (1 | Site), data = hill) # Site size improves it
summary(richness)
ranova(richness)
r2(richness)

diversity <- lmer(Hill_q1 ~ TPI_S_New + FHD_Simp + Floral_Richness + Size + (1 | Site), data = hill)
summary(diversity)
ranova(diversity)
r2(diversity)
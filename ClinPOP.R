# load required packaged
library(tidyverse)
library(openxlsx)
library(countrycode)

rm(list = ls())
# read the data and retrive clinvar marker information from database
                    
populations <- read.xlsx('/Users/dominiquefastus/ClinPop/raw_data/Eurasian - Dataset_tims.xlsx',
                         sheet = "Eurasian", cols = c(13,19,25:27,33))

# assign countries to continent regions to group them into populations
egnations <- populations$Country
populations$Country <- countrycode(sourcevar = egnations, origin = "country.name",
                                  destination = "region")

populations <- populations %>%
  rename("MasterID" = 1,
         "Time" = 2) #  time in BP in Years before 1950 CE


# read and prepare the clinvar data
clinvar <- read.csv('/Users/dominiquefastus/ClinPop/raw_data/variant_summary.txt', 
                    sep = "\t")

# filter the clinvar data with un
clinvar <- clinvar %>%
                    filter(ClinicalSignificance == "Pathogenic" |
                           ClinicalSignificance == "Likely Pathogenic",
                           RS...dbSNP. != -1) %>%
                    rename("rsID" = RS...dbSNP.)

clinvar$rsID <- paste0("rs", clinvar$rsID)

bim <- read.csv('/Users/dominiquefastus/ClinPop/PLINK_data/Data_public.bim',
             sep = "\t", header = FALSE)

# get only the rsid contained in the bim file and create a file, only containing the
# pathogenic or likely pathogenic ones
rsid_match <- bim[,2] %in% clinvar[,10]
rsid_match <- bim[rsid_match,] %>% 
  rename("rsID" = V2) %>%
  filter(str_detect(rsID, "rs"))

write.table(rsid_match$rsID,file = "/Users/dominiquefastus/ClinPop/rsID_data/rsID_data_public.txt",
            row.names = FALSE, quote = FALSE)

rs_ids <- read.csv('/Users/dominiquefastus/ClinPop/rsID_data/Data_rs_ids.txt',
                sep = "\t", header = FALSE)

# get only the rsid contained in the bim file and create a file, only containing the
# pathogenic or likely pathogenic ones
msid_match <- populations[,1] %in% rs_ids[,1]
msid_match <- rs_ids[msid_match,] %>% 
  rename("MasterID" = V1, "rsID" = V2)

populations <- merge(populations,msid_match,by = "MasterID")
populations <- select(populations, MasterID, Time, Country, Sex, rsID)

write.table(populations, file = "/Users/dominiquefastus/ClinPop/populations_readin.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)

# also write the clinvar table only for interested columns and the found markers
found_clinvar <- select(clinvar, Type,  ClinicalSignificance,
                        rsID, PhenotypeList) 

unique_rsids <- unique(select(msid_match, rsID))
clinvar_table <- merge(unique_rsids, found_clinvar, by = "rsID")

write.table(found_clinvar, file = "/Users/dominiquefastus/ClinPop/clinvar.txt", 
            sep = "\t", row.names = FALSE, quote = FALSE)

rm(list = ls())

# load required packages to load / read the datasets
# package for reading excel tables
library(tidyverse)
library(openxlsx)

# read the data and retrieve clinvar marker information from database
                    
populations <- read.xlsx('/Users/dominiquefastus/ClinPop/raw_data/Eurasian - Dataset_tims.xlsx',
                         sheet = "Eurasian", cols = c(13,19,25,33))

# table with the countries and their regional / continental location
countries <- read.csv('/Users/dominiquefastus/ClinPop/countries.txt', sep = '\t')

# get the continents for the countries of each individual
# this is needed to group them later in populations
populations <- merge(populations, countries, by = "Country") 

# rename the colums in the population dataframe to match with the clinvar table
populations <- populations %>%
  rename("MasterID" = 2, "Time" = 3)

# select only the columns of interest
populations <-select(populations, MasterID, Time, Sex, Continent)

# read and prepare the clinvar data
# can take long, better use  other large read in options
clinvar <- read.csv('/Users/dominiquefastus/ClinPop/raw_data/variant_summary.txt', 
                    sep = "\t")

# filter the clinvar data with only pathodenic significance and useful information
# rename the column for a standard
clinvar <- clinvar %>%
                    filter(ClinicalSignificance == "Pathogenic",
                           RS...dbSNP. != -1) %>%
                    rename("rsID" = RS...dbSNP.)

# the rsIDs should all start with rs.... for merging and matchind purpose
clinvar$rsID <- paste0("rs", clinvar$rsID)

# load in the bim file of a plink dataset 
bim <- read.csv('/Users/dominiquefastus/ClinPop/PLINK_data/Data_public.bim',
             sep = "\t", header = FALSE)

# get only the rsid contained in the bim file and create a file, only containing the
# pathogenic, rename the column of interest and filter out the numbers without beginnig with rs
rsid_match <- bim[,2] %in% clinvar[,10]
rsid_match <- bim[rsid_match,] %>% 
  rename("rsID" = V2) %>%
  filter(str_detect(rsID, "rs"))

# write this information as a table with only that row
write.table(rsid_match$rsID,file = "/Users/dominiquefastus/ClinPop/rsID_data/rsID_data_public.txt",
            row.names = FALSE, quote = FALSE)

# here use the plink (v. 1.9) and REplink.py (v. 1.0.2) programs to get the matching MasterIDs and corresponding rsIDs
# first run the pling to create a long format file for the matched snps, the snps need to be provided comma seperated
# first run: 
#            python REplink.py --rsID rsID_data_public.txt [only column with rsIDs returnes comma seperated rsIDs to STDOUT]
# second paste the rsIDs to only include these in the plink recoding: 
#            plink --bfile Data_public [original .bed file] --recode lgen --snps [insert all snps from STD] --out Data_rs_ids [name of output file]
# then get only the second MasterID and third rsID (snp identifier) column with following REplink command argument:
#            python REplink.py --lgen Data_rs_ids.lgen [filter lgen format plink file] --out Data_rs_ids.txt [optional output file]
# continue in r ClinPOP_setup.r

# read in the MasterIDs with the corresponding filtered out rsIDs 
rs_ids <- read.csv('/Users/dominiquefastus/ClinPop/rsID_data/Data_rs_ids.txt',
                sep = "\t", header = FALSE)

# get only the msid contained in the bim file and create a file, only containing the
# pathogenic ClinVAR markers
msid_match <- rs_ids[,1] %in% populations[,1]
msid_match <- rs_ids[msid_match,] %>% 
  rename("MasterID" = V1, "rsID" = V2)

# create the populations dataframe with the inviduals and their ClinVAR markers
# select the column needed to create the plots and application
populations <- merge(populations,msid_match,by = "MasterID")
populations <- select(populations, MasterID, Time, Continent, Sex, rsID)

# write the table in a file
write.table(populations, file = "/Users/dominiquefastus/ClinPop/populations_readin.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)

# also write the clinvar table only for interested columns and the found markers
found_clinvar <- select(clinvar, Type,  ClinicalSignificance,
                        rsID, PhenotypeList) 

# only contain the unique entries
unique_rsids <- unique(select(msid_match, rsID))
clinvar_table <- merge(unique_rsids, found_clinvar, by = "rsID")

# write the modified and selected clinvar table to a file
write.table(found_clinvar, file = "/Users/dominiquefastus/ClinPop/clinvar.txt", 
            sep = "\t", row.names = FALSE, quote = FALSE)
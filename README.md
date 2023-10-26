# ClinPOP

ClinPOP is a web application to visualize and analyse selected ClinVAR marker frequencies in regional populations over time. The interactive web interface, build in shiny dashboard web application, contains different frequency plots and overviews for the selected input.

```
Version:    1.0.8
Author:     Dominique Fastus

Requires:   R (shiny, shinydashboard, dplyr,
               tidyverse, openxlsx, ggplot2,
               plotly, DT)
            Python (argparse) [OPTIONAL]

Run:        ClinPOPapp.R for shiny application
            ClinPOPsetup.R + REplink.py for data
            implementation and setup changes

Website:    to be deployed ...
```

### Data preprocessing and ClinPOP setup
The following datasets were provided and used:
```
Plink files
    - Data_public.bed 
    - Data_public.bim 
    - Data_public.fam
 To read more over plink and the file formats : https://www.cog-genomics.org/plink/
``` 

```
Ancient dataset with indivdual informations
- Eurasian - Dataset_tims.xlsx
```

The ClinVAR marker information or variant summary of the NCBI database can be downloaded through the ftp site:
```bash
wget https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz
```

To prepare the data follow the steps in the `ClinPOPsetup.R` script.


### Run application
To run the application open `ClinPOPapp.R`in R studio and install necessary packages and open the web application in your browser from the viewer panel.

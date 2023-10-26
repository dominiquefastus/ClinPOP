connect <- dbConnect(SQLite(), dbname = "/Users/ClinPop/plink_fam_snpID.db")

# Create an empty data frame to store the results
all_ids <- data.frame()

for (mastid in populations$MasterID) {
  query <- paste("SELECT * FROM Identifiers WHERE MasterID = '", mastid, "'", sep = "")
  matching_data <- dbGetQuery(connect, query)
  all_ids <- rbind(all_data, matching_data)
  print(mastid)
}

# Add the data to the existing table in the database
dbWriteTable(connect, "matching_ids", all_ids, append = TRUE)

# Create an empty data frame to store the results
all_rsids <- data.frame()

for (rsid in filtered_clinvar$rsID) {
  query2 <- paste("SELECT * FROM matching_ids WHERE rsID = '", rsid, "'", sep = "")
  matching_data2 <- dbGetQuery(connect, query2)
  all_rsids <- rbind(all_rsids, matching_data2)
  print(rsid)
}
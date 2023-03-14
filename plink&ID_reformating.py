# this is a reformatting for the lgen file into a tab seperated text file
# the file contains a cloumn for the within family ID or Member ID and the snp identifier
with open('/Users/dominiquefastus/ClinPop/PLINK_data/Data_rs_ids.lgen', 'r') as infile, open('/Users/dominiquefastus/ClinPop/rsID_data/Data_rs_ids.txt', 'w') as outfile:
    for line in infile:
        columns = line.split()
        second_column = columns[1]
        third_column = columns[2]
        outfile.write(f"{second_column}\t{third_column}\n")

# basic printing of all rsIDs in the standard output for snp identifiers filtering
rs_ids = []

with open("/Users/dominiquefastus/ClinPop/rsID_data/rsids_Data_public.txt","r") as infile:
    for line in infile:
        if line.startswith("rs"):
            rs_ids.append(line.rstrip('\n'))
        else:
            continue

print(", ".join(rs_ids))
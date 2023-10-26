with open('/Users/dominiquefastus/ClinPop/countries.txt','r') as input, open('/Users/dominiquefastus/ClinPop/countriconti.txt', 'w') as output:
        output.write("Country\tContinent\n")
        for line in input:
            columns = line.split("\t")
            country = columns[1]
            continent = columns[4] + ' ' + columns[5]
            output.write(f"{country}\t{continent}\n")
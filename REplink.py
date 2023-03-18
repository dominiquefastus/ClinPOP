import argparse

# define command line arguments
parser = argparse.ArgumentParser(description='Reformat plink files or information into files and more!', prog="REplink")
parser.add_argument('--lgen', '-l', metavar='lgen_file', type=str,
                    help='path to input file in lgen format')
parser.add_argument('--rsID', '-r', metavar='rsID_file', type=str,
                    help='''path to input file with rsIDs Prints all snp identifiers comma seperated to the STDOUT''')
parser.add_argument('--output', '-o', metavar='output_file', type=str, default='Data_rs_ids.txt',
                    help='path to output file giving the within-family id and snps identifiers tab seperated')

# parse command line arguments
args = parser.parse_args()

# reformat input file into tab-separated output file
if args.lgen:
    with open(args.lgen, 'r') as infile, open(args.output, 'w') as outfile:
        for line in infile:
            columns = line.split()
            second_column = columns[1]
            third_column = columns[2]
            outfile.write(f"{second_column}\t{third_column}\n")
    print(f'''
    ---------------------------------------------------------------- 
    {args.lgen} was successfull rewritten into {args.output}!
    ----------------------------------------------------------------
    ''')

# print all rsIDs in the standard output for snp identifiers filtering
if args.rsID:
    rs_ids = []

    with open(args.rsID, "r") as infile:
        for line in infile:
            print(line)
            if line.startswith("rs"):
                rs_ids.append(line.rstrip('\n'))
            else:
                continue
    
    print(f'''

    The ids are the following and can be copied for any --snp / --snps extraction in plink:
    
    {", ".join(rs_ids)}
    

    ''')

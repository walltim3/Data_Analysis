from datetime import datetime
from importlib.metadata import requires
from elasticsearch import Elasticsearch
import re
import json
import sys
import os

# make single record
# load file 
# flags:
#   -h or --help: help
#   -f or --file: select file to load
#   -a or --address: "http://192.168.56.102:9200" 


def load_file(filename, address):

    regex = r"\{(.|\n)*?\}"
    
    try:
        with open(filename, 'r') as infile:
            test_str = infile.read()
    except:
        exit(f"error reading the file {filename}")
    try:    
        matches = re.finditer(regex, test_str, re.MULTILINE)
    except:
        exit(f"wrong data format in {filename}")
    try:
        es = Elasticsearch("http://192.168.56.102:9200")
    except:
        exit("error connecting ot Elasticsearch with address {address}")

    for matchNum, match in enumerate(matches, start=1):
            
            y = json.loads(match.group())
            print(matchNum, "::", y['title'])
            
            try:
                index_filename = os.path.basename(filename)
                resp = es.index(index=index_filename, id=matchNum, document=y)
                print(resp['result'])
            except:
                print(f"error loading {matchNum}::\n{y}")

    ...

def main():
    
    filename = ""
    address = ""

    usage = '''
    # script to load json files to Elasticsearch
    # flags:
    #   -h or --help: help
    #   -f or --file: select file to load
    #   -a or --address: "http://192.168.56.102:9200" 
    #   -r: print requirements
    '''

    requirements = '''
    # elastic-transport 8.1.2    
    # elasticsearch     8.1.2    
    # pip               20.0.2   
    # pkg-resources     0.0.0    
    # setuptools        44.0.0   
    # urllib3           1.26.9   
    # wheel             0.34.2  
    '''

    if len(sys.argv) <= 1:
        exit(usage)
    else:
        for index, item in enumerate(sys.argv):
            if item == '-f' and filename == "":
                try:
                    filename = sys.argv[index+1]
                except:
                    exit("error reading file")
            if item == '-a' and address == "":
                try:
                    address = sys.argv[index+1]
                except:
                    exit("error reading address")
            if item == '-r':
                    exit(f"{requirements}")
                    
        print("OK")
        print(filename)
        print(address)
        
        load_file(filename, address)

        exit("Exited successfuly")

    ...

if __name__ == "__main__":
    main()


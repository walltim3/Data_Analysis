from datetime import datetime
from my_counter import ngram_ctr 
import numpy as np
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


def process_data(documents):
    full_text = ''.join(documents.values())
    documents_names = list(documents.keys())

    full_text_vector = {name:0 for name in full_text.split(' ')}
    doc_freq = [ngram_ctr(text.split(' ')) for text in documents.values()]
    doc_freq = [ {**full_text_vector, **item} for item in doc_freq]
    
    for docNum, doc in enumerate(documents_names):
        print(f"{docNum} :: {doc}:")
        for k , v in doc_freq[docNum].items(): # iterating doc dictionary
            if v != 0:
                print('\t'+k+"\t", v)
        print()

def load_file(filename):

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
    
    documents = {}

    for matchNum, match in enumerate(matches, start=1):
            
            y = json.loads(match.group())
            documents =dict(**documents, **{y['title']: y['textBody']})

    process_data(documents)
    ...

def main():
    
    filename = ""
    address = ""

    usage = '''
    # script to load json files to Elasticsearch
    # flags:
    #   -h or --help: help
    #   -f or --file: select file to load
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
            if item == '-r':
                    exit(f"{requirements}")
                    
        print("OK")
        print(filename)
        
        load_file(filename)

        exit("Exited successfuly")

    ...

if __name__ == "__main__":
    main()


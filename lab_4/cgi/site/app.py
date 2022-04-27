from flask import Flask, render_template, url_for
import requests
from elasticsearch import Elasticsearch
import re
import json
import sys
import os

#used to create records
def load_file(filename, address):

    regex = r"\{(.|\n)*?\}"
    
    test_str = ''

    try:    
        matches = re.finditer(regex, test_str, re.MULTILINE)
    except:
        print(f"wrong data format in {filename}")
    try:
        es = Elasticsearch(address)
    except:
        print("error connecting ot Elasticsearch with address {address}")

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




app = Flask(__name__)

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/upload')
def upload():
    return render_template('upload.html')

@app.route('/download')
def download():
    return render_template('download.html')

if __name__ == "__main__":
    app.run(debug = True)

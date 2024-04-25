import os
os.environ['SWI_HOME_DIR'] = 'C:\\Program Files\\swipl'
import sys
import argparse
from pyswip import Prolog

def read_file(filename):
    with open(filename, 'r') as file:
        return file.read()



def pass_content_to_prolog(content):
    prolog = Prolog()
    prolog.consult("porygongrammer.pl")    
    results = []
    for result in prolog.query("block(T, " + content + ", [])"):
        results.append(result)
    return results

def extract_t_blk(results):
    extracted_results = []
    for result in results:
        # Extract the value associated with the key 'T'
        prolog_result = result['T']
        # Find the index of 't_blk('
        index = prolog_result.find('t_blk(')
        if index != -1:
            # Extract the substring after 't_blk('
            extracted_results.append(prolog_result[index:])
    return extracted_results    

if __name__ == "__main__":
    filename = "program.pgontokens"  
    content = read_file(filename)
    print(type(content))

    results = pass_content_to_prolog(content)
    print(extract_t_blk(results))


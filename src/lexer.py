import os
os.environ['SWI_HOME_DIR'] = 'C:\\Program Files\\swipl'
import sys
import argparse
from sly import Lexer
import re

from pyswip import Prolog


class Constants:
    TOKEN_FILE_EXTENSION = 'pgontokens'
    PRINT_GREEN_TEXT = '\033[92m'
    PRINT_NORMAL_TEXT = '\033[0m'
    PRINT_YELLOW_TEXT = '\033[93m'
    PRINT_RED_TEXT = '\033[91m'

# Reference: https://sly.readthedocs.io/en/latest/sly.html
class PgonLexer(Lexer):
    # SET OF TOKENS
    tokens = {FLOAT, ID, NUMBER, STRING_VALUE, 
              INC, DEC, EQ, GE, LE, NE, MOD, PRINTNL,
              CONST, INT, STRING_KEYWORD, BOOL, FLOAT_KEYWORD,
              SQRT, CBRT, SQ, CUBE, AND, OR, NOT, TRUE, FALSE,
              IF, ELSE, ELIF, FOR, IN, RANGE, WHILE, PRINT, STRLEN,
              ADD_SHORTHAND, SUB_SHORTHAND, MUL_SHORTHAND, DIV_SHORTHAND, EXP_SHORTHAND}

    # LITERALS
    literals = {'{', '}', '?', ';', ':', '(', ')','=',
                '/','*','+','-','%','^','<','>',','}

    # IGNORE
    ignore = ' \t'
    ignore_newline = r'\n+'
    ignore_comment = r'\#(.*)'

    # TOKENS
    # Identifiers and Keywords
    ID = r'[a-z]([a-zA-Z0-9_]*[a-zA-Z0-9]+)?'
    ID['const'] = CONST
    ID['int'] = INT
    ID['string'] = STRING_KEYWORD
    ID['bool'] = BOOL
    ID['float'] = FLOAT_KEYWORD
    ID['sqrt'] = SQRT
    ID['cbrt'] = CBRT
    ID['sq'] = SQ
    ID['cube'] = CUBE
    ID['mod'] = MOD
    ID['and'] = AND
    ID['or'] = OR
    ID['not'] = NOT
    ID['true'] = TRUE
    ID['false'] = FALSE
    ID['if'] = IF
    ID['else'] = ELSE
    ID['elif'] = ELIF
    ID['while'] = WHILE
    ID['for'] = FOR
    ID['in'] = IN
    ID['range'] = RANGE
    ID['print'] = PRINT
    ID['printnl'] = PRINTNL
    ID['strlen'] = STRLEN
    INC = r'\+\+'
    DEC = r'--'
    EQ = r'=='
    GE = r'>='
    LE = r'<='
    NE = r'!='
    ADD_SHORTHAND = r'\+='
    SUB_SHORTHAND = r'\-='
    MUL_SHORTHAND = r'\*='
    DIV_SHORTHAND = r'\/='
    EXP_SHORTHAND = r'\^='

    @_(r'\d+\.\d*') # Regex if decimal point is not there FLOAT SHOULD BE ENDING WITH '.'
    def FLOAT(self, t):
        t.value = float(t.value)
        return t

    # Match integers
    @_(r'\d+')
    def NUMBER(self, t):
        t.value = int(t.value)
        return t

    @_(r'\".*\"')
    def STRING_VALUE(self, t):
        t.value = t.value.replace('\"','')
        return t
    

def parse_arguments():
    parser = argparse.ArgumentParser(
        description='Porygon Lexer - Converts the Porygon source code into a list of tokens and saves it as '
                    '<InputFileName>' + Constants.TOKEN_FILE_EXTENSION)
    parser.add_argument('input', metavar='InputFileName', type=str,
                        nargs=1, help='Filepath to Porygon source code')
    parser.add_argument('--evaluate', action='store_true', help='Evaluate the generated tokens')
    return parser.parse_args()

def read_input_file(filename, num):
    data = None
    if (num == 1) and not(filename.endswith(".pgon")):
        print(Constants.PRINT_RED_TEXT + "This is not a Porygon file: "+  sys.argv[1] + Constants.PRINT_NORMAL_TEXT)
        return data
    try:
        with open(filename, "r") as input_file:
            data = input_file.read()
    except FileNotFoundError:
        print(Constants.PRINT_RED_TEXT + "No such file exists in path: "+  sys.argv[1] + Constants.PRINT_NORMAL_TEXT)
        return data
    if(num == 1):
        print("Reading your program: " + Constants.PRINT_GREEN_TEXT + 'SUCCESS!' + Constants.PRINT_NORMAL_TEXT)
    return data

def replace_str_with_single_quotes(text):
    pattern = r'(\bstr\((.*?)\))'
    return re.sub(pattern, lambda match: f"str('{match.group(2)}')", text)

def write_tokens_to_file(tokens, filename):
    with open(filename, "w") as file:
        allTokens = []
        for token in tokens:
            if token.type == 'STRING_VALUE':
                allTokens.append('"')
                allTokens.append(token.value)
                allTokens.append('"')
            else:
                allTokens.append(token.value)
        file.write('{}'.format(allTokens))
        
        print("Lexical analysis: " + Constants.PRINT_GREEN_TEXT +
              'SUCCESS!' + Constants.PRINT_NORMAL_TEXT)

def passing_tokens_to_prolog(content):
    prolog = Prolog()
    prolog.consult("porygongrammer.pl")   
    results = [] 
    if any (prolog.query("block(T, " + content + ", [])")):
        print("Parse tree generation: "+Constants.PRINT_GREEN_TEXT + "SUCCESS!" + Constants.PRINT_NORMAL_TEXT)
        
        for result in prolog.query("block(T, " + content + ", [])"):
            results.append(result) 
    else :
        print("Parse tree generation: "+Constants.PRINT_RED_TEXT + "FAILED :(" + Constants.PRINT_NORMAL_TEXT)
        exit(1)
    return results

def passing_tree_to_prolog(content):
    prolog = Prolog()
    prolog.consult("porygonSemantics.pl")   
    results = [] 
    print(Constants.PRINT_YELLOW_TEXT + "Output:" + Constants.PRINT_NORMAL_TEXT)
    if any (prolog.query("eval_block(" + content + ",[[],[]], NEnv, Val)")):
        print("Execution: "+Constants.PRINT_GREEN_TEXT + "SUCCESS!" + Constants.PRINT_NORMAL_TEXT)
        
       
    else :
        print("Execution: "+Constants.PRINT_RED_TEXT + "FAILED :(" + Constants.PRINT_NORMAL_TEXT)
    return results

if __name__ == '__main__':
    parsed_args = parse_arguments()
    input_filename = parsed_args.input[0]
    output_filename = "program." + Constants.TOKEN_FILE_EXTENSION 
    file_data = read_input_file(input_filename, 1)
    if(file_data != None):
        print(Constants.PRINT_YELLOW_TEXT + "You wrote a Porygon program!" + Constants.PRINT_NORMAL_TEXT)

        try:
            lexer = PgonLexer()
            tokens = lexer.tokenize(file_data)
            write_tokens_to_file(tokens, output_filename)
            data = read_input_file(output_filename, 0)
            results = passing_tokens_to_prolog(data)
            
            tree = ''.join(results[0].get('T'))
            processed_data = replace_str_with_single_quotes(tree)

            final_results = passing_tree_to_prolog(processed_data)
        
        except Exception as e:
                print(Constants.PRINT_RED_TEXT + "SYNTAX ERROR !!!! \nERROR: " + Constants.PRINT_NORMAL_TEXT +str(e))
        

        

        
        

    
        
    





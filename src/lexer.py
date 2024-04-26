import os
os.environ['SWI_HOME_DIR'] = 'C:\\Program Files\\swipl'
import sys
import argparse
from sly import Lexer

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
    tokens = {FLOAT, ID, NUMBER, 
              #STRING, 
              INC, DEC, EQ, GE, LE, NE,
              CONST, INT, STRING_KEYWORD, BOOL, FLOAT_KEYWORD,
              SQRT, CBRT, SQ, CUBE, AND, OR, NOT, TRUE, FALSE,
              IF, ELSE, ELIF, FOR, IN, RANGE, WHILE, PRINT, STRLEN,
              ADD_SHORTHAND, SUB_SHORTHAND, MUL_SHORTHAND, DIV_SHORTHAND, MOD_SHORTHAND, EXP_SHORTHAND}

    # LITERALS
    literals = {'{', '}', '?', ';', ':', '(', ')','=',
                '/','*','+','-','%','^','"','<','>',','}

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
    ID['strlen'] = STRLEN
    # FLOAT = r'\d+(\.\d+)?'
    # NUMBER = r'\d+'
    # STRING = r'\".*\"'
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
    MOD_SHORTHAND = r'\%='
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



def parse_arguments():
    parser = argparse.ArgumentParser(
        description='Porygon Lexer - Converts the Porygon source code into a list of tokens and saves it as '
                    '<InputFileName>' + Constants.TOKEN_FILE_EXTENSION)
    parser.add_argument('input', metavar='InputFileName', type=str,
                        nargs=1, help='Filepath to Porygon source code')
    parser.add_argument('--evaluate', action='store_true', help='Evaluate the generated tokens')
    return parser.parse_args()


def read_input_file(filename):
    data = None
   
    try:
        with open(filename, "r") as input_file:
            data = input_file.read()
    except FileNotFoundError:
        print("No such file in path:", sys.argv[1])
    print("Reading Source Code: " + Constants.PRINT_GREEN_TEXT + 'SUCCESSFUL' + Constants.PRINT_NORMAL_TEXT)
    return data


def write_tokens_to_file(tokens, filename):
    with open(filename, "w") as file:
        allTokens = []
        for token in tokens:
            allTokens.append(token.value)
        file.write('{}'.format(allTokens))
        
        print("Writing Tokens in " + filename + ": " + Constants.PRINT_GREEN_TEXT +
              'SUCCESSFUL' + Constants.PRINT_NORMAL_TEXT)



def passing_tokens_to_prolog(content):
    prolog = Prolog()
    prolog.consult("porygongrammer.pl")   
    results = [] 
    if any (prolog.query("block(T, " + content + ", [])")):
        print("PARSE TREE GENERATION: "+Constants.PRINT_GREEN_TEXT + "SUCCESSFUL" + Constants.PRINT_NORMAL_TEXT)
        for result in prolog.query("block(T, " + content + ", [])"):
            results.append(result) #After evalutor print JUST IT WILL EVALUATE results
    else :
        print("PARSE TREE GENERATION: "+Constants.PRINT_RED_TEXT + "FAILURE" + Constants.PRINT_NORMAL_TEXT)
   
    return results



if __name__ == '__main__':
    print(Constants.PRINT_YELLOW_TEXT + "Starting Lexer" + Constants.PRINT_NORMAL_TEXT)
    parsed_args = parse_arguments()
    print(parsed_args)
    input_filename = parsed_args.input[0]
    output_filename = parsed_args.input[0][:-4:] + Constants.TOKEN_FILE_EXTENSION
    file_data = read_input_file(input_filename)
    lexer = PgonLexer()
    tokens = lexer.tokenize(file_data)
    write_tokens_to_file(tokens, output_filename)
    data = read_input_file(output_filename)
    results = passing_tokens_to_prolog(data)
    print(results)    

    # should_evaluate = parsed_args.evaluate
    # print(should_evaluate)
    # if should_evaluate:
    #     os.system("swipl -g \"main('" + output_filename + "')\" main.pl")

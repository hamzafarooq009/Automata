import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [
    
    'INT',
    'DOUBLE',
    'CHAR',
    'STRING',
    'BOOL',

    'DISPLAY',#Display
    'ASSIGN',#=
    'COMMA',#,
    
    #expressions
    'PLUS',#+
    'MINUS',#-
    'DIVIDE',#/
    'MULTIPLY',#*
    'POW',#^
    'PERCENTAGE',#%
    'PLUSPLUS',#++
    'MINUSMINUS',#--

    #logical operators
    'LESSTHAN', #<
    'GREATERTHAN', #>
    'LESSEQUAL', #<=
    'GREATEREQUAL', #>=
    'NOTEQUAL',#!=
    'EQUALS',#==
    'NOT',#NOT
    'AND',#AND
    'OR',#OR

    #NESTED PARATHESIS
    'RSQBRAC',#]
    'LSQBRAC',#[
    'RCURLY', # }
    'LROUND', # (
    'RROUND', # )
    'LCURLY', # {
    
    #type
    'TYPE', #int double string char bool
    'NAME', #variable name
    'DOT', #end of line
    'FUNC', #FUNCTION NAME

]

t_PLUS = r'\+'
t_MINUS = r'\-'
t_DIVIDE = r'\/'
t_MULTIPLY = r'\*'
t_EQUALS = r'\='
t_LESSTHAN = r'\<'
t_GREATERTHAN = r'\>'
t_LESSEQUAL = r'\<='
t_GREATEREQUAL = r'\>='
t_NOTEQUAL = r'\!='
t_EQUALS = r'\=='
t_NOT = r'\NOT'
t_AND = r'\AND'
t_OR = r'\OR'


t_ignore = r' '#ignoring spaces

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# def t_FLOAT(t):
#     r'\d+'
#     t.value = float(t.value)
#     return t

def t_DOUBLE(t):
    r'\d+\.\d+' #any character followed by a dot and zero or many characters
    t.value = float(t.value)
    return t

def t_NAME(t): #variable name
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

def t_BOOL(t):
    r'true|false'
    return t

def t_error(t):
    print("illegal characters")
    t.lexer.skip(1)

lexer = lex.lex()

lexer.input("1+2")

while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)
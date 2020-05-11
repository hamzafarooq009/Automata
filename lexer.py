import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [

    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',


    
    
    'DOUBLE',
    'CHAR',
    'STRING',
    'BOOL',

]

t_PLUS = r'\+'
t_MINUS = r'\-'
t_DIVIDE = r'\/'
t_MULTIPLY = r'\*'
t_EQUALS = r'\='

t_ignore = r' '#ignoring spaces

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_FLOAT(t):
    r'\d+'
    t.value = float(t.value)
    return t

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
import ply.yacc as yacc
import lexer
import ply.lex as lex
tokens = lexer.tokens

#setting up precedence to reduce the 
precedence = (
    ('left','PLUS','MINUS'),
    ('left','MULTIPLY','DIVIDE') #divide and multiply have higher precedence than PLUS and MINUS
)

#making grammer
#statements
#parsing display

def p_statement(p):
    'element : stmt SEMICOLON'
    p[0] = ('stmt', p[1])

#optparams could be an expression or statement as well 
def p_stmt_display(p):
    'stmt : DISPLAY LROUND optparams RROUND'
    p[0] = ("display", p[3])
    # print("display step 1 call: ", p[3])
    
def p_optparams(p):
    'optparams : params'
    p[0] = p[1]
    # print("Main display1: ", p[1])

def p_optparams_empty(p):
    'optparams : '
    p[0] = []
    # print("Main display2: ", p[0])

#params could be exp
def p_params(p):
    'params : exp COMMA params'
    p[0] = p[1] + p[3]

def p_params_exp(p):
    'params : exp'
    p[0] = p[1]
    print("hello: params")

#assignment x = 5
def p_stmt_assignment(p):
    'stmt : IDENTIFIER ASSIGN exp'
    p[0] = ('assign', p[1], p[3])

#assignment initialization int x
def p_stmt_init_empty(p):
    'stmt : TYPE IDENTIFIER'
    p[0] = ('init', p[1], p[2], None)

#statement initialization int x = 12
def p_stmt_init_assign(p):
    'stmt : TYPE IDENTIFIER ASSIGN exp'
    p[0] = ('init', p[1], p[2], p[4])

# Expression
def p_stmt_exp(p):
    'stmt : exp'
    p[0] = ('exp', p[1])

#expressions
def p_exp_para(p):
    'exp : LROUND exp RROUND'
    p[0] = ('exp', p[2])
#x
def p_exp_identifier(p):
    'exp : IDENTIFIER'
    p[0] = ("identifier",p[1])
# Types
def p_exp_string(p):
    'exp : STRING'
    p[0] = ("string", p[1])
def p_exp_bool(p):
    'exp : BOOL'
    p[0] = ("bool", p[1])
def p_exp_int(p):
    'exp : INT'
    p[0] = ("int", p[1])
def p_exp_double(p):
    'exp : DOUBLE'
    p[0] = ("double", p[1])
def p_exp_char(p):
    'exp : CHAR'
    p[0] = ("char", p[1])

# #aritmetic
def p_exp_plus(p):
    'exp : exp PLUS exp'
    p[0] = ("plus", p[1], p[3])
    # print("plus :", p[1], p[3])
    
def p_exp_minus(p):
    'exp : exp MINUS exp'
    p[0] = ("minus", p[1], p[3])
def p_exp_multiply(p):
    'exp : exp MULTIPLY exp'
    p[0] = ("multiply", p[1], p[3])
def p_exp_divide(p):
    'exp : exp DIVIDE exp'
    p[0] = ("divide", p[1], p[3])
def p_exp_not(p):
    'exp : NOT exp'
    p[0] = ("not", p[2])
def p_exp_and(p):
    'exp : exp AND exp'
    p[0] = ("and", p[1], p[3])
def p_exp_or(p):
    'exp : exp OR exp'
    p[0] = ("or", p[1], p[3])

def p_error(p):
    # print("lelel", p)
    print("Syntax Error in Input!")

# tokenizer
# LEXER TESTING
# alexer = lex.lex(module=lexer)
# alexer.input("1+1")
# while True:
#     tok = alexer.token()
#     if not tok:
#         break
#     print(tok)


# myLexer = lex.lex(module=lexer)
# myParser = yacc.yacc()

# while True:
#     try:
#         x = input('')
#     except EOFError:
#         break
#     tuple = myParser.parse(x, lexer=myLexer)
#     print(tuple)

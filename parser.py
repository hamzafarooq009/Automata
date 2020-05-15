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
    print("display step 1 call: ", p[3])
    
def p_optparams(p):
    'optparams : params'
    p[0] = p[1]
    print("Main display1: ", p[1])

def p_optparams_empty(p):
    'optparams : '
    p[0] = []
    print("Main display2: ", p[0])

#params could be exp
def p_params(p):
    'params : exp COMMA params'
    p[0] = p[1] + p[3]

def p_params_exp(p):
    'params : exp'
    p[0] = p[1]
    # print("hello: params")

#in order to use if elseif and else we need to make a compound statment
'''
IF ELSEIF ELSE////////////////////////////////////////////////
'''
def p_if_elseif_else(p):
    'stmt : if elseif else'
    p[0] = ('if', p[1], p[2], p[3])
    # print("if else says hello")

def p_if(p):
    'if : IF exp c_stmt'
    p[0] = ('exp', p[2], p[3])
    # print(p[2])

def p_if_elseif(p):
    'elseif : ELSEIF c_exp c_stmt elseif'
    p[0] = ('elseif', p[2], p[3])

def p_c_exp(p):
    'c_exp : exp'
    p[0] = ('exp', p[1])
#if there is no elseif
def p_if_elseif_empty(p):
    'elseif : '
    p[0] = []

def p_else(p):
    'else : ELSE c_stmt'
    p[0] = ('else', p[2])
    # print("in else", p[2])

def p_else_empty(p):
    'else : '
    p[0] = []

def p_compoundstmt(p):
    'c_stmt : LCURLY c_stmt RCURLY'
    p[0] = p[2]
    # print(p[2])

def p_compoundstmt_stmt(p):
    'c_stmt : stmt'
    p[0] = ('stmt', p[1])

def p_compoundstmt_empty(p):
    'c_stmt : '
    p[0] = ''
'''
/////////////////////////////////////////////////////////
'''

'''
list parser//////////////////////////////////////////////
'''
# int x = [optparams]
def p_stmt_init_list(p):
    # print("initial list")
    'stmt : TYPE IDENTIFIER ASSIGN LSQBRAC listparams RSQBRAC'
    p[0] = ('list_initialization', p[1], p[2], [p[5]])

#////////////LIST PARAMS////////////////////////////////////
def p_listcomma(p):
    'l_comma : COMMA'
    p[0] = p[1]

def p_listcomma_empty(p):
    'l_comma : '
    p[0] = ''

def p_listparams_int(p):
    'listparams : INT l_comma listparams'
    p[0] = p[1]

def p_listparams_double(p):
    'listparams : DOUBLE l_comma listparams'
    p[0] = p[1]

def p_listparams_string(p):
    'listparams : STRING l_comma listparams'
    p[0] = p[1]

def p_listparams_char(p):
    'listparams : CHAR l_comma listparams'
    p[0] = p[1]

def p_listparams_bool(p):
    'listparams : BOOL l_comma listparams'
    p[0] = p[1]

def p_listparams_empty(p):
    'listparams : '
    p[0] = []
#/////////////////////////////////////////////////////////////

#a[optparams] to get the value a[1]
def p_list_exp(p):
    'exp : IDENTIFIER LSQBRAC optparams RSQBRAC'
    p[0] = ('listindex', p[1], p[3])

#calling the list functions
def p_list_func(p):
    'exp : IDENTIFIER DOT list_functions '
    p[0] = ('listfunc', p[1], p[3])

def p_list_func_push(p):
    'list_functions : PUSH LROUND pparams RROUND'
    p[0] = ('push', p[3])

# a.pop(position)
def p_list_func_pop(p):
    'list_functions : POP LROUND pparams RROUND'
    p[0] = ('pop', p[3])

#A.slice(start_index, end_index)->
def p_list_func_slice(p):
    'list_functions : SLICE LROUND sliceparams RROUND'
    p[0] = ('slice', p[3])

def p_sliceparams(p):
    'sliceparams : INT'
    p[0] = (p[1],None)
def p_sliceparams_comma(p):
    'sliceparams : INT COMMA INT'
    p[0] = (p[1],p[3])
def p_sliceparams_empty(p):
    'sliceparams : '
    p[0] = (None, None)

# a.index('hamza') gives index of 'hamza' in list a 
def p_list_func_index(p):
    'list_functions : INDEX LROUND indexparams RROUND'
    p[0] = ('index', p[3])

def p_indexparams(p):
    'indexparams : exp'
    p[0] = ('exp', p[1])

def p_pparams(p):
    'pparams : exp'
    p[0] = ('exp', p[1])

def p_pparams_empty(p):
    'pparams : '
    p[0] = ('no args')

'''
/////////////////////////////////////////////////////////
'''
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
    'stmt : TYPE IDENTIFIER ASSIGN exp' #bool a = False
    p[0] = ('init', p[1], p[2], p[4])

# Expression
def p_stmt_exp(p):
    'stmt : exp'
    p[0] = ('exp', p[1])

#expressions
def p_exp_para(p):
    'exp : LROUND exp RROUND'
    p[0] = ('exp', p[2])

def p_exp_bool(p):
    'exp : BOOL'
    p[0] = ("bool", p[1])

#x
def p_exp_identifier(p):
    'exp : IDENTIFIER'
    p[0] = ("identifier",p[1])
# Types
def p_exp_string(p):
    'exp : STRING'
    p[0] = ("string", p[1])
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

#boolian
def p_exp_lessthan(p):
    'exp : exp LESSTHAN exp'
    p[0] = ('lessthan', p[1], p[3])
    print(p[0])
def p_exp_greaterthan(p):
    'exp : exp GREATERTHAN exp'
    p[0] = ('greaterthan', p[1], p[3])
def p_exp_lessequal(p):
    'exp : exp LESSEQUAL exp'
    p[0] = ('lessequal', p[1], p[3])
def p_exp_greaterequal(p):
    'exp : exp GREATEREQUAL exp'
    p[0] = ('greaterthan', p[1], p[3])
def p_exp_notequal(p):
    'exp : exp NOTEQUAL exp'
    p[0] = ('notequal', p[1], p[3])
def p_exp_equal(p):
    'exp : exp EQUALS exp'
    p[0] = ('equal', p[1], p[3])
def p_exp_power(p):
    'exp : exp POW exp'
    p[0] = ('pow', p[1], p[3])

def p_error(p):
    # print("lelel", p)
    print("Syntax Error in Input!")

# myLexer = lex.lex(module=lexer)
# myParser = yacc.yacc()

# while True:
#     try:
#         x = input('>>')
#     except EOFError:
#         break
#     tuple = myParser.parse(x, lexer=myLexer)
#     print(tuple)

import parser
import ply.lex as lex
import ply.yacc as yacc
import sys

'''
-our env cannot be a flat mapping, cant use single dictionary, need to set up the scope of a variable
-making a function call creates a new storage box
-env -> keep storage places of variables| making variable name to values (x:2)
-every env except the global one have parent env
-global environment -keeping track of all the env
-on function call
-local environment -keep track of local env (global env as parent env)
'''
'''
-need to keep the states and to do enviroment loop to know the state value
-calling it as:
-e = {"x":2, "y":3}
-env_loopup(e,"y") which will give us 3
-variable lookup
'''
#making global enviroment

#env = (parent_pointer,dictionary)
def env_lookup(env, var_name):
    print("lookup_env: ", env)
    print("variable name: ", var_name)
    if var_name in env: #do we have it?
        return env[var_name] 
    elif var_name not in env: #if no parent exist in the env
        return None
    else: #if not in current env and global then check other envs
        return env_lookup(env[0],var_name)


#there is a need for an env to declare variables:
def env_declare(env, var_name, var_type):
    #here there could be 2 scenerios
    #1 this var is already present in envs(redeclaration)
    #2 no previous record
    
    var_env = env_lookup(env, var_name) 

    if var_env == None:
        #creating dictionary
        env[var_name] = {}
        env['parent'] = None
        env[var_name]['type'] = var_type
        env[var_name]['value'] = -69 #need to set an arbitary value
        # print("declarations: ", env[var_name])
        # print("env : ", env)
        return env
    else:
        raise Exception('Redeclaration Error')


#env = (parent_pointer,dictionary)
# #env_update(env, x, 15)
def env_update(env, var_name, updated_value):
    #first need to check the variable env
    #after getting the env
    var_env = env_lookup(env, var_name)

    #if env lookup return none means that there is no declaration matlab ke no global mein x nhi declare tou uski value kese update ho skti
    #in this case error dena ke var_name is not declared
    if var_env == None:
        print(var_name," is not declared")
    
    #but if var exists in env tou value, type update kardo
    else:   
        var_env['type'] = updated_value['type']
        var_env['value'] = updated_value['value']
        # print(var_env)
        return var_env

#x = 2+2
def eval_stmts(tree, env):
    print("env at the begining: ", env)
    stmt_type = tree[0]
    
    if stmt_type == "assign": #x = 5+4 # x = exp
        var_name = tree[1]
        right_child_exp = tree[2]
        #since we dont know about if the right child is an expression or a simple number
        #send it to eval_exp
        # eval_exp((2+4), env)
        new_value = eval_exp(right_child_exp,env)

        #new_value is returned need to change it in the environment
        #now there must be an env update to update the corresponding vals
        env_update(env,var_name,new_value)
    elif stmt_type == "display":
        pass
    
    elif stmt_type == "init": #int x, int x = 5, int x = 15+1
        # print("tree in init: ", tree)
        var_type = tree[1]
        var_name = tree[2]
        # print("Initialization call: ", var_type, " ", var_name)
        returned_env = env_declare(env,var_name,var_type)
        if (tree[3] == None):
            pass
        else:
            #if its not none then it must be an expression, need to evaluate it
            # print("sending expression," , tree[3], " in eval_exp")
            expression = tree[3] #a + b
            new_val = eval_exp(expression,returned_env)
            # print("returned env from new value: ", new_val)
            #new value is returned, now need to update the env
            returned_env = env_update(returned_env,var_name,new_val)
            print("updated env: ", returned_env)
            return returned_env
            # print("updated_env: ", returned_env)
            # print("env: ", env)
            
    elif stmt_type == "exp":
        print("before calling eval_exp, currently in eval_stmts: ", tree)
        #tree[0] is exp
        exp = tree
        eval_exp(exp,env)


def eval_exp(tree,env):
    print("aval_exp env: ", env)
    node_type = tree[0]
    print("node_type in eval_Exp: ", node_type)
    
    #expressions
    if node_type == "exp":
        print("value in node_type == exp: ", tree)
        exp_tree = tree[1] #in this way skipping exp and running rest if the expression
        eval_exp(exp_tree, env)

    elif node_type == "func": #list
        pass
    #identifier
    elif node_type == "identifier":
        var_name = tree[1]
        return env_lookup(env,var_name)
        
    #arithmetic operations
    elif node_type == "plus":
        #receiving (('int', 1), ('int', 1))
        left_child = tree[1]
        right_child = tree[2]
        print("left_child : ", left_child)
        print("right_child : ", right_child)

        
        #need to run recursive call
        left_value = eval_exp(left_child,env)
        print("left value in plus: ", left_value)
        right_value = eval_exp(right_child,env)
        print("right value in plus: ", right_value)

        #now to add the left and right child
        lc_type, lc_value = left_value['type'], left_value['value']
        rc_type, rc_value = right_value['type'], right_value['value']

        #now need to check if the data type are okay to add or not

        #{int,double,char,string,bool}
        
        if lc_type == rc_type:
            print("added value and types\n", {'value' : (lc_value + rc_value), 'type': lc_type })
            # print(lc_value + rc_value)
            return {'value' : lc_value + rc_value, 'type': lc_type }
        elif lc_type == "int" & rc_type == "double":
            return {'value' : float(lc_value + rc_value), 'type': rc_type }
        elif lc_type == "double" & rc_type == 'int':
            return {'value' : float(lc_value + rc_value), 'type': lc_type }
        elif lc_type == "bool" & rc_type == 'bool':
            print("cant add bools")
        else:
            print("types does not match!")

    elif node_type == "minus":
        left_child = tree[1]
        right_child = tree[2]
        #need to run recursive call
        left_value = eval_exp(left_child,env)
        right_value = eval_exp(right_child,env)

        #now to add the left and right child
        lc_type, lc_value = left_value['type'], left_value['value']
        rc_type, rc_value = right_value['type'], right_value['value']

        #now need to check if the data type are okay to add or not

        #{int,double,char,string,bool}
        
        if lc_type == rc_type:
            return {'value' : lc_value - rc_value, 'type': lc_type }
        elif lc_type == "int" & rc_type == 'double':
            return {'value' : float(lc_value - rc_value), 'type': rc_type }
        elif lc_type == "double" & rc_type == 'int':
            return {'value' : float(lc_value - rc_value), 'type': lc_type }
        elif lc_type == "bool" or rc_type == 'bool':
            print("cant subtract bools")
        else:
            print("types does not match!")

    elif node_type == "multiply":
        left_child = tree[1]
        right_child = tree[2]
        #need to run recursive call
        left_value = eval_exp(left_child,env)
        right_value = eval_exp(right_child,env)
        #now to add the left and right child
        lc_type, lc_value = left_value['type'], left_value['value']
        rc_type, rc_value = right_value['type'], right_value['value']
        #now need to check if the data type are okay to add or not
        #{int,double,char,string,bool}
        
        if lc_type == rc_type:
            return {'value' : lc_value * rc_value, 'type': lc_type }
        elif lc_type == "int" & rc_type == 'double':
            return {'value' : float(lc_value * rc_value), 'type': rc_type }
        elif lc_type == "double" & rc_type == 'int':
            return {'value' : float(lc_value * rc_value), 'type': lc_type }
        elif lc_type == "bool" or rc_type == 'bool':
            print("cant multiply bools")
        elif lc_type == "string" or rc_type == 'string':
            print("cant multiply string types")
        elif lc_type == "char" or rc_type == 'char':
            print("cant multiply chars")
        else:
            print("types does not match!")

    elif node_type == "divide":
        left_child = tree[1]
        right_child = tree[2]
        #need to run recursive call
        left_value = eval_exp(left_child,env)
        right_value = eval_exp(right_child,env)

        #now to add the left and right child
        lc_type, lc_value = left_value['type'], left_value['value']
        rc_type, rc_value = right_value['type'], right_value['value']

        #now need to check if the data type are okay to add or not

        #{int,double,char,string,bool}
        
        if lc_type == rc_type:
            return {'value' : lc_value / rc_value, 'type': lc_type }
        elif lc_type == "int" & rc_type == 'double':
            return {'value' : float(lc_value / rc_value), 'type': rc_type }
        elif lc_type == "double" & rc_type == 'int':
            return {'value' : float(lc_value / rc_value), 'type': lc_type }
        elif lc_type == "bool" or rc_type == 'bool':
            print("cant multiply bools")
        elif lc_type == "string" or rc_type == 'string':
            print("cant multiply string types")
        elif lc_type == "char" or rc_type == 'char':
            print("cant multiply chars")
        else:
            print("types does not match!")

    elif node_type == "not":
        exp_tree = tree[1]
        lc = eval_exp(exp_tree,env)
        updated_value = not lc['value']
        updated_type = bool
        return {'value': updated_value, 'type': updated_type }
    
    elif node_type == "and":
        left_tree = tree[1]
        right_tree = tree[2]
        lc = eval_exp(left_tree,env)
        rc = eval_exp(right_tree,env)
        
        updated_value = lc and rc
        updated_type = bool
        return {'value': updated_value , 'type': updated_type}
        
    elif node_type == "or":
        left_tree = tree[1]
        right_tree = tree[2]
        lc = eval_exp(left_tree,env)
        rc = eval_exp(right_tree,env)
        
        updated_value = lc or rc
        updated_type = bool
        return {'value': updated_value , 'type': updated_type}
        
    #types
    elif node_type == "bool":
        value = tree[1]
        if value == True:
            return {'value': True, 'type': 'bool'}
        if value == False:
            return {'value': False, 'type': 'bool'}

    elif node_type == "double":
        updated_value = float(tree[1])
        return {'value': updated_value, 'type': 'double'}
    elif node_type == "int":
        updated_value = int(tree[1])
        print("updated value : ", updated_value)
        return {'value': updated_value, 'type': 'int'}
    elif node_type == "string":
        updated_value = str(tree[1])
        print("updated value : ", updated_value)
        return {'value': updated_value, 'type': 'string'}
        


def interpreter(trees, parent_env, env):
    #firstly need to initialize an enviroment
    #in env we have parent pointer an a dictionary
    # print("interpreter tree: ", trees)
    
    if parent_env is not None:
        env[0] = parent_env
    
    #have 2 option, one is being a statement and the other one is being an expression
    # for tree in trees:
        # print(tree)
    node_type = trees[0]
    # print("node_type: ", node_type)
    if node_type == "stmt":
        # print("Calling evaluating statements")
        env = eval_stmts(trees[1], env)
        # print("env in interpreter: ", env)
    else:
        print("Unkown node type : ", node_type)

# def file_read(filename):
#     with open(filename, 'r') as f:
#         data = f.read()
#     return data

def main():
    # token = file_read(sys.argv[0]) 
    # parser = file_read(sys.argv[1])

    print("Welcome to Urdu x pikachu language")
    env = {}
    yaplParser = yacc.yacc(module = parser)

    while True:
        try:
            x = input('>> ')
        except EOFError:
            break
        trees = yaplParser.parse(x)
        
        interpreter(trees, None,env)
main()
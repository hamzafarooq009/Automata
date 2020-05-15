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
    # print("received tree: ", tree)
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
            # print("updated env: ", returned_env)
            return returned_env
            # print("updated_env: ", returned_env)
            # print("env: ", env)
            
    elif stmt_type == "exp":
        # print("before calling eval_exp, currently in eval_stmts: ", tree)
        #tree[0] is exp
        exp = tree
        eval_exp(exp,env)
    elif stmt_type == "if":
        # print("IN IF BLOCK: ", tree)
        #there will be three categories
        if_stmt = tree[1]
        elseif_stmt = tree[2]
        else_stmt = tree[3]
        # print("elseifstmt: ", _stmt)
        # print()
        #4 scenerios
        # s#1 if
        # print("only if stmt: ", if_stmt)
        expression = if_stmt[1]
        statement = if_stmt[2]
        # print(expression)
        returned_value = eval_exp(expression,env)
        # print("returned value in if: ", returned_value)
        
        if returned_value['value'] == True:
            # print("statement: ", statement)
            updated_env = eval_stmts(statement[1],env) #for statement
            # print(updated_env)
        else:
            # print("calling eval_stmts on elseif: ", elseif_stmt)
            
            #need to check ke we have elseif or not
            if elseif_stmt[0] == 'elseif':
                elseif_updated_env = eval_stmts(elseif_stmt,env)
                if elseif_updated_env == None and else_stmt != []:
                    updated_env = eval_stmts(else_stmt,env)
                    # print("updated env : ", updated_env)
                    return updated_env
                else:
                    return None
        
    elif stmt_type == "elseif":
        #2things expression and statement
        expression = tree[1]
        statement = tree[2]
        # print("expression : ", expression[1])
        returned_value = eval_exp(expression[1],env)
        # print("returned value in elseif: ", returned_value)
        if returned_value['value'] == True:
            # print("statement: ", statement)
            updated_env = eval_stmts(statement[1],env)
            # print(updated_env)
            return updated_env
        else:
            return None

    elif stmt_type == "else":
        # print(tree)
        statement = tree[1]
        # print("statment in else: ", statement[1])
        returned_value = eval_stmts(statement[1],env)
        # print("returned value: ", returned_value)
        return returned_value
        
    elif stmt_type == "list_initialization":
        var_type = tree[1]
        var_name = tree[2]
        returned_env = env_declare(env,var_name,var_type)
        if (tree[3] == None):
            pass
        else:
            expression = tree[3] #[1,2,3,4]
            #what if [1+2,3+2]given?
            returned_env = env_update(returned_env,var_name,expression)
            return returned_env

    elif stmt_type == "listindex": #x[2], x[arg]        
        var_name = tree[1]
        arg = tree[2]

        objlist = env_lookup(env, var_name)
        #{value: [1,2,3], type: 'int'}
        final_list = objlist['value']
        final_index = final_list.index(arg)
        # print("index: ", final_index)
    
    elif stmt_type == "listfunc":
        var_name = tree[1]
        list_functions = tree[2]
        func_name = list_functions[0]
        list_params = list_functions[1]

        if func_name == 'push':
            env_list = env_lookup(env, var_name)
            final_list = env_list['value'].push(list_params)
            returned_env = env_update(env, var_name, final_list)
    
        elif func_name == 'pop': #
            # default list(-1)
            if list_params == 'no args':
                env_list = env_lookup(env, var_name)
                final_list = env_list['value'].pop()
                returned_env = env_update(env, var_name, final_list)
            else:
                env_list = env_lookup(env, var_name)
                final_list = env_list['value'].pop(list_params)
                returned_env = env_update(env, var_name, final_list)

        elif func_name == 'index':
            env_list = env_lookup(env, var_name)
            index = env_list['value'].index(list_params)
            print("index is : ", index)
        elif func_name == 'slice':
            slice_params = tree[1]
            start_index = slice_params[0]
            end_index = slice_params[1]

            if len(slice_params) == 0:
                print("ERROR: TypeError: slice expected at least 1 arguments, got 0")
            elif len(slice_params) == 1:
                env_list = env_lookup(env, var_name)
                final_list = env_list['value'].slice(start_index)
                returned_env = env_update(env, var_name, final_list)
            elif len(slice_params) == 2:
                env_list = env_lookup(env, var_name)
                final_list = env_list['value'].slice(start_index, end_index)
                returned_env = env_update(env, var_name, final_list)



def eval_exp(tree,env):
    # print("aval_exp env: ", env)
    # print("tree: ", tree)
    node_type = tree[0]
    # print("node_type in eval_Exp: ", node_type)
    
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
        print("bool call: ", tree)
        value = tree[1]
        if value == 'true':
            return {'value': True, 'type': 'bool'}
        if value == 'false':
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
        
    #boolian
    elif node_type == "GREATEREQUAL":
        left_child = eval_exp(tree[1], env)
        right_child = eval_exp(tree[2], env)
        value = left_child['value'] >= right_child['value']
        v_type = bool
        return {'value': value, 'type': 'bool'}
    
    elif node_type == "lessequal":
        left_child = eval_exp(tree[1], env)
        right_child = eval_exp(tree[2], env)
        value = left_child['value'] <= right_child['value']
        v_type = bool
        return {'value': value, 'type': v_type}
    elif node_type == "lessthan":
        left_child = eval_exp(tree[1], env)
        right_child = eval_exp(tree[2], env)
        print("left child in less than: ", left_child)
        print("right child in less than: ", right_child)
        value = left_child['value'] < right_child['value']
        print("value in less than: ", value)
        v_type = 'bool'
        
        return {'value': value,'type': v_type}
    
    elif node_type == "notequal":
        left_child = eval_exp(tree[1], env)
        right_child = eval_exp(tree[2], env)
        value = left_child['value'] != right_child['value']
        v_type = bool
        
        return {'value': value, 'type': v_type}
    elif node_type == "equal":
        left_child = eval_exp(tree[1], env)
        right_child = eval_exp(tree[2], env)
        value = left_child['value'] == right_child['value']
        v_type = bool
        
        return {'value': value, 'type': v_type}
    elif node_type == "greaterthan":
        left_child = eval_exp(tree[1], env)
        right_child = eval_exp(tree[2], env)
        value = left_child['value'] > right_child['value']
        # print("value: ", value)
        v_type = 'bool'
        return {'value': value, 'type': v_type}
    
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
        # print("Calling evaluating statements", trees[1])
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
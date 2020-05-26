import parser
import ply.lex as lex
import ply.yacc as yacc
import sys
import os
# from fabulous.color import bold, magenta, highlight_red

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
    try:
        if var_name in env: #do we have it?
            # print("lookup: ", env)
            return env
        elif var_name not in env: #if no parent exist in the env
            
            return None
        else: #if not in current env and global then check other envs
            return env_lookup(env[0],var_name)
    except:
        print("Error in look ups")

#there is a need for an env to declare variables:
def env_declare(env, var_name, var_type):
    #here there could be 2 scenerios
    #1 this var is already present in envs(redeclaration)
    #2 no previous record
    try:
        var_env = env_lookup(env, var_name) 

        if var_env == None:
            #creating dictionary
            env[var_name] = {}
            env['parent'] = None
            env[var_name]['type'] = var_type
            env[var_name]['value'] = -69 #need to set an arbitary value
            return env
        else:
            raise Exception('Redeclaration Error')
    except:
        print("Error in declaration")

#env = (parent_pointer,dictionary)
# #env_update(env, x, 15)
def env_update(env, var_name, updated_value):
    #first need to check the variable env
    #after getting the env
    try:
        var_env = env_lookup(env, var_name)

        #if env lookup return none means that there is no declaration matlab ke no global mein x nhi declare tou uski value kese update ho skti
        #in this case error dena ke var_name is not declared
        if var_env == None:
            print(var_name," is not declared")
        
        #but if var exists in env tou value, type update kardo
        else:
            var_env['type'] = updated_value['type']
            var_env['value'] = updated_value['value']
            return var_env
    except:
        print("Error in environment update")

#x = 2+2
def eval_stmts(tree, env):
    print("env at the begining: ", env)
    print("received tree in eval_stmts: ", tree)
    stmt_type = tree[0]
    
    if stmt_type == "assign": #x = 5+4 # x = exp
        try:
            var_name = tree[1]
            exp = tree[2]
            #since we dont know about if the right child is an expression or a simple number
            #send it to eval_exp
            # eval_exp((2+4), env)
            var_env = env_lookup(env, var_name)
            new_env = eval_exp(exp,env)
            
            value = new_env['value']
            v_type = new_env['type']
            var_env[var_name]['value'] = value
            var_env[var_name]['type'] = v_type
            
            #new_value is returned need to change it in the environment
            #now there must be an env update to update the corresponding vals
            # updated_env = env_update(env,var_name,new_value)
            # print("var_env: ", var_env)
            return var_env
        except:
            print("Error in assignment")
    elif stmt_type == "display": #recheck
        # display_items = tree[1]
        # (a(a(a(a))))
        try:
            # print("tree : ", tree[1])
            result = ""
            for branch in tree[1]:
                fetch = eval_exp(branch,env)
                # print("fetched data : ", fetch)
                result = result + str(fetch['value']) + " "
            print(result)
        except:
            print("Error in display")

        
    elif stmt_type == "init": #int x, int x = 5, int x = 15+1
        try:
            print("tree in init: ", tree)
            var_type = tree[1]
            var_name = tree[2]
            expression = tree[3]
            # print("Initialization call: ", var_type, " ", var_name)
            returned_env = env_declare(env,var_name,var_type)
            if (tree[3] == None):
                pass
            else:
                #if its not none then it must be an expression, need to evaluate it
                new_env = eval_exp(expression,returned_env)
                #new value is returned, now need to update the env
                print("new_eve in INIT is : ", new_env)
                value = new_env['value']
                v_type = new_env['type']
                returned_env[var_name]['value'] = value
                returned_env[var_name]['type'] = v_type
                # print("returnedenv: ", returned_env)
                return returned_env
        except:
            print("Error in initialization")    
    elif stmt_type == "exp":
        try:
            # print("before calling eval_exp, currently in eval_stmts: ", tree)
            #tree[0] is exp
            exp = tree
            eval_exp(exp,env)
        except:
            print("Error in calling expression")
    elif stmt_type == "if":
        try:
            #there will be three categories
            if_stmt = tree[1]
            elseif_stmt = tree[2]
            else_stmt = tree[3]
            expression = if_stmt[1]
            statement = if_stmt[2]
            returned_value = eval_exp(expression,env)
            
            if returned_value['value'] == True:
                updated_env = eval_stmts(statement[1],env) #for statement
            else:
                # print("calling eval_stmts on elseif: ", elseif_stmt)
                
                #need to check ke we have elseif or not
                if elseif_stmt != []:
                    # print("else if stmt : ", elseif_stmt)
                    elseif_updated_env = eval_stmts(elseif_stmt,env)
                    if elseif_updated_env == False and else_stmt != []:
                        updated_env = eval_stmts(else_stmt,env)
                        # print("updated env : ", updated_env)
                        return updated_env
                    else:
                        return None
                elif else_stmt != []:
                    updated_env = eval_stmts(else_stmt,env)
                    return updated_env
        except:
            print("Error in if block")
    elif stmt_type == "elseif":
        try:
            #2things expression and statement
            expression = tree[1]
            statement = tree[2]
            returned_value = eval_exp(expression[1],env)
            if returned_value['value'] == True:
                print("statement: ", statement)
                updated_env = eval_stmts(statement[1],env)
                return updated_env
            else:
                return None
        except:
            print("Error in elseif block")
    elif stmt_type == "else":
        try:
            statement = tree[1]
            returned_value = eval_stmts(statement[1],env)
            return returned_value
        except:
            print("Error in else block")
    elif stmt_type == "list_initialization": #int a = [1,2]
        try:
            var_type = tree[1]
            var_name = tree[2]
            print("list ini: ", tree)
            returned_env = env_declare(env,var_name,var_type)
            print("returned in init", returned_env)
            if (tree[3] == None):
                pass
            else:
                v_list = tree[3]
                # print("v_list: ", v_list)
                returned_env[var_name]['value'] = v_list[0]
                print("returned: ", returned_env)
                return returned_env
        except:
            print("Error in list Initialization")

def eval_exp(tree,env):
    # print("aval_exp env: ", env)
    # print("tree in eval exp: ", tree)
    node_type = tree[0]
    
    #expressions
    if node_type == "exp":
        try:
            # print("value in node_type == exp: ", tree)
            exp_tree = tree[1] #in this way skipping exp and running rest if the expression
            return eval_exp(exp_tree, env)
        except:
            print("Error in expression tree")
    #identifier
    elif node_type == "identifier":
        try:
            # print("TREE IN IDENTIFIER: ", tree)
            var_name = tree[1]
            # print(var_name)
            returned_env = env_lookup(env,var_name)
            # print("elo: ", returned_env)
            # print("RETURNED IN IDENTIFIER: ", returned_env[var_name])
            return returned_env[var_name]
        except:
            print("Error in Identifier")
    #arithmetic operations
    elif node_type == "plus":
        try:
            # print("plus: ", tree)
            left_child = tree[1]
            right_child = tree[2]
            # print("left_child : ", left_child)
            # print("right_child : ", right_child)

            
            #need to run recursive call
            left_value = eval_exp(left_child,env)
            # print("left value in plus: ", left_value)
            right_value = eval_exp(right_child,env)
            # print("right value in plus: ", right_value)

            #now to add the left and right child
            lc_type, lc_value = left_value['type'], left_value['value']
            rc_type, rc_value = right_value['type'], right_value['value']

            #now need to check if the data type are okay to add or not

            #{int,double,char,string,bool} 
            if lc_type == rc_type:
                if lc_type == 'string' and rc_type == 'string':
                    return {'value' : lc_value + " " + rc_value, 'type': lc_type }    
                else:
                    return {'value' : lc_value + rc_value, 'type': lc_type }
            elif lc_type == "int" or rc_type == "double":
                return {'value' : float(lc_value + rc_value), 'type': rc_type }
            elif lc_type == "double" or rc_type == 'int':
                return {'value' : float(lc_value + rc_value), 'type': lc_type }
            elif lc_type == "bool" & rc_type == 'bool':
                print("cant add bools")
                raise Exception('Adding bool error')
                
            else:
                print("types does not match!")
                raise Exception('Types mismatch')
        except:
            raise Exception('TypeError')
            print("TypeError")
    elif node_type == "minus":
        try:
            left_child = tree[1]
            right_child = tree[2]
            left_value = eval_exp(left_child,env)
            right_value = eval_exp(right_child,env)

            #now to add the left and right child
            lc_type, lc_value = left_value['type'], left_value['value']
            rc_type, rc_value = right_value['type'], right_value['value']

            #now need to check if the data type are okay to add or not

            #{int,double,char,string,bool}
            
            if lc_type == rc_type:
                return {'value' : lc_value - rc_value, 'type': lc_type }
            elif lc_type == "int" or rc_type == 'double':
                return {'value' : float(lc_value - rc_value), 'type': rc_type }
            elif lc_type == "double" or rc_type == 'int':
                return {'value' : float(lc_value - rc_value), 'type': lc_type }
            elif lc_type == "bool" and rc_type == 'bool':
                print("cant subtract bools")
                raise Exception('Subtracting bool error')
            else:
                print("types does not match!")
                raise Exception('types does not match!')
        except:
            print("Error in minus expression")
    elif node_type == "multiply":
        try:
            left_child = tree[1]
            right_child = tree[2]
            #need to run recursive call
            left_value = eval_exp(left_child,env)
            # print("left child: ", left_value)
            right_value = eval_exp(right_child,env)
            #now to add the left and right child
            lc_type, lc_value = left_value['type'], left_value['value']
            rc_type, rc_value = right_value['type'], right_value['value']
            #now need to check if the data type are okay to add or not
            
            if lc_type == rc_type:
                return {'value' : lc_value * rc_value, 'type': lc_type }
            elif lc_type == "int" or rc_type == 'double':
                return {'value' : float(lc_value * rc_value), 'type': rc_type }
            elif lc_type == "double" or rc_type == 'int':
                return {'value' : float(lc_value * rc_value), 'type': lc_type }
            elif lc_type == "bool" or rc_type == 'bool':
                print("cant multiply bools")
            elif lc_type == "string" or rc_type == 'string':
                print("cant multiply string types")
            elif lc_type == "char" or rc_type == 'char':
                print("cant multiply chars")
            else:
                print("types does not match!")
        except:
            print("Error in Multiplication expression")
    elif node_type == "divide":
        try:
            left_child = tree[1]
            right_child = tree[2]
            #need to run recursive call
            left_value = eval_exp(left_child,env)
            right_value = eval_exp(right_child,env)

            #now to add the left and right child
            lc_type, lc_value = left_value['type'], left_value['value']
            rc_type, rc_value = right_value['type'], right_value['value']

            #now need to check if the data type are okay to add or not

            
            if lc_type == rc_type:
                return {'value' : lc_value / rc_value, 'type': lc_type }
            elif lc_type == "int" or rc_type == 'double':
                return {'value' : float(lc_value / rc_value), 'type': rc_type }
            elif lc_type == "double" or rc_type == 'int':
                return {'value' : float(lc_value / rc_value), 'type': lc_type }
            elif lc_type == "bool" or rc_type == 'bool':
                print("cant multiply bools")
            elif lc_type == "string" or rc_type == 'string':
                print("cant multiply string types")
            elif lc_type == "char" or rc_type == 'char':
                print("cant multiply chars")
            else:
                print("types does not match!")
        except:
            print("Error in Division expression")
    elif node_type == "notequal":
        try:
            exp1 = tree[1]
            exp2 = tree[2]
            

            lc = eval_exp(exp1,env)
            rc = eval_exp(exp2,env)
            
            
            if lc['value'] == 0:
                updated_value = False != rc['value']
            elif lc['value'] == 1:
                updated_value = True != rc['value']
            if rc['value'] == 0:
                updated_value = lc['value'] != False
            if rc['value'] == 1:
                updated_value = lc['value'] != True
            
            updated_type = 'bool'
            
            return {'value': updated_value, 'type': updated_type }
        except:
            print("Error in not equal")
        
    elif node_type == "and":
        try:
            left_tree = tree[1]
            right_tree = tree[2]
            lc = eval_exp(left_tree,env)
            rc = eval_exp(right_tree,env)
            
            updated_value = lc and rc
            updated_type = 'bool'
            return {'value': updated_value['value'] , 'type': updated_type}
        except:
            print("Error in And operation")
        
    elif node_type == "or":
        try:
            left_tree = tree[1]
            right_tree = tree[2]
            lc = eval_exp(left_tree,env)
            rc = eval_exp(right_tree,env)
            
            updated_value = lc or rc
            updated_type = bool
            return {'value': updated_value['value'] , 'type': updated_type}
        except:
            print("Error in Or operation")
    #types
    elif node_type == "bool":
        try:
            value = tree[1]
            if value == 'True':
                return {'value': True, 'type': 'bool'}
            if value == 'False':
                return {'value': False, 'type': 'bool'}
        except:
            print("Error in bool operation")
    elif node_type == "double":
        try:
            updated_value = float(tree[1])
            return {'value': updated_value, 'type': 'double'}
        except:
            print("Error in decalaring double")
    elif node_type == "int":
        try:
            updated_value = int(tree[1])
            # print("updated value : ", updated_value)
            return {'value': updated_value, 'type': 'int'}
        except:
            print("Error in int declaration")
    elif node_type == "string":
        try:
            updated_value = str(tree[1])
            updated_value = updated_value[1:-1]
            # print("updated value : ", updated_value)
            # updated_value = updated_value[1:-1]
            return {'value': updated_value, 'type': 'string'}
        except:
            print("Error in string declaration")

    
    #boolian
    elif node_type == "greaterthan":
        try:
            left_child = eval_exp(tree[1], env)
            right_child = eval_exp(tree[2], env)
            value = left_child['value'] >= right_child['value']
            v_type = bool
            return {'value': value, 'type': 'bool'}
        except:
            print("Error in greater equal operation")
    elif node_type == "lessequal":
        try:
            left_child = eval_exp(tree[1], env)
            right_child = eval_exp(tree[2], env)
            value = left_child['value'] <= right_child['value']
            v_type = bool
            return {'value': value, 'type': v_type}
        except:
            print("Error in less equal")
    elif node_type == "lessthan":
        try:
            left_child = eval_exp(tree[1], env)
            right_child = eval_exp(tree[2], env)
            # print("left child in less than: ", left_child)
            # print("right child in less than: ", right_child)
            value = left_child['value'] < right_child['value']
            # print("value in less than: ", value)
            v_type = 'bool'
            
            return {'value': value,'type': v_type}
        except:
            print("Error in less than")
    elif node_type == "equal":
        try:
            left_child = eval_exp(tree[1], env)
            # print("left child in equal : ", left_child)
            right_child = eval_exp(tree[2], env)
            # print("right child in equal : ", right_child)
            value = left_child['value'] == right_child['value']
            v_type = 'bool'
            
            # print("value in equal: ", value)
            return {'value': value, 'type': v_type}
        except:
            print("Error in equal")
    elif node_type == "greaterthan":
        try:
            left_child = eval_exp(tree[1], env)
            right_child = eval_exp(tree[2], env)
            value = left_child['value'] > right_child['value']
            # print("value: ", value)
            v_type = 'bool'
            return {'value': value, 'type': v_type}
        except:
            print("Error in GT")
    elif node_type == "listindex": #x[2], x[arg]        
        try:
            # print("tree in list index: ", tree[2])
            index = [x[1] for x in tree[2]]
            var_name = tree[1]
            objlist = env_lookup(env, var_name)
            v_list = objlist[var_name]['value']

            if (len(v_list) < index[0]):
                print("Index out of bound")
            else:
                value_at_index = v_list[index[0]]
                print("Value at index: ", value_at_index)
                return {'value': value_at_index, 'type': 'int'}
        except:
            print("Error in list index")
    elif node_type == "listfunc":
        
        var_name = tree[1]
        list_functions = tree[2]
        # print("tree in list func: ", list_functions)
        func_name = list_functions[0]
        list_params = list_functions[1]

        if func_name == 'push':
            # print("in push bbay")
            # print("var name: ", var_name)
            # print("env in push: ",env)
            env_list = env_lookup(env, var_name)
            print("env list in push: ", env_list)
            
            print("list params in push: ", list_params)
            env_list[var_name]['value'].append(list_params[1])
            # print(env_list[var_name]['value'])
            print("final_returned_env push : ", env_list)
            return env_list
    
        elif func_name == 'pop': #
            print("pop tree: ", tree)
            var_name = tree[1]
            list_functions = tree[2]
            func_name = list_functions[0]
            list_index = list_functions[1]

            if list_params == 'no args':
                env_list = env_lookup(env, var_name)
                env_list[var_name]['value'].pop()
                # returned_env = env_update(env, var_name, final_list)
                return {'value' : env_list, 'type': 'int'}
            else:
                env_list = env_lookup(env, var_name)
                value = env_list[var_name]['value'].pop(list_index[1])
                print("pooped value : ", value)
                v_type = env_list[var_name]['type']
                print("v_type: ", value)
                return {'value' : value, 'type': v_type}

        elif func_name == 'index':
            print("tree in index: ", tree)
            try:
                print("tree in index: ", tree)
                var_name = tree[1]
                list_functions = tree[2]
                func_name = list_functions[0]
                list_params = list_functions[1]
                print("list params: ", list_params)
                v_type = list_params[1][0]
                v_value = list_params[1][1]
                env_list = env_lookup(env, var_name)
                if v_type == "string":
                    v_value = v_value[1:-1]
                    value = env_list[var_name]['value'].index(v_value)
                    print("hello: ", value)
                    return {'value': value, 'type': 'int'}
                else: 
                    value = env_list[var_name]['value'].index(list_params[1][1])
                    return {'value': value, 'type': 'int'}
            except:
                print("Error in index")

        elif func_name == 'slice':
            try:
                # print("tree in slice: ", tree)
                var_name = tree[1]
                list_functions = tree[2]
                func_name = list_functions[0]
                list_params = list_functions[1]
                start_index = list_params[0]
                end_index = list_params[1]

                if start_index == None and end_index == None:
                    print("ERROR: TypeError: slice expected at least 1 arguments, got 0")
                elif start_index != None and end_index == None:
                    env_list = env_lookup(env, var_name)
                    # print("start index: ", start_index)
                    slice_object = slice(start_index)
                    final_env = env_list[var_name]['value']
                    sliced = final_env[slice_object]
                    # print("final in slice: ", sliced)
                    return sliced
                elif start_index != None and end_index != None:
                    env_list = env_lookup(env, var_name)
                    slice_object = slice(start_index, end_index)
                    final_env = env_list[var_name]['value']
                    sliced = final_env[slice_object]
                    # print("final in slice: ", sliced)
                    return {'value': sliced, 'type': 'int'}
            except:
                print("Error in slice")

    elif node_type == "plusplus":
        try:
            var_name = tree[1]
            fetched_env = env_lookup(env, var_name)
            value = fetched_env[var_name]['value']
            value = value + 1
            fetched_env[var_name]['value'] = value
            # print("updated env: ", fetched_env)
            return fetched_env
        except:
                print("Error in plus plus")

    elif node_type == "minusminus":
        try:
            var_name = tree[1]
            fetched_env = env_lookup(env, var_name)
            value = fetched_env[var_name]['value']
            value = value - 1
            fetched_env[var_name]['value'] = value
            # print("updated env: ", fetched_env)
            return fetched_env
        except:
                print("Error in minus minus")

    elif node_type == "pow":
        v1 = tree[1]
        v2 = tree[2]
        lc = eval_exp(v1,env)
        rc= eval_exp(v2,env)
        print(lc,rc)
        return {'value': lc['value']**rc['value'], 'type': v1[0]}

    elif node_type == "not":
        var = tree[1]
        # print("in not True: ", var)
        return {'value' : not var[1], 'type': var[0]}
    
    elif node_type == "neg_type":
        if tree[1][0] == "identifier":
            var_name = tree[1][1]
            returned_env = env_lookup(env,var_name)
            # print("elo: ", returned_env)
            return {'value': -1*returned_env[var_name]['value'], 'type': returned_env[var_name]['type']}
        else:
            # print("else return: ", tree[1])
            return {'value': (-1*tree[1][1]) , 'type': tree[1][0]}

def interpreter(trees, env):
    #firstly need to initialize an enviroment
    #in env we have parent pointer an a dictionary
    # print("interpreter tree: ", trees)
    try:
        # if parent_env is not None:
        #     env[0] = parent_env
        
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
    except:
        print("Syntax Error (laughs in semi-colon)")

def main():
    print ("Welcome to Motu Cato language")
    env = {}

    yaplParser = yacc.yacc(module = parser)
    
    parent = None
    with open(os.path.join("test_cases", sys.argv[1]), "r") as file:
        data = file.readlines()
        for x in data:
            try:
                # print(x)
                text = yaplParser.parse(x.strip())
                # print(text)
                interpreter(text,env)
            except EOFError:
                break
        
            # print(text)
    # print(data)
    # for x in data:
    #     print(x);
    # l = data.readline();
    # for x in l:
    #     print (x)
    # parse_data = yaplParser.parse(data)
    # interpreter(parse_data,None,env)
    
    # print(data)
    # except EOFError:
    #     print("error in file reading")
    # print(data)
    # yaplParser = yacc.yacc(module = parser)
    # for x in data:
        # parse_data = yaplParser.parse(x)
        # print(x)
    # for x in parse_data:
    #     print("x is : ", x)
    # interpreter(parse_data,None,env)
    
    # yaplParser = yacc.yacc(module = parser)
    # while True:
    #     trees = yaplParser.parse(data)
    #     print(trees) 
    # while True:
    #     try:
    #         x = input('>> ')
    #     except EOFError:
    #         break
    #     trees = yaplParser.parse(x)
        
    #     interpreter(trees, None,env)
main()
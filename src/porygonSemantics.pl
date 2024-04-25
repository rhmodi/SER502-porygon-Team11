eval_blk(t_blk(X;Y),[[],[]],New_Env):-
    eval_decl(X,[[],[]], Env1),
    eval_cmd(Y,Env1,New_Env).

eval_decl(t_decl(X;Y),Env,New_Env):-
    eval_decl(X,Env,Env1),
    eval_decl(Y,Env1,New_Env).
eval_decl(t_decl(X),Env,New_Env):-
    eval_decl(X,Env,New_Env).

eval_decl(t_int_decl(X),Env,New_Env):-
    update((X,0),Env,New_Env).
eval_decl(t_str_decl(X),Env,New_Env):-
    update((X,""),Env,New_Env).
eval_decl(t_bool_decl(X),Env,New_Env):-
    update((X,false),Env,New_Env).
eval_decl(t_flt_decl(X),Env,New_Env):-
    update((X,0),Env,New_Env).

eval_decl(t_const_int(X,Y),Env,New_Env):-
    number(Y),
    %is_of_type(), check that it is not float and only integer
    init_const((X,Y),Env,New_Env).
eval_decl(t_const_flt(X,Y),Env,New_Env):-
    number(Y),
    init_const((X,Y),Env,New_Env).
eval_decl(t_const_bool(X,Y),Env,New_Env):-
    is_bool(Y), %write this predicate to check that Y is either true or false
    init_const((X,Y),Env,New_Env).
eval_decl(t_const_str(X,Y),Env,New_Env):-
    is_string(Y), %write this predicate to check that Y is a string enclosed in double quotes
    init_const((X,Y),Env,New_Env).

eval_decl(t_const_int_e(X,Y),Env,New_Env):-
    eval_expr(Y,Env, Env1,R),
    number(R),
    %is_of_type(), check that it is not float and only integer
    init_const((X,R),Env,New_Env).
eval_decl(t_const_flt_e(X,Y),Env,New_Env):-
    eval_expr(Y,Env, Env1,R),
    number(R),
    init_const((X,R),Env,New_Env).
eval_decl(t_const_bool_e(X,Y),Env,New_Env):-
    eval_bool(Y,Env, Env1,R),
    is_bool(R), %write this predicate to check that Y is either true or false
    init_const((X,R),Env,New_Env).
eval_decl(t_const_str_e(X,Y),Env,New_Env):-
    eval_str(Y,Env, Env1,R),
    is_string(R), %write this predicate to check that Y is a string enclosed in double quotes
    init_const((X,R),Env,New_Env).

%%%% TO DO %%%%%
is_bool(X).
is_string(X).
eval_str(Y). % Shall we do str concatenation?
%%%%%%%%%%%%%%%%

eval_cmd(t_cmd(X;Y),Env,New_Env):-
    eval_cmd(X,Env,Env1),
    eval_cmd(Y,Env1,New_Env).
eval_cmd(t_cmd(X),Env,New_Env):-
    eval_cmd(X,Env,New_Env).



lookUp(X,Env,Val):-
    flatten(Env, FEnv), %to include constant variables in search
    loop_Up(X, FEnv, Val).
look_Up(X,[],_Val):-
    write('Uninitialized variable: '),
    write(X),
    fail.
look_Up(HVar,[(HVar,HVal)|_T],HVal).
look_Up(X,[(HVar,_HVal)|T],Val):-
    X \= HVar,
    lookUp(X,T,Val).

update((Var,Val),[[],Const],[[(Var,Val)],Const]):-
    \+ look_Up(Var, Const, _). % avaoid update from creating duplicates of const vars
update((Var,Val),[[(HVar,HVal)|T],Const],[[(HVar,HVal)|R],Const]):-
    Var \= HVar,
    update((Var,Val),T,R).
update((Var,Val),[[(Var,_)|T],Const],[[(Var,Val)|T],Const]).

init_const((Var,Val),[_,[]],[_,[(Var,Val)]]).
init_const((Var,Val),[_,[(HVar,HVal)|T]],[_,[(HVar,HVal)|R]]):-
    Var \= HVar,
    update((Var,Val),T,R).
init_const((Var,Val),[_,[(Var,_)|T]],[_,[(Var,Val)|T]]).

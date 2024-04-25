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

eval_cmd(t_assign(X:=Y),Env,New_Env):-
    eval_expr(Y,Env, Env1,R),
    update((X,R),Env1,New_Env).

eval_cmd(t_ifte(X,Y,_Z),Env, New_Env):-
    eval_bool(X, Env, Env1, true),
    eval_cmd(Y, Env1, New_Env).

eval_cmd(t_ifte(X,_Y,Z),Env, New_Env):-
    eval_bool(X, Env, Env1, false),
    eval_cmd(Z, Env1, New_Env).

eval_cmd(t_while(X,_Y), Env, Env1):-
    eval_bool(X, Env, Env1, false).
eval_cmd(t_while(X,Y), Env, New_Env):-
    eval_bool(X, Env, Env1, true),
    eval_cmd(Y, Env1, Env2),
    eval_cmd(t_while(X,Y),Env2, New_Env).

eval_cmd(t_blk(X;Y),Env,New_Env):-
    eval_decl(X,Env, Env1),
    eval_cmd(Y,Env1,New_Env).

eval_bool(true,_Env,_New_Env,true).
eval_bool(false,_Env,_New_Env,false).
eval_bool(t_bool(not,X),Env,New_Env,R):-
    eval_bool(X, Env, New_Env, Bool),
    not(Bool,R).
eval_bool(t_bool(X=Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx = Ry,
    R = true.
eval_bool(t_bool(X=Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx \= Ry,
    R = false.

not(true,false).
not(false, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RELATIONAL OP evaluation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_bool(t_bool(X<Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx < Ry,
    R = true.
eval_bool(t_bool(X<Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx >= Ry,
    R = false.

eval_bool(t_bool(X>Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx > Ry,
    R = true.

eval_bool(t_bool(X>Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx =< Ry,
    R = false.

eval_bool(t_bool(X<=Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx =< Ry,
    R = true.

eval_bool(t_bool(X<=Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx > Ry,
    R = false.

eval_bool(t_bool(X>=Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx >= Ry,
    R = true.

eval_bool(t_bool(X>=Y),Env, New_Env, R):-
    eval_expr(Y,Env, Env1, Ry),
    eval_expr(X,Env1,New_Env,Rx),
    Rx < Ry,
    R = false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% expr evaluation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_expr(t_expr(X+Y),Env, New_Env,R):-
    eval_expr(X, Env, Env1, Rx),
    eval_expr(Y, Env1, New_Env, Ry),
    R is Rx + Ry.

eval_expr(t_expr(X-Y),Env, New_Env,R):-
    eval_expr(X, Env, Env1, Rx),
    eval_expr(Y, Env1, New_Env, Ry),
    R is Rx - Ry.

eval_expr(t_expr(X),Env, New_Env,R):-
    eval_expr(X, Env, New_Env,R).

eval_expr(t_term(X*Y),Env, New_Env,R):-
    eval_expr(X, Env, Env1, Rx),
    eval_expr(Y, Env1, New_Env, Ry),
    R is Rx * Ry.

eval_expr(t_term(X/Y),Env, New_Env,R):-
    eval_expr(X, Env, Env1, Rx),
    eval_expr(Y, Env1, New_Env, Ry),
    R is Rx / Ry.

eval_expr(t_term(X^Y),Env, New_Env,R):-
    eval_expr(X, Env, Env1, Rx),
    eval_expr(Y, Env1, New_Env, Ry),
    R is Rx ** Ry.

eval_expr(t_term(X mod Y),Env, New_Env,R):- %modulus change in lexer,parser(varname,expression,keyword)
    eval_expr(X, Env, Env1, R).
    eval_expr(Y, Env, New_Env, R).
    R is Rx mod Ry.

eval_expr(t_expr(sq(X)), Env, New_Env, R):-
    eval_expr(X, Env, New_Env, Rx),
    R is Rx * Rx.

eval_expr(t_expr(sqrt(X)), Env, New_Env, R):-
    eval_expr(X, Env, New_Env, Rx),
    R is sqrt(Rx).

eval_expr(t_expr(cube(X)), Env, New_Env, R):-
    eval_expr(X, Env, New_Env, Rx),
    R is Rx * Rx * Rx.

eval_expr(t_expr(cbrt(X)), Env, New_Env, R):-
    eval_expr(X, Env, New_Env, Rx),
    R is cbrt(Rx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ternary, incr, decr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_expr(t_expr(X), Env, New_Env, R):-
    eval_expr(X, Env, New_Env, R).

eval_expr(t_expr(num(N)), Env, Env, N).

eval_expr(t_expr(ternary(T)), Env, New_Env, R):-
    eval_expr(T, Env, New_Env, R).

eval_expr(t_expr(increment_operation(I)), Env, New_Env, R):-
    eval_expr(I, Env, New_Env, Rx),
    R is Rx + 1.

eval_expr(t_expr(decrement_operation(Dec)), Env, New_Env, R):-
    eval_expr(Dec, Env, New_Env, Rx),
    R is Rx - 1.

eval_expr(t_term(X),Env, New_Env,R):-
    eval_expr(X, Env, New_Env, R).

eval_expr(t_factor(X),Env, New_Env,R):-
    eval_expr(X, Env, New_Env,R).

eval_expr(t_assign(X:=Y),Env, New_Env,R):-
    eval_expr(Y,Env, Env1, R),
    update((X,R), Env1, New_Env).

eval_expr(t_num(X), Env, Env, X).

eval_expr(t_id(X), Env, Env,R):-
    look_Up(X, Env, R).

look_Up(X,Env,Val):-
    flatten(Env, FEnv), %to include constant variables in search
    look_Up(X, FEnv, Val).

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

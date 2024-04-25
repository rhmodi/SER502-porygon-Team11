%%%% TO DO %%%%%
is_bool(X).
is_string(X).
eval_str(Y). % Shall we do str concatenation?
%%%%%%%%%%%%%%%%

hard_look_up(X,Env,Val):-
    flatten(Env, FEnv), %to include constant variables in search
    hlu(X, FEnv, Val).

hlu(X,[],_Val):-
    write('Uninitialized variable: '),
    write(X),
    fail.

hlu(HVar,[(HVar,HVal,_HType)|_T],HVal).

hlu(X,[(HVar,_HVal,_HType)|T],Val):-
    X \= HVar,
    hlu(X,T,Val).

soft_look_up(X,Env,Val):-
    flatten(Env, FEnv), %to include constant variables in search
    slu(X, FEnv, Val).

slu(_X,[],_Val):-
    fail.

slu(HVar,[(HVar,HVal,_HType)|_T],HVal).

slu(X,[(HVar,_HVal,_HType)|T],Val):-
    X \= HVar,
    slu(X,T,Val).



update((Var, _Val, _Type), [_Variables, Const], _R):-
    soft_look_up(Var, Const, _SomeVal),
    write('Cannot update Const '),
    write(Var),
    nl,
    fail.
update((Var, Val, Type), [Variables, Const], UpdatedEnv):-
    not(soft_look_up(Var, Const, _SomeVal)),
    update_var((Var, Val, Type), [Variables, Const], UpdatedEnv).


update_var((Var, Val, Type), [[],Const], [[(Var,Val, Type)],Const]).
update_var((Var, Val, Type), [[(Var, _Val, Type) | T], Const], [[(Var, Val, Type)|T], Const]).
update_var((Var, Val,Type), [[(SomeVar, SomeVal, SomeType)|T], Const], UpdatedEnv):-
    Var \= SomeVar,
    update((Var, Val, Type), [T, Const], [UpdatedVar, Const]),
    UpdatedEnv = [[(SomeVar, SomeVal, SomeType)|UpdatedVar], Const].

init_const((Var,_Val),Env,_):-
    soft_look_up(Var,Env, _SomeVal),
    write('Const '),
    write(Var),
    write(' is already defined'),
    fail.
init_const((Var,Val,Type),[Variables,[]],[Variables,[(Var,Val,Type)]]).
init_const((Var,Val,Type),[Variables,Const],[Variables,[(Var,Val,Type)|Const]]).


init_var((Var,_Val,_Type),Env,_):-
    soft_look_up(Var,Env, _SomeVal),
    write('Variable '),
    write(Var),
    write(' is already defined'),
    nl,
    fail.
init_var((Var,Val,Type),[Const, []],[[(Var,Val,Type)], Const]):-
    not(soft_look_up(Var,[Const,[]], _SomeVal)).
init_var((Var,Val,Type),[Variables,Const],[[(Var,Val,Type) |Variables], Const]):-
    not(soft_look_up(Var,[Variables,Const], _SomeVal)).

eval_constassign(t_const_int_e(X,Y),EVT, UEVT):-
    eval_expr(X, Var),
    eval_expr(Y, Num),
    init_const((Var,Num,int),EVT,UEVT).
eval_constassign(t_const_float_e(X,Y),EVT, UEVT):-
    eval_expr(X, Var),
    eval_expr(Y, Num),
    init_const((Var,Num,float),EVT,UEVT).
eval_constassign(t_const_str_e(X,Y),EVT, UEVT):-
    eval_expr(X, Var),
    eval_expr(Y, Num),
    init_const((Var,Y,str),EVT,UEVT).
eval_constassign(t_const_bool_e(X,Y),EVT, UEVT):-
    eval_expr(X, Var),
    eval_expr(Y, Num),
    init_const((Var,Num,bool),EVT,UEVT).


eval_expr(var(Var), Var).
eval_expr(num(Num), Num).
eval_expr(bool(Bool),Bool).


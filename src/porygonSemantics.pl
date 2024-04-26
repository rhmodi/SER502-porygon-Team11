boolean(true).
boolean(false).

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


typecheck(X,Y):- integer(X),integer(Y).
typecheck(X,Y):- float(X),float(Y).
typecheckstring(X,Y):- atom(X),atom(Y).

init_const((Var,_Val,_Type),Env,_):-
    soft_look_up(Var,Env, _SomeVal),
    write('Const '),
    write(Var),
    write(' is already defined'),
    fail.
init_const((Var,Val,Type),[Variables,[]],[Variables,[(Var,Val,Type)]]).
init_const((Var,Val,Type),[Variables,Const],[Variables,[(Var,Val,Type)|Const]]):-
    Const \= [].




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

eval_assign(t_const_int_e(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Num),
    integer(Num),
    init_const((Var,Num,int),EVT2, UEVT).
eval_assign(t_const_float_e(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Flt),
    float(Flt),
    init_const((Var,Flt,float),EVT2,UEVT).
eval_assign(t_const_str_e(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Str),
    atom(Str),
    init_const((Var,Str,str),EVT2,UEVT).
eval_assign(t_const_bool_e(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Bool),
    boolean(Bool),
    init_const((Var,Bool,bool),EVT2,UEVT).
eval_assign(t_int(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Num),
    integer(Num),
    init_var((Var,Num,int),EVT2, UEVT).
eval_assign(t_flt(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Flt),
    float(Flt),
    init_var((Var,Flt,float),EVT2,UEVT).
eval_assign(t_str(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Str),
    atom(Str),
    init_var((Var,Str,str),EVT2,UEVT).
eval_assign(t_bool(X,Y),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Bool),
    boolean(Bool),
    init_var((Var,Bool,bool),EVT2,UEVT).
eval_assign(t_bool_decl(X),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    init_var((Var,undef,bool),EVT1,UEVT).
eval_assign(t_int_decl(X),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    init_var((Var,undef,int),EVT1,UEVT).
eval_assign(t_str_decl(X),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    init_var((Var,undef,str),EVT1,UEVT).
eval_assign(t_flt_decl(X),EVT, UEVT):-
    eval_expr(X, EVT, EVT1, Var),
    init_var((Var,undef,float),EVT1,UEVT).


% At the end add halt whenever we encounter incompatible 

eval_expr(addition(X,Y),EVT,NEVT,Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val is Val1+Val2.

eval_expr(addition(X,Y),EVT,NEVT,Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),concat(Val1,Val2,Val).
%Halt
eval_expr(addition(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)) ,write("Incompatible Datatype while evaluating expressions").

eval_expr(subtraction(X,Y),EVT,NEVT,Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val is Val1-Val2.

%Halt
eval_expr(subtraction(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),write("Operation Not possible on strings").
%Halt
eval_expr(subtraction(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),write("Incompatible Datatype while evaluating expressions").


eval_expr(multiplication(X,Y),EVT,NEVT,Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val is Val1*Val2.
%Halt
eval_expr(multiplication(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),write("Operation Not possible on strings").
%Halt
eval_expr(multiplication(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),write("Incompatible Datatype while evaluating expressions").


%Halt
eval_expr(division(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=:=0, write("Divide by 0 error").

eval_expr(division(X,Y),EVT,NEVT,Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=\=0, Val is Val1/Val2.
%Halt
eval_expr(division(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),write("Operation Not possible on strings").
%Halt
eval_expr(division(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),write("Incompatible Datatype while evaluating expressions").



%Halt
eval_expr(modulus(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=:=0 , write("Modulus by 0 error").

eval_expr(modulus(X,Y),EVT,NEVT,Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=:=0, Val is Val1 mod Val2.

%Halt
eval_expr(modulus(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),write("Operation Not possible on strings").
%Halt
eval_expr(modulus(X,Y),EVT,NEVT,_Val):- 
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),write("Incompatible Datatype while evaluating expressions").


eval_expr(square(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val1),
    Val is Val1**2.

eval_expr(cube(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val1),
    Val is Val1**3.

eval_expr(sqrt(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val1),
    Val is Val1**(1/2).

eval_expr(cbrt(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val1),
    Val is Val1**(1/3).

eval_expr(exponent(X,Y),EVT,NEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,NEVT,Val2),
    Val is Val1**Val2.

eval_expr(var(Var), EVT, EVT, Var).
eval_expr(num(Num),  EVT, EVT, Num):- number(Num).
eval_expr(bool(Bool), EVT, EVT, Bool):- boolean(Bool).
eval_expr(str(Str), EVT, EVT, Str):- atom(Str).

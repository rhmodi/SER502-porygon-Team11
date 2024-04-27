

boolean(true).
boolean(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% hard look up to check variables presence and return it %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hard_look_up(X,Env,Val):-
    flatten(Env, FEnv), %to include constant variables in search
    hlu(X, FEnv, Val).

hlu(X,[],_Val):-
    concat('Uninitialized variable: ',X,Str),
    print_message(Str),
    halt.

hlu(HVar,[(HVar,HVal,_HType)|_T],HVal).

hlu(X,[(HVar,_HVal,_HType)|T],Val):-
    X \= HVar,
    hlu(X,T,Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% soft look up to check variables presence  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
soft_look_up(X,Env,Val):-
    flatten(Env, FEnv), %to include constant variables in search
    slu(X, FEnv, Val).

slu(_X,[],_Val):-
    fail.

slu(HVar,[(HVar,HVal,_HType)|_T],HVal).

slu(X,[(HVar,_HVal,_HType)|T],Val):-
    X \= HVar,
    slu(X,T,Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% update to update variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update((Var, _Val, _Type), [_Variables, Const], _R):-
    soft_look_up(Var, Const, _SomeVal),
    concat('Cannot update Const ',Var,Str),
    print_message(Str),
    nl,
    halt.
update((Var, Val, Type), [Variables, Const], UpdatedEnv):-
    not(soft_look_up(Var, Const, _SomeVal)),
    update_var((Var, Val, Type), [Variables, Const], UpdatedEnv).




update_var((Var, Val, Type), [[],Const], [[(Var,Val, Type)],Const]).
update_var((Var, Val, Type), [[(Var, _Val, Type) | T], Const], [[(Var, Val, Type)|T], Const]).
update_var((Var, Val,Type), [[(SomeVar, SomeVal, SomeType)|T], Const], UpdatedEnv):-
    Var \= SomeVar,
    update((Var, Val, Type), [T, Const], [UpdatedVar, Const]),
    UpdatedEnv = [[(SomeVar, SomeVal, SomeType)|UpdatedVar], Const].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% type checking of datatypes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheck(X,Y):- integer(X),integer(Y).
typecheck(X,Y):- float(X),float(Y).
typecheckstring(X,Y):- atom(X),atom(Y).

init_const((Var,_Val,_Type),Env,_):-
    soft_look_up(Var,Env, _SomeVal),
    concat("Const ",Var,Str),
    concat(Str," is already defined",Str1),
    print_message(Str1),

    halt.
init_const((Var,Val,Type),[Variables,[]],[Variables,[(Var,Val,Type)]]).
init_const((Var,Val,Type),[Variables,Const],[Variables,[(Var,Val,Type)|Const]]):-
    Const \= [].




init_var((Var,_Val,_Type),Env,_):-
    soft_look_up(Var,Env, _SomeVal),
    concat("Variable ",Var,Str),
    concat(Str," is already defined",Str1),
    print_message(Str1),
    nl,
    halt.
init_var((Var,Val,Type),[Const, []],[[(Var,Val,Type)], Const]):-
    not(soft_look_up(Var,[Const,[]], _SomeVal)).
init_var((Var,Val,Type),[Variables,Const],[[(Var,Val,Type) |Variables], Const]):-
    not(soft_look_up(Var,[Variables,Const], _SomeVal)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Evaluation of keywords and expressions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_assign(t_const_int_e(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(X, EVT, EVT1, Var),
    eval_expr(Y, EVT1, EVT2, Num),
    integer(Num),
    init_const((Var,Num,int),EVT2, UEVT)).
eval_assign(t_const_float_e(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Flt),
    float(Flt),
    init_const((X,Flt,float),EVT2,UEVT)).
eval_assign(t_const_str_e(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Str),
    atom(Str),
    init_const((X,Str,str),EVT2,UEVT)).
eval_assign(t_const_bool_e(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Bool),
    boolean(Bool),
    init_const((X,Bool,bool),EVT2,UEVT)).
eval_assign(t_int(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Num),
    integer(Num),
    init_var((X,Num,int),EVT2, UEVT)).
eval_assign(t_flt(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Flt),
    float(Flt),
    init_var((X,Flt,float),EVT2,UEVT)).
eval_assign(t_str(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Str),
    atom(Str),
    init_var((X,Str,str),EVT2,UEVT)).
eval_assign(t_bool(var(X),Y),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    eval_expr(Y, EVT, EVT2, Bool),
    boolean(Bool),
    init_var((X,Bool,bool),EVT2,UEVT)).
eval_assign(t_bool_decl(var(X)),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    init_var((X,false,bool),EVT,UEVT)).
eval_assign(t_int_decl(var(X)),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    init_var((X,0,int),EVT,UEVT)).
eval_assign(t_str_decl(var(X)),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    init_var((X,0,str),EVT,UEVT)).
eval_assign(t_flt_decl(var(X)),EVT, UEVT):-
    (   soft_look_up(X,EVT,_Val) ->  concat(X," is already defined",Y),print_message(Y),nl,halt;
    init_var((X,0.0,float),EVT,UEVT)).


% At the end add halt whenever we encounter incompatible
% this acts as a layer of exception handling %%%%%

eval_expr(addition(X,Y),EVT,NEVT,Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val is Val1+Val2.

eval_expr(addition(X,Y),EVT,NEVT,Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),concat(Val1,Val2,Val).
%Halt
eval_expr(addition(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)) ,print_message("Incompatible Datatype while evaluating expressions"),halt.

eval_expr(subtraction(X,Y),EVT,NEVT,Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val is Val1-Val2.

%Halt
eval_expr(subtraction(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),print_message("Operation Not possible on strings"),halt.
%Halt
eval_expr(subtraction(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),print_message("Incompatible Datatype while evaluating expressions"),halt.


eval_expr(multiplication(X,Y),EVT,NEVT,Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val is Val1*Val2.
%Halt
eval_expr(multiplication(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),print_message("Operation Not possible on strings"),halt.
%Halt
eval_expr(multiplication(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),print_message("Incompatible Datatype while evaluating expressions"),halt.


%Halt
eval_expr(division(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=:=0, print_message("Divide by 0 error"),halt.

eval_expr(division(X,Y),EVT,NEVT,Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=\=0, Val is Val1/Val2.
%Halt
eval_expr(division(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),print_message("Operation Not possible on strings"),halt.
%Halt
eval_expr(division(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),print_message("Incompatible Datatype while evaluating expressions"),halt.



%Halt
eval_expr(modulus(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=:=0 , print_message("Modulus by 0 error"),halt.

eval_expr(modulus(X,Y),EVT,NEVT,Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheck(Val1,Val2),Val2=:=0, Val is Val1 mod Val2.

%Halt
eval_expr(modulus(X,Y),EVT,NEVT,_Val):-
    	eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), typecheckstring(Val1,Val2),print_message("Operation Not possible on strings"),halt.
%Halt
eval_expr(modulus(X,Y),EVT,NEVT,_Val):-
        eval_expr(X,EVT,EVT1,Val1),eval_expr(Y,EVT1,NEVT,Val2), not(typecheck(Val1,Val2)),not(typecheckstring(Val1,Val2)),print_message("Incompatible Datatype while evaluating expressions"),halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% evaluation of various arithmetic operators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

eval_expr(iassign(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,_Val),
    eval_expr(Y,EVT,EVT1,Val),
    update((X,Val,_),EVT1,NEVT).

eval_expr(shassignadd(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,Val1),
    eval_expr(Y,EVT,EVT1,Val2),
    eval_expr(addition(num(Val1),num(Val2)),EVT1,EVT2,Val),
    update((X,Val,_),EVT2,NEVT).

eval_expr(shassignsub(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,Val1),
    eval_expr(Y,EVT,EVT1,Val2),
    eval_expr(subtraction(num(Val1),num(Val2)),EVT1,EVT2,Val),
    update((X,Val,_),EVT2,NEVT).

eval_expr(shassignmul(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,Val1),
    eval_expr(Y,EVT,EVT1,Val2),
    eval_expr(multiplication(num(Val1),num(Val2)),EVT1,EVT2,Val),
    update((X,Val,_),EVT2,NEVT).

eval_expr(shassigndiv(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,Val1),
    eval_expr(Y,EVT,EVT1,Val2),
    eval_expr(division(num(Val1),num(Val2)),EVT1,EVT2,Val),
    update((X,Val,_),EVT2,NEVT).

eval_expr(shassignmod(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,Val1),
    eval_expr(Y,EVT,EVT1,Val2),
    eval_expr(modulus(num(Val1),num(Val2)),EVT1,EVT2,Val),
    update((X,Val,_),EVT2,NEVT).

eval_expr(shassignexpo(var(X),Y), EVT, NEVT, Val):-
    hard_look_up(X,EVT,Val1),
    eval_expr(Y,EVT,EVT1,Val2),
    eval_expr(exponent(num(Val1),num(Val2)),EVT1,EVT2,Val),
    update((X,Val,_),EVT2,NEVT).


eval_expr(assign(X), EVT, NEVT, Val):-
    eval_expr(X,EVT,NEVT,Val).
eval_expr(ternary(X, Y, Z), EVT, NEVT, Val) :-
    eval_condition(X, EVT, EVT1, Val1),
    (   Val1 = true ->
        eval_expr(Y, EVT1, NEVT, Val)
    ;   eval_expr(Z, EVT1, NEVT, Val)
    ).
eval_expr(increment(var(Var)),EVT,NEVT,Val):-
    eval_expr(var(Var),EVT,EVT1,Val1),
    Val is Val1+1,
    update((Var,Val,_),EVT1,NEVT).
eval_expr(decrement(var(Var)),EVT,NEVT,Val):-
    eval_expr(var(Var),EVT,EVT1,Val1),
    Val is Val1-1,
    update((Var,Val,_),EVT1,NEVT).

% eval_expr(X,EVT,NEVT,Val):- eval_condition(X,EVT,NEVT,Val).

%%%%%%%%%%%%%%%%%%%%%%%% eval block variable lookup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_expr(var(Var), EVT, EVT, Val):-hard_look_up(Var,EVT,Val).
eval_expr(num(Num),  EVT, EVT, Num):- number(Num).
eval_expr(bool(Bool), EVT, EVT, Bool):- boolean(Bool).
eval_expr(str(Str), EVT, EVT, Str):- atom(Str).

%%%%%%%%%%%%%%%%%%%%%%%% eval block for boolean %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_condition(bool(X),EVT,UEVT,Val):-
    eval_expr(bool(X),EVT,UEVT,Val).

eval_condition(boolvalue(Bool),EVT,UEVT,Val):-
    eval_expr(Bool,EVT,UEVT,Val).

eval_condition(greaterthan_orequalto(X,Y),EVT,UEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,UEVT,Val2),
    (Val1>=Val2 -> Val = true; Val = false).

eval_condition(greaterthan(X,Y),EVT,UEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,UEVT,Val2),
    (Val1>Val2 -> Val = true; Val = false).

eval_condition(lessthan_orequalto(X,Y),EVT,UEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,UEVT,Val2),
    (Val1=<Val2 -> Val = true; Val = false).

eval_condition(lessthan(X,Y),EVT,UEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,UEVT,Val2),
    (Val1<Val2 -> Val = true; Val = false).

eval_condition(notequalsto(X,Y),EVT,UEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,UEVT,Val2),
    (Val1\=Val2 -> Val = true; Val = false).

eval_condition(equivalance(X,Y),EVT,UEVT,Val):-
    eval_expr(X,EVT,EVT1,Val1),
    eval_expr(Y,EVT1,UEVT,Val2),
    (Val1=Val2 -> Val = true; Val = false).

eval_condition(or(X,Y),EVT,UEVT,Val):-
    eval_condition(X,EVT,EVT1,Val1),
    eval_condition(Y,EVT1,UEVT,Val2),
    ((Val1=true;Val2=true)->Val=true;Val=false).

eval_condition(and(X,Y),EVT,UEVT,Val):-
    eval_condition(X,EVT,EVT1,Val1),
    eval_condition(Y,EVT1,UEVT,Val2),
    ((Val1=true,Val2=true)->Val=true;Val=false).

eval_condition(not(X),EVT,UEVT,Val):-
    eval_condition(X,EVT,UEVT,Val1),
    negate(Val1,Val).

negate(true,false).
negate(false,true).

%%%%%%%%%%%%%%%%%%%%%%%% eval block command for conditionals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_blk_command(if(X,Y),EVT,NEVT,Val):-
    eval_condition(X,EVT,EVT1,Val1),
    Val1 = true,
    eval_commandlist(Y,EVT1,NEVT,Val).
eval_blk_command(elseif(X,Y,Z),EVT,NEVT,Val):-
    eval_condition(X,EVT,EVT1,Val1),
    (Val1 = true ->  eval_commandlist(Y,EVT1,NEVT,Val);
    eval_blk_command(Z,EVT1,NEVT,Val)).
eval_blk_command(elseif(X,Y),EVT,NEVT,Val):-
    eval_condition(X,EVT,EVT1,Val1),
    (Val1 = true ->  eval_commandlist(Y,EVT1,NEVT,Val)).
eval_blk_command(else(X),EVT,NEVT,Val):-
    eval_commandlist(X,EVT,NEVT,Val).

eval_blk_command(ifelse(X,_Y),EVT,NEVT,Val):-
    eval_blk_command(X,EVT,NEVT,Val).
eval_blk_command(ifelse(X,Y),EVT,NEVT,Val):-
    not(eval_blk_command(X,EVT,_EVT1,_Val1)),
    eval_blk_command(Y,EVT,NEVT,Val).
eval_blk_command(ifelseladder(X, _Y, _Z),EVT,NEVT,Val):-
    eval_blk_command(X,EVT,NEVT,Val).
eval_blk_command(ifelseladder(X, Y, _Z),EVT,NEVT,Val):-
    not(eval_blk_command(X,EVT,_EVT1,_Val1)),
    eval_blk_command(Y,EVT,NEVT,Val).
eval_blk_command(ifelseladder(X, Y, Z),EVT,NEVT,Val):-
    not(eval_blk_command(X,EVT,_EVT1,_Val1)),
    not(eval_blk_command(Y,EVT,NEVT,Val)),
    eval_blk_command(Z,EVT,NEVT,Val).


%%%%%%%%%%%%%%%%%%%%%%%% eval block command for loop (traditional) and for in range %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_blk_command(for(Assign, BoolCondition, Valupdation, C), EVT, NEVT, Val):-
    eval_expr(Assign, EVT, EVT1, _),
    eval_forloop(BoolCondition, Valupdation, C, EVT1, NEVT, Val).

eval_blk_command(forinrange(Var,Sr,Er,Command),EVT,NEVT,Val):-
    eval_expr(Sr,EVT,EVT1,Val1),
    eval_expr(Er,EVT1,EVT2,Val2),
    eval_blk_command(for(assign(iassign(Var,num(Val1))),lessthan(Var,num(Val2)),increment(Var),Command),
                     EVT2,NEVT,Val).


%%%%%%%%%%%%%%%%%%%%%%%% eval block for traditional while loop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_blk_command(while(X,Y),EVT,NEVT,Val):-
    eval_condition(X,EVT,EVT1,Val1),
    (Val1 = true ->  
        eval_commandlist(Y,EVT1,EVT2,_Val2),
    eval_blk_command(while(X,Y),EVT2,NEVT,Val)
    ;
        NEVT = EVT,
        Val = false
    ),!.


eval_forloop(BoolCondition, Valupdation, C, EVT, NEVT, Val):-
    eval_condition(BoolCondition, EVT, EVT1, BoolVal),
    (BoolVal == true ->  
        eval_commandlist(C, EVT1, EVT2, _),
        eval_expr(Valupdation, EVT2, EVT3, _),
        eval_forloop(BoolCondition, Valupdation, C, EVT3, NEVT, Val)
    ;
        NEVT = EVT,
        Val = false),!.


%%%%%%%%%%%%%%%%%%%%%%%% eval block for plain assignment, increment, ternary, decrement %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_plaincommand(plain_assign(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val).
eval_plaincommand(plain_terenary(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val).
eval_plaincommand(plain_increment(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val).
eval_plaincommand(plain_decrement(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val).
eval_plaincommand(plain_print(X),EVT,NEVT,Val):-
    eval_print(X,EVT,NEVT,Val),
    write(Val).
eval_plaincommand(plain_strlen(X),EVT,NEVT,Val):-
    eval_stringlength(X,EVT,NEVT,Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%% eval block for print and stringlength %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_print(print(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val).
eval_stringlength(stringlength(X),EVT,NEVT,Val):-
    eval_expr(X,EVT,NEVT,Val1),
    string_length(Val1,Val).

eval_blockcommand(blkcmd(X),EVT,NEVT,Val):-
    eval_blk_command(X,EVT,NEVT,Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% eval block for commandlist %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_commandlist(t_cmd(X,Y), EVT, UEVT, Val):-
    eval_plaincommand(X,EVT,EVT1,_Val1),
    eval_commandlist(Y,EVT1,UEVT,Val).
eval_commandlist(t_cmd(X), EVT, UEVT, Val):-
    eval_plaincommand(X,EVT,UEVT,Val).
eval_commandlist(t_cmd(X,Y), EVT, UEVT, Val):-
    eval_blockcommand(X,EVT,EVT1,_Val1),
    eval_commandlist(Y,EVT1,UEVT,Val).
eval_commandlist(t_cmd(X), EVT, UEVT, Val):-
    eval_blockcommand(X,EVT,UEVT,Val).


eval_decl(t_decl(X,Y),EVT,UEVT):-
    eval_assign(X,EVT,EVT1),
    eval_decl(Y,EVT1,UEVT).
eval_decl(t_decl(X),EVT,UEVT):-
    eval_assign(X,EVT,UEVT).

eval_block(t_blk(X,Y),EVT,UEVT,Val):-   
    eval_decl(X,EVT,EVT1),
    eval_commandlist(Y,EVT1,UEVT,Val),!.


print_message(Str):- string_concat("ERROR: ",Str,Error),ansi_format([bold,fg(red)], Error, []).





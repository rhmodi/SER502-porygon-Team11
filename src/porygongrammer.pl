%:-use_rendering(svgtree).
:-table expr/3,factor/3,term/3,boolcondition/3, and_condition/3.

%Programming block begins here
block(t_blk(D,C))--> ['{'],decl(D),commandlist(C),['}'].

% Declaration statments here for constants,variables etc.

decl(t_decl(D,DL))--> decls(D),[';'],decl(DL).
decl(t_decl(D)) --> decls(D),[';'].

decls(D)--> constassign(D).
decls(D)--> declassign(D).
decls(D)--> plainassign(D).

%constassign(t_const_int(C,N)) --> ['const'], ['int'], variablename(C),['='],num(N).
%constassign(t_const_str(C,S)) --> ['const'], ['string'], variablename(C),['='],stringvalue(S).
%constassign(t_const_bool(C,B)) --> ['const'], ['bool'], variablename(C),['='],boolvalue(B).
constassign(t_const_flt(C,F)) --> ['const'], ['float'], variablename(C),['='],floatvalue(F).
constassign(t_const_int_e(C,Expr)) --> ['const'], ['int'], variablename(C),['='],expr(Expr).
constassign(t_const_str_e(C,Expr)) --> ['const'], ['string'], variablename(C),['='],expr(Expr).
constassign(t_const_bool_e(C,Expr)) --> ['const'], ['bool'], variablename(C),['='],expr(Expr).
constassign(t_const_flt_e(C,Expr)) --> ['const'], ['float'], variablename(C),['='],expr(Expr).

%declassign(t_str(Var,Value)) --> ['string'], variablename(Var),['='],stringvalue(Value).
%declassign(t_bool(Var,Value)) --> ['bool'], variablename(Var),['='],boolvalue(Value).
declassign(t_int(Var,Value)) --> ['int'], variablename(Var),['='],expr(Value).
declassign(t_str(Var,Value)) --> ['string'], variablename(Var),['='],expr(Value).
declassign(t_bool(Var,Value)) --> ['bool'], variablename(Var),['='],expr(Value).
declassign(t_flt(Var,Value)) --> ['float'], variablename(Var),['='],expr(Value).

plainassign(t_int_decl(Var)) --> ['int'], variablename(Var).
plainassign(t_str_decl(Var)) --> ['string'], variablename(Var).
plainassign(t_bool_decl(Var)) --> ['bool'], variablename(Var).
plainassign(t_flt_decl(Var)) --> ['float'], variablename(Var).

% Commands List start here
commandlist(t_cmd(PlainCmnd,CmndList)) --> plaincommand(PlainCmnd),[;],commandlist(CmndList).
commandlist(t_cmd(PlainCmnd,CmndList)) --> blockcommand(PlainCmnd),commandlist(CmndList).
commandlist(t_cmd(PlainCmnd)) --> plaincommand(PlainCmnd), [;].
commandlist(t_cmd(BlkCmnd)) --> blockcommand(BlkCmnd).

% Declaration of plain commands
plaincommand(plain_assign(Assign)) --> assignment(Assign).
plaincommand(plain_ternary(Ternary)) --> ternary(Ternary).
plaincommand(plain_print(Print)) --> printStmt(Print).
plaincommand(plain_strlen(StrLen)) --> strlen(StrLen).
plaincommand(plain_increment(Value)) -->increment_operation(Value).
plaincommand(plain_decrement(Value)) -->decrement_operation(Value).

%Separeted syntax structures which need {} so that they do not need ; too.
blockcommand(blkcmd(If)) --> ifcommand(If).
blockcommand(blkcmd(IfElse)) --> ifelsecommand(IfElse).
blockcommand(blkcmd(IfELseLadder)) --> ifELseLaddercommand(IfELseLadder).
blockcommand(blkcmd(While)) --> whilecommand(While).
blockcommand(blkcmd(For)) --> forcommand(For).
blockcommand(blkcmd(ForInRange)) --> forinrangecommand(ForInRange).


% Declaration of types of assignment
assignment(assign(A))--> initialassignment(A).
%assignment(assign(A))--> declassign(A).
assignment(assign(A))--> shorthandAssign(A).

shorthandAssign(shassignadd(Var,Expr))--> variablename(Var),['+='],expr(Expr).
shorthandAssign(shassignsub(Var,Expr))--> variablename(Var),['-='],expr(Expr).
shorthandAssign(shassignmul(Var,Expr))--> variablename(Var),['*='],expr(Expr).
shorthandAssign(shassigndiv(Var,Expr))--> variablename(Var),['/='],expr(Expr).
shorthandAssign(shassignmod(Var,Expr))--> variablename(Var),['mod='],expr(Expr).
shorthandAssign(shassignexpo(Var,Expr))--> variablename(Var),['^='],expr(Expr).

initialassignment(iassign(Var,Expr))--> variablename(Var),['='],expr(Expr).

% EXPRESSIONS STARTS HERE

% Change in grammer make it left recursive
expr(addition(X,Y))--> expr(X),['+'],term(Y).
expr(subtraction(X,Y))--> expr(X),['-'],term(Y).
expr(X)--> term(X).

term(multiplication(X,Y))--> term(X),['*'],factor(Y).
term(division(X,Y))--> term(X),['/'],factor(Y).
term(modulus(X,Y))--> term(X),['mod'],factor(Y).
term(X)--> factor(X).

factor(exponent(X,Y))--> factor(X),['^'],exponent(Y).
factor(X)--> exponent(X).

exponent(S)--> square(S).
exponent(Sr)--> squareRoot(Sr).
exponent(C)--> cube(C).
exponent(Cr)--> cubeRoot(Cr).
exponent(X)--> ['('],expr(X),[')'].
exponent(A)--> initialassignment(A).
exponent(Var)--> variablename(Var).
exponent(N)--> num(N).
exponent(T) --> ternary(T).
exponent(B) --> boolvalue(B).
exponent(S) -->stringvalue(S).
exponent(I)--> increment_operation(I).
exponent(Dec)--> decrement_operation(Dec).
exponent(B) --> boolcondition(B).
exponent(S) --> strlen(S).

square(square(S))--> ['sq'],['('],expr(S),[')'].
squareRoot(squareroot(Sr))--> ['sqrt'],['('],expr(Sr),[')'].
cube(cube(C))--> ['cube'],['('],expr(C),[')'].
cubeRoot(cuberoot(S))--> ['cbrt'],['('],expr(S),[')'].

%Boolean Expressions start here including logical operators 'and', 'or', 'not'.

boolcondition(or(X,Y))--> boolcondition(X),['or'],and_condition(Y).
boolcondition(X) --> and_condition(X).
and_condition(and(X,Y))--> and_condition(X),['and'],condition(Y).
and_condition(X) --> condition(X).
condition(not(X))--> ['not'],condition(X).
condition(X)--> ['('],boolcondition(X),[')'].
condition(InitAssign) --> initialassignment(InitAssign).
%condition(not(Expression))--> ['not'], expr(Expression).
condition(equivalance(Expr1,Expr2))--> expr(Expr1),['=='],expr(Expr2).
condition(notequalsto(Expr1,Expr2))--> expr(Expr1),['!='],expr(Expr2).
condition(lessthan(Expr1,Expr2))--> expr(Expr1),['<'],expr(Expr2).
condition(lessthan_orequalto(Expr1,Expr2))--> expr(Expr1),['<='],expr(Expr2).
condition(greaterthan(Expr1,Expr2))--> expr(Expr1),['>'],expr(Expr2).
condition(greaterthan_orequalto(Expr1,Expr2))--> expr(Expr1),['>='],expr(Expr2).
condition(Bool)--> boolvalue(Bool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IF STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%

ifcommand((If))--> ifpart(If).
ifelsecommand(ifelse(If,Else))--> ifpart(If),elsepart(Else).
ifELseLaddercommand(ifelseladder(If ,Elif ,Else))--> ifpart(If),elseifpart(Elif),elsepart(Else).

ifpart(if(B,X))--> ['if'],['('],boolcondition(B), [')'],['{'],commandlist(X),['}'].
elseifpart(elseif(B,E1,E2))--> ['elif'],['('],boolcondition(B), [')'],['{'],commandlist(E1),['}'],elseifpart(E2).
elseifpart(elseif(B,E))--> ['elif'],['('],boolcondition(B), [')'],['{'],commandlist(E),['}'].
elsepart(else(C))--> ['else'],['{'],commandlist(C),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WHILE STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%
whilecommand(while(Condition,C))--> ['while'],['('],boolcondition(Condition), [')'],['{'],commandlist(C),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FOR STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forcommand(for(Assign,BoolCondition,Valupdation,C))--> ['for'],['('],assignment(Assign),[';'],boolcondition(BoolCondition),[';'],variableupdation(Valupdation),[')'],['{'],commandlist(C),['}'].
%forcommand(for(Assign,BoolCondition,Valupdation,C))--> ['for'],['('],declassign(Assign),[';'],boolcondition(BoolCondition),[';'],variableupdation(Valupdation),[')'],['{'],commandlist(C),['}'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% variable updation%%%%%%%%%%%%%%%%%%%%%%%%%%%%
variableupdation((Ops))--> increment_operation(Ops).
variableupdation((Ops))--> decrement_operation(Ops).
variableupdation((Assign))--> assignment(Assign).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% increment and decrement operations%%%%%%%%%%%%%%%%%%%%%%%%%%%%
increment_operation(increment(Var)) --> variablename(Var),['++'].
increment_operation(increment(Var)) --> ['++'],variablename(Var).

decrement_operation(decrement(Var)) --> variablename(Var),['--'].
decrement_operation(decrement(Var)) --> ['--'],variablename(Var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FOR IN RANGE STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forinrangecommand(forinrange(Var,SR,ER,C))--> ['for'],variablename(Var),['in'],['range'],['('], range(SR),[','],range(ER),[')'],['{'],commandlist(C),['}'].
range(Range)--> variablename(Range).
range(Range)--> num(Range).

%%%%%%%%%%%%check before pushing%%%%%%%%%%%%%% Terenary Statement%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ternary(ternary(Bool,Expr1,Expr2))-->['('], boolcondition(Bool),[')'],['?'],expr(Expr1),[':'],expr(Expr2).
%ternary(ternary(Bool,C1,C2))--> boolcondition(Bool),['?'],['{'],commandlist(C1),['}'],[':'],['{'],commandlist(C2),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRINT STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printStmt(print(Print))--> ['print'],['('],expr(Print),[')'].
printStmt(printnl(Print))--> ['printnl'],['('],expr(Print),[')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRING LENGTH STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strlen(stringlength(Strlen)) --> ['strlen'],['('],stringvalue(Strlen),[')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRING VALUE DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stringvalue(str(Str))--> ['\"'],[Str],['\"'].
stringvalue((Str))--> variablename(Str).
%stringvalue(string(Str))--> ['\"'],['\"'].
% LEXER is handling it
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FLOAT VALUE DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

floatvalue(N)--> num(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BOOLEAN VALUE DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boolvalue(bool(true))--> ['true'].
boolvalue(bool(false))--> ['false'].
%boolvalue(bool(X))-->  num(X). % 0 for false and 1 for true (check this again).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VARIABLE NAME DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% VAR NAME SHOULD NOT START WITH A LOWERCASE LETTER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% VAR NAME SHOULD NOT START WITH A SPECIAL CHARACTER%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%% VAR NAME CAN BE ALPHANUMERIC AND CAN CONTAIN UNDERSCORE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VAR NAME SHOULD NOT END WITH UNDERSCORE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
variablename(var(Atom)) -->
    [Atom],
    {\+ member(Atom, ['const','int','string','float','bool','mod','and','or','not','print','printnl','strlen',
        'sqrt','cbrt','sq','cube','true','false','if','elif','else', 'for', 'in','range','while'])},
    {\+number(Atom)},
    { atom_chars(Atom, [First|RestChars]) },
    { code_type(First, lower) },
    {restOfVariableName(RestChars)}.

restOfVariableName([Char|Rest]):-
    code_type(Char, alnum); Char == '_',
    restOfVariableName(Rest).
restOfVariableName([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ALPHANUMERIC DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%alphanumeric(Alpha) --> character(Alpha), alphanumeric(Alpha).
%alphanumeric(Alpha) --> character(Alpha).
%character(Char) --> letter(Char).
%character(Char) --> num(Char).
%character(Char) --> special_char(Char).

num(num(Num)) --> [Num],{number(Num)}.

%letter(Letter) --> [Letter],lowercase_letter(Letter).
%letter(Letter) --> [Letter],uppercase_letter(Letter).
%lowercase_letter(Lower)--> {member(Lower, ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'])}.
%uppercase_letter(Upper) --> {member(Upper, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'])}.
%digit(Digit) --> {member(Digit, [0,1,2,3,4,5,6,7,8,9])}.
%special_char(Special) --> {member(Special,  ['!','@','#','$','%','^','&','*','(',')','_','-','+','=','{','}','',']','|',':',';','"','<','>',',','.','?','/'])}.






% evaluator(T,V):- block(P,T,[]),eval_block(P,[[],[]],NEVT,V).

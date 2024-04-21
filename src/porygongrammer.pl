%Programming block begins here
block(block(D,E))--> ['{'],decl(D),commd(C),['}'].

% Declaration statments here for constants,variables etc.

decl(dec(D,DL))--> decls(D),[';'],decl(DL).
decl(dec(D)) --> decls(D),[';'].

decls(D)--> constassign(D).
decls(D)--> declassign(D).
decls(D)--> plainassign(D).

constassign(const(C,N)) :- ['const'], ['int'], variablename(C),[=],num(N).
constassign(const(C,S)) :- ['const'], ['string'], variablename(C),[=],stringvalue(S).
constassign(const(C,BE)) :- ['const'], ['bool'], variablename(C),[=],boolvalue(BE).
constassign(const(C,F)) :- ['const'], ['float'], variablename(C),[=],floatvalue(F).
constassign(const(C,Expr)) :- ['const'], ['int'], variablename(C),[=],expr(Expr).
constassign(const(C,Expr)) :- ['const'], ['string'], variablename(C),[=],expr(Expr).
constassign(const(C,Expr)) :- ['const'], ['bool'], variablename(C),[=],expr(Expr).
constassign(const(C,Expr)) :- ['const'], ['float'], variablename(C),[=],expr(Expr).

declassign(declaration(Var,Value)) :- ['int'], variablename(Var),[=],num(Value).
declassign(declaration(Var,Value)) :- ['string'], variablename(Var),[=],stringvalue(Value).
declassign(declaration(Var,Value)) :- ['bool'], variablename(Var),[=],boolvalue(Value).
declassign(declaration(Var,Value)) :- ['float'], variablename(Var),[=],floatvalue(Value).
declassign(declaration(Var,Value)) :- ['int'], variablename(Var),[=],expr(Value).
declassign(declaration(Var,Value)) :- ['string'], variablename(Var),[=],expr(Value). 
declassign(declaration(Var,Value)) :- ['bool'], variablename(Var),[=],expr(Value).
declassign(declaration(Var,Value)) :- ['float'], variablename(Var),[=],expr(Value).

plainassign(plaindeclaration(Var)) :- ['int'], variablename(Var).
plainassign(plaindeclaration(Var)) :- ['string'], variablename(Var).
plainassign(plaindeclaration(Var)) :- ['bool'], variablename(Var).
plainassign(plaindeclaration(Var)) :- ['float'], variablename(Var).

% Commands List start here
commandlist(commd(PlainCmnd,CmndList)) --> plaincommand(PlainCmnd),[;],commandlist(CmndList)
commandlist(commd(PlainCmnd)) --> plaincommand(PlainCmnd), [;].

% Declaration of plain commands
plaincommand(plain(Assign)) --> assignment(Assign).
plaincommand(plain(Ternary)) --> ternary(Ternary).
plaincommand(plain(Print)) --> printStmt(Print).
plaincommand(plain(StrLen)) --> stringLen(StrLen).
plaincommand(plain(If)) --> ifcommand(If).
plaincommand(plain(IfElse)) --> ifelsecommand(IfElse).
plaincommand(plain(IfELseLadder)) --> ifELseLaddercommand(IfELseLadder).
plaincommand(plain(While)) --> whilecommand(While).
plaincommand(plain(For)) --> forcommand(For).
plaincommand(plain(ForInRange)) --> forinrangecommand(ForInRange).


% Declaration of types of assignment
assignment(assign(A))--> initialassignment(A).
assignment(assign(A))--> declassign(A).
assignment(assign(A))--> shortantAssign(A).

shorthandAssign(shassign(Var,Expr))--> variablename(Var),['+='],expr(Expr).
shorthandAssign(shassign(Var,Expr))--> variablename(Var),['-='],expr(Expr).
shorthandAssign(shassign(Var,Expr))--> variablename(Var),['*='],expr(Expr).
shorthandAssign(shassign(Var,Expr))--> variablename(Var),['/='],expr(Expr).
shorthandAssign(shassign(Var,Expr))--> variablename(Var),['%='],expr(Expr).
shorthandAssign(shassign(Var,Expr))--> variablename(Var),['^='],expr(Expr).


initialassignment(iassign(Var,Expr))--> variablename(Var),['='],expr(Expr).

% EXPRESSIONS STARTS HERE

% Change in grammer make it left recursive
expr(addition(X,Y))--> expr(X),['+'],term(Y).
expr(subtraction(X,Y))--> expr(X),['-'],term(Y).
expr(X)--> term(X).

term(multiplication(X,Y))--> term(X),['*'],factor(Y).
term(division(X,Y))--> term(X),['/'],factor(Y).
term(modulus(X,Y))--> term(X),['%'],factor(Y).
term(X)--> factor(X).

factor(exponent(X,Y))--> factor(X),['^'],exponent(Y).
factor(X)--> exponent(X).

exponent(Square)--> square(Square).
exponent(Squareroot)--> squareRoot(Squareroot).
exponent(Cube)--> cube(Cube).
exponent(Cuberoot)--> cubeRoot(Cuberoot).
exponent(X)--> ['('],expr(X),[')'].
exponent(InitAssign)--> initialassignment(InitAssign).
exponent(Var)--> variablename(Var).
exponent(Num)--> num(Num).

square(sq(Sq))--> ['sq'],['('],expr(Sq),[')'].
squareRoot(sqrt(Sqrt))--> ['sqrt'],['('],expr(Sqrt),[')'].
cube(cube(Cube))--> ['cube'],['('],expr(Cube),[')'].
cubeRoot(cbrt(Cbrt))--> ['cbrt'],['('],expr(Cbrt),[')'].

%Boolean Expressions start here including logical operators 'and', 'or', 'not'.

boolcondition(or(X,Y))--> and_condition(X),['or'],boolcondition(Y).
boolcondition(X) --> and_condition(X).
boolcondition(and(X,Y))--> condition(X),['and'],and_condition(Y).
and_condition(X) --> condition(X).
condition(not(X))--> ['not'],boolcondition(X).
condition(X)--> ['('],boolcondition(X),[')'].
condition(InitAssign) --> initialassignment(InitAssign).
condition(Expression)--> ['not'], expr(Expression).
condition(equivalance(Expr1,Expr2))--> expr(Expr1),['=='],expr(Expr2).
condition(notequalsto(Expr1,Expr2))--> expr(Expr1),['!='],expr(Expr2).
condition(lessthan(Expr1,Expr2))--> expr(Expr1),['<'],expr(Expr2).
condition(lessthan_orequalto(Expr1,Expr2))--> expr(Expr1),['<='],expr(Expr2).
condition(greaterthan(Expr1,Expr2))--> expr(Expr1),['>'],expr(Expr2).
condition(greaterthan_orequalto(Expr1,Expr2))--> expr(Expr1),['>='],expr(Expr2).
condition(boolvalue(Bool))--> boolvalue(Bool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IF STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%

ifcommand(If)--> ifpart(If).
ifelsecommand(If,Else)--> ifpart(If),elsepart(Else).
ifELseLaddercommand(If,Elif,Else)--> ifpart(If),elseifpart(Elif),elsepart(Else).

ifpart(if(B,X))--> ['if'],['('],boolcondition(B), [')'],['{'],commandlist(X),['}'].
elseifpart(elseif(B,E1,E2))--> ['elif'],['('],boolcondition(B), [')'],['{'],commandlist(E1),['}'],elseifpart(E2). 
elseifpart(elseif(B,E))--> ['elif'],['('],boolcondition(B), [')'],['{'],commandlist(E),['}'].
elsepart(else(C))--> ['else'],['{'],commandlist(C),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WHILE STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%
whilecommand(Condition)--> ['while'],['('],boolcondition(Condition), [')'],['{'],commandlist(C),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FOR STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for_command(For)--> ['for'],['('],assignment(Assign),[';'],boolcondition(BoolCondition),[';'],variableupdation(Varupdation),[')'],['{'],commandlist(C),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% variable updation%%%%%%%%%%%%%%%%%%%%%%%%%%%%
variableupdation(increment(Ops))--> increment_operation(Ops).
variableupdation(decrement(Ops))--> decrement_operation(Ops).
variableupdation(assignment(Assign))--> expr(Assign)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% increment and decrement operations%%%%%%%%%%%%%%%%%%%%%%%%%%%%
increment_operation(increment(Var)) --> variablename(Var),['++'].
increment_operation(increment(Var)) --> ['++'],variablename(Var).

decrement_operation(decrement(Var)) --> variablename(Var),['--'].
decrement_operation(decrement(Var)) --> ['--'],variablename(Var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FOR IN RANGE STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forinrangecommand(For)--> ['for'],variablename(Var),['in'],['range'],['('], range(Range),',',range(Range),',',range(Range),[')'],['{'],commandlist(C),['}'].
range(Range)--> variablename(Range).
range(Range)--> num(Range).

%%%%%%%%%%%%check before pushing%%%%%%%%%%%%%% Terenary Statement%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ternary(ternary(Bool,Expr1,Expr2))-->['('], boolcondition(Bool),[')'],['?'],expr(Expr1),[':'],expr(Expr2).
ternary(ternary(Bool,Expr1,Expr2))--> boolcondition(Bool),['?'],['{'],commandlist(C1),['}'],[':'],['{'],commandlist(C2),['}'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRINT STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printStmt(print(Print))--> ['print'],['('],expr(Print),[')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRING LENGTH STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strlen(stringlength(Strlen)) --> ['strlen'],['('],stringvalue(Strlen),[')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRING VALUE DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stringvalue(string(Str))--> ['"'],alphanumeric(Str),['"'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FLOAT VALUE DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
floatvalue(float(Flt))--> num(Num),'.',num(Num2).
floatvalue(float(Flt))--> num(Num).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BOOLEAN VALUE DEFINITION STATEMENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boolvalue(bool(Bool))--> ['true'].
boolvalue(bool(Bool))--> ['false'].
boolvalue(bool(Bool))-->  num(Bool). % 0 for false and 1 for true (check this again).

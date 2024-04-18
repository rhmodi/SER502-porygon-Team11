block(blck(D,E)):- ['{'],decl(D),commd(C),['}'].

decl(dec(D,DL)):- decls(D),[';'],decl(DL).
decl(dec(D)) :- decls(D),[';'].

decls(D):- constassign(D).
decls(D):- declassign(D).
decls(D):- plainassign(D).


%% Declaration statments here



% Commands List start here
commandlist(commd(PlainCmnd,CmndList)) :- plaincommand(PlainCmnd),[;],commandlist(CmndList)
commandlist(commd(PlainCmnd)) :- plaincommand(PlainCmnd), [;].

% Declaration of plain commands
plaincommand(plain(Assign)) :- assignment(Assign).
plaincommand(plain(Ternary)) :- ternary(Ternary).
plaincommand(plain(Print)) :- printStmt(Print).
plaincommand(plain(StrLen)) :- stringLen(StrLen).
plaincommand(plain(If)) :- ifcommand(If).
plaincommand(plain(IfElse)) :- ifelsecommand(IfElse).
plaincommand(plain(IfELseLadder)) :- ifELseLaddercommand(IfELseLadder).
plaincommand(plain(While)) :- whilecommand(While).
plaincommand(plain(For)) :- forcommand(For).
plaincommand(plain(ForInRange)) :- forinrangecommand(ForInRange).


% Declaration of types of assignment
assignment(assign(A)):- initialAssignment(A).
assignment(assign(A)):- declassign(A).
assignment(assign(A)):- shortantAssign(A).

shortantAssign(sassign(Var,Expr)):- variablename(Var),['+='],expr(Expr).
shortantAssign(sassign(Var,Expr)):- variablename(Var),['-='],expr(Expr).
shortantAssign(sassign(Var,Expr)):- variablename(Var),['*='],expr(Expr).
shortantAssign(sassign(Var,Expr)):- variablename(Var),['/='],expr(Expr).
shortantAssign(sassign(Var,Expr)):- variablename(Var),['%='],expr(Expr).
shortantAssign(sassign(Var,Expr)):- variablename(Var),['^='],expr(Expr).


initialAssignment(iassign(Var,Expr)):- variablename(Var),['='],expr(Expr).


% Complete other expression







%%%% IF COMMANDS%%%%%%%%%%%%%%%%%%%%%%%%%%

ifcommand(If):- ifpart(If).
ifelsecommand(If,Else):- ifpart(If),elsepart(Else).
ifELseLaddercommand(If,Elif,Else):- ifpart(If),elseifpart(Elif),elsepart(Else).

ifpart(if(B,X)):- ['if'],['('],boolcondition(B), [')'],['{'],commandlist(X),['}'].
elseifpart(elseif(B,E1,E2)):- ['else if'],['('],boolcondition(B), [')'],['{'],commandlist(E1),['}'],elseifpart(E2). 
elseifpart(elseif(B,E)):- ['else if'],['('],boolcondition(B), [')'],['{'],commandlist(E),['}'].
elsepart(else(C)):- ['else'],['{'],commandlist(C),['}'].


%% EXPRESSIONS STARTS HERE


% Change in grammer make it left recursive
expr(add(X,Y)):- expr(X),['+'],term(Y).
expr(sub(X,Y)):- expr(X),['-'],term(Y).
expr(X):- term(X).

term(mult(X,Y)):- term(X),['*'],factor(Y).
term(divi(X,Y)):- term(X),['/'],factor(Y).
term(modulo(X,Y)):- term(X),['%'],factor(Y).
term(X):- factor(X).

factor(expo(X,Y)):- factor(X),['^'],exponent(Y).
factor(X):- exponent(X).

exponent(S):- square(S).
exponent(Sr):- squareRoot(Sr).
exponent(C):- cube(C).
exponent(Cr):- cubeRoot(Cr).
exponent(X):- ['('],expr(X),[')'].
exponent(A):- initialAssignment(A).
exponent(Var):- variablename(Var).
exponent(N):- num(N).

square(sq(S)):- ['sq'],['('],expr(S),[')'].
squareRoot(sqrt(Sr)):- ['sqrt'],['('],expr(Sr),[')'].
cube(cube(C)):- ['cube'],['('],expr(C),[')'].
cubeRoot(cbrt(S)):- ['cbrt'],['('],expr(S),[')'].






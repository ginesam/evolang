:- module(induction, [simplify/1]).

% System modules
:- use_package(dcg).
:- use_module(library(dcg_expansion)).
:- use_module(library(dynamic)).
:- use_module(library(terms)).
:- use_module(library(write)).

% Own modules
:- use_module(agent_params, [new_rule_id/1]).
:- use_module(utils, [clean_body/2]).

:- dynamic s/7, p/7.
:- multifile s/7, p/7.

% SHORT DESCRIPTION:
% This module implements the induction mechanisms used by the agents.
% Here, we use the simplification operator only.

% simplify(Rule) (+)
% R is a newly created rule, which is simplified against others.
simplify(R) :-
   % break rule R apart in various components
   new_components(R,M,N,Rest1E,Rest2E,E2P),
   % X is a variable which replaces a proposition in the original meaning
   nth(N,M,X), 
   % adapt rule and proceed to second step of simplification
   simplify_cont(M,X,Rest1E,Rest2E,E2P), !,
   % remove original rule from grammar 
   retract(R).
simplify(_R) :- nl, write('Could not simplify'), nl. 


% new_components(R,M,VPos,Rest1E,Rest2E,E2P) (+,-,-,-,-,-)
% R: original rule
% M: original meaning in R, but replacing a proposition with a variable _X
% VPos: position of _X in M
% Rest1E: part of the original expression appearing before _X
% Rest2E: part of the original expression appearing after _X
% E2P: position of _X in the original expression 

% new_components for  meanings of type [not,_]
new_components(R,[M1,_],2,Rest1E,Rest2E,E2P) :- 
   % get meaning and expression from rule
   check_clause([M1,M2],E1,R),
   % get expression for the argument
   check_clause(M2,E2,_),
   % separate part of the expression corresponding to the argument
   rest_exp(E2,E1,Rest1E,Rest2E,E2P).

% new_components for meanings of type [f,_,_]
new_components(R,NewM,VPos,Rest1E,Rest2E,E2P) :- 
   % get meaning and expression from rule
   check_clause([M1,M2,M3],E1,R),
   % replace argument with variable and get its expression
   new_mean([M1,M2,M3],E2,NewM,VPos),
   % separate part of the expression corresponding to the argument
   rest_exp(E2,E1,Rest1E,Rest2E,E2P).


% gets meaning M and expression E from a given rule C
check_clause(M,E,C) :- 
   % break rule in head :- body
   \+ var(C), !, C = (H :- Body),
   % get meaning from head and body clause
   H = s(M,_,_,_,_,_,_), clause(H,Body),
   % split body into expression chain and score parts
   Body=..[',',Exp,Rest], Rest=..[is,_,_],
   % get expression  
   translate(Exp,E), !.

check_clause(M,E,_) :- 
   % obtain body for propositional rule
   H = p(M,_,_,_,_,_,_), clause(H,Body),
   % separate body and get expression 
   Body=..[',',Exp,Rest], 
   Rest=..[is,_,_], translate(Exp,E), \+ var(E). 

new_mean([M1,M2,M3],E2,[M1,_,M3],2) :- check_clause(M2,E2,_). 
new_mean([M1,M2,M3],E2,[M1,M2,_],3) :- check_clause(M3,E2,_). 

% obtains character sequence from C/3 chains (Prolog DCG translation)
translate('C'(_,Car,_),[Car]).
translate(E,[Car|TE]) :- E=..[',','C'(_,Car,_),T], translate(T,TE).  


% rest_exp(E2,E1,Rest1E,Rest2E,P) (+,+,-,-,-)
% E2 : expression for an argument
% E1 : expression for a meaning containing E2
% Rest1E, Rest2E : part of E1 located before and after E2 respectively
% P : position [1,2,3] of E2 in E1
rest_exp(E2,E1,_Rest1E,Rest2E,1) :-
   % E2 at the beginning (1)
   append(E2,Rest2E,E1), 
   Rest2E = [Elem | _], \+ number(Elem).

rest_exp(E2,E1,Rest1E,_Rest2E,3) :- 
   % E2 at the end (3)
   append(Rest1E,E2,E1), 
   Rest1E = [Elem | _], \+ number(Elem).

rest_exp(E2,E1,Rest1E,Rest2E,2) :- 
   % E2 in the middle (2)
   append(Rest1E,Rest,E1), 
   Rest1E = [Elem1 | _], \+ number(Elem1), 
   append(E2,Rest2E,Rest),
   Rest2E = [Elem2 | _], \+ number(Elem2).


% meaning, an anynoymous variable (proposition deleted from M), add before, add after, position
% argument independent, can be whatever
% create new rule depending on position -> then simplify2

% simplify_cont(M,X,Rest1E,Rest2E,P)
% M : original meaning of the rule with an argument replaced
% X : new proposition variable
% Rest1E : expression bit prior to X
% Rest2E : expression bit after X
% P : position of X in the expression
simplify_cont(M,X,_Rest1E,Rest2E,1) :- 
   % ensure rule does not exist already
   \+ var(Rest2E), !, \+ repeated1(M,_Rest1E,Rest2E,1),
   % create new simplified rule 
   new_rule_id(Id), L=[Id | T], 
   dcg_translation( (s(M,S,0,Id,L) --> p(X,S1,_,_,T), Rest2E, {S is S1*0.1}), R2), 
   % continue simplification from the new rule
   simplify2(R2, M).

simplify_cont(M,X,Rest1E,Rest2E,2) :- 
   % ensure rule does not exist already
   \+ var(Rest1E), \+ var(Rest2E), !, \+ repeated1(M,Rest1E,Rest2E,2),
   % create new simplified rule 
   new_rule_id(Id), L=[Id | T], 
   dcg_translation( (s(M,S,0,Id,L) --> Rest1E, p(X,S1,_,_,T), Rest2E, {S is S1*0.1}), R2), 
   % continue simplification from the new rule
   simplify2(R2, M).

simplify_cont(M,X,Rest1E,_Rest2E,3) :- 
   % ensure rule does not exist already
   \+ var(Rest1E), !, \+ repeated1(M,Rest1E,_Rest2E,3), 
   % create new simplified rule
   new_rule_id(Id), L=[Id | T], 
   dcg_translation( (s(M,S,0,Id,L) --> Rest1E, p(X,S1,_,_,T), {S is S1*0.1}), R2), 
   % continue simplification from the new rule
   simplify2(R2, M).

repeated1(M,_Rest1E,Rest2E,1) :-  clause(s(M,_,_,_,_,_,_),B), 
   clean_body(B,B2), B2 =..[',',S,Tail], Tail =..[',',Exp,I], 
   translate(Exp,Rest2E), I =..[is,_,_], S =..['p'|_]. 

repeated1(M,Rest1E,Rest2E,2) :-  clause(s(M,_,_,_,_,_,_),B), 
   clean_body(B,B2), B2 =..[',',Exp,Tail], translate(Exp,Rest1E), 
   Tail =..[',',S,SR], S =..['p'|_], SR =..[',',Exp2,I], 
   translate(Exp2,Rest2E), I =..[is,_,_].

repeated1(M,Rest1E,_Rest2E,3) :-  clause(s(M,_,_,_,_,_,_),B), 
   clean_body(B,B2), B2 =..[',',Exp,Tail], translate(Exp,Rest1E), 
   Tail=..[',',S,I], I=..[is,_,_], S =..['p'|_]. 



% here first simplification step has succeeded
% R is a one-variable simplified rule, M is the meaning with a simplified argument
% now attempt to simplify the other argument : [and,do,da] -> [and,_X,da] -> [and,_X,_Y]
simplify2(R2,M2) :- 
   length(M2,3),
   % break apart rule R2 in its components
   new_components2(R2,M3,X,Y,ConExpression,PosConnective,Inversion),
   % ensure rule does not exist already
   \+ repeated2(M3,X,Y,ConExpression,PosConnective,Inversion), !, 
   % create new rule and add to grammar
   new_rule(M3,X,Y,ConExpression,PosConnective,Inversion,T), 
   dcg_translation(T,R3), assert(R3), !.

% if not further simplification is possible (or meaning is [not,_X]), add current rule
simplify2(R2,_) :-  assert(R2).


% new_components2(R2,M3,X,Y,ConExpression,PosConnective,Inversion)
% R2: simplified rule (1 time)
% M3: meaning of R2 with only variables as arguments
% X,Y: argument variables
% ConExpression: expression for the connective alone
% PosConnective: position of the connective in the expression [1,2,3]
% Inversion: whether arguments in the formula are inverted or not
new_components2(R2,M3,X,Y,ConExpression,PosConnective,Inversion) :- 
   \+ var(R2), R2=..[':-',s(M,_,_,_,_,_,_),Body], length(M,3), !, 
   % find position of the variable and atom in the meaning
   n_var_atom(M,NVar,NAtom), 
   % get the atom to simplify and its expression
   nth(NAtom,M,A2), check_clause(A2,EA,_),
   % create new meaning replacing atom with a variable
   new_mean2(M,NVar,NAtom,M3),
   % get variables
   nth(2,M3,X), var(X), nth(3,M3,Y), var(Y),
   % gather info of the new rule
   info_new_rule(Body,EA,NVar,ConExpression,PosConnective,Inversion). 

n_var_atom([M1,M2,M3],2,3) :- \+ var(M1), var(M2), \+ var(M3). 
n_var_atom([M1,M2,M3],3,2) :- \+ var(M1), var(M3), \+ var(M2). 

new_mean2([M1,M2,_],2,3,[M1,M2,_]).
new_mean2([M1,_,M3],3,2,[M1,_,M3]).


% info_new_rule(Body,Atom_exp,NVar,ConExpression,PosConnective,Inversion) (+,+,+,-,-,-)
info_new_rule(Body,E2,NVar,ConE,PosConnective,Invierte) :- 
   clean_body(Body,B), B =.. [',',Exp1,T1], translate(Exp1,E2), 
   T1 =.. [',',P,T2], P =.. [p|_], T2 =.. [',',Exp2,I], translate(Exp2,ConE), 
   I =.. [is|_], PosConnective = 3, NV is NVar - 1, invert(NV,2,Invierte), !.

info_new_rule(Body,E2,NVar,ConE,PosConnective,Invierte) :- 
   clean_body(Body,B), B =.. [',',Exp1,T1], translate(Exp1,ConE), 
   T1 =.. [',',P,T2], P =.. [p|_], T2 =.. [',',Exp2,I], translate(Exp2,E2), 
   I =.. [is|_], PosConnective = 1, NV is NVar - 1, invert(NV,1,Invierte), !.

info_new_rule(Body,E2,NVar,ConE,PosConnective,Invierte) :- 
   clean_body(Body,B), B =.. [',',Exp1,T1], translate(Exp1,Rest), append(E2,ConE,Rest), 
   T1 =.. [',',P,I], P =.. [p|_], I =.. [is|_], 
   PosConnective = 2, NV is NVar - 1, invert(NV,2,Invierte), !.

info_new_rule(Body,E2,NVar,ConE,PosConnective,Invierte) :- 
   clean_body(Body,B), B =.. [',',Exp1,T1], translate(Exp1,Rest), append(ConE,E2,Rest), 
   T1 =.. [',',P,I], P =.. [p|_], I =.. [is|_], 
   PosConnective = 1, NV is NVar - 1, invert(NV,2,Invierte), !.

info_new_rule(Body,E2,NVar,ConE,PosConnective,Invierte) :- 
   clean_body(Body,B), B =.. [',',P,T1], P =.. [p|_], 
   T1 =.. [',',Exp1,I], translate(Exp1,Rest), append(E2,ConE,Rest), I =.. [is|_], 
   PosConnective = 3, NV is NVar - 1, invert(NV,1,Invierte), !.

info_new_rule(Body,E2,NVar,ConE,PosConnective,Invierte) :- 
   clean_body(Body,B), B =.. [',',P,T1], P =.. [p|_], 
   T1 =.. [',',Exp1,I], translate(Exp1,Rest), append(ConE,E2,Rest), I =.. [is|_], 
   PosConnective = 2, NV is NVar - 1, invert(NV,1,Invierte), !.

% inversion only occurs when the relative position of the variable in the formula (1/2)
% is different from the relative position (1/2) of the matching expression
invert(NVar,PosExpression,0) :- NVar == PosExpression, !. 
invert(_,_,1).


% new_rule(Meaning,X,Y,ConExpression,PosConnective,Inversion,Rule) (+,+,+,+,+,+,-)
new_rule(M,X,Y,ConExpression,1,0,Rule) :- new_rule_id(Id), 
   Rule = (s(M,R,0,Id,[Id|T]) --> ConExpression, p(X,R1,_,_,L1), p(Y,R2,_,_,L2), 
   {append(L1,L2,T), R is R1*R2*0.1}).

new_rule(M,X,Y,ConExpression,1,1,Rule) :- new_rule_id(Id), 
   Rule = (s(M,R,0,Id,[Id|T]) --> ConExpression, p(Y,R1,_,_,L1), p(X,R2,_,_,L2), 
   {append(L1,L2,T), R is R1*R2*0.1}).

new_rule(M,X,Y,ConExpression,2,0,Rule) :- new_rule_id(Id), 
   Rule = (s(M,R,0,Id,[Id|T]) --> p(X,R1,_,_,L1), ConExpression, p(Y,R2,_,_,L2), 
   {append(L1,L2,T), R is R1*R2*0.1}).

new_rule(M,X,Y,ConExpression,2,1,Rule) :- new_rule_id(Id), 
   Rule = (s(M,R,0,Id,[Id|T]) --> p(Y,R1,_,_,L1), ConExpression, p(X,R2,_,_,L2), 
   {append(L1,L2,T), R is R1*R2*0.1}).

new_rule(M,X,Y,ConExpression,3,0,Rule) :- new_rule_id(Id), 
   Rule=(s(M,R,0,Id,[Id|T]) --> p(X,R1,_,_,L1), p(Y,R2,_,_,L2), ConExpression,  
   {append(L1,L2,T), R is R1*R2*0.1}).

new_rule(M,X,Y,ConExpression,3,1,Rule) :- new_rule_id(Id), 
   Rule=(s(M,R,0,Id,[Id|T]) --> p(Y,R1,_,_,L1), p(X,R2,_,_,L2), ConExpression,  
   {append(L1,L2,T), R is R1*R2*0.1}).


% repeated2(M,X,Y,ConExpression,PosConnective,Inversion)
repeated2(M,X,Y,ConExpression,1,0) :- 
   clause(s(M,_,_,_,_,_,_),B2), clean_body(B2,B),
   B=..[',',Exp,Tail], translate(Exp,ConExpression), 
   Tail=..[',',S1,Tail2], S1=p(X,_,_,_,_,_,_), 
   Tail2=..[',',S2,_], S2=p(Y,_,_,_,_,_,_), !.

repeated2(M,X,Y,ConExpression,1,1) :- 
   clause(s(M,_,_,_,_,_,_),B2), clean_body(B2,B),
   B=..[',',Exp,Tail], translate(Exp,ConExpression), 
   Tail=..[',',S1,Tail2], S1=p(Y,_,_,_,_,_,_), 
   Tail2=..[',',S2,_], S2=p(X,_,_,_,_,_,_), !.

repeated2(M,X,Y,ConExpression,2,0) :- 
   clause(s(M,_,_,_,_,_,_),B2), clean_body(B2,B),
   B=..[',',S1,Tail], S1=p(X,_,_,_,_,_,_), 
   Tail=..[',',Exp,Tail2], translate(Exp,ConExpression), 
   Tail2=..[',',S2,_], S2=p(Y,_,_,_,_,_,_), !.

repeated2(M,X,Y,ConExpression,2,1) :- 
   clause(s(M,_,_,_,_,_,_),B2), clean_body(B2,B),
   B=..[',',S1,Tail], S1=p(Y,_,_,_,_,_,_), 
   Tail=..[',',Exp,Tail2], translate(Exp,ConExpression), 
   Tail2=..[',',S2,_], S2=p(X,_,_,_,_,_,_), !.

repeated2(M,X,Y,ConExpression,3,0) :- 
   clause(s(M,_,_,_,_,_,_),B2), clean_body(B2,B),
   B=..[',',S1,Tail], S1=p(X,_,_,_,_,_,_),
   Tail=..[',',S2,Tail2], S2=p(Y,_,_,_,_,_,_), 
   Tail2=..[',',Exp,_], translate(Exp,ConExpression), !.

repeated2(M,X,Y,ConExpression,3,1) :- 
   clause(s(M,_,_,_,_,_,_),B2), clean_body(B2,B),
   B=..[',',S1,Tail], S1=p(Y,_,_,_,_,_,_),
   Tail=..[',',S2,Tail2], S2=p(X,_,_,_,_,_,_), 
   Tail2=..[',',Exp,_], translate(Exp,ConExpression), !.

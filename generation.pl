:- module(generation, [coherence/4, first_max/2, generates/5]).

% System modules
:- use_module(library(dcg_expansion), [dcg_translation/2]).
:- use_module(library(dynamic), [dynamic/1]).
:- use_module(library(random), [random/3]).
:- use_module(library(write), [portray_clause/1]).

% Own modules
:- use_module(agent_params, [games_played/1, initialise/0, new_rule_id/1]).
:- use_module(conceptual_sys, [commutative_conns/1, new_exp/1]).
:- use_module(induction,[simplify/1]).
:- use_module(utils, [rnd_select/3, rnd_permu/2, flatten/2, subseq_rest/4]).

:- dynamic s/7, p/7.
:- multifile s/7, p/7.

% SHORT DESCRIPTION:
% This module implements the manner in which agents generate sentences when 
% they want to express a certain meaning. It also provides how
% to check whether 2 expressions are logically equivalent.

% generates(SpeakerMeaning,Expresion,Rest_expressions,CanSay,Invent)
% CanSay is true if the speaker can express the meaning using its lexicon/grammar
% Invent is true if the speaker invents the expression
generates(SM,E,T,1,0) :- 
   expressions(SM,[H|T]), H=[_,E], \+ var(E), !.

generates(SM,E,_,0,1) :- 
   invention(SM,E), new_rule_id(Id),
   dcg_translation( (s(SM,R,0,Id,[Id]) --> E, {R is 0.1}), T), 
   % apply induction on the new rule
   assert(T), simplify(T), !.

% expressions(Meaning,Expressions)
% produces a list of pairs of the form [Score,Expression] for
% each expression denoting the meaning
expressions(M,Expressions) :- \+ atom(M), !, 
   findall([S,E],s(M,S,_,_,_,E,[]),Set), \+ Set == [],
   first_max(Set,Expressions).

expressions(M,Expressions) :- atom(M), !,
   findall([S,E],p(M,S,_,_,_,E,[]),Set), \+ Set == [],
   first_max(Set,Expressions).


% first ahead is an element [Score,Expression] of maximum score
first_max([[HS,HE]|T],NewL) :- 
   max_first(T,HS,HE,Exp,Hole,First),
   Hole = [], random_first(Exp,First,NewL). 

max_first([],HS,HE,Hole,Hole,[HS,HE]).
max_first([[HS,HE]|T],MaxS,MaxE,[[MaxS,MaxE]|Exp],Hole,F) :-
   % MaxS, MaxE current maximum
   % change values if current element is better
   HS > MaxS, !, max_first(T,HS,HE,Exp,Hole,F).
   
max_first([[HS,HE]|T],MaxS,MaxE,[[HS,HE]|Exp],Hole,F) :- 
   % keep looking for alternatives
   max_first(T,MaxS,MaxE,Exp,Hole,F).  


% pick one element of maximum score at random
random_first(Exp,First,NewL) :- 
   First = [SMax,_], findall([S,E], max_score([S,E],SMax,[First|Exp]), G),
   rnd_select(G,1,[HX]), NewL = [HX,First|Exp].

max_score([S,E],SMax,L) :- member([S,E],L), S = SMax.


% invention(Meaning,Expression) (+,-)
% returns an expression to express the meaning
invention(M,E) :- \+ list(M), generates(M,E,_,1,0), !.

invention(M,E) :- \+ list(M), new_exp(E).

% invention for meanings of type [not,p1]
invention([M1,M2],E) :- 
   invention(M1,E1), invention(M2,E2), 
   rnd_permu([E1,E2],X), flatten(X,E). 

% invention for meanings of type [f,p1,p2]
invention([M1,M2,M3],E) :- 
   invention(M1,E1), invention(M2,E2), invention(M3,E3), 
   rnd_permu([E1,E2,E3],X), flatten(X,E).


% coherence(SpeakerMeaning, SpeakerExpression, HearerExpression, Result)
% determines if both expressions are logically equivalent
% that is, they use the same word and position for the connective,
% but if the connective is a commutative one, it is allowed to also
% invert the arguments
coherence(SM,SE,HE,1) :- 
   generates(SM,HE,_,1,0), compatible(SM,SE,HE), !.
coherence(_,_,_,0) :- !.

compatible(_,SE,HE) :- SE = HE, !.
compatible([BF|P],SE,HE) :- 
   length(SE,N), length(HE,N), 
   commutative_conns(CC), member(BF,CC),
   P = [P1,P2], generates(P1,E1,_,1,0), generates(P2,E2,_,1,0),
   % C is the expression of the connective used by the speaker
   subseq_rest(E1,SE,X,_), subseq_rest(E2,X,C,_), 
   % PosC1 is where C occurs in speaker expression
   subseq_rest(C,SE,SE2,PosC1),
   % PosC2 is where C occurs in hearer expression (fails if non-existent)
   subseq_rest(C,HE,HE2,PosC2), 
   % check connective in the same position
   same_position(C,PosC1,PosC2,N),
   % check that both remains contain E1 and that remains are equal
   subseq_rest(E1,SE2,SE3,_), subseq_rest(E1,HE2,HE3,_), SE3 = HE3, !.

% compare positions, both beginnnig or end
same_position(_,P,P,_) :- !.
% compare positions, middle position
same_position(C,P1,P2,EL) :- P1 > 1, P2 > 1, length(C,CL), X is EL-CL+1, P1 < X, P2 < X, !.

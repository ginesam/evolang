:- module(adaptation, [repair_scores/5]).

% System modules
:- use_module(library(dynamic), [dynamic/1]).

% Own modules
:- use_module(conceptual_sys, [commutative_conns/1, propositions/1]).
:- use_module(generation, [generates/5]).
:- use_module(utils, [clean_body/2]).

:- dynamic s/7, p/7.
:- multifile s/7, p/7.

% SHORT DESCRIPTION:
% This module implements the adaptation of connective rule weights 
% according to the results of each language game.

% alignment rate value
alignment_rate(0.1).


% repair_scores(Expression, HearerMeaning, RestMeanings, CanUnderstand, Agree)
% propositional meanings are already repaired
repair_scores(_,HM,_,_,_) :- propositions(L), member(HM,L), !.
 
repair_scores(E,HM,RM,1,1) :- !, 
   % reinforce rule used by hearer to infer when both agents logically agree
   rule_reinforce(HM,E), 
   rest_meanings(RM, HM, Comp_Meanings),
   % dicourage competing meanings which use the same expression
   discourage_comp_meanings(Comp_Meanings,E),
   % hearer tries to utter HM and discourages competing expressions
   repair_scores_cont(HM,E), !.

repair_scores(E,HM,_,1,0) :- !,
   % dicourage rule to obtain HM if agents in the game do not logically agree
   rule_discourage(HM,E).

% nothing to do when no meaning is inferred by the hearer
repair_scores(_,_,_,_,_) :- !.


rule_reinforce(M,E) :- 
   s(M,_,_,Id,_,E,[]), !, clause(s(M,_,_,Id,_,_,_),B), 
   \+ var(B), \+ var(Id), reinforce_clause(_M,Id). 
rule_reinforce(_M,_E).

rule_discourage(M,E) :- 
   s(M,_,_,Id,_IdL,E,[]), !, clause(s(M,_,_,Id,_,_,_),B), 
   \+ var(B), \+ var(Id), discourage_clause(_M,Id). 
rule_discourage(_M,_E).


reinforce_clause(M,Id) :- 
   Head=..[s,M,S,U,Id,IdL,L1,L2], clause(Head,B2), 
   \+ var(B2), clean_body(B2,Body), \+ var(U), U1 is U+1, 
   NewHead =..[s,M,S,U1,Id,IdL,L1,L2], 
   update_score(Body, reinforce, NB), 
   retract((Head :- Body)), assert((NewHead :- NB)), !. 

discourage_clause(M,Id) :- 
   Head=..[s,M,S,U,Id,IdL,L1,L2], clause(Head,B2), 
   \+ var(B2), clean_body(B2,Body), \+ var(U), U1 is U+1, 
   NewHead =..[s,M,S,U1,Id,IdL,L1,L2], 
   update_score(Body, discourage, NB), 
   retract((Head :- Body)), assert((NewHead :- NB)), !. 


rest_meanings([],_,[]) :- !. 
rest_meanings(Rest_Meanings,MH,Comp_Meanings) :- 
   delete(Rest_Meanings,MH,Comp_Meanings).

rest_expressions([],_,[]) :- !. 
rest_expressions(Rest_Expressions,E,Comp_Expres) :- 
   delete(Rest_Expressions,E,Comp_Expres).


discourage_comp_meanings([],_E). 
discourage_comp_meanings([H|T],E) :- rule_discourage(H,E), 
   discourage_comp_meanings(T,E). 

discourage_comp_expressions(_M,[]). 
discourage_comp_expressions(M,[H|T]) :- rule_discourage(M,H), 
   discourage_comp_expressions(M,T). 


repair_scores_cont(MH,E) :-
   generates(MH,HE,Rest_Expressions,1,_), 
   rest_expressions([HE|Rest_Expressions],E,Comp_Expres), 
   discourage_comp_expressions(MH,Comp_Expres), !. 

repair_scores_cont(_MS,_E) :- !.


update_score(B,reinforce,RB) :- B=..[is,R, E], \+ E=..['*'|_], !, 
   reinforce_s(E,Score), RB=..[is,R, Score].  

update_score(B,reinforce,RB) :- B=..[is,R, E], E=..['*',T,S], !, 
   reinforce_s(S,Score), RP=..['*',T,Score], RB=..[is,R,RP].  

update_score(B,reinforce,RB) :- B=..[',',H,T], 
   update_score(T,reinforce,RT), RB=..[',',H,RT]. 

update_score(B,discourage,RB) :- B=(R is E), \+ E='*'(_,_), !,  
   discourage_s(E,Score), RB=(R is Score).  

update_score(B,discourage,RB) :- B=(R is E), E='*'(T,S), !, 
   discourage_s(S,Score), RB=(R is (T*Score)).  

update_score(B,discourage,RB) :- B=..[',',H,T],   
   update_score(T,discourage,RT), RB=..[',',H,RT]. 


discourage_s(Score,UpdatedScore) :- alignment_rate(X), UpdatedScore is Score*(1-X).
reinforce_s(Score,UpdatedScore) :- alignment_rate(X), UpdatedScore is X + Score*(1-X).

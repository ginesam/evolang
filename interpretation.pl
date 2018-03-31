:- module(interpretation, [understands/7]).

% System modules
:- use_module(library(dcg_expansion)).
:- use_module(library(dynamic), [dynamic/1]).
:- use_module(library(lists), [delete/3]).

% Own modules
:- use_module(agent_params, [new_rule_id/1]).
:- use_module(conceptual_sys, [commutative_conns/1]).
:- use_module(generation, [generates/5, first_max/2]).
:- use_module(induction,[simplify/1]).
:- use_module(utils, [clean_body/2]).

:- dynamic s/7, p/7.
:- multifile s/7, p/7.

% SHORT DESCRIPTION
% This module defines how agents interpret sentences and try to parse them.

% understands(Expression,SpeakerMeaning,HearerMeaning,Rest_meanings,CanUnd,Agree,Adopt)
understands(E,SM,HM,Rest_Meanings,1,R,Adopta) :- 
   % find possible meanings and the most probable HM
   interprets(E,SM,HM,Rest_Meanings,1,Adopta), 
   \+ var(SM), \+ var(HM), 
   % determine if both original and inferred meanings are logically equivalent
   guessed_right(SM,HM,R), !.

% if the speaker is not able to guess, then it invents a new rule for the expression
understands(E,SM,SM,_,0,0,Adopta) :- interprets(E,SM,SM,_,0,Adopta), !.


% given an expression, find the most probable meaning (HM) and others possible (T)
interprets(E,_,HM,T,1,0) :- meanings(E,[H|T]), H=[_,HM], !. 

% invent a new rule if guessing is not possible
interprets(E,SM,SM,_,0,1) :- new_rule_id(Id), 
   dcg_translation( (s(SM,R,0,Id,[Id]) --> E, {R is 0.1}), T),
   % apply induction on the new rule
   assert(T), simplify(T), !.

meanings(E,Meanings) :- 
   findall([S,M],s(M,S,_,_,_,E,[]),Set), 
   \+ Set == [], first_max(Set,Meanings).

meanings(E,Meanings) :- 
   findall([S,M],p(M,S,_,_,_,E,[]),Set), 
   \+ Set == [], first_max(Set,Meanings).


% logically equivalent expressions
guessed_right(SM,HM,1) :- log_equivalent_meaning(SM,HM), !.
guessed_right(_,_,0).

log_equivalent_meaning(A,A).
log_equivalent_meaning([A,B,C],[A,C,B]) :- 
   commutative_conns(L), member(A,L), !. 
log_equivalent_meaning([if,B,C],[oif,C,B]).  
log_equivalent_meaning([oif,B,C],[if,C,B]).  
log_equivalent_meaning([nif,B,C],[noif,C,B]).  
log_equivalent_meaning([noif,B,C],[nif,C,B]).

:- module(agent_params, [games_played/1, initialise/0, new_rule_id/1, update_games_played/0]).

% System  modules
:- use_module(library(dcg_expansion), [dcg_translation/2]).
:- use_module(library(dynamic), [dynamic/1]).

% Own modules
:- use_module(conceptual_sys, [new_exp/1, propositions/1]).

:- data games_played/1.
:- data rule_id/1, rule_date/2.
:- dynamic s/7, p/7. 
:- multifile s/7, p/7.

% SHORT DESCRIPTION:
% This module collects several parameters and functionalities which
% are used by all agents during the simulation.

% update number of completed games
update_games_played :- 
   retract_fact(games_played(N)), 
   N1 is N+1, asserta_fact(games_played(N1)).  


% obtain available id for a new rule
new_rule_id(Id) :- 
   rule_id(Id), retract_fact(rule_id(Id)), 
   Id1 is Id+1, asserta_fact(rule_id(Id1)), !, 
   games_played(D), asserta_fact(rule_date(Id,D)).


% Initialise each agent with the common vocabulary 
% (see module conceptual_sys)
initialise :- 
   propositions(P), 
   % initial rules use negative ids
   length(P,NP), NP2 is -NP, lex_prop(P,NP2).

lex_prop(_,0) :- 
   asserta_fact(rule_id(1)), 
   asserta_fact(games_played(0)).

lex_prop([P|Ps],Id) :- new_exp(E), write(E), nl,
   % common vocabulary rules (propositions)
   dcg_translation( (p(P,S,0,Id,[Id]) --> E, {S is 1.0}),T ),
   assert(T), asserta_fact(rule_date(Id,0)),
   Id2 is Id+1, lex_prop(Ps,Id2).


% NOTE: In a general rule of the form
% p(M,S,U,Id,L) --> [w,o,r,d], {S is 1.0}

% M stands for meaning, S for score and U for the number of times
% this rule has been used. Id is the rule unique identifier and L
% is a list containing the identifiers of other rules used by this
% rule (including itself). 

% There are also different syntactic categories: s means compound
% sentence, p propositional sentence and c connective.

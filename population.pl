:- module(population, [ini_players/1, players/3, population_size/1]).

% System modules
:- use_module(library(random), [random/3]).

% Own modules
:- use_module(utils, [rnd_select/3]).

:- data population_size/1.
:- data hearers_count/2.

% SHORT DESCRIPTION:
% This module contains information related to the population of agents.
% It defines which agents take part in the current language game.
% For each agent, there is a counter with information about how many
% times this agent has played with the others.

% initialisation procedure
% ini_players(num_agents) (+)
ini_players(0).
ini_players(Agent) :- 
   count_list(Agent,L), asserta_fact(hearers_count(Agent,L)),
   Agent1 is Agent-1, ini_players(Agent1).

% create counters for an agent
count_list(Agent,L) :- population_size(N), count_list_down(Agent,N,L).

count_list_down(_,0,[]) :- !.
count_list_down(N,N,L) :- N2 is N-1, !, count_list_down(N,N2,L).
count_list_down(A,N,L) :- N2 is N-1, count_list_down(A,N2,L2), append(L2,[[0,N]],L).


% players(Game,Speaker,Hearer) (+,-,-)
% gives the speaker and hearer that take part in the current game Game
players(N,S,H) :- 
   population_size(P), 
   % speakers are chosen using a queue based approach
   S is (N mod P) + 1, 
   % choose hearer randomly among the ones which have spoken less with speaker
   hearer(S,H), !. 

hearer(S,H) :- 
   % get number of times spoken to other agents
   hearers_count(S,L),
   % obtain minimum and pick a random agent
   minim(L,C), setof(A, member([C,A],L), AS),
   rnd_select(AS,1,[H]),
   % add one to count
   update_count([C,H],L,L1), 
   retract_fact(hearers_count(S,L)), 
   asserta_fact(hearers_count(S,L1)), !.

minim([[F,_]|T],M) :- minimac(T,F,M). 

minimac([],A,A).
minimac([[H,_]|T],A,M) :- H < A, !, minimac(T,H,M). 
minimac([_|T],A,M) :- minimac(T,A,M).

update_count(_,[],[]).
update_count([C,H],[[C,H]|T],[[C1,H]|T]) :- !, C1 is C + 1. 
update_count([C,H],[Head|T],[Head|T1]) :- update_count([C,H],T,T1). 

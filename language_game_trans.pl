:- module(language_game_trans, [main/1]).

% System modules
:- use_module(library(dec10_io), [tell/1, told/0]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists)).
:- use_module(library(random), [srandom/1, random/3]).
:- use_module(library(read), [read/2]).
:- use_module(library(sockets), [connect_to_socket/3, socket_shutdown/2]).
:- use_module(library(system), [current_host/1]).
:- use_module(library(write), [write_canonical/2, write/1]).

% Own modules
:- use_module(agent, [ag_socket_port/2, write_stream/2]).
:- use_module(conceptual_sys, [meaning/1]).
:- use_module(language_game, [roles_to_ports/4, report_ind/5]).
:- use_module(population, [players/3, ini_players/1, population_size/1]).
:- use_module(utils, [remove_at/4, insert_at/4, rnd_select/3,atom_and_number/2]).

% count agree and coherence
:- data last_resultS/1, last_resultC/1.

% agents belonging to each group
:- data eldest/1, adults/1, young/1.

% SHORT DESCRIPTION:
% This module implements a particular type of language game, the transmission game.
% The transmission game is based on the naming game, but agents are split into 3 
% generational groups: elder, adult and young.
% After a certain number of games (500), generation shift occurs: a group of newborn 
% agents with no knowledge of the language replace the youngest group, the old 
% youngest group becomes the new adult group, the old adult group becomes the new
% eldery, and the old eldery is deleted (dies).

main([NGames, NAgents, Step]):-
   atom_and_number(NGames, N), atom_and_number(NAgents, NA),
   atom_and_number(Step,S),
   asserta_fact(population_size(NA)),
   % initialise counts and parameters for each agent
   ini_players(NA),
   asserta_fact(last_resultS(0)), asserta_fact(last_resultC(0)),
   % name of the current host
   current_host(Host),
   % split agents evenly in 3 groups 
   generation_split(NA,[X,Y,Z]),
   asserta_fact(eldest(X)), 
   asserta_fact(adults(Y)),
   asserta_fact(young(Z)),
   % open file evol_com and make current output
   tell('run/evol_com.txt'),
   % run NGames 
   game_run(N,N,Host,NA,S),
   % close current output stream
   told.
main([]):- format("Usage: client <how_many_games> <number_of_agents>\r\f", []).

generation_split(0,[[],[],[]]) :- !.
generation_split(N,G) :- 
   I is (N rem 3)+1, N1 is N-1,
   generation_split(N1,G2), nth(I,G2,X),
   insert_at([N|X],G2,I,G3), I2 is I+1, 
   remove_at(X,G3,I2,G).


% game_run(Game,NGames,Host,NAgents,Step)
% Game: number of the current game (descending)
% NGames: total number of games
% Host: current host
% NAgents: number of agents
% Step: report interval

% when finished running games, close and report agents' grammars and stats
game_run(0,NGames,_Host,0,Step) :- report_est(0,NGames,[1,1],Step).
game_run(0,NGames,Host,N,Step) :- 
   population_size(PS), N > 0, N < PS, ag_socket_port(Aport,N), 
   connect_to_socket(Host, Aport, Astream), write_stream(Astream,[end]), 
   socket_shutdown(Astream,read_write), close(Astream), 
   N1 is N - 1, game_run(0,NGames,Host,N1,Step).

% when all games are executed, report adaptation, invention of each agent (for graphical purposes)
game_run(0,NGames,Host,NA,Step) :- 
   population_size(NA), report_ind(0,NGames,NA,Host,Step), ag_socket_port(Aport,NA),
   connect_to_socket(Host, Aport, Astream), write_stream(Astream,[end]), 
   socket_shutdown(Astream,read_write), close(Astream), 
   N1 is NA - 1, game_run(0,NGames,Host,N1,Step).

% run a certain game
game_run(Game,NGames,Host,NA,Step) :- 
   Game > 0, nl(user), display(user,[game,Game]),
   % set random seed for the game 
   time(T), RS is (T rem (Game*NA))+Game, srandom(RS),
   % get hearer, speaker and meaning to express (random)
   players(Game, Speaker, Hearer), meaning(SM),
   nl(user), display(user,[players,Speaker,Hearer]), 
   nl(user), display(user,[meaning,SM]), 
   % generation shift if appropriate
   blanc_agent(Game,NGames,Host),
   % get ports to transmit
   roles_to_ports(Speaker,Hearer,Sport,Hport),
   % pass meaning to speaker and read the expression it produces
   connect_to_socket(Host, Sport, Sstream), 
   write_stream(Sstream,[speaker,Game,SM]), read(Sstream,Exp),
   nl(user), display(user,[expresion,Exp]), 
   % pass expression and speaker's meaning to hearer
   connect_to_socket(Host, Hport, Hstream), 
   write_stream(Hstream,[hearer,Game,SM,Exp]),
   % read communicative success and coherence
   read(Hstream,Agree), read(Hstream,Coher), close(Hstream),
   % communicate outcome to speaker
   write_stream(Sstream,Agree), close(Sstream),
   % report stats (for graphical purposes)
   report_est(Game,NGames,[Agree,Coher],Step),
   report_ind(Game,NGames,NA,Host,Step),
   % next game
   Next is Game - 1, !, game_run(Next,NGames,Host,NA,Step).

% every a certain number of games, write number of time communicative success 
% and coherence occurred in average, and reset counters
report_est(Game,NGames,[R1,R2],Step) :-
   % game multiple of Step
   0 is (NGames-Game) rem Step, !,
   % write success
   last_resultS(N1), add_or_not(N1,R1,NR1), 
   CS is NR1/Step, 
   write(CS), tab(3),
   % write coherence 
   last_resultC(N2), add_or_not(N2,R2,NR2), 
   CO is NR2/Step, 
   write(CO), nl, 
   % reset counters
   retract_fact(last_resultS(N1)), asserta_fact(last_resultS(0)), 
   retract_fact(last_resultC(N2)), asserta_fact(last_resultC(0)), !.

% update, but not write if the game is not a multiple of Step
report_est(_,_,[R1,R2],_) :-
   % update success 
   last_resultS(N1), add_or_not(N1,R1,NR1), 
   change_or_notS(N1,NR1),
   % update coherence 
   last_resultC(N2), add_or_not(N2,R2,NR2),
   change_or_notC(N2,NR2), !.

add_or_not(N,1,NR) :- NR is N + 1. 
add_or_not(N,0,N). 

change_or_notS(N,NR) :- N < NR, retract_fact(last_resultS(N)), 
  asserta_fact(last_resultS(NR)), !. 
change_or_notS(_,_).

change_or_notC(N,NR) :- N < NR, retract_fact(last_resultC(N)), 
  asserta_fact(last_resultC(NR)), !. 
change_or_notC(_,_).


% generation shift every 500 games (change value here)
blanc_agent(Game,NGames,Host) :- 
   0 is (NGames-Game) rem 500, !, 
   retract_fact(eldest(LO)), retract_fact(adults(LA)), 
   asserta_fact(eldest(LA)), retract_fact(young(LY)), 
   asserta_fact(adults(LY)), asserta_fact(young(LO)),
   new_agents(LO,Host), 
   nl(user), display(user, 'Generation shift:'),
   nl(user), display(user, [elders,LA,adults,LY,young,LO]),
   !.
blanc_agent(_,_,_) :- !. 

new_agents([],_Host) :- !.
new_agents([H|T],Host) :- new_agent(H,Host), !, new_agents(T,Host).

new_agent(NA,Host) :- 
   ag_socket_port(BPort,NA), 
   connect_to_socket(Host, BPort, Bstream), 
   write_stream(Bstream,[blanc]), !.

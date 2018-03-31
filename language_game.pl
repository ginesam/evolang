:- module(language_game, [main/1, roles_to_ports/4, report_ind/5]).

% System modules
:- use_module(library(dec10_io), [tell/1, told/0]).
:- use_module(library(format), [format/2]).
:- use_module(library(random),  [srandom/1, random/3]).
:- use_module(library(read), [read/2]).  
:- use_module(library(sockets), [connect_to_socket/3, socket_shutdown/2]).
:- use_module(library(system), [current_host/1]).
:- use_module(library(write), [write_canonical/2, write/1]).

% Own modules
:- use_module(agent, [ag_socket_port/2, write_stream/2]).
:- use_module(conceptual_sys, [meaning/1]).
:- use_module(population,[players/3,ini_players/1, population_size/1]).
:- use_module(utils, [rnd_select/3,atom_and_number/2]).

% count agree and coherence
:- data last_resultS/1, last_resultC/1.

% SHORT DESCRIPTION:
% This module implements a particular type of language game, the naming game.
% In the naming game the agents interact using language games in pairs.
% The speaker tries to communicate a sentence to express a logical formula,
% and the hearer tries to parse it and infer the meaning that the speaker
% tried to communicate. Finally, the speaker informs the hearer if the 
% meaning it understood is the correct one or not.

main([NGames, NAgents, Step]) :-
   atom_and_number(NGames, N), atom_and_number(NAgents, NA),
   atom_and_number(Step,S),
   asserta_fact(population_size(NA)),
   % initialise counts and parameters for each agent
   ini_players(NA), 
   asserta_fact(last_resultS(0)), asserta_fact(last_resultC(0)),
   % name of the current host
   current_host(Host),
   % open file evol_com.txt and make current output
   tell('run/evol_com.txt'),
   % run NGames
   game_run(N,N,Host,NA,S),
   % close current output stream
   told. 
main([]) :- format("Usage: client <how_many_games> <number_of_agents>\r\f", []).


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


% obtain ports for speaker and hearer
roles_to_ports(Speaker,Hearer,SPort,HPort) :- 
   ag_socket_port(SPort,Speaker), ag_socket_port(HPort,Hearer).

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

% every a certain number of games, also tell each agent to report its number 
% of adaptations and inventions. Those counters are NOT reset.
report_ind(Game,NGames,NA,Host,Step) :- 
   0 is (NGames-Game) rem Step, !, 
   report_agents(NA,Host).
report_ind(_,_,_,_,_).

report_agents(0,_).
report_agents(A,Host) :- 
   ag_socket_port(Sport,A), connect_to_socket(Host,Sport,Sstream),
   write_stream(Sstream,[report]), 
   close(Sstream), 
   A1 is A-1, report_agents(A1,Host).

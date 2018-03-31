:- module(agent, [main/1, ag_socket_port/2, write_stream/2]).

% System modules
:- use_module(library(concurrency), [concurrent/1, eng_call/3]).
:- use_module(library(dynamic), [dynamic/1]).
:- use_module(library(format), [format/2]).
:- use_module(library(random), [srandom/1]).
:- use_module(library(read), [read/2]).
:- use_module(library(sockets), [bind_socket/3, socket_accept/2]).
:- use_module(library(system), [current_host/1]).
:- use_module(library(write), [write/2, write_canonical/2]).

% Own modules
:- use_module(adaptation, [repair_scores/5]).
:- use_module(agent_params, [games_played/1, update_games_played/0, initialise/0]).
:- use_module(generation, [generates/5, coherence/4]).
:- use_module(interpretation, [understands/7]).
:- use_module(utils,[atom_and_number/2, clean_atom/2, clean_body/2]).

% count inventions and adoptions
:- data invent/1, adopt/1.
:- data agent_number/1.
:- dynamic s/7, p/7.
:- multifile s/7, p/7.
:- concurrent connection/1.

% SHORT DESCRIPTION:
% This module represents an agent and implements the functionalities
% associated to it. These include receiving and responding to messages.

main([AgentNumber,Seed]) :- 
   atom_and_number(AgentNumber, N),
   asserta_fact(agent_number(N)),
   atom_and_number(Seed,RS),
   srandom(RS),
   % handle_connection runs on thread
   create_thread, 
   % link to socket and initialise
   get_socket(Socket,N), initialise, !, 
   asserta_fact(invent(0)), asserta_fact(adopt(0)),
   % wait for messages
   wait_for_connections(Socket).

main(_) :- format("Usage: agent <agent_number> <random_seed>\r\f", []).


% bound agent to socket
get_socket(Socket,N) :- 
   current_host(Host), ag_socket_port(Port,N), 
   bind_socket(Port,1,Socket), 
   display('Bound to port '), display(Port), display(' in host '),
   display(Host), nl.

% define port associated to each agent (based on number)
ag_socket_port(NPort,NA) :- NPort is 3227 + NA.

% creates new streams connected to the socket
wait_for_connections(Socket) :- 
   repeat, socket_accept(Socket, Stream),
   assertz_fact(connection(Stream)), fail.


create_thread :- 
   eng_call(handle_connection, create, create).

handle_connection :-
   retract_fact(connection(Stream)),
   % reads message and deals with it
   handle_stream(Stream,[Role|Message]),
   % keep failing until role is end (simulation ended)
   Role = end, Message = [], 
   display([end,of,simulation]).

handle_stream(Stream,[Role|Message]) :- 
   read(Stream,[Role|Message]),
   handle_message(Role,Message,Stream). 


% how to handle messaiges as speaker
handle_message(speaker,[NGames,SM],Stream) :- !,
   % nl, display([speaker, game, NGames]), nl, display([meaning, SM]),
   update_games_played, 
   time(T), agent_number(N), N1 is (T rem (NGames*N)) + NGames, srandom(N1),
   % obtain expression for the random formula SM
   generates(SM,Exp,_RE,_CanSay,Invent),
   % nl, display([cansay,CanSay,expresion,Exp,inventa,Invent]),
   % return expression generated
   write_stream(Stream,Exp), read(Stream,_Agree),
   %nl, display([agree,Agree,rest_expresions,RE]),
   close(Stream),
   update_invent(Invent).

% how to handle messages as hearer
handle_message(hearer,[NGames,SM,E],Stream) :- !,
   % nl, display([hearer,game,NGames]), nl, display([expression,E]), 
   update_games_played, 
   time(T), agent_number(N), N1 is (T rem (NGames*N)) + NGames, srandom(N1),
   % try to interpret message received                   
   understands(E,SM,HM,RM,CanUnd,Agree,Adopt),
   % nl, display([can_understand,CanUnd, hearer_meaning,HM,adopta,Adopt]),
   % adapt scores
   repair_scores(E,HM,RM,CanUnd,Agree),
   % check gramatically equivalent
   call_coherence(Agree,SM,E,Coher), \+ var(Coher),
   % nl, display([agree,Agree,coher,Coher,rest_meanings,RM]), 
   write_stream(Stream,Agree), write_stream(Stream,Coher),   
   close(Stream), 
   update_adopt(Adopt).

% how to report statistics when requested
handle_message(report,_,_) :- !,
   report_adopt, report_invent.

% how to reset the agent (born again). Used in transmission game
handle_message(blanc,[],Stream) :- !,
   purge_all_rules, 
   retract_fact(adopt(_N1)), asserta_fact(adopt(0)),
   retract_fact(invent(_N2)), asserta_fact(invent(0)),
   close(Stream). 

% how to end the simulation
handle_message(end,[],Stream) :- !,
   % write grammar and close
   report_ag, close(Stream). 


% clear the agent grammar
purge_all_rules :- 
   Head=..[s,_,_,_,Id,_,_,_], 
   clause(Head,B2), \+ var(Head), \+ var(B2), 
   delete_rule(Id,Head,B2), fail. 
purge_all_rules. 

delete_rule(Id,Head,B2) :-
   Id > 0, clean_body(B2,Body), 
   retract((Head:-Body)), !. 
delete_rule(_,_,_) :- !.


report_adopt :- 
   agent_number(N), 
   % open and write run/ad_agentnumber file
   name('run/ad_',L1), name(N,L2), append(L1,L2,L3),
   name('.txt',L4), append(L3,L4,L5), 
   name(FileName,L5), open(FileName,append,Stream), 
   adopt(NA), write(Stream,NA), nl(Stream), 
   close(Stream), !.

report_invent :- agent_number(N), 				 
   % open and write run/inv_agentnumber file
   name('run/inv_',L1), name(N,L2), append(L1,L2,L3), 
   name('.txt',L4), append(L3,L4,L5), 
   name(FileName,L5), open(FileName,append,Stream), 
   invent(NA), write(Stream,NA), nl(Stream), 
   close(Stream), !.


update_adopt(1) :- retract_fact(adopt(N)), N1 is N+1, asserta_fact(adopt(N1)), !.
update_adopt(0) :- !.

update_invent(1) :- retract_fact(invent(N)), N1 is N+1, asserta_fact(invent(N1)), !.
update_invent(0) :- !.


call_coherence(0,_,_,0) :- !.
call_coherence(1,SM,SE,Coher) :-
   generates(SM,HE,_,1,_), coherence(SM,SE,HE,Coher), !.
call_coherence(1,_,_,0) :- !.


report_ag :- 
   % open and write run/gram_agentnumber file
   name('run/gram_',L1), agent_number(N), name(N,L2), 
   append(L1,L2,L3), name('.txt',L4), append(L3,L4,L5), 
   name(FileName,L5), tell(FileName), nl, write('Agent '), 
   write(N), games_played(N2), write(' games '), 
   write(N2), write_g, told. 

write_g :- nl, write('updated grammar'), nl, write_gram.

write_gram :- 
   H = s(M,_,_,_,_,_,_), clause(H,B), 
   \+ var(M), clean_atom(H,HC), clean_body(B,BC), 
   portray_clause((HC:-BC)), nl, fail.
write_gram.


% write term on the stream
write_stream(Stream, Term) :- 
   write_canonical(Stream, Term),
   % follow with dot and whitespace
   put_code(Stream,46), tab(Stream,1).

:- module(utils, _).

% System modules
:- use_module(library(random), [random/3]).

% SHORT DESCRIPTION
% This module implements various functionalities used in various modules
% Most of the time, those deal with data structures or particular details.

% remove_at(X,L,K,R) (?,?,+,?)
% X is the kth element of the list L
% R is the remaining list after the Kth element is removed from L
remove_at(X,[X|Xs],1,Xs).
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K>1, K1 is K-1, remove_at(X,Xs,K1,Ys).


% insert_at(X,L,K,R) (?,?,+,?)
% R is the result of inserting element X at the Kth position of L
insert_at(X,L,K,R) :- remove_at(X,R,K,L).


% rnd_select(L,N,R) (+,+,-)
% R contains N randomly selected elements from L
rnd_select(_,0,[]).
rnd_select(Xs,N,[X|Zs]) :- N>0, length(Xs,L), L2 is L*32,
    random(1,L2,I2), I is (I2 rem L)+1, remove_at(X,Xs,I,Ys), 
    N1 is N-1, rnd_select(Ys,N1,Zs).


% rnd_permu(L1,L2) (+,-)
% list L2 is a random permutation of L1
rnd_permu(L1,L2) :- length(L1,N), rnd_select(L1,N,L2).


% flatten(L1,L2) (+,?)
% list L2 is obtained after flattening L1 
flatten(X,[X]) :- \+ list(X).
flatten([],[]).
flatten([X|Xs],Zs) :- flatten(X,Y), flatten(Xs,Ys), append(Y,Ys,Zs).


% subseq_rest(S,L,R,P) (+,+,?,?)
% list R is obtained after removing list S from list L
% elements of S must appear in L in the exact same order
% P is the position of L where S matches 
subseq_rest([],L,L,1):- !.
subseq_rest(S,L,R,1) :- append(S,X,L), R = X.
subseq_rest(S,[H|T],R,P2) :- length(S,A), length([H|T],B), A =< B, 
  subseq_rest(S,T,Q,P), P2 is P+1, append([H],Q,R).


% atom_and_number(A,N) (?,?)
% convert an atom to a number and the other way around
atom_and_number(A, N):- 
   atom_codes(A, Codes), number_codes(N, Codes).


% cleaning predicates
% delete prefixes 'dao_chunk:', 'multifile:','lists:' and 'iso_misc:' 
% from grammar rule bodies, arguments and atoms 
clean_rule((H:-B),(H:-B1)) :- clean_body(B,B1).

clean_body(','(BH,BT),','(BHC,BTC)) :- 
   clean_arg(BH,BHC), clean_arg(BT,BTC), !. 
clean_body(B,B1) :- clean_atom(B,B1).

clean_arg(H,CH) :- H=..[','|_], !, clean_body(H,CH). 
clean_arg(H,CH) :- clean_atom(H,CH). 

clean_atom(A,A) :- \+ var(A), A =.. [is,_,_], !.
clean_atom(A,D) :- 
   \+ var(A), A =.. [C|T], name(C,L),
   mod_name(_,Pre), append(Pre,Suf,L), !, name(CC,Suf), 
   \+ var(CC), B =.. [CC|T], clean_atom(B,D). 
clean_atom(A,A) :- \+ var(A). 

mod_name('dao_chunk:',Pre) :- name('dao_chunk:',Pre). 
mod_name('multifile:',Pre) :- name('multifile:',Pre).  
mod_name('lists:',Pre) :- name('lists:',Pre). 
mod_name('iso_misc:',Pre) :- name('iso_misc:',Pre). 

:- module(conceptual_sys, [alphabet/1, connectives/1, commutative_conns/1, meaning/1, new_exp/1, propositions/1]).

% System modules
:- use_module(library(random), [random/3]).

% Library modules
:- use_module(utils, [rnd_select/3]).

% SHORT DESCRIPTION:
% This module represents the conceptual system. It provides the way
% of constructing formulas used by speakers in a language game,
% among other related functionalities.

% set of letters that can be used in sentences
alphabet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,y,u,v,w,z]).

% set of propositions that can be used as building blocks
propositions([up,do,li,da,le,ri]).

% set of connectives that can be used as logical functions
connectives([not,and,nand,or,nor,if,nif,oif,noif,xor,iff]).

% set of commutative connectives
commutative_conns([and,nand,or,nor,xor,iff]).


% meaning(SM) (-)
% SM is a randomly generated meaning in prefix notation (eg [and,le,up])
meaning(SM) :- 
   propositions(P), connectives(B),
   rnd_select(B,1,[F]), meaning_list(F,P,SM).

meaning_list(not,P,SM) :- rnd_select(P,1,P1), append([not],P1,SM).
meaning_list(F,P,SM) :- F \= not, rnd_select(P,2,P2), append([F],P2,SM).


% new_exp(E) (-)
% E is a randomly generated expression using the letters in the alphabet
new_exp(E) :- 
   random(3,6,N), alphabet(A), 
   length(A,AS), new_word(N,A,E,AS), !.

new_word(0,_,[],_).
new_word(N,A,[HC|T],AS) :- 
   random(1,AS,HN), nth(HN,A,HC), 
   N1 is N-1, new_word(N1,A,T,AS).

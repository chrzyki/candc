
:- module(freeVarCheck,[freeVarCheckDrs/1]).

:- use_module(library(lists),[member/2]).


/*========================================================================
   Free Variable Check (main predicate)
========================================================================*/

freeVarCheckDrs(Drs):-
   freeVarCheckDrs(Drs,[]-_).


/*========================================================================
   Free Variable Check (DRSs)
========================================================================*/

freeVarCheckDrs(drs([_:X|D],C),L1-L2):- !,
   freeVarCheckDrs(drs(D,C),[X|L1]-L2).

freeVarCheckDrs(drs([],C),L-L):- !,
   freeVarCheckConds(C,L).

freeVarCheckDrs(merge(B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(smerge(B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(alfa(_,B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).


/*========================================================================
   Free Variable Check (List of conditions)
========================================================================*/

freeVarCheckConds([],_):- !.

freeVarCheckConds([X|C],L):-
   freeVarCheckCond(X,L),
   freeVarCheckConds(C,L).


/*========================================================================
   Free Variable Check (Conditions)
========================================================================*/

%freeVarCheckCond(X,_):- write(X), nl, fail.

freeVarCheckCond(_:X,L):- !,
   freeVarCheckCond(X,L).

freeVarCheckCond(not(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(prop(X,B),L):- !,
   checkTerms([X],L),
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(imp(B1,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(whq(B1,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(whq(_,B1,X,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   checkTerms([X],L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(or(B1,B2),L):- !,
   freeVarCheckDrs(B1,L-_),
   freeVarCheckDrs(B2,L-_).

freeVarCheckCond(pred(Arg,_,_,_),L):- !,
   checkTerms([Arg],L).

freeVarCheckCond(rel(Arg1,Arg2,_,_),L):- !,
   checkTerms([Arg1,Arg2],L).

freeVarCheckCond(card(Arg1,Arg2,_),L):- !,
   checkTerms([Arg1,Arg2],L).

freeVarCheckCond(named(Arg,_,_,_),L):- !,
   checkTerms([Arg],L).

freeVarCheckCond(timex(Arg,_),L):- !,
   checkTerms([Arg],L).

freeVarCheckCond(eq(Arg1,Arg2),L):- !,
   checkTerms([Arg1,Arg2],L).


/*========================================================================
   Check Terms
========================================================================*/

checkTerms([],_):- !.

checkTerms([X|T],L):-
   var(X),
   member(Y,L),
   X==Y, !,
   checkTerms(T,L).

checkTerms([X|T],L):-
   atomic(X), !,
   checkTerms(T,L).

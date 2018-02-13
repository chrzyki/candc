
:- module(sortalCheck,[sortalCheckDrs/1]).

:- use_module(ontology,[isa/2,isnota/2]).

:- use_module(library(lists),[member/2,select/3]).


/*========================================================================
   Sortal Check (main)
========================================================================*/

sortalCheckDrs(B):-
   sortalCheckDrs(B,[],P),  
   allconsistent(P).


/*========================================================================
   Sortal Check (DRSs)
========================================================================*/

sortalCheckDrs(drs([],C),P1,P2):- !,
   sortalCheckConds(C,P1,P2).

sortalCheckDrs(drs([_:X|D],C),P1,P2):- !,
   sortalCheckDrs(drs(D,C),[ref(X,[])|P1],P2).

sortalCheckDrs(merge(B1,B2),P1,P3):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3).

sortalCheckDrs(smerge(B1,B2),P1,P3):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3).

sortalCheckDrs(alfa(_,B1,B2),P1,P3):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3).


/*========================================================================
   Sortal Check (DRS-Conditions)
========================================================================*/

sortalCheckConds([],P,P):- !.

sortalCheckConds([_:Cond|C],P1,P2):- !,
   sortalCheckConds([Cond|C],P1,P2).

sortalCheckConds([not(drs([],[_:eq(X,Y)]))|_],_,_):- 
   X==Y, !, fail.

sortalCheckConds([not(B)|C],P1,P3):- !,
   sortalCheckDrs(B,P1,P2),
   sortalCheckConds(C,P2,P3).

sortalCheckConds([prop(_,B)|C],P1,P3):- !,
   sortalCheckDrs(B,P1,P2),
   sortalCheckConds(C,P2,P3).

sortalCheckConds([imp(B1,B2)|C],P1,P4):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3),
   sortalCheckConds(C,P3,P4).

sortalCheckConds([whq(B1,B2)|C],P1,P4):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3),
   sortalCheckConds(C,P3,P4).

sortalCheckConds([whq(_,B1,_,B2)|C],P1,P4):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3),
   sortalCheckConds(C,P3,P4).

sortalCheckConds([or(B1,B2)|C],P1,P4):- !,
   sortalCheckDrs(B1,P1,P2),
   sortalCheckDrs(B2,P2,P3),
   sortalCheckConds(C,P3,P4).

sortalCheckConds([pred(X,Sym,_,_)|C],P1,P3):-
   select(ref(Y,Prop),P1,P2), Y==X, !,
   sortalCheckConds(C,[ref(X,[Sym|Prop])|P2],P3).

sortalCheckConds([pred(_,_,_,_)|C],P1,P2):-  
   sortalCheckConds(C,P1,P2).

sortalCheckConds([named(X,Sym,_,_)|C],P1,P3):-  
   select(ref(Y,Prop),P1,P2), Y==X, !,
   sortalCheckConds(C,[ref(X,[Sym|Prop])|P2],P3).

sortalCheckConds([named(_,_,_,_)|C],P1,P2):-  
   sortalCheckConds(C,P1,P2).

sortalCheckConds([rel(_,_,_,_)|C],P1,P2):-  
   sortalCheckConds(C,P1,P2).

sortalCheckConds([card(_,_,_)|C],P1,P2):-  
   sortalCheckConds(C,P1,P2).

sortalCheckConds([timex(_,_)|C],P1,P2):-  
   sortalCheckConds(C,P1,P2).

sortalCheckConds([eq(_,_)|C],P1,P2):-  
   sortalCheckConds(C,P1,P2).


/*========================================================================
   Consistency Check (all referents)
========================================================================*/

allconsistent([]).

allconsistent([ref(_,Concepts)|L]):-  
   consistent(Concepts),
   allconsistent(L).


/*========================================================================
   Consistency Check
========================================================================*/

consistent(L1):- 
   addSuperConcepts(L1,L2),
   \+ conflict(L2).


/*========================================================================
   Add super concepts
========================================================================*/

addSuperConcepts(C1,C2):-
   addSuperConcepts(C1,[],C3),
   (
      length(C1,Len),
      length(C3,Len), !,
      C3=C2
   ;
      addSuperConcepts(C3,C2)
   ).
   

addSuperConcepts([],L,L).

addSuperConcepts([X|L1],Accu,L2):-
   isa(X,Y),
   \+ member(Y,L1),
   \+ member(Y,Accu),
   !,
   addSuperConcepts(L1,[X,Y|Accu],L2).

addSuperConcepts([X|L1],Accu,L2):-
   addSuperConcepts(L1,[X|Accu],L2).


/*========================================================================
   Check for a conflict
========================================================================*/

conflict(L):-
   member(X,L),
   isnota(X,Y),
   member(Y,L), !. 
   


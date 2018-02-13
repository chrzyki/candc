
:- module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(library(lists),[member/2]).


/*========================================================================
   DRS with no Binding Violation
========================================================================*/

bindingViolationDrs(drs(_,C)):-
   bindingViolationConds(C).

bindingViolationDrs(merge(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationDrs(smerge(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationDrs(alfa(_,B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationConds([_:Cond|L]):-
   ( 
      bindingViolationCond(Cond), !
   ;
      bindingViolationConds(L)
   ), !.

bindingViolationConds(Conds):-
   member(_:rel(E1,X1,agent,_),Conds),
   member(_:rel(E2,X2,theme,_),Conds),
   E1==E2, X1==X2, !.

bindingViolationConds(Conds):-
   member(_:rel(E1,X1,agent,_),Conds),
   member(_:rel(E2,X2,patient,_),Conds),
   E1==E2, X1==X2, !.

bindingViolationConds(Conds):-
   member(_:pred(theme,[E1,X1]),Conds),
   member(_:pred(patient,[E2,X2]),Conds),
   E1==E2, X1==X2, !.

bindingViolationConds(Conds):-
   member(_:not(drs([],[_:eq(X,Y)])),Conds),
   X==Y, !.

bindingViolationCond(not(B)):-
   bindingViolationDrs(B).

bindingViolationCond(prop(_,B)):-
   bindingViolationDrs(B).

bindingViolationCond(imp(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationCond(whq(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationCond(whq(_,B1,_,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationCond(or(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationCond(rel(X,Y,_Sym,_Sense)):-
   X==Y, !.
   


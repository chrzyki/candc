
:- module(drs2fdrs,[drs2fdrs/2,
	            elimEqDrs/2,
                    instDrs/1]).


/*========================================================================
   Main Predicates
========================================================================*/

drs2fdrs(B,Flat):-
   drs2fdrs(B,0,_,[]-Flat,_).

instDrs(B):-
   instDrs(B,0,_).


/*========================================================================
   Label
========================================================================*/

label(X,Label,Y):-
   name(X,Codes),
   name(Label,[108|Codes]),
   Y is X + 1.


/*========================================================================
   Flattening DRSs
========================================================================*/

drs2fdrs(drs(Dom,Conds),L1,L3,F1-[Label:drs(Dom,CondsLabels)|F2],Label):-
   label(L1,Label,L2),
   conds2fconds(Conds,L2,L3,F1-F2,CondsLabels).

drs2fdrs(merge(A1,A2),L1,L4,F1-[Label:merge(Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F2-F3,Label1),
   drs2fdrs(A2,L3,L4,F1-F2,Label2).

drs2fdrs(smerge(A1,A2),L1,L4,F1-[Label:smerge(Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2).

drs2fdrs(alfa(Type,A1,A2),L1,L4,F1-[Label:alfa(Type,Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F2-F3,Label1),
   drs2fdrs(A2,L3,L4,F1-F2,Label2).


/*========================================================================
   Flattening DRS-Conditions
========================================================================*/

conds2fconds([],L,L,F-F,[]).

conds2fconds([I:imp(A1,A2)|Conds],L1,L5,F1-[Label:I:imp(Label1,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:or(A1,A2)|Conds],L1,L5,F1-[Label:I:or(Label1,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:whq(A1,A2)|Conds],L1,L5,F1-[Label:I:whq(Label1,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:whq(Type,A1,Var,A2)|Conds],L1,L5,F1-[Label:I:whq(Type,Label1,Var,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:not(A)|Conds],L1,L4,F1-[Label:I:not(Label1)|F3],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label1),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([I:prop(X,A)|Conds],L1,L4,F1-[Label:I:prop(X,Label1)|F3],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label1),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([I:Cond|Conds],L1,L3,F1-[Label:I:Cond|F2],[Label|L]):- !,
   label(L1,Label,L2),
   conds2fconds(Conds,L2,L3,F1-F2,L).


/*========================================================================
   Referent
========================================================================*/

ref(X,[],X).

ref(X,[_:Ref|L],Z):-
   var(Ref), !,
   name(X,Codes),
   name(Ref,[120|Codes]),
   Y is X + 1,
   ref(Y,L,Z).

ref(X,[_|L],Z):-
   ref(X,L,Z).


/*========================================================================
   Instantiating DRSs
========================================================================*/

instDrs(drs(Dom,Conds),L1,L3):-
   ref(L1,Dom,L2), 
   instConds(Conds,L2,L3).

instDrs(merge(A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).

instDrs(smerge(A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).

instDrs(alfa(_,A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).


/*========================================================================
   Instantiating DRS-Conditions
========================================================================*/

instConds([],L,L).

instConds([_:imp(A1,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:or(A1,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:whq(A1,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:whq(_,A1,_,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:not(A)|Conds],L1,L3):- !,
   instDrs(A,L1,L2),
   instConds(Conds,L2,L3).

instConds([_:prop(_,A)|Conds],L1,L3):- !,
   instDrs(A,L1,L2),
   instConds(Conds,L2,L3).

instConds([_|Conds],L1,L2):- !,
   instConds(Conds,L1,L2).




/*========================================================================
   Eliminate Equality
========================================================================*/

elimEqDrs(drs(Dom,Conds1),drs(Dom,Conds2)):-
   elimEqConds(Conds1,Conds2).

elimEqDrs(merge(A1,A2),merge(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(smerge(A1,A2),smerge(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(alfa(T,A1,A2),alfa(T,B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).


/*========================================================================
   Instantiating DRS-Conditions
========================================================================*/

elimEqConds([],[]).

elimEqConds([I:imp(A1,A2)|Conds1],[I:imp(B1,B2)|Conds2]):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2).

elimEqConds([I:or(A1,A2)|Conds1],[I:or(B1,B2)|Conds2]):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2).

elimEqConds([I:whq(A1,A2)|Conds1],[I:whq(B1,B2)|Conds2]):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2).

elimEqConds([I:whq(X,A1,T,A2)|Conds1],[I:whq(X,B1,T,B2)|Conds2]):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2).

elimEqConds([I:not(A1)|Conds1],[I:not(B1)|Conds2]):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2).

elimEqConds([I:prop(X,A1)|Conds1],[I:prop(X,B1)|Conds2]):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2).

elimEqConds([I:eq(X,X)|Conds1],[I:eq(X,X)|Conds2]):- !,
   elimEqConds(Conds1,Conds2).

elimEqConds([C|Conds1],[C|Conds2]):- !,
   elimEqConds(Conds1,Conds2).


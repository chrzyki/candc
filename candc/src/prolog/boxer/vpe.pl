

:- module(vpe,[resolveVPE/2]).

/* =============================================================
   Importing predicates
============================================================= */

:- use_module(library(lists),[member/2,select/3]).
:- use_module(betaConversionDRT,[betaConvert/2]).


/* =============================================================
   Main
============================================================= */

resolveVPE(In,Out):-
    detectVPE(In,VPE:P,Temp), 
    abstractDRS(In,VPE,P), !,
    betaConvert(Temp,Out).

resolveVPE(B,B).


/* =============================================================
   VPE detection (fails when no VPE is found)
============================================================= */

detectVPE(merge(B1,B2),VPE,R):-
   !, ( detectVPE(B1,VPE,U), !, R = merge(U,B2) ; detectVPE(B2,VPE,U), !, R = merge(B1,U) ).

detectVPE(smerge(B1,B2),VPE,R):-
   !, ( detectVPE(B1,VPE,U), !, R = smerge(U,B2) ; detectVPE(B2,VPE,U), !, R = smerge(B1,U) ).

detectVPE(alfa(T,B1,B2),VPE,Result):-
   !, ( detectVPE(B1,VPE,U), !, Result = alfa(T,U,B2) ; detectVPE(B2,VPE,U), !, Result = alfa(T,B1,U) ).

detectVPE(drs(Dom,Conds),I:P,app(P,lam(Q,merge(drs(Dom,[I:pred(E,do,v,99)|NewConds]),app(Q,E))))):-
   select(I:pred(E,do,v,0),Conds,NewConds), 
   member(_:pred(E1,event,n,1),Conds), E==E1,

   member(_:rel(E2,_,agent,_),Conds), E==E2, 

   \+ ( member(_:rel(E3,_,patient,_),Conds), E==E3 ), 
   \+ ( member(_:rel(E3,_,theme,_),Conds), E==E3 ), 

   \+ ( member(_:rel(E3,_,that,_),Conds), E==E3 ), 

   \+ ( member(_:rel(E3,X1,rel,_),Conds), E==E3,
        member(_:pred(X2,thing,n,12),Conds), X1==X2 ),

   \+ ( member(_:pred(E3,Sym,a,_),Conds), E==E3,
        member(Sym,[good,well,best,better,
                    little,much,
                    poor,poorly,bad,badly,
                    that,away]) ),
   !.


detectVPE(drs(Dom,Conds1),VPE,drs(Dom,Conds2)):-
   detectVPEc(Conds1,VPE,Conds2), !.


/* =============================================================
   VPE detection (DRS-conditions)
============================================================= */

detectVPEc([I:not(B)|L1],VPE,L3):- !, 
   ( detectVPE(B,VPE,U), !, L3 = [I:not(U)|L1] 
   ; detectVPEc(L1,VPE,L2), L3 = [I:not(B)|L2] ).

detectVPEc([I:prop(X,B)|L1],VPE,L3):- !, 
   ( detectVPE(B,VPE,U), !, L3 = [I:prop(X,U)|L1] 
   ; detectVPEc(L1,VPE,L2), L3 = [I:prop(X,B)|L2] ).

detectVPEc([I:imp(B1,B2)|L1],VPE,L3):- !, 
   ( detectVPE(B1,VPE,U), !, L3 = [I:imp(U,B2)|L1] 
   ; detectVPE(B2,VPE,U), !, L3 = [I:imp(B1,U)|L1] 
   ; detectVPEc(L1,VPE,L2), L3 = [I:imp(B1,B2)|L2] ).

detectVPEc([I:or(B1,B2)|L1],VPE,L3):- !, 
   ( detectVPE(B1,VPE,U), !, L3 = [I:or(U,B2)|L1] 
   ; detectVPE(B2,VPE,U), !, L3 = [I:or(B1,U)|L1] 
   ; detectVPEc(L1,VPE,L2), L3 = [I:or(B1,B2)|L2] ).

detectVPEc([C|L1],VPE,[C|L2]):-
   detectVPEc(L1,VPE,L2), !.


/* =============================================================
   Abstraction (DRSs)
============================================================= */

abstractDRS(merge(B1,B2),Pos,P):-
   !, ( abstractDRS(B2,Pos,P), !; abstractDRS(B1,Pos,P) ).

abstractDRS(smerge(B1,B2),Pos,P):-
   !, ( abstractDRS(B2,Pos,P), !; abstractDRS(B1,Pos,P) ).

abstractDRS(alfa(_,B1,B2),Pos,P):-
   !, ( abstractDRS(B2,Pos,P), !; abstractDRS(B1,Pos,P) ).

abstractDRS(drs(_,Conds),Pos,P):-
   abstractConds(Conds,Pos,P), !.

abstractDRS(drs(Dom,Conds),Pos,P):-
   member(_:pred(E,event,n,1),Conds),
   member(I:rel(E1,_,agent,_),Conds), E==E1,
   before(I,Pos),
   member(AI:pred(E2,Sym,v,Sense),Conds), E==E2, !,
   abstractConditions(drs(Dom,Conds),E,[]-AbsDom,[]-AbsCond),
   P=lam(F,app(F,lam(E,drs(AbsDom,[[-11|AI]:pred(E,Sym,v,Sense)|AbsCond])))).


/* =============================================================
   Abstraction (DRS-conditions)
============================================================= */

abstractConds([_:not(B)|L],Pos,P):-
   !, ( abstractDRS(B,Pos,P), !; abstractConds(L,Pos,P) ).

abstractConds([_:prop(_,B)|L],Pos,P):-
   !, ( abstractDRS(B,Pos,P), !; abstractConds(L,Pos,P) ).

abstractConds([I:imp(B1,B2)|L],Pos,P):-
   !, ( abstractDRS(B1,Pos,P), !; 
        abstractDRS(B2,Pos,lam(E,ADRS)), !, P=lam(E,drs([],[[-11|I]:imp(B1,ADRS)])); 
        abstractConds(L,Pos,P) ).

abstractConds([_:or(B1,B2)|L],Pos,P):-
   !, ( abstractDRS(B1,Pos,P), !; abstractDRS(B2,Pos,P), !; abstractConds(L,Pos,P) ).

abstractConds([_|L],Pos,P):-
   abstractConds(L,Pos,P).


/* =============================================================
   Conditions Abstraction
============================================================= */

abstractConditions(drs(Dom,C1),E,AD1-AD3,AC1-AC3):- 
   select(I:rel(E1,X,Sym,Sense),C1,C2), E==E1, 
   \+ Sym=agent, %%% this is the default parallel element
   \+ Sym=as,    %%% ignore comparative clause
   abstractConcept(X,C2,AD1-AD2,AC1-AC2),
   !,
   abstractConditions(drs(Dom,C2),E,AD2-AD3,[[-11|I]:rel(E,X,Sym,Sense)|AC2]-AC3).

abstractConditions(_,_,D-D,C-C).


/* =============================================================
   Concept Abstraction
============================================================= */

abstractConcept(X,C1,AD1-AD2,AC1-AC2):-
   select(I:timex(X1,Timex),C1,C2), X==X1, !,
   abstractConcept(X,C2,AD1-AD2,[[-11|I]:timex(X,Timex)|AC1]-AC2).

abstractConcept(X,C1,AD1-AD2,AC1-AC2):-
   select(I:card(X1,Num,Type),C1,C2), X==X1, !,
   abstractConcept(X,C2,AD1-AD2,[[-11|I]:card(X,Num,Type)|AC1]-AC2).

abstractConcept(X,C1,AD1-AD2,AC1-AC2):-
   select(I:named(X1,Sym,Type,Sense),C1,C2), X==X1, !,
   abstractConcept(X,C2,AD1-AD2,[[-11|I]:named(X,Sym,Type,Sense)|AC1]-AC2).

abstractConcept(X,C1,AD1-AD2,AC1-AC2):-
   select(I:pred(X1,Sym,Type,Sense),C1,C2), X==X1, !,
   abstractConcept(X,C2,AD1-AD2,[[-11|I]:pred(X,Sym,Type,Sense)|AC1]-AC2).

abstractConcept(_,_,AD-AD,[]-[]):- !.

abstractConcept(X,_,AD-[[]:X|AD],AC-AC).


/* =============================================================
   Word order
============================================================= */

before(L1,L2):-
   member(I1,L1),
   member(I2,L2), I1 < I2, !.

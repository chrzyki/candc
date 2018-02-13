
:- module(tuples,[tuples/2,write_tuples/2]).

:- use_module(library(lists),[member/2,append/3]).


tuples(XDRS,Tuples):-
   XDRS=xdrs(_Words,_POS,_NEtags,DRS),
   numbervars(DRS,0,_),
   conditions(DRS,[],C),
   setof(Tuple,tuple(C,Tuple),Tuples), !.
 
tuples(_,[]).


/* =======================================================================
   Get all conditions from a DRS
======================================================================= */

conditions(drs(_,[]),C,C).

conditions(drs(D,[X|L]),C1,[X|C2]):-
   conditions(drs(D,L),C1,C2).

conditions(smerge(B1,B2),C1,C3):-
   conditions(B1,C1,C2),
   conditions(B2,C2,C3).

conditions(merge(B1,B2),C1,C3):-
   conditions(B1,C1,C2),
   conditions(B2,C2,C3).

conditions(alfa(_,B1,B2),C1,C3):-
   conditions(B1,C1,C2),
   conditions(B2,C2,C3).


/* =======================================================================
   Output tuples to stream
======================================================================= */

write_tuples([],_).

write_tuples([tuple(A,B,C)|L],Stream):-
   format(Stream,'~w ~w ~w~n',[A,B,C]),
   write_tuples(L,Stream).


/* =======================================================================
   Find tuple in DRS-conditions
======================================================================= */

tuple(C,tuple([XX:1|XM],[YY:2|YM],[EE,0:ArgX:1,0:ArgY:2|Mod])):-
   event(C,E,EE),
   member(ArgX:ArgY,[agent:theme,agent:patient,patient:theme]),
   member(_:rel(E,X,ArgX,_),C),
   member(_:rel(E,Y,ArgY,_),C),
   noun(C,X,XX),
   nmod(C,X,1,XM),
   noun(C,Y,YY),
   nmod(C,Y,2,YM),
   emod(C,E,Mod).

tuple(C,tuple([XX:1|XM],[YY:2|YM],[EE,0:ArgX:1,0:ArgY:2|Mod])):-
   event(C,E,EE),
   member(ArgX,[agent,theme,patient]),
   member(_:rel(E,X,ArgX,_),C),
   member(_:rel(E,Y,ArgY,_),C),
   \+ member(ArgY,[agent,theme,patient]),
   noun(C,X,XX),
   nmod(C,X,1,XM),
   noun(C,Y,YY),
   nmod(C,Y,2,YM),
   emod(C,E,Mod).


/* =======================================================================
   Concepts
======================================================================= */

noun(C,X,pred(Pred)):- 
   member(_:pred(X,Pred,n,_),C), 
   \+ member(Pred,[person,male,human,neuter,female,there_expl,thing]).

noun(C,X,name(Pred,Type)):- member(_:named(X,Pred,Type,_),C).

noun(C,X,date(D0,D1,D2,D3)):- member(_:timex(X,date(_:D0,_:D1,_:D2,_:D3)),C).

event(C,E,event(Event):0):- member(_:pred(E,Event,v,_),C).


/* =======================================================================
   Modification
======================================================================= */

emod(C,E,Mod):- findall(mod(M):0,member(_:pred(E,M,a,_),C),Mod).

nmod(C,X,N,Mod):- findall(mod(M):N,member(_:pred(X,M,a,_),C),Mod1),
                  findall(card(Card):N,member(_:card(X,Card,_),C),Mod2),
                  append(Mod2,Mod1,Mod).

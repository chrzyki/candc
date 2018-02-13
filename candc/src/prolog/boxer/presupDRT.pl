
:- module(presupDRT,[resolveDrs/2,resolveDrs/3]).

:- use_module(mergeDRT,[mergeDrs/2]).

:- use_module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(freeVarCheck,[freeVarCheckDrs/1]).

:- use_module(sortalCheck,[sortalCheckDrs/1]).

:- use_module(library(lists),[member/2,append/3,select/3]).


/*========================================================================
   Main predicates
========================================================================*/

resolveDrs(smerge(B1,B2),RDRS):- !,
   resolveDrs(smerge(B1,B2),RDRS,[],_).

resolveDrs(B,RDRS):- 
   \+ B = smerge(_,_), !,
   resolveDrs(smerge(drs([],[]),B),RDRS,[],_).

resolveDrs(smerge(B1,B2),RDRS,Links):- !,
   resolveDrs(smerge(B1,B2),RDRS,[],Links).

resolveDrs(B,RDRS,Links):- 
   \+ B = smerge(_,_), !,
   resolveDrs(smerge(drs([],[]),B),RDRS,[],Links).


/*========================================================================
   Resolution

   Resolves alpha-DRSs one by one until they are all resolved.  Alfa
   is the anaphoric DRS, Type the corresponding alpha-type.  The third
   and fourth argument of resolveDrs/4 represent the binding links. Ac
   is a list of accommodation sites (represented as a/1 terms), and Bi
   a list of binding sites (represented as r/2 terms).

resolveDrs(smerge(GDRS,ADRS),DRS,L1,L3):-
   findAlfaDrs(ADRS,RDRS,alfa(Type,Alfa),Ac,[]-Bi), !,
   NewDRS = smerge(merge(GDRS,Global),RDRS),
   resolveAlfa(Alfa,Type,[a(Global)|Ac],Bi,NewDRS,L1,L2), 
   !, %%% Required for large DRSs (too many choice points)
   resolveDrs(NewDRS,DRS,L2,L3).

========================================================================*/

resolveDrs(smerge(GDRS,ADRS),DRS,L1,L3):-
   findAlfaDrs(ADRS,RDRS,alfa(Type,Alfa),Ac,[]-Bi), !,
   NewDRS = smerge(merge(GDRS,Global),RDRS),
   resolveAlfa(Alfa,Type,[a(Global)|Ac],[r(GDRS,NewGDRS)|Bi],NewDRS,L1,L2), 
   !, %%% Required for large DRSs (too many choice points)
   NewNewDRS = smerge(merge(NewGDRS,Global),RDRS),
   resolveDrs(NewNewDRS,DRS,L2,L3).

resolveDrs(RDRS,DRS,L0,L0):-
%   \+ bindingViolationDrs(B),
   mergeDrs(RDRS,DRS).


/*========================================================================
   Find First Alfa-DRS (DRSs)
========================================================================*/

findAlfaDrs(alfa(T,B1,B2),Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res=alfa(T,R1,B2)
   ;
      Res=merge(A,B2),
      mergeDrs(B1,M1),
      Alfa=alfa(T,M1),
      Ac=[a(A)],
      Bi1=Bi2
   ).

findAlfaDrs(merge(B1,B2),Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res = merge(R1,B2)
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
      Res = merge(R1,R2)
   ). 

findAlfaDrs(smerge(B1,B2),Res,Alfa,Ac,Bi1-Bi2):- !,
   (
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res = smerge(R1,B2)
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
      Res = smerge(R1,R2)
%      mergeDrs(B1,M1),
%      findAlfaDrs(B2,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
%      Res = smerge(merge(R1,A),R2),
%      Ac = [a(A)|Ac0]
   ).

findAlfaDrs(drs(D,C1),merge(A,R),Alfa,[a(A)|Ac],Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,[r(drs(D,C2),R)|Bi1]-Bi2).


/*========================================================================
   Find First Alfa-DRS (DRS-Conditions)
========================================================================*/

findAlfaConds([I:X1|C1],[I:X2|C2],Alfa,Ac,Bi1-Bi2):- !,
   findAlfaConds([X1|C1],[X2|C2],Alfa,Ac,Bi1-Bi2).

findAlfaConds([imp(B1,B3)|C],Res,Alfa,Ac,Bi1-Bi2):- 
   (
      findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !,
      Res = [imp(B2,B3)|C]
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B3,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
      Res = [imp(merge(R1,A),R2)|C],
      Ac = [a(A)|Ac0]
   ), !.

findAlfaConds([whq(B1,B3)|C],Res,Alfa,Ac,Bi1-Bi2):-
   (
      findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !,
      Res = [whq(B2,B3)|C]
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B3,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
      Res = [whq(merge(R1,A),R2)|C],
      Ac = [a(A)|Ac0]
   ), !.

findAlfaConds([whq(Type,B1,Var,B3)|C],Res,Alfa,Ac,Bi1-Bi2):- 
   (
      findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !,
      Res = [whq(Type,B2,Var,B3)|C]
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B3,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
      Ac = [a(A)|Ac0],
      Res = [whq(Type,merge(R1,A),Var,R2)|C]
   ), !.      

findAlfaConds([or(B1,B2)|C],Res,Alfa,Ac,Bi1-Bi2):-
   (
      findAlfaDrs(B1,B3,Alfa,Ac,Bi1-Bi2), !,
      Res = [or(B3,B2)|C]
   ;
      findAlfaDrs(B2,B3,Alfa,Ac,Bi1-Bi2), 
      Res = [or(B1,B3)|C]
   ), !.

findAlfaConds([not(B1)|C],[not(B2)|C],Alfa,Ac,Bi1-Bi2):- 
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([prop(X,B1)|C],[prop(X,B2)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([Cond|C1],[Cond|C2],Alfa,Ac,Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,Bi1-Bi2).


/*========================================================================
   Resolve Alfa

   Resolution of Alpha-DRSs. There are two possibilities: binding or
   accommodation. However, these possibilities are constrained by the
   the Alpha-Type, of which the current setting is:

     Alpha-Type   Binding Accommodation

     pro           yes       yes
     def           no        yes
     ref           no        yes
     nam           no        yes
     dei           no        yes

========================================================================*/

resolveAlfa(Alfa,Type,Ac,Bi,B,L1,L2):-
   bindAlfa(Bi,Type,Alfa,L1,L2),
   dontResolve(Ac),
   \+ bindingViolationDrs(B),
   freeVarCheckDrs(B).                 %%% Seems to be necessary! See EXAMPLES/problem.ccg
%   sortalCheckDrs(B).                 %%% Not needed for now.


resolveAlfa(Alfa,Type,Ac,Bi,B,L0,L0):-
   accommodateAlfa(Ac,Type,Alfa),
   dontResolve(Bi),
   freeVarCheckDrs(B).


/*------------------------------------------------------------------------
   Binding

   Select an antecedent, check if coordinated items are resolved to the
   same antecedent (note the (ab)use of the indices here), the merge
   the domain and the conditions.

------------------------------------------------------------------------*/

bindAlfa(P1,pro,drs([AnaIndex:X|D1],C1),L0,[bind(AnaIndex,AntIndex)|L0]):-
   select(r(drs(D2,C2),drs(D3,C3)),P1,P2),
   member([]:pred(Top,topic,a,1),C2),                                                 %% Tailored to TREC 2004 (binding to topic)
   member(AntIndex:X,D2), X==Top, 
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P2).

bindAlfa(P1,def,drs([AnaIndex:X|D1],C1),L0,[bind(AnaIndex,AntIndex)|L0]):-
   select(r(drs(D2,C2),drs(D3,C3)),P1,P2),
   member([]:pred(Top,topic,a,1),C2),                                                 %% Tailored to TREC 2004 (binding to topic)
   member(AntIndex:X,D2), X==Top,
   commonSemanticMaterial(C1,C2,X),
   \+ (member(bind(AnaIndex,OtherIndex),L0),\+ OtherIndex=AntIndex),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P2).

bindAlfa(P1,nam,drs([AnaIndex:X|D1],C1),L0,[bind(AnaIndex,AntIndex)|L0]):-
   select(r(drs(D2,C2),drs(D3,C3)),P1,P2),
   member([]:pred(Top,topic,a,1),C2),                                                 %% Tailored to TREC 2004 (binding to topic)
   member(AntIndex:X,D2), X==Top,
   commonSemanticMaterial(C1,C2,X),
   \+ (member(bind(AnaIndex,OtherIndex),L0),\+ OtherIndex=AntIndex),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P2).

bindAlfa([r(drs(D2,C2),drs(D3,C3))|P],pro,drs([AnaIndex:X|D1],C1),L0,[bind(AnaIndex,AntIndex)|L0]):-
   member(AntIndex:X,D2),
   namedEntity(C1,C2,X),
   \+ (member(bind(AnaIndex,OtherIndex),L0),\+ OtherIndex=AntIndex),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P).

bindAlfa([r(drs(D2,C2),drs(D3,C3))|P],def,drs([AnaIndex:X|D1],C1),L0,[bind(AnaIndex,AntIndex)|L0]):-
   member(AntIndex:X,D2),
   commonSemanticMaterial(C1,C2,X),
   \+ (member(bind(AnaIndex,OtherIndex),L0),\+ OtherIndex=AntIndex),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P).

bindAlfa([r(drs(D2,C2),drs(D3,C3))|P],nam,drs([AnaIndex:X|D1],C1),L0,[bind(AnaIndex,AntIndex)|L0]):-
   member(AntIndex:X,D2),
   commonSemanticMaterial(C1,C2,X),
   \+ (member(bind(AnaIndex,OtherIndex),L0),\+ OtherIndex=AntIndex),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P).

bindAlfa([r(R,R)|P],Type,Alfa,L1,L2):-
   bindAlfa(P,Type,Alfa,L1,L2).

bindAlfa([a(drs([],[]))|P],Type,Alfa,L1,L2):-
   bindAlfa(P,Type,Alfa,L1,L2).


/*------------------------------------------------------------------------
   Check for partial match
------------------------------------------------------------------------*/

commonSemanticMaterial(C1,C2,X):-
   member(_:named(X1,Sym,Type,_),C1), X==X1, 
   \+ Type=ttl, \+ Sym='"',
   member(_:named(X2,Sym,Type,_),C2), X1==X2, !.

commonSemanticMaterial(C1,C2,X):-
   member(_:pred(X1,Sym,n,_),C1), X==X1,  \+ Sym='"',
   member(_:pred(X2,Sym,n,_),C2), X1==X2, !.


/*------------------------------------------------------------------------
   Check for named entity (he/she)
------------------------------------------------------------------------*/

namedEntity(C1,C2,X):-
   member(_:pred(X1,male,a,_),C1), X==X1, 
   member(_:named(X2,_,per,_),C2), X1==X2, !.

namedEntity(C1,C2,X):-
   member(_:pred(X1,female,a,_),C1), X==X1, 
   member(_:named(X2,_,per,_),C2), X1==X2, !.


/*------------------------------------------------------------------------
   Accommodation

   P is a list of accommodation sites (represented as a/1 terms). The
   order of the list corresponds to the level of the accommodation
   site: the first a/1 term is the most global site, the last element
   the most local accommodation site. Only definite descriptions
   license non-global accommodation:

                    A C C O M M O D A T I O N
     Alpha-Type   Global  Intermediate   Local

        pro        yes         no          no
        def        yes         yes         yes
        nam        yes         no          no
        ref        yes         no          no
        dei        yes         no          no
   
------------------------------------------------------------------------*/

accommodateAlfa([r(R,R)|P],Type,Alfa):- !,
   accommodateAlfa(P,Type,Alfa).


/*
%%% only local accommodation ...

accommodateAlfa([a(Alfa)],_,Alfa):- !.

accommodateAlfa([a(drs([],[]))|P],Type,Alfa):- !,
   accommodateAlfa(P,Type,Alfa).
*/


accommodateAlfa([a(Alfa)|P],pro,Alfa):- !,
   dontResolve(P).

accommodateAlfa([a(Alfa)|P],dei,Alfa):- !,
   dontResolve(P).

accommodateAlfa([a(Alfa)|P],nam,Alfa):- !,
   dontResolve(P).

accommodateAlfa([a(Alfa)|P],ref,Alfa):- !,
   dontResolve(P).

accommodateAlfa([a(Alfa)|P],def,Alfa):-
   dontResolve(P).

accommodateAlfa([a(drs([],[]))|P],def,Alfa):-
   accommodateAlfa(P,def,Alfa).


/*========================================================================
    Do not resolve remaining of projection path
========================================================================*/

dontResolve([]):- !.

dontResolve([a(drs([],[]))|L]):- !,
   dontResolve(L).

dontResolve([r(X,X)|L]):- !,
   dontResolve(L).


/*========================================================================
   Merge Lists - Check for Duplicates; Copy Indexes
========================================================================*/

mergeLists([],L,L):- !.

mergeLists([I1:X|R],L1,L3):-
   select(I2:Y,L1,L2), X==Y, !,
   append(I1,I2,I3),
   sort(I3,I4),
   mergeLists(R,[I4:X|L2],L3).

mergeLists([X|R],L1,[X|L2]):-
   mergeLists(R,L1,L2).


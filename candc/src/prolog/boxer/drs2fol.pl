

:- module(drs2fol,[pdrs2fol/2,drs2fol/2,symbol/4]).


/*========================================================================
   Translate DRSs into FOL formulas 
========================================================================*/

drs2fol(smerge(B1,B2),Form):- !, drs2fol(merge(B1,B2),Form).

drs2fol(alfa(_,B1,B2),Form):- !, drs2fol(merge(B1,B2),Form).

drs2fol(drs([],[Cond]),Formula):- !, cond2fol(Cond,Formula).

drs2fol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):- !,
   cond2fol(Cond1,Formula1), drs2fol(drs([],[Cond2|Conds]),Formula2).

drs2fol(drs([_:X|Referents],Conds),some(X,Formula)):- !,
   drs2fol(drs(Referents,Conds),Formula).

drs2fol(merge(B1,B2),Form):- !, 
   drs2folGap(B1,Gap^Form), drs2fol(B2,Gap).

drs2fol(X,_):-
   user:option('--warnings',true),
   format(user_error,'WARNING: drs2fol/2 failed for:~n~p~n',[X]), fail.


/*========================================================================
   Translate DRS into FOL formula with "gap"

   This is to ensure that discourse referents in the LHS of a merge
   bind occurrences of DRSs in the RHS of a merge. 
========================================================================*/

drs2folGap(smerge(B1,B2),Gap^F):- !, drs2folGap(merge(B1,B2),Gap^F).  

drs2folGap(alfa(_,B1,B2),Gap^F):- !, drs2folGap(merge(B1,B2),Gap^F).  

drs2folGap(merge(B1,B2),Gap2^F):- !, 
   drs2folGap(B1,Gap1^F), drs2folGap(B2,Gap2^Gap1).

drs2folGap(drs([_:X|Dom],Conds),Gap^some(X,F)):- !, 
   drs2folGap(drs(Dom,Conds),Gap^F).

drs2folGap(drs([],Conds),Gap^and(F,Gap)):-
   drs2fol(drs([],Conds),F).


/*========================================================================
   Translate DRS-Conditions into FOL formulas 
========================================================================*/

cond2fol(_:C,F):- !,
   cond2fol(C,F).

cond2fol(not(Drs),not(Formula)):- !,
   drs2fol(Drs,Formula).
 
cond2fol(prop(_,Drs),Formula):- !,
   drs2fol(Drs,Formula).

cond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):- !,
   drs2fol(Drs1,Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(whq(Drs1,Drs2),F):- !, 
   cond2fol(imp(Drs1,Drs2),F).

cond2fol(whq(_,Drs1,_,Drs2),F):- !, 
   cond2fol(imp(Drs1,Drs2),F).

cond2fol(imp(drs([],Conds),Drs2),imp(Formula1,Formula2)):- !,
   drs2fol(drs([],Conds),Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([_:X|Referents],Conds),Drs2),all(X,Formula)):- !,
   cond2fol(imp(drs(Referents,Conds),Drs2),Formula).

%cond2fol(card(X,C1,T),card(X,C2,T)):-
%   symbol(C1,c,num,C2), !.

cond2fol(card(X,C,_),some(Y,and(card(X,Y),and(F1,F2)))):-
   symbol(C,c,num,S1), 
   symbol(numeral,n,1,S2),
   F1=..[S1,Y],
   F2=..[S2,Y], !.

cond2fol(named(X,N1,Type,Sense),F):-
   symbol(N1,Type,Sense,N2), !,
   F=..[N2,X].

cond2fol(timex(X,D1),F):-
   timex(D1,D2),
   F=..[D2,X], !.

cond2fol(eq(X,Y),eq(X,Y)):- 
   var(X), var(Y), !.

cond2fol(pred(X,Sym1,Type,Sense),F):- 
   symbol(Sym1,Type,Sense,Sym2), !,
   F=..[Sym2,X].

cond2fol(rel(X,Y,Sym1,Sense),F):- 
   symbol(Sym1,r,Sense,Sym2), !,
   F=..[Sym2,X,Y].

cond2fol(X,_):-
   user:option('--warnings',true),
   format(user_error,'WARNING: cond2fol/2 failed for ~p~n',[X]), fail.


/*========================================================================
   Translate PDRSs into FOL formulas 
========================================================================*/

pdrs2fol(drs([],[Cond]),Formula):- !,
   pcond2fol(Cond,Formula).

pdrs2fol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):- !,
   pcond2fol(Cond1,Formula1),
   pdrs2fol(drs([],[Cond2|Conds]),Formula2).

pdrs2fol(drs([_:X|Referents],Conds),some(X,and(some(Y,member(Y,X)),Formula))):- !,
   pdrs2fol(drs(Referents,Conds),Formula).

pdrs2fol(X,_):-
   user:option('--warnings',true),
   format(user_error,'WARNING: drs2fol/2 failed for ~p~n',[X]), fail.


/*========================================================================
   Translate DRS-Conditions into FOL formulas 
========================================================================*/

pcond2fol(_:C,F):- !,
   pcond2fol(C,F).

pcond2fol(not(Drs),not(Formula)):- !,
   pdrs2fol(Drs,Formula).
 
pcond2fol(prop(_,Drs),Formula):- !,
   pdrs2fol(Drs,Formula).

pcond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):- !,
   pdrs2fol(Drs1,Formula1),
   pdrs2fol(Drs2,Formula2).

pcond2fol(whq(Drs1,Drs2),F):- !, 
   pcond2fol(imp(Drs1,Drs2),F).

pcond2fol(whq(_,Drs1,_,Drs2),F):- !, 
   pcond2fol(imp(Drs1,Drs2),F).

pcond2fol(imp(drs([],Conds),Drs2),imp(Formula1,Formula2)):- !,
   pdrs2fol(drs([],Conds),Formula1),
   pdrs2fol(Drs2,Formula2).

pcond2fol(imp(drs([_:X|Referents],Conds),Drs2),all(X,Formula)):- !,
   pcond2fol(imp(drs(Referents,Conds),Drs2),Formula).

pcond2fol(card(X,1,eq),some(Y,and(member(Y,X),all(Z,imp(member(Z,X),eq(Z,Y)))))):- !.

pcond2fol(card(X,2,ge),some(Y,and(member(Y,X),some(Z,and(member(Z,X),not(eq(Z,Y))))))):- !.

pcond2fol(card(X,C1,T),card(X,C2,T)):-
   symbol(C1,p,0,C2), !.

pcond2fol(named(X,N1,_,_),named(X,N2)):-
   symbol(N1,p,0,N2), !.

pcond2fol(timex(X,D1),timex(X,D2)):-
   timex(D1,D2), !.

pcond2fol(eq(X,Y),all(A,all(B,imp(and(member(A,X),member(B,Y)),eq(X,Y))))):- 
   var(X), var(Y), !.

pcond2fol(pred(X,Sym1,Type,Sense),all(Y,imp(member(Y,X),F))):- 
   symbol(Sym1,Type,Sense,Sym2), !,
   F=..[Sym2,Y].

pcond2fol(rel(X,Y,Sym1,Sense),all(A,all(B,imp(and(member(A,X),member(B,Y)),F)))):- 
   symbol(Sym1,r,Sense,Sym2), !,
   F=..[Sym2,A,B].

pcond2fol(X,_):-
   user:option('--warnings',true),
   format(user_error,'WARNING: cond2fol/2 failed for ~p~n',[X]), 
   fail.



/*========================================================================
   Ensure F is a number or atom
========================================================================*/

symbol(F1,Cat,Sense,F2):- 
   var(Sense), !,
   symbol(F1,Cat,1,F2).

symbol(F1,per,Sense,F2):- !,
   symbol(F1,p,Sense,F2).

symbol(F1,loc,Sense,F2):-
   symbol(F1,l,Sense,F2).

symbol(F1,org,Sense,F2):-
   symbol(F1,o,Sense,F2).

symbol(F1,nam,Sense,F2):-
   symbol(F1,p,Sense,F2).

symbol(F1,ttl,Sense,F2):-
   symbol(F1,p,Sense,F2).

symbol(D,t,_Sense,F):- !,
   timex(D,F).

symbol(F1,Type,0,F2):- !,
   symbol(F1,Type,1,F2).

symbol(F1,Type,Sense,F2):- 
   (number(F1);atom(F1)),
   name(F1,A1),
   sym(A1,A2),
   name(Type,[T]),
   name(Sense,A3),
   append(A2,[95|A3],A4),
   name(F2,[T,95|A4]),  
   atom(F2), !.


sym([],[]).

%%% exclamation mark
%%%
sym([33|L1],[101,120,99,108,97,109,97,116,105,111,110,109,97,114,107|L2]):- !,
   sym(L1,L2).

%%% double quote
%%%
sym([34|L1],[100,111,117,98,108,101,113,117,111,116,101|L2]):- !,
   sym(L1,L2).

%%% hekje
%%%
sym([35|L1],[104,101,107,106,101|L2]):- !,
   sym(L1,L2).

%%% dollar
%%%
sym([36|L1],[100,111,108,108,97,114|L2]):- !,
   sym(L1,L2).

%%% percent
%%%
sym([37|L1],[112,101,114,99,101,110,116|L2]):- !,
   sym(L1,L2).

%%% ampersand
%%%
sym([38|L1],[97,109,112,101,114,115,97,110,100|L2]):- !,
   sym(L1,L2).

%%% single quote
%%%
sym([39|L1],[115,105,110,103,108,101,113,117,111,116,101|L2]):- !,
   sym(L1,L2).

sym([40|L1],[108,114,98|L2]):- !,
   sym(L1,L2).

sym([41|L1],[114,114,98|L2]):- !,
   sym(L1,L2).

%%% times
%%%
sym([42|L1],[116,105,109,101,115|L2]):- !,
   sym(L1,L2).

%%% plus
%%%
sym([43|L1],[112,108,117,115|L2]):- !,
   sym(L1,L2).

%%% comma
%%%
sym([44|L1],L2):- !,
   sym(L1,L2).

%%% hyphen
%%%
sym([45|L1],[104,121,112,104,101,110|L2]):- !,
   sym(L1,L2).

%%% full stop
%%%
sym([46|L1],L2):- !,
   sym(L1,L2).

%%% forward slash
%%%
sym([47|L1],L2):- !,
   sym(L1,L2).

%%% colon
%%%
sym([58|L1],[99,111,108,111,110|L2]):- !,
   sym(L1,L2).

%%% semicolon
%%%
sym([59|L1],[115,101,109,105,99,111,108,111,110|L2]):- !,
   sym(L1,L2).

%%% @
%%%
sym([64|L1],[97,116|L2]):- !,
   sym(L1,L2).


sym([91|L1],[108,114,98|L2]):- !,
   sym(L1,L2).

%%% backward slash
%%%
sym([92|L1],L2):- !,
   sym(L1,L2).

sym([93|L1],[114,114,98|L2]):- !,
   sym(L1,L2).

sym([123|L1],[108,114,98|L2]):- !,
   sym(L1,L2).

%%% |
%%%
sym([124|L1],L2):- !,
   sym(L1,L2).

sym([125|L1],[114,114,98|L2]):- !,
   sym(L1,L2).

sym([X|L1],[Y|L2]):- 
   X > 64, X < 91, !,
   Y is X + 32,
  sym(L1,L2).

sym([X|L1],[X|L2]):- 
  sym(L1,L2).


/*========================================================================
   Time Expressions
========================================================================*/

timex(date(_:_,_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(date(_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(time(_:H,_:M,_:S),Timex):- !,
   timex(time(H,M,S),Timex).

timex(date(Y,M,D),Timex):-
   year(Y,[Y1,Y2,Y3,Y4]),
   month(M,[M1,M2]),
   day(D,[D1,D2]),
   name(Timex,[116,95,Y1,Y2,Y3,Y4,M1,M2,D1,D2]).

timex(time(H,M,S),Timex):-
   hour(H,[H1,H2]),
   minute(M,[M1,M2]),
   second(S,[S1,S2]),
   name(Timex,[116,95,H1,H2,M1,M2,S1,S2]).

year(Y,C):- var(Y), !, name('XXXX',C).
year(Y,C):- name(Y,C).

month(Y,C):- var(Y), !, name('XX',C).
month(Y,C):- name(Y,C).

day(Y,C):- var(Y), !, name('XX',C).
day(Y,C):- name(Y,C).

hour(A,C):- day(A,C).
minute(A,C):- day(A,C).
second(A,C):- day(A,C).


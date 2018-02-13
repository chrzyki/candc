
:- module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(library(lists),[member/2]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDRS(B1,B2):-
   alphaConvertDRS(B1,[]-_,B2), !.

alphaConvertDRS(B,_):-
   user:option('--warnings',true),
   format(user_error,'WARNING: cannot alpha-convert ~p.~n',[B]), 
   fail.


/*========================================================================
   Variable
========================================================================*/

variable(X):- var(X), !.
variable(X):- functor(X,'$VAR',1), !.


/*========================================================================
   Alpha Conversion (terms)
========================================================================*/

alphaConvertVar(X,Vars,New):-
   variable(X), !,
   (
      member(sub(Z,Y),Vars),           %%% BOUND
      X==Z, !,
      New=Y
   ;
      New=X                            %%% FREE
   ).

alphaConvertVar(X,_,X).



/*========================================================================
   Alpha Conversion (DRSs)
========================================================================*/

alphaConvertDRS(X1,Vars-Vars,X2):-
   variable(X1), !,
   alphaConvertVar(X1,Vars,X2).

alphaConvertDRS(lam(X,B1),Vars-Vars,lam(Y,B2)):- !,
   alphaConvertDRS(B1,[sub(X,Y)|Vars]-_,B2).

alphaConvertDRS(drs([],[]),Vars-Vars,drs([],[])):- !.

alphaConvertDRS(drs([],[C1|Conds1]),Vars1-Vars2,drs([],[C2|Conds2])):- !,
   alphaConvertCondition(C1,Vars1,C2), 
   alphaConvertDRS(drs([],Conds1),Vars1-Vars2,drs([],Conds2)).

alphaConvertDRS(drs([Index:Ref|L1],C1),Vars1-Vars2,drs([Index:New|L2],C2)):- !,
   alphaConvertDRS(drs(L1,C1),[sub(Ref,New)|Vars1]-Vars2,drs(L2,C2)).

alphaConvertDRS(alfa(Type,B1,B2),Vars1-Vars3,alfa(Type,B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(merge(B1,B2),Vars1-Vars3,merge(B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(smerge(B1,B2),Vars1-Vars3,smerge(B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(app(E1,E2),Vars-Vars,app(E3,E4)):- !,
   alphaConvertDRS(E1,Vars-_,E3),
   alphaConvertDRS(E2,Vars-_,E4).


/*========================================================================
   Alpha Conversion (DRS-Conditions)
========================================================================*/

alphaConvertCondition(F:Cond1,Vars,F:Cond2):- !,
   alphaConvertCondition(Cond1,Vars,Cond2).

alphaConvertCondition(not(B1),Vars,not(B2)):- !,
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(prop(X,B1),Vars,prop(Y,B2)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(imp(B1,B2),Vars,imp(B3,B4)):- !,
   alphaConvertDRS(B1,Vars-Vars1,B3),
   alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(whq(B1,B2),Vars,whq(B3,B4)):- !,
   alphaConvertDRS(B1,Vars-Vars1,B3),
   alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(whq(Type,B1,X,B2),Vars,whq(Type,B3,Y,B4)):- !,
   alphaConvertDRS(B1,Vars-Vars1,B3),
   alphaConvertVar(X,Vars1,Y),
   alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(or(B1,B2),Vars,or(B3,B4)):- !,
   alphaConvertDRS(B1,Vars-_,B3),
   alphaConvertDRS(B2,Vars-_,B4).

alphaConvertCondition(pred(Arg1,Sym,Type,Sense),Vars,pred(Arg2,Sym,Type,Sense)):- !,
   alphaConvertVar(Arg1,Vars,Arg2).

alphaConvertCondition(rel(Arg1,Arg2,Sym,Sense),Vars,rel(Arg3,Arg4,Sym,Sense)):- !,
   alphaConvertVar(Arg1,Vars,Arg3),
   alphaConvertVar(Arg2,Vars,Arg4).

%alphaConvertCondition(named(X,Sym,Type),Vars,named(Y,Sym,Type)):- !,
%   alphaConvertVar(X,Vars,Y).

alphaConvertCondition(named(X,Sym,Type,Sense),Vars,named(Y,Sym,Type,Sense)):- !,
   alphaConvertVar(X,Vars,Y).

alphaConvertCondition(card(X,Sym1,T),Vars,card(Y,Sym2,T)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertVar(Sym1,Vars,Sym2).

alphaConvertCondition(timex(X,Sym),Vars,timex(Y,Sym)):- !,
   alphaConvertVar(X,Vars,Y).

alphaConvertCondition(eq(X1,X2),Vars,eq(Y1,Y2)):- !,
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).


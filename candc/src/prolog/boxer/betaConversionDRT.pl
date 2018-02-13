
:- module(betaConversionDRT,[betaConvert/2]).

:- use_module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(library(lists),[append/3]).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvert(X,Y):-
   betaConvert(X,Y,[]).


/*========================================================================
   Beta-Conversion (core stuff)
========================================================================*/

betaConvert(X,Y,[]):-
   var(X), 
   !,
   Y=X.

betaConvert(Expression,Result,Stack):- 
   Expression = app(Functor,Argument),
   nonvar(Functor), 
   !,
   alphaConvertDRS(Functor,Converted), 
   betaConvert(Converted,Result,[Argument|Stack]).

betaConvert(Expression,Result,[X|Stack]):-
   Expression = lam(X,Formula), 
   !,
   betaConvert(Formula,Result,Stack).

betaConvert(Formulas,ResultFormulas,[]):-
   Formulas = [_|_], !,
   betaConvertList(Formulas,ResultFormulas).

betaConvert(Formula,Result,[]):- 
   Formula = merge(B1,B2), !,
   betaConvert(B1,K1),
   betaConvert(B2,K2),
   reduce(K1,K2,Result).

betaConvert(Formula,Result,[]):- !,
   Formula =.. [Functor|Formulas],
   betaConvertList(Formulas,ResultFormulas),
   Result =.. [Functor|ResultFormulas].

betaConvert(Formula,_,Stack):-
   user:option('--warnings',true),
   format(user_error,'WARNING: betaConvert/3~n  Expression: ~p~n  Stack: ~p',[Formula,Stack]), fail.


/*========================================================================
   Beta-Convert a list
========================================================================*/

betaConvertList([],[]):- !.

betaConvertList([Formula|Others],[Result|ResultOthers]):-
   betaConvert(Formula,Result),
   betaConvertList(Others,ResultOthers).


/*========================================================================
   Merge reduction
========================================================================*/

reduce(drs(D1,C1),drs(D2,C2),drs(D3,C3)):- !,
   append(D1,D2,D3),
   append(C1,C2,C3).

reduce(drs(D1,C1),merge(drs(D2,C2),B),merge(drs(D3,C3),B)):- !,
   append(D1,D2,D3),
   append(C1,C2,C3).

reduce(B1,B2,merge(B1,B2)).

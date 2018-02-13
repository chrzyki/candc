
:- module(output,[printHeader/2,
                  printFooter/1,
                  printSem/4,
                  printBox/2]).

:- use_module(xdrs2xml,[xfdrs2xml/2,xdrs2xml/2]).
:- use_module(drs2fdrs,[drs2fdrs/2,instDrs/1,elimEqDrs/2]).
:- use_module(tuples,[tuples/2,write_tuples/2]).
:- use_module(drs2fol,[drs2fol/2]).
:- use_module(printDrs,[printDrs/1]).


/*========================================================================
   Portraying DRSs (quite hacky, I know)
========================================================================*/

user:portray(drs(D,C)):- printDrs(drs(D,C)).
user:portray(merge(B,C)):- printDrs(merge(B,C)).
user:portray(smerge(B,C)):- printDrs(smerge(B,C)).
user:portray(alfa(A,B,C)):- printDrs(alfa(A,B,C)).
user:portray(xdrs(_,_,_,B)):- printDrs(B).


/*------------------------------------------------------------------------
   Header
------------------------------------------------------------------------*/

printHeader(_,_):-
   user:option('--format',no), !.

printHeader(Stream,_):-
   user:option('--format',prolog),
   user:option('--semantics',drs), !, 
   format(Stream,'~n:- multifile     sem/5, id/2.',[]),
   format(Stream,'~n:- discontiguous sem/5, id/2.',[]),
   format(Stream,'~n:- dynamic       sem/5, id/2.~n',[]).

printHeader(_,_):-
   user:option('--format',prolog), !.

printHeader(Stream,Version):-
   user:option('--format',xml), 
   user:option('--flat',true), !,
   format(Stream,'<?xml version="1.0" encoding="UTF-8"?>~n',[]),
   format(Stream,'<!DOCTYPE xfdrs-output SYSTEM "xfdrs.dtd">~n',[]),
   user:version(Version),
   format(Stream,'<xfdrs-output version="~p">~n',[Version]).

printHeader(Stream,Version):-
   user:option('--format',xml), 
   user:option('--flat',false), !,
   format(Stream,'<?xml version="1.0" encoding="UTF-8"?>~n',[]),
   format(Stream,'<!DOCTYPE xdrs-output SYSTEM "xdrs.dtd">~n',[]),
   format(Stream,'<xdrs-output version="~p">~n',[Version]).

printHeader(_,_):-
   user:option('--format',xml), 
   format(user_error,'WARNING: unable to output header.~n',[]).


/*------------------------------------------------------------------------
   Footer
------------------------------------------------------------------------*/

printFooter(Stream):-
   user:option('--format',xml), 
   user:option('--flat',true), !,
   format(Stream,'</xfdrs-output>~n',[]).

printFooter(Stream):-
   user:option('--format',xml), 
   user:option('--flat',false), !,
   format(Stream,'</xdrs-output>~n',[]).

printFooter(_):-
   user:option('--format',prolog), !.

printFooter(_):-
   user:option('--format',no), !.


/*========================================================================
   Print Sem
========================================================================*/

printSem(Stream,_Id,_Index,XDRS):-
   user:option('--semantics',triples), !,   
   tuples(XDRS,Tuples),
   write_tuples(Tuples,Stream).

printSem(Stream,_Id,_Index,XDRS):-
   user:option('--semantics',fol), !,
   XDRS=xdrs(_,_,_,DRS),     
   drs2fol(DRS,FOL),
   write(Stream,FOL),
   nl(Stream).

printSem(Stream,Id,Index,XDRS):-
   user:option('--semantics',drs),
   user:option('--format',prolog), !,  
   format(Stream,'~nid(~q,~p).~n',[Id,Index]),
   XDRS=xdrs(Words,POStags,NEtags,DRS),  
   printUtterance(Words,Stream),
   format(Stream,'~nsem(~p,',[Index]),
   printStuff(Words,Stream,','),
   printStuff(POStags,Stream,','),
   printStuff(NEtags,Stream,','),
   eqDrs(DRS,EDRS),
   instantiateDrs(EDRS),
   flattenDrs(EDRS,DRS0),
   printStuff(DRS0,Stream,' ).'),
   nl(Stream).

printSem(Stream,Id,_Index,XDRS):-
   user:option('--semantics',drs),
   user:option('--format',xml), 
   user:option('--flat',false), !, 
   format(Stream,'<xdrs xml:id="d~p">~n',[Id]),
   eqDrs(XDRS,EXDRS),
   instantiateDrs(EXDRS),  
   xdrs2xml(EXDRS,Stream),
   format(Stream,'</xdrs>~n',[]).

printSem(Stream,Id,_Index,XDRS):-
   user:option('--semantics',drs),
   user:option('--format',xml), 
   user:option('--flat',true), !, 
   format(Stream,'<xfdrs xml:id="d~p">~n',[Id]),
   eqDrs(XDRS,EXDRS),
   instantiateDrs(EXDRS),  
   flattenDrs(EXDRS,XFDRS),
   xfdrs2xml(XFDRS,Stream),
   format(Stream,'</xfdrs>~n',[]).

printSem(_,_,_,_):-
   user:option('--format',no), !.

printSem(_,_,_,_):-
   format(user_error,'WARNING: unable to output.~n',[]).


/*========================================================================
   Print Box
========================================================================*/

printBox(Stream,XDRS):-
   user:option('--box',true), 
   user:option('--format',xml), !,
   format(Stream,'~n<!-- ~n',[]),
   print(Stream,XDRS),
   format(Stream,'~n--> ~n~n',[]).

printBox(Stream,XDRS):-
   user:option('--box',true), !,
   print(Stream,XDRS).

printBox(_,_).


/*========================================================================
   Print Utterance
========================================================================*/

printUtterance(L,Stream):-
   format(Stream,'~n%%% ',[]),
   printUtterance2(L,Stream).

printUtterance2([],Stream):- !,
   format(Stream,'~n',[]).

printUtterance2([word(_,Word)|L],Stream):-
   format(Stream,'~w ',[Word]),
   printUtterance2(L,Stream).

printUtterance2([word(_,Word,_,_)|L],Stream):-
   format(Stream,'~w ',[Word]),
   printUtterance2(L,Stream).

printUtterance2([p(_,Word)|L],Stream):-
   format(Stream,'~w ',[Word]),
   printUtterance2(L,Stream).


/*========================================================================
   Print XDRS stuff flattened
========================================================================*/

printFlat(L,Stream,Final):-
   nl(Stream),
   format(Stream,'    [~n',[]),
   printFlat2(L,Stream),
   format(Stream,'~p',[Final]).


printFlat2([],Stream):- !,
   format(Stream,'    ]',[]).

printFlat2([X],Stream):- !,
   format(Stream,'     ~q~n    ]',[X]).

printFlat2([X|L],Stream):-
   format(Stream,'     ~q,~n',[X]),
   printFlat2(L,Stream).


/*========================================================================
   Instantiate DRS 
========================================================================*/

instantiateDrs(xdrs(_,_,_,DRS)):-
   user:option('--instantiate',true), !,
   instDrs(DRS).

instantiateDrs(DRS):-
   user:option('--instantiate',true), !,
   instDrs(DRS).

instantiateDrs(_).


/*========================================================================
   Eliminate Eqaility from DRS 
========================================================================*/

eqDrs(xdrs(A,B,C,DRS1),xdrs(A,B,C,DRS2)):-
   user:option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS1,DRS2):-
   user:option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS,DRS).


/*========================================================================
   Flatten DRS 
========================================================================*/

flattenDrs(xdrs(A,B,C,DRS),xdrs(A,B,C,FDRS)):-
   user:option('--flat',true), !,
   drs2fdrs(DRS,FDRS).

flattenDrs(DRS,FDRS):-
   user:option('--flat',true), !,
   drs2fdrs(DRS,FDRS).

flattenDrs(DRS,DRS).


/*========================================================================
   Print Stuff
========================================================================*/

printStuff(DRS,Stream,Closing):-
   user:option('--flat',true), !,
   printFlat(DRS,Stream,Closing).

printStuff(DRS,Stream,Closing):-
   format(Stream,'~n    ~q~p',[DRS,Closing]).


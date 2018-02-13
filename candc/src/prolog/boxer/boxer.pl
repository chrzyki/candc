
/*========================================================================
    Main File of Boxer
========================================================================*/

:- use_module(ccg2drs,[ccg2drs/3]).

:- use_module(drs2fdrs,[drs2fdrs/2,instDrs/1,elimEqDrs/2]).

:- use_module(library(lists),[member/2]).

:- use_module(output,[printHeader/2,printFooter/1,printSem/4,printBox/2]).


/*========================================================================
    Declare Dynamic Predicates
========================================================================*/

:- multifile ccg/2, w/8, id/2.
:- discontiguous ccg/2, w/8, id/2.
:- dynamic ccg/2, w/8, id/2.

:- dynamic attempted/1, completed/1, user:option/2.
 

/*========================================================================
   Main
========================================================================*/

box:-
   user:option('--input',File), \+ File = user_input, !,
   checkInput(File),
   retractall(attempted(_)), retractall(completed(_)),
   assert(attempted(0)), assert(completed(0)),
   openOutput(Stream),
   version(Version),
   printHeader(Stream,Version),
   identifyIDs(List),
   buildList(List,1,Stream),
   printFooter(Stream),
   close(Stream),
   attempted(At), completed(Co),
   Percentage is (100*Co/At),
   format(user_error,'Attempted: ~p. Completed: ~p (~2f%).~n',[At,Co,Percentage]), 
   !.

box:-
   user:option(Option,do), 
   member(Option,['--version','--help']), !.


/*------------------------------------------------------------------------
   Open Output File
------------------------------------------------------------------------*/

openOutput(Stream):-
   user:option('--output',Output),
   atomic(Output), \+ Output=user_output, !,
   open(Output,write,Stream).

openOutput(user_output).


/*------------------------------------------------------------------------
   Identify ID's in the input file
------------------------------------------------------------------------*/

identifyIDs(List):-
   findall(id(Id,Numbers),id(Id,Numbers),List),
   \+ List=[], !.

identifyIDs(List):-
   ccg(_,_),
   user:option('--window','2'), 
   findall(X,ccg(X,_),Sems),
   slidingWindow(Sems,List), !.

identifyIDs(List):-
   ccg(_,_),
   setof(id(Id,[Id]),X^ccg(Id,X),List), !.

identifyIDs([]):-
   \+ id(_,_), \+ ccg(_,_), !,
   format(user_error,'ERROR: input file contains no ccg/2 terms.~n',[]), 
   fail.

identifyIDs([]):-
   format(user_error,'ERROR: processing input file.~n',[]), 
   fail.


/*------------------------------------------------------------------------
   Generate IDs using a sliding window
------------------------------------------------------------------------*/

slidingWindow([A],[id(A,[A])]):- !.

slidingWindow([A,B],[id(A,[A,B])]):- !.

slidingWindow([A,B|L1],[id(A,[A,B])|L2]):- !,
   slidingWindow([B|L1],L2).


   
/*------------------------------------------------------------------------
   Context Parameters
------------------------------------------------------------------------*/

contextParameters(C1,C2):-
   contextParameters(C1,[],C2).

contextParameters([],_,[]):- !.

contextParameters(L1,Old,L3):- 
   select(poss(Pos),L1,L2), !,
   contextParameters(L2,[poss(Pos)|Old],L3).

contextParameters(['DOCID':DOCID|L1],Pos,[Context|L2]):- 
   atom_chars(DOCID,Chars),
   ( Chars = ['A','P','W',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_]
   ; Chars = ['N','Y','T',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_]
   ; Chars = ['X','I','E',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_] ), !,
   atom_chars(Year,[Y1,Y2,Y3,Y4]), 
   atom_chars(Month,[M1,M2]), 
   atom_chars(Day,[D1,D2]), !,
   Context=[year:Year,month:Month,day:Day],
   contextParameters(L1,Pos,L2).

contextParameters([role(A,B1,C1)|L1],Pos,[role(A,B2,C2)|L2]):- !,
   correct(Pos,B1,B2),
   correct(Pos,C1,C2),
   contextParameters(L1,Pos,L2).

contextParameters([target(A,B1,C1)|L1],Pos,[target(A,B2,C2)|L2]):- !,
   correct(Pos,B1,B2),
   correct(Pos,C1,C2),
   contextParameters(L1,Pos,L2).

contextParameters([_|L1],Pos,L2):- !,
   contextParameters(L1,Pos,L2).

contextParameters(_,_,[]).


correct([],N,N).

correct([poss(X)|L],N1,N3):-
   (X < N1, !, N2 is N1 + 1; N2 = N1),
   correct(L,N2,N3).



/*------------------------------------------------------------------------
   Build a DRS from a list of ID's 
------------------------------------------------------------------------*/

buildList([id(Id,Numbers)|L],Index,Stream):- 
   retract(attempted(At1)), At2 is At1 + 1, assert(attempted(At2)),
   sort(Numbers,Sorted),
   contextParameters(Id,Context),
   ccg2drs(Sorted,XDRS,Context),
   retract(completed(Co1)), Co2 is Co1 + 1, assert(completed(Co2)),
   printSem(Stream,Id,Index,XDRS),
   printBox(Stream,XDRS),
   NewIndex is Index + 1, !,
   buildList(L,NewIndex,Stream).

buildList([_|L],Index,Stream):-
   buildList(L,Index,Stream).

buildList([],_,_).


/*========================================================================
   Check Input File
========================================================================*/

checkInput(File):-
   atom(File),
   exists_file(File),
   retractall(ccg(_,_)),
   retractall(w(_,_,_,_,_,_,_,_)),
   retractall(id(_,_)),
   on_exception(_,consult(File),fail), !.

checkInput(File):-
   format(user_error,'ERROR: file ~p does not exist or not Prolog readable.~n',[File]),
   !, fail.


/*========================================================================
   Instantiate DRS 
========================================================================*/

instantiateDrs(xdrs(_,_,_,DRS)):-
   option('--instantiate',true), !,
   instDrs(DRS).

instantiateDrs(DRS):-
   option('--instantiate',true), !,
   instDrs(DRS).

instantiateDrs(_).


/*========================================================================
   Eliminate Eqaility from DRS 
========================================================================*/

eqDrs(xdrs(A,B,C,DRS1),xdrs(A,B,C,DRS2)):-
   option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS1,DRS2):-
   option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS,DRS).


/*========================================================================
   Flatten DRS 
========================================================================*/

flattenDrs(xdrs(A,B,C,DRS),xdrs(A,B,C,FDRS)):-
   option('--flat',true), !,
   drs2fdrs(DRS,FDRS).

flattenDrs(DRS,FDRS):-
   option('--flat',true), !,
   drs2fdrs(DRS,FDRS).

flattenDrs(DRS,DRS).


/* =======================================================================
   Version
========================================================================*/

version('Version 1.0 (Nov 2006)').

version:-
   option('--version',do), !,
   version(V),
   write(V), nl.

version.


/* =======================================================================
   Help
========================================================================*/

help:-
   option('--help',do), !,
   format('usage: boxer [options]~n~n',[]),
   setof(O,V^D^(option(O,0,V,D),format('  ~p~n',[O])),_), 
   setof(O,V^D^(option(O,-1,V,D),format('  ~p <file>~n',[O])),_), 
   setof(o(O,D),V^option(O,1,V,D),Options),
   findall(_,(member(o(O,D),Options),findall(V,option(O,1,V,_),L),format('  ~p <arg> (possible values: ~p, default: ~p)~n',[O,L,D])),_), nl.

help:-
   option('--help',dont), !.


/* =======================================================================
   Check User Options
========================================================================*/

assertOptions([]).

assertOptions([Option|L]):- 
   option(Option,0,_,_), !,
   retract(option(Option,_)),
   assert(option(Option,do)),
   assertOptions(L).

assertOptions([Option,Value|L]):- 
   atom(Option),
   option(Option,-1,_,_), 
   atomic(Value), !,
   retract(option(Option,_)),
   assert(option(Option,Value)),
   assertOptions(L).

assertOptions([Option,Value|L]):- 
   atom(Option),
   atomic(Value), 
   option(Option,1,Value,_), !,
   retract(option(Option,_)),
   assert(option(Option,Value)),
   assertOptions(L).

assertOptions([Option|L]):- 
   format(user_error,'WARNING: option ~p not supported.~n',[Option]),
   assertOptions(L).


/* =======================================================================
   Default Options
========================================================================*/

setDefaultOptions([]):- !.

setDefaultOptions([X|L]):-  
   option(X,_,_,D), !,
   assert(option(X,D)),  
   setDefaultOptions(L).
  

/* =======================================================================
   Boxer Options         % option(Option,NumberArgs,Value,Default)
========================================================================*/

option( '--help',       0, _, dont       ).
option( '--version',    0, _, dont       ).
option( '--resolve',    1, V, false      ):- member(V,[true,false]).
option( '--window',     1, V, '1'        ):- member(V,['1','2']).
option( '--warnings',   1, V, false      ):- member(V,[true,false]).
option( '--instantiate',1, V, false      ):- member(V,[true,false]).
option( '--flat',       1, V, false      ):- member(V,[true,false]).
option( '--elimeq',     1, V, false      ):- member(V,[true,false]).
option( '--box',        1, V, false      ):- member(V,[true,false]).
option( '--robust',     1, V, false      ):- member(V,[true,false]).
option( '--vpe',        1, V, false      ):- member(V,[true,false]).
option( '--format',     1, V, prolog     ):- member(V,[prolog,xml,no]).
option( '--semantics',  1, V, drs        ):- member(V,[drs,fol,triples]).
option( '--framenet',   1, V, no         ):- member(V,[training,testing,no]).
option( '--input',     -1, _, user_input ).
option( '--output',    -1, _, user_output).


/* =======================================================================
   Definition of start
========================================================================*/

start:-
   prolog_flag(argv,[_|Args]), \+ Args = [],
   set_prolog_flag(float_format,'%.20g'),
   retractall(option(_,_)), 
   setof(Op,Ar^Val^Def^option(Op,Ar,Val,Def),Options), 
   setDefaultOptions(Options), 
   assertOptions(Args),
   version,
   box, !,
   help,
   halt.

start:-
   retractall(option(_,_)),
   assert(option('--help',do)),
   help, 
   halt.



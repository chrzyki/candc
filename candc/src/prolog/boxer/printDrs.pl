
:- module(printDrs,[printDrs/1]).

:- use_module(library(lists),[append/3,member/2]).


/*========================================================================
     Counter for discourse referents
========================================================================*/

:- dynamic counter/1.

counter(0).
   

/*========================================================================
     Print Predicates
========================================================================*/

printDrs(Drs):- 
   retract(counter(_)), 
   assert(counter(1)),
   \+ \+ (formatDrs(Drs,Lines,_), 
          printDrsLines(Lines)).


/*========================================================================
     Print DRS Lines
========================================================================*/

printDrsLines([]):- !.

printDrsLines([Line|Rest]):-
   name(L,Line), 
%   nl, write(L),
   write('%%% '), write(L), nl,
   printDrsLines(Rest).


/*========================================================================
     Format DRSs
========================================================================*/

formatDrs(drs(Dom,Cond),[[32|Top],Refs2,[124|Line]|CondLines2],Length):- !,
   formatConds(Cond,[]-CondLines1,0-CondLength),
   formatRefs(Dom,Refs),
   length([_,_|Refs],RefLength),
   (RefLength > CondLength, !, Length = RefLength ; Length = CondLength), 
   closeConds(CondLines1,CondLines2,Length),
   Dif is (Length - RefLength) + 1,
   closeLine([124|Refs],Refs2,Dif,[124]),
   formatLine(95,Length,[32]-Top),
   formatLine(95,Length,[124]-Line).

formatDrs(merge(Drs1,Drs2),Lines3,Length):- !,
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,';',M1,M2),
   Length is N1 + N2 + 4.

formatDrs(smerge(Drs1,Drs2),Lines3,Length):- !,
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,'+',M1,M2),
   Length is N1 + N2 + 4.

formatDrs(alfa(_,Drs1,Drs2),Lines3,Length):- !,
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,'A',M1,M2),
   Length is N1 + N2 + 4.


/*========================================================================
     Format Discourse Referents
========================================================================*/

formatRefs([],[]):- !.

formatRefs([_:Ref|Rest],Out):-
   makeConstant(Ref,Code), 
   append([32|Code],Temp,Out),
   formatRefs(Rest,Temp).


/*========================================================================
   Turn a discourse referent into a Prolog constant
========================================================================*/

makeConstant(X,Code):- 
   atomic(X), !,
   name(X,Code).

makeConstant(X,[120|Codes]):-
   nonvar(X),
   X =.. ['$VAR',Number],
   atomic(Number), !,
   name(Number,Codes).

makeConstant(X,[C|Odes]):-
   nonvar(X), 
   X =.. ['$VAR',[C|Odes]], !.

makeConstant(X,[120|Number]):- 
   var(X),
   retract(counter(N)),
   name(N,Number), 
   name(X,[120|Number]),
   M is N+1,
   assert(counter(M)).


/*========================================================================
     Format a Line
========================================================================*/

formatLine(_,1,L-L):- !.

formatLine(Code,N,In-[Code|Out]):-
   M is N - 1, 
   formatLine(Code,M,In-Out).


/*========================================================================
     Formatting Conditions
========================================================================*/

formatConds([],L-L,N-N):- !.

formatConds([_:X|Rest],L1-L2,N1-N2):- !,
   formatConds([X|Rest],L1-L2,N1-N2).

formatConds([imp(Drs1,Drs2)|Rest],L1-L2,N0-N4):- !,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combLinesConds(Lines1,Lines2,Lines3,' ==> ',M),
   append(Lines3,Lines,L2),
   Length is N1 + N2 + 10,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([or(Drs1,Drs2)|Rest],L1-L2,N0-N4):- !,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 5,
   combLinesConds(Lines1,Lines2,Lines3,' V ',M),
   append(Lines3,Lines,L2),
   Length is N1 + N2 + 8,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([whq(_,Drs1,_,Drs2)|Rest],L1-L2,N0-N4):- !,
   formatConds([whq(Drs1,Drs2)|Rest],L1-L2,N0-N4).
 
formatConds([whq(Drs1,Drs2)|Rest],L1-L2,N0-N4):- !,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 5,
   combLinesConds(Lines1,Lines2,Lines3,' ? ',M),
   append(Lines3,Lines,L2),
   Length is N1 + N2 + 8,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([not(Drs)|Rest],L1-L2,N0-N3):- !,
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,5,''),
   append([[124,32,32,32,32,32|A],
                [124,32,32,32,32,32|B],
                [124,32,95,95,32,32|C],
                [124,32,32,32,124,32|D]|Lines2],Lines,L2),
   Length is N2 + 8,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([prop(Arg,Drs)|Rest],L1-L2,N0-N3):- !,
   formatConds(Rest,L1-Lines,N0-N1),
   makeConstant(Arg,A1),
   addSpaces(A1,[K1,K2,K3,K4,K5]),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,6,''),
   append([[124,32,32,32,32,32,32|A],
                [124,32,32,32,32,32,32|B],
                [124,K1,K2,K3,K4,K5,58|C],
                [124,32,32,32,32,32,32|D]|Lines2],Lines,L2),
   Length is N2 + 9,
   (   Length > N1, !, N3 = Length; N3 = N1).


formatConds([eq(A,B)|Rest],In-[[124,32|Line]|Out],N0-N2):- !,
   formatConds(Rest,In-Out,N0-N1),
   makeConstant(A,L1),
   makeConstant(B,L2),
   append(L1,[32,61,32|L2],Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   member(Basic,[pred(_,_,_,_),rel(_,_,_,_)]), !,
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   Basic = named(Arg,Sym,Type,Sense), !,
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(named(Arg,Sym,Type,Sense),Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   Basic = card(Arg,Digit,Type), !,
   formatConds(Rest,In-Out,N0-N1),   
   makeConstant(Arg,A),
   makeConstant(Digit,D),
   ( Type = eq, !, TypeCode1 = 61, TypeCode2 = 61 ;         %%% ==
     Type = ge, !, TypeCode1 = 62, TypeCode2 = 61 ;         %%% >=
     Type = le, !, TypeCode1 = 61, TypeCode2 = 60 ),        %%% =<
   append([124|A],[124,32,TypeCode1,TypeCode2,32|D],Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   Basic = timex(Arg,Timex), !,
   formatConds(Rest,In-Out,N0-N1),   
   name('timex',F),          
   makeConstant(Arg,A),
   timex(Timex,D),
   append(F,[40|A],T),
   append(T,[41,61|D],Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).


/*========================================================================
     Formatting Basic Conditions
========================================================================*/

formatBasic(Basic,Line):-
   Basic = pred(Arg,Functor,_,_), !,
   name(Functor,F),
   makeConstant(Arg,A),   
   append(F,[40|A],T),
   append(T,[41],Line).
   
formatBasic(Basic,Line):-
   Basic = rel(Arg1,Arg2,higher,1), !,
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   append(A1,[32,62,32|A2],Line).

formatBasic(Basic,Line):-
   Basic = rel(Arg1,Arg2,Functor,_), !,
   name(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   append(F,[40|A1],T1),
   append(T1,[44|A2],T2),
   append(T2,[41],Line).

formatBasic(Basic,Line):-
   Basic = named(Arg,Sym,Type,_), !,
   name(named,F),
   makeConstant(Arg,A),
   makeConstant(Sym,S),
   makeConstant(Type,T),
   append(F,[40|A],T1),
   append(T1,[44|S],T2),
   append(T2,[44|T],T3),
   append(T3,[41],Line).
 

/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/
    
combLinesConds([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N):-
   combLinesConds2([A1,B1,C1],[A2,B2,C2],Firsts,N,Op),
   name(Op,Code),
   append(Code,D2,T),
   append([124,32|D1],T,D),
   combLinesConds2(Rest1,Rest2,Rest,N,Op),
   append(Firsts,[D|Rest],Result).


/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/

combLinesConds2([],[],[],_,_):- !.

combLinesConds2([],[A2|Rest2],[A|Rest],N,Op):- !,
   closeLine([124],A1,N,[]),
   append(A1,A2,A),
   combLinesConds2([],Rest2,Rest,N,Op).

combLinesConds2([A1|Rest1],[],[[124,32|A1]|Rest],N,Op):- !,
   combLinesConds2(Rest1,[],Rest,N,Op).

combLinesConds2([A1|Rest1],[A2|Rest2],[A|Rest],N,' ==> '):- !,
   append([124,32|A1],[32,32,32,32,32|A2],A),
   combLinesConds2(Rest1,Rest2,Rest,N,' ==> ').

combLinesConds2([A1|Rest1],[A2|Rest2],[A|Rest],N,' V '):- !,
   append([124,32|A1],[32,32,32|A2],A),
   combLinesConds2(Rest1,Rest2,Rest,N,' V ').

combLinesConds2([A1|Rest1],[A2|Rest2],[A|Rest],N,' ? '):- !,
   append([124,32|A1],[32,32,32|A2],A),
   combLinesConds2(Rest1,Rest2,Rest,N,' ? ').


/*========================================================================
   Combining Lines of Characters (Complex DRSs)
========================================================================*/
    
combLinesDrs([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N1,N2):-
   combLinesDrs([A1,B1,C1],[A2,B2,C2],Firsts,N1,N2),
   name(Op,Code),
   append(Code,D2,T1),
   append(T1,[41],T2),
   append([40|D1],T2,D),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2),
   append(Firsts,[D|Rest],Result).


combLinesDrs([],[],[],_,_):- !.

combLinesDrs([],[A2|Rest2],[A3|Rest],N1,N2):- !,
   closeLine([],A1,N1,[]),
   append(A1,A2,A0),
   append(A0,[32],A3),
   combLinesDrs([],Rest2,Rest,N1,N2).

combLinesDrs([A1|Rest1],[],[Closed|Rest],N1,N2):- !,
   combLinesDrs(Rest1,[],Rest,N1,N2),
   closeLine([32|A1],Closed,N2,[]).

combLinesDrs([A1|Rest1],[A2|Rest2],[A3|Rest],N1,N2):- !,
   append([32|A1],[32|A2],A0),
   append(A0,[32],A3),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2).


/*========================================================================
     Close Conditions (add '|')
========================================================================*/

closeConds([],[[124|Bottom]],Length):- !,
   formatLine(95,Length,[124]-Bottom).

closeConds([Line|Rest1],[New|Rest2],Length):-
   length(Line,L),
   R is Length - L,
   closeLine(Line,New,R,[124]),
   closeConds(Rest1,Rest2,Length).


/*========================================================================
     Close Line
========================================================================*/

closeLine(Line,New,N,Accu):- 
   N < 1, !, 
   append(Line,Accu,New).

closeLine(Line,New,N,Accu):- 
   M is N - 1, !,
   closeLine(Line,New,M,[32|Accu]).


/*========================================================================
    Add Space
========================================================================*/

addSpaces([A,B],[32,32,32,A,B]):- !.
addSpaces([A,B,C],[32,32,A,B,C]):- !.
addSpaces([A,B,C,D],[32,A,B,C,D]):- !.
addSpaces([A,B,C,D,E],[A,B,C,D,E]):- !.


/*========================================================================
   Time Expressions
========================================================================*/

timex(date(_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(date(_:_,_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(time(_:Y,_:M,_:D),Timex):- !,
   timex(time(Y,M,D),Timex).

timex(date(Y,M,D),Timex):-
   year(Y,[Y1,Y2,Y3,Y4]),
   month(M,[M1,M2]),
   day(D,[D1,D2]),
   Timex = [Y1,Y2,Y3,Y4,M1,M2,D1,D2].

timex(time(H,M,S),Timex):-
   day(H,[H1,H2]),
   day(M,[M1,M2]),
   day(S,[S1,S2]),
   Timex = [H1,H2,58,M1,M2,58,S1,S2].

year(Y,C):- var(Y), !, name('XXXX',C).
year(Y,C):- name(Y,C).

month(Y,C):- var(Y), !, name('XX',C).
month(Y,C):- name(Y,C).

day(Y,C):- var(Y), !, name('XX',C).
day(Y,C):- name(Y,C).


:- module(punctuation,[semlex_punct/4]).

:- use_module(boxer(slashes)).
:- use_module(boxer(categories),[category/3]).
:- use_module(library(lists),[member/2]).
:- use_module(semlib(options),[option/2]).


/* --------------------------------------------------------------
   Aux predicates
-------------------------------------------------------------- */

punctuation('.').
punctuation(',').
punctuation(':').
punctuation(';').
punctuation('LQU').
punctuation('RQU').
punctuation('LRB').
punctuation('RRB').
punctuation('LSB').
punctuation('RSB').
punctuation('LCB').
punctuation('RCB').


/* --------------------------------------------------------------
   Punctuation
-------------------------------------------------------------- */

semlex_punct(t:_\s:_,_,_,Sem):- !,
   Sem = lam(S,app(S,lam(_,drs([],[])))).

/* --------------------------------------------------------------
   Punctuation (tuples)

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),
   member(Cat,[s:X\s:X,s:X/s:X,np\np,np/np]), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,punctuation,a,42)]),app(F,E)))))).

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),
   member(Cat,[n:X/n:X,n:X\n:X,pp/pp,pp\pp]), !,
   Sem = lam(P,lam(Y,merge(drs([],[Index:pred(Y,punctuation,a,42)]),app(P,Y)))).

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),   
   category(vpadv,Cat,_), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([],[Index:pred(E,punctuation,a,42)]),app(F,E))))))).

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),   
   member(Cat,[np\np, np/np]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:pred(X,punctuation,a,42)]),
                                       app(P,X)))))).

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),   
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([],[Index:pred(E,punctuation,a,42)]),app(F,E)))))))).

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),   
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   Sem = lam(M,lam(S,lam(F,app(app(M,S),lam(E,merge(drs([],[Index:pred(E,punctuation,a,42)]),app(F,E))))))).

semlex_punct(Cat,POS,Index,Sem):- 
   option('--semantics',tuple),
   punctuation(POS),   
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(drs([],[Index:pred(X,punctuation,a,42)]),
                                                    app(P,X))))))).
-------------------------------------------------------------- */

semlex_punct(Cat,POS,_Index,Sem):- 
   member(Cat,[(s:X/s:X)/(s:Y/s:Y),
               (s:X/s:X)/(s:Y\s:Y),
               (s:X/s:X)\(s:Y/s:Y),
               (s:X/s:X)\(s:Y\s:Y),
               (s:X\s:X)/(s:Y\s:Y),
               (s:X\s:X)/(s:Y/s:Y),
               (s:X\s:X)\(s:Y/s:Y),
               (s:X\s:X)\(s:Y\s:Y)]),
   punctuation(POS), !,
   Sem = lam(P,P).

semlex_punct(C\C,POS,_Index,Sem):- 
   punctuation(POS), !,
   Sem = lam(X,X).

semlex_punct(C/C,POS,_Index,Sem):- 
   punctuation(POS), !,
   Sem = lam(X,X).



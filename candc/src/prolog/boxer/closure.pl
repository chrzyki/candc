
:- module(closure,[closure/3]).

:- use_module(library(lists),[member/2]).
:- use_module(lexicon,[semlex/5]).


/* -------------------------------------------------------------------------
   Closure
------------------------------------------------------------------------- */

closure(Cat,Sem,Closed):-
   member(Cat,['S[wq]','S[dcl]','S[q]','S','S[pss]','S[em]','S[b]','S[bem]',
               'S[to]','S[ng]','S[pt]','S[qem]','S[X]','S[adj]','S[intj]','S[inv]',
               'S[frg]','S[poss]']), !,
   Closed = app(Sem,lam(E,drs([],[[]:pred(E,event,n,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['S[dcl]/NP','S[dcl]\NP','S[b]\NP','S[b]/NP','S[pt]/NP',
               'S[to]\NP','S[ng]\NP','S[adj]\NP','S[pss]\NP','S[q]/NP',
               'S[pss]/NP','S[pt]\NP']), !,
   Closed = app(app(Sem,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),lam(E,drs([],[[]:pred(E,event,n,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['S[dcl]/PP','S[wq]/PP']), !,
   Closed = app(app(Sem,lam(X,drs([],[[]:pred(X,thing,n,12)]))),lam(E,drs([],[[]:pred(E,event,n,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['S[X]/S[X]',
               'S[X]\S[X]',
               'S[b]/S[dcl]',
               'S[dcl]/S[dcl]',
               'S[dcl]/S[em]',
               'S[pss]/S[em]',
               'S[dcl]\S[dcl]',
               'S[b]\S[b]',
               'S[ng]\S[ng]',
               'S[wq]/S[q]']), !,
   Closed = app(app(Sem,lam(P,merge(drs([[]:X],[[]:pred(X,event,n,1)]),app(P,X)))),lam(E,drs([],[[]:pred(E,event,n,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['(S[X]/S[X])/(S[X]/S[X])',
               '(S[X]/S[X])\(S[X]/S[X])',
               '(S[X]\S[X])/(S[X]\S[X])',
               '(S[X]\S[X])\(S[X]\S[X])']), !,
   Closed = app(app(app(Sem,lam(S,lam(F,app(S,lam(E,app(F,E)))))),
                    lam(P,merge(drs([[]:X],[]),app(P,X)))),
                lam(E,drs([],[[]:pred(E,event,n,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['NP','NP[nb]']), !,
   Closed = app(Sem,lam(X,drs([],[[]:pred(X,topic,a,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['NP[nb]/N']), !,
   Closed = app(app(Sem,lam(_,drs([],[]))),lam(X,drs([],[[]:pred(X,topic,a,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['NP\NP']), !,
   Closed = app(app(Sem,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),
                lam(X,drs([],[[]:pred(X,thing,n,12)]))).

closure(Cat,Sem,Closed):-
   member(Cat,['N','PP']), !,
   Closed = merge(drs([[]:X],[[]:pred(X,topic,a,1)]),app(Sem,X)).

closure(Cat,Sem,Closed):-
   member(Cat,['(S[X]\NP)\((S[X]\NP)/NP)']), !,
   semlex('(S[dcl]\NP)/NP',event,_,[],TV),
   closure('S[dcl]\NP',app(Sem,TV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['(S[X]\NP)\((S[X]\NP)/PP)']), !,
   semlex('(S[dcl]\NP)/PP',event,_,[],TV),
   closure('S[dcl]\NP',app(Sem,TV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['(S[X]\NP)\(((S[X]\NP)/PP)/NP)']), !,
   semlex('((S[dcl]\NP)/PP)/NP',event,_,[],DTV),
   closure('S[dcl]\NP',app(Sem,DTV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['(S[dcl]\NP)/(S[b]\NP)',
               '(S[to]\NP)(S[to]\NP)',
               '(S[b]\NP)\(S[b]\NP)']), !,
   semlex('S[b]\NP',event,_,[],IV),
   closure('S[dcl]\NP',app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['(S[X]\NP)\(S[X]\NP)',
               '(S[X]\NP)/(S[X]\NP)',
               '(S[dcl]/NP)\(S[dcl]/NP)',
               '(S[dcl]\NP)\(S[dcl]\NP)']), !,
   semlex('S[dcl]\NP',event,_,[],IV),
   closure('S[dcl]\NP',app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['S[dcl]/(S[adj]\NP)']), !,
   semlex('S[adj]\NP',event,_,[],IV),
   closure('S[dcl]',app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['S[X]/(S[X]\NP)']), !,
   semlex('S[dcl]\NP',event,_,[],IV),
   closure('S[dcl]',app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['S[wq]/(S[q]/NP)','S[wq]/(S[adj]\NP)']), !,
   semlex('S[dcl]\NP',event,_,[],IV),
   closure('S[wq]',app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['S[wq]/(S[pss]\NP)','S[q]/(S[pss]\NP)']), !,
   semlex('S[pss]\NP',event,_,[],IV),
   closure('S[wq]',app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,['S[wq]/(S[b]\NP)',
               'S[q]/(S[b]\NP)',
               'S[q]/(S[ng]\NP)',
               'S[qem]/(S[dcl]/NP)']), !,
   semlex('S[dcl]\NP',event,_,[],IV),
   closure('S[wq]',app(Sem,IV),Closed).

closure(X,_,_):-
   user:option('--warnings',true),
   write(user_error,'WARNING: no closure operation for '), writeq(X), nl, 
   fail.



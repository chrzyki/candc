
:- module(coordination,[coordsem/4]).


/* -------------------------------------------------------------------------
   Coordinators
------------------------------------------------------------------------- */

coordinator(conjunction,and).
coordinator(conjunction,but).
%coordinator(conjunction,than).
coordinator(conjunction,if).
coordinator(conjunction,yet).
coordinator(conjunction,as).
coordinator(conjunction,so).
coordinator(conjunction,plus).
coordinator(conjunction,both).
coordinator(conjunction,'&').
coordinator(conjunction,'--').
coordinator(conjunction,'/').
coordinator(conjunction,'-').
coordinator(conjunction,':').
coordinator(conjunction,';').
coordinator(conjunction,',').
coordinator(conjunction,'(').
coordinator(conjunction,'"').

coordinator(disjunction,or).
coordinator(disjunction,neither).
coordinator(disjunction,either).
coordinator(disjunction,nor).

coordinator(negation,not).
coordinator(negation,than).  % rather than
coordinator(negation,of).    % instead of


/* -------------------------------------------------------------------------
   Apposition 
------------------------------------------------------------------------- */

coordsem('NP',apposition,Index,Sem):- !,
   Sem = lam(Q2,lam(Q1,lam(P,app(Q1,lam(X,merge(app(Q2,lam(Y,drs([Index:E],
                                                                 [Index:prop(E,drs([],[Index:eq(X,Y)]))]))),app(P,X))))))).


/* -------------------------------------------------------------------------
   Nouns
------------------------------------------------------------------------- */

coordsem('N',Lemma,Index,Sem):-
   coordinator(conjunction,Lemma), !,
   Sem = lam(Q2,lam(Q1,lam(P,drs([],[Index:or(app(Q1,P),app(Q2,P))])))).


/* -------------------------------------------------------------------------
   "X versus Y"
------------------------------------------------------------------------- */

coordsem(Cat,Lemma,Index,Sem):-
   member(Cat,['NP','NP[nb]']), 
   member(Lemma,['v.','vs.','versus']), !,    
   Sem = lam(Q2,lam(Q1,lam(P,app(Q1,lam(U,merge(app(Q2,lam(Y,drs([],[Index:rel(U,Y,versus,0)]))),app(P,U))))))).


/* -------------------------------------------------------------------------
   General Case
------------------------------------------------------------------------- */

coordsem(Cat,Lemma,Index,Sem):- 
   arguments(Cat,N),
   coordinator(Type,Lemma), 
   coordMacro(Type,N,Index,Sem), !.


/* -------------------------------------------------------------------------
   VP Coordination
------------------------------------------------------------------------- */

%  Dit lijkt me wel een betere oplossing maar dan blijken zinnen met negatie het niet te doen!
%
%   Sem = lam(VP2,lam(VP1,lam(NP,lam(E,app(NP,lam(X,merge(app(app(app(VP1,lam(I,I)),E),X),
%                                                         app(app(app(VP2,lam(I,I)),E),X)))))))).
%




/* =========================================================================
   Coordination Macros
========================================================================= */

coordMacro(conjunction,1,_,
           lam(X2,lam(X1,lam(A,merge(app(X1,A),
                                     app(X2,A)))))):- !.

coordMacro(disjunction,1,Index,
           lam(X2,lam(X1,lam(A,drs([],[Index:or(app(X1,A),
                                                app(X2,A))]))))):- !.

coordMacro(negation,1,Index,
           lam(X2,lam(X1,lam(A,merge(app(X1,A),
                                     drs([],[Index:not(app(X2,A))])))))):- !.


coordMacro(conjunction,2,_,
           lam(X2,lam(X1,lam(A,lam(B,merge(app(app(X1,A),B),
                                           app(app(X2,A),B))))))):- !.

coordMacro(disjunction,2,Index,
           lam(X2,lam(X1,lam(A,lam(B,drs([],[Index:or(app(app(X1,A),B),
                                                      app(app(X2,A),B))])))))):- !.

coordMacro(negation,2,Index,
           lam(X2,lam(X1,lam(A,lam(B,merge(app(app(X1,A),B),
                                           drs([],[Index:not(app(app(X2,A),B))]))))))):- !.


coordMacro(conjunction,3,_,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,merge(app(app(app(X1,A),B),C),
                                                 app(app(app(X2,A),B),C)))))))):- !.

coordMacro(disjunction,3,Index,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,drs([],[Index:or(app(app(app(X1,A),B),C),
                                                            app(app(app(X2,A),B),C))]))))))):- !.

coordMacro(negation,3,Index,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,merge(app(app(app(X1,A),B),C),
                                                 drs([],[Index:not(app(app(app(X2,A),B),C))])))))))):- !.


coordMacro(conjunction,4,_,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,lam(D,merge(app(app(app(app(X1,A),B),C),D),
                                                       app(app(app(app(X2,A),B),C),D))))))))):- !.

coordMacro(disjunction,4,Index,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,lam(D,drs([],[Index:or(app(app(app(app(X1,A),B),C),D),
                                                                  app(app(app(app(X2,A),B),C),D))])))))))):- !.

coordMacro(negation,4,Index,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,lam(D,merge(app(app(app(app(X1,A),B),C),D),
                                                       drs([],[Index:not(app(app(app(app(X2,A),B),C),D))]))))))))):- !.

coordMacro(conjunction,5,_,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,lam(D,lam(E,merge(app(app(app(app(app(X1,A),B),C),D),E),
                                                             app(app(app(app(app(X2,A),B),C),D),E)))))))))):- !.

coordMacro(disjunction,5,Index,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,lam(D,lam(E,drs([],[Index:or(app(app(app(app(app(X1,A),B),C),D),E),
                                                                        app(app(app(app(app(X2,A),B),C),D),E))]))))))))):- !.
          
coordMacro(negation,5,Index,
           lam(X2,lam(X1,lam(A,lam(B,lam(C,lam(D,lam(E,merge(app(app(app(app(app(X1,A),B),C),D),E),
                                                             drs([],[Index:not(app(app(app(app(app(X2,A),B),C),D),E))])))))))))):- !.


arguments('N',1):- !.
arguments('NP',1):- !.
arguments('NP[nb]',1):- !.
arguments('NP[expl]',1):- !. 
arguments('NP[thr]',1):- !. 
arguments('PP',1):- !.
arguments('S',1):- !.
arguments('S[dcl]',1):- !.
arguments('S[em]',1):- !.
arguments('S[qem]',1):- !.
arguments('S[X]',1):- !.
arguments('S[intj]',1):- !.
arguments('S[frg]',1):- !.
arguments('S[inv]',1):- !.
arguments('S[for]',1):- !.
arguments('S[b]',1):- !.
arguments('S[ng]',1):- !.
arguments('S[pss]',1):- !.
arguments('S[wq]',1):- !.
arguments('S[q]',1):- !.
arguments('S[qem]',1):- !.

arguments('S\NP',2):- !.
arguments('S[dcl]\NP',2):- !.
arguments('S[dcl]/NP',2):- !.
arguments('S[frg]/NP',2):- !.
arguments('S[inv]/NP',2):- !.
arguments('S[to]\NP',2):- !.
arguments('S[ng]\NP',2):- !.
arguments('S[adj]\NP',2):- !.
arguments('S[pss]\NP',2):- !.
arguments('S[b]\NP',2):- !.
arguments('S[pt]\NP',2):- !.
arguments('S[adj]\NP',2):- !.
arguments('S[asup]\NP',2):- !.
arguments('NP/N',2):- !.
arguments('NP[nb]/N',2):- !.
arguments('N/N',2):- !.
arguments('N/N[num]',2):- !.
arguments('N\N',2):- !.
arguments('N\N[num]',2):- !.
arguments('PP/NP',2):- !.
arguments('NP\NP',2):- !.
arguments('NP/NP',2):- !.
arguments('S[X]/S[X]',2):- !.
arguments('S[dcl]/S[dcl]',2):- !.
arguments('S[qem]/S[dcl]',2):- !.
arguments('S[wq]/S[q]',2):- !.
arguments('S[qem]/(S[to]\NP)',2):- !.
arguments('S[wq]/(S[q]/PP)',2):- !.
arguments('S[wq]/(S[b]\NP)',2):- !.
arguments('S[wq]/(S[q]/NP)',2):- !.

arguments('(S[dcl]\NP)/NP',3):- !.
arguments('(S[dcl]/NP)/NP',3):- !.
arguments('(S[frg]/NP)/NP',3):- !.
arguments('(S[inv]/NP)/NP',3):- !.
arguments('(S[q]/NP)/NP',3):- !.
arguments('(S[b]\NP)/NP',3):- !.
arguments('(S[pt]\NP)/NP',3):- !.
arguments('(S[to]\NP)/NP',3):- !.
arguments('(S[ng]\NP)/NP',3):- !.
arguments('(S[pss]\NP)/NP',3):- !.
arguments('(S[adj]\NP)/NP',3):- !.
arguments('(S[dcl]\NP)/PP',3):- !.
arguments('(S[pss]\NP)/PP',3):- !.
arguments('(S[adj]\NP)/PP',3):- !.
arguments('(S[dcl]/NP)/PP',3):- !.
arguments('(S[frg]/NP)/PP',3):- !.
arguments('(S[inv]/NP)/PP',3):- !.
arguments('(S[b]\NP)/PP',3):- !.
arguments('(S[pt]\NP)/PP',3):- !.
arguments('(S[to]\NP)/PP',3):- !.
arguments('(S[ng]\NP)/PP',3):- !.
arguments('(S[q]/NP)/PP',3):- !.
arguments('(S[dcl]\PP)/NP',3):- !.
arguments('(S[dcl]/PP)/NP',3):- !.
arguments('(S[frg]/PP)/NP',3):- !.
arguments('(S[inv]/PP)/NP',3):- !.
arguments('(S[b]\PP)/NP',3):- !.
arguments('(S[pt]\PP)/NP',3):- !.
arguments('(S[to]\PP)/NP',3):- !.
arguments('(S[ng]\PP)/NP',3):- !.
arguments('(S[pss]\PP)/NP',3):- !.
arguments('(S[q]/PP)/NP',3):- !.
arguments('(S[X]\NP)\(S[X]\NP)',3):- !.
arguments('(S[X]\NP)/(S[X]\NP)',3):- !.
arguments('(S[adj]\NP)/(S[adj]\NP)',3):- !.
arguments('(S[adj]\NP)\(S[adj]\NP)',3):- !.
arguments('(S[X]\NP)\((S[X]\NP)/NP)',3):- !.
arguments('(S[adj]\NP)/((S[to]\NP)/NP)',3):- !.
arguments('(S[X]\NP)\(((S[X]\NP)/PP)/NP)',3):- !.
arguments('(NP\NP)/NP',3):- !.
arguments('(S[X]/S[X])/S[dcl]',3):- !.
arguments('(S[X]\NP)\((S[X]\NP)/(S[adj]\NP))',3):- !.
arguments('(S[X]\NP)\(((S[X]\NP)/(S[to]\NP))/NP)',3):- !.
arguments('(S[dcl]\S[dcl])\NP',3):- !.
arguments('(S[dcl]\S[dcl])/NP',3):- !.
arguments('(S[dcl]/S[dcl])/NP',3):- !.
arguments('(S[dcl]/S[dcl])\NP',3):- !.
arguments('(S[dcl]\S[wq])/NP',3):- !.
arguments('(S[dcl]\NP)/S[dcl]',  3):- !.
arguments('(S[dcl]\NP)/S',       3):- !.
arguments('(S[dcl]\NP)/S[b]',    3):- !.
arguments('(S[dcl]\NP)/S[pss]',  3):- !.
arguments('(S[dcl]\NP)/S[ng]',   3):- !.
arguments('(S[dcl]\NP)/S[for]',  3):- !.
arguments('(S[dcl]\NP)/S[wq]',   3):- !.
arguments('(S[dcl]\NP)/S[em]',   3):- !.
arguments('(S[dcl]\NP)/S[qem]',  3):- !.
arguments('(S[b]\NP)/S[qem]',    3):- !.
arguments('(S[b]\NP)/S[em]',     3):- !.
arguments('(S[b]\NP)/S[pss]',    3):- !.
arguments('(S[b]\NP)/S[dcl]',    3):- !.
arguments('(S[pss]\NP)/S[qem]',  3):- !.
arguments('(S[pss]\NP)/S[dcl]',  3):- !.
arguments('(S[pss]\NP)/S[em]',   3):- !.
arguments('(S[pt]\NP)/S[em]',    3):- !.
arguments('(S[pt]\NP)/S[qem]',   3):- !.
arguments('(S[pt]\NP)/S[dcl]',   3):- !.
arguments('(S[ng]\NP)/S[dcl]',   3):- !.
arguments('(S[ng]\NP)/S[qem]',   3):- !.
arguments('(S[ng]\NP)/S[em]',    3):- !.
arguments('(S[ng]\NP)/S[pss]',   3):- !.
arguments('(S[ng]\NP)/S[b]',     3):- !.
arguments('(S[ng]\NP)/S[for]',   3):- !.
arguments('(S[adj]\NP)/S[qem]',  3):- !.
arguments('(S[adj]\NP)/S[for]',  3):- !.
arguments('(S[adj]\NP)/S[em]',   3):- !.
arguments('(S[adj]\NP)/S[dcl]',  3):- !.
arguments('(S[dcl]\NP)/S[qem]',  3):- !.
arguments('(S[dcl]\NP)/S[em]',   3):- !.
arguments('(S[b]\NP)/S[dcl]',    3):- !.
arguments('(S[dcl]\NP)/(S[to]\NP)',        3):- !.
arguments('(S[adj]\NP)/(S[to]\NP)',        3):- !.
arguments('(S[pt]\NP)/(S[to]\NP)',         3):- !.
arguments('(S[pss]\NP)/(S[to]\NP)',        3):- !.
arguments('(S[ng]\NP)/(S[to]\NP)',         3):- !.
arguments('(S[b]\NP)/(S[to]\NP)',          3):- !.
arguments('(S[adj]\NP)/(S[ng]\NP)',        3):- !.
arguments('(S[ng]\NP)/(S[ng]\NP)',         3):- !.
arguments('(S[pt]\NP)/(S[ng]\NP)',         3):- !.
arguments('(S[b]\NP)/(S[ng]\NP)',          3):- !.
arguments('(S[pss]\NP)/(S[ng]\NP)',        3):- !.
arguments('(S[ng]\NP)/(S[dcl]\NP)',        3):- !.
arguments('(S[pss]\NP)/(S[dcl]\NP)',       3):- !.
arguments('(S[pss]\NP)/(S[b]\NP)',         3):- !.
arguments('(S[ng]\NP)/(S[b]\NP)',          3):- !.
arguments('(S[pt]\NP)/(S[b]\NP)',          3):- !.
arguments('(S[b]\NP)/(S[b]\NP)',           3):- !.
arguments('(S[ng]\NP)\(S[adj]\NP)',        3):- !.
arguments('(S[pss]\NP)/(S[pt]\NP)',        3):- !.
arguments('(S[dcl]\NP[expl])/(S[b]\NP)',   3):- !.
arguments('(S[dcl]\NP[expl])/(S[pss]\NP)', 3):- !.
arguments('(S[dcl]\NP[expl])/(S[pt]\NP)',  3):- !.
arguments('(S\NP)/(S[b]\NP)',        3):- !.
arguments('(S[dcl]\NP)/(S[dcl]\NP)', 3):- !.     % have
arguments('(S[dcl]\NP)/(S[b]\NP)',   3):- !.     % modal verb (do, will, may)
arguments('(S[dcl]\NP)/(S[pt]\NP)',  3):- !.     % auxiliary verb (has)
arguments('(S[dcl]\NP)/(S[ng]\NP)',  3):- !.     % to be + ing
arguments('(S[dcl]\NP)/(S[pss]\NP)', 3):- !.     % was, were
arguments('(S[dcl]\NP)/(S[adj]\NP)', 3):- !.     % to be + adj
arguments('(S[to]\NP)/(S[b]\NP)',    3):- !.     % to
arguments('(S[to]\NP)/(S[pt]\NP)',   3):- !.     % to
arguments('(S[to]\NP)/(S[ng]\NP)',   3):- !.     % to
arguments('(S[b]\NP)/(S[dcl]\NP)',   3):- !.   
arguments('(S[b]\NP)/(S[pt]\NP)',    3):- !.     % have
arguments('(S[b]\NP)/(S[pss]\NP)',   3):- !.     % be
arguments('(S[b]\NP)/(S[adj]\NP)',   3):- !.     % to be + adj
arguments('(S[pt]\NP)/(S[pss]\NP)',  3):- !.     % been
arguments('(S[pt]\NP)/(S[adj]\NP)',  3):- !.  
arguments('(S[ng]\NP)/(S[pss]\NP)',  3):- !. 
arguments('(S[ng]\NP)/(S[adj]\NP)',  3):- !.
arguments('(S[ng]\NP)/(S[pt]\NP)',   3):- !.     % having
arguments('(S[pss]\NP)/(S[pss]\NP)', 3):- !.
arguments('(S[pss]\NP)/(S[adj]\NP)', 3):- !.
arguments('(S[em]\NP)/(S[b]\NP)',    3):- !.
arguments('(S[dcl]/(S[b]\NP))/NP',   3):- !.
arguments('(S[dcl]\(S[adj]\NP))/NP', 3):- !.
arguments('(S[q]/(S[pt]\NP))/NP',    3):- !.
arguments('(S[q]/(S[dcl]\NP))/NP',   3):- !.
arguments('(S[dcl]/(S[pt]\NP))/NP',  3):- !. 
arguments('(N/N)/(N/N)',             3):- !. 

arguments('((S[dcl]\NP)/(S[to]\NP))/NP',       4):- !.
arguments('((S[dcl]\NP[expl])/(S[to]\NP))/NP', 4):- !. 
arguments('((S[ng]\NP)/(S[to]\NP))/NP',        4):- !. 
arguments('((S[b]\NP)/(S[to]\NP))/NP',         4):- !. 
arguments('((S[pt]\NP)/(S[to]\NP))/NP',        4):- !. 
arguments('((S[dcl]\NP)/(S[b]\NP))/NP',        4):- !. 
arguments('((S[ng]\NP)/(S[b]\NP))/NP',         4):- !. 
arguments('((S[b]\NP)/(S[b]\NP))/NP',          4):- !. 
arguments('((S[dcl]\NP)/(S[ng]\NP))/NP',       4):- !. 
arguments('((S[b]\NP)/(S[ng]\NP))/NP',         4):- !. 
arguments('((S[ng]\NP)/(S[ng]\NP))/NP',        4):- !. 
arguments('((S[dcl]\NP)/(S[adj]\NP))/NP',      4):- !. 
arguments('((S[b]\NP)/(S[adj]\NP))/NP',        4):- !.  
arguments('((S[ng]\NP)/(S[adj]\NP))/NP',       4):- !. 
arguments('((S[pt]\NP)/(S[adj]\NP))/NP',       4):- !. 
arguments('((S[dcl]\NP)/(S[pss]\NP))/NP',      4):- !. 
arguments('((S[b]\NP)/(S[pss]\NP))/NP',        4):- !. 
arguments('((S[dcl]\NP)/(S[pt]\NP))/NP',       4):- !. 
arguments('((S[b]\NP)/(S[pt]\NP))/NP',         4):- !. 
arguments('((S[ng]\NP)/(S[pss]\NP))/NP',       4):- !. 
arguments('((S[ng]\NP)/(S[pt]\NP))/NP',        4):- !. 
arguments('((S[pt]\NP)/(S[b]\NP))/NP',         4):- !. 
arguments('((S[pt]\NP)/(S[ng]\NP))/NP',        4):- !.
arguments('((S[dcl]\NP)/(S[to]\NP))/PP', 4):- !.
arguments('((S[ng]\NP)/(S[to]\NP))/PP',  4):- !.
arguments('((S[pss]\NP)/(S[to]\NP))/PP', 4):- !.
arguments('((S[b]\NP)/(S[to]\NP))/PP',   4):- !.
arguments('((S[pt]\NP)/(S[to]\NP))/PP',  4):- !.
arguments('((S[dcl]\NP)/NP)/NP',4):- !.
arguments('((S[dcl]/NP)/NP)/NP',4):- !.
arguments('((S[frg]/NP)/NP)/NP',4):- !.
arguments('((S[inv]/NP)/NP)/NP',4):- !.
arguments('((S[b]\NP)/NP)/NP',4):- !.
arguments('((S[pt]\NP)/NP)/NP',4):- !.
arguments('((S[to]\NP)/NP)/NP',4):- !.
arguments('((S[ng]\NP)/NP)/NP',4):- !.
arguments('((S[pss]\NP)/NP)/NP',4):- !.
arguments('((S[q]/NP)/NP)/NP',4):- !.
arguments('((S[dcl]\NP)/PP)/NP',4):- !.
arguments('((S[dcl]/NP)/PP)/NP',4):- !.
arguments('((S[frg]/NP)/PP)/NP',4):- !.
arguments('((S[inv]/NP)/PP)/NP',4):- !.
arguments('((S[ng]\NP)/PP)/NP',4):- !.
arguments('((S[b]\NP)/PP)/NP',4):- !.
arguments('((S[pt]\NP)/PP)/NP',4):- !.
arguments('((S[to]\NP)/PP)/NP',4):- !.
arguments('((S[pss]\NP)/PP)/NP',4):- !.
arguments('((S[q]/NP)/PP)/NP',4):- !.
arguments('((S[dcl]\NP)/NP)/PP',4):- !.
arguments('((S[dcl]/NP)/NP)/PP',4):- !.
arguments('((S[frg]/NP)/NP)/PP',4):- !.
arguments('((S[inv]/NP)/NP)/PP',4):- !.
arguments('((S[ng]\NP)/NP)/PP',4):- !.
arguments('((S[b]\NP)/NP)/PP',4):- !.
arguments('((S[pt]\NP)/NP)/PP',4):- !.
arguments('((S[to]\NP)/NP)/PP',4):- !.
arguments('((S[pss]\NP)/NP)/PP',4):- !.
arguments('((S[q]/NP)/NP)/PP',4):- !.
arguments('((S[dcl]\PP)/NP)/NP',4):- !.
arguments('((S[dcl]/PP)/NP)/NP',4):- !.
arguments('((S[frg]/PP)/NP)/NP',4):- !.
arguments('((S[inv]/PP)/NP)/NP',4):- !.
arguments('((S[ng]\PP)/NP)/NP',4):- !.
arguments('((S[b]\PP)/NP)/NP',4):- !.
arguments('((S[pt]\PP)/NP)/NP',4):- !.
arguments('((S[to]\PP)/NP)/NP',4):- !.
arguments('((S[pss]\PP)/NP)/NP',4):- !.
arguments('((S[q]/PP)/NP)/NP',4):- !.
arguments('((S[X]\PP)\(S[X]\NP))/NP',4):- !.
arguments('((S[X]\NP)\(S[X]\NP))/NP',4):- !.
arguments('((S[X]\NP)\(S[X]\NP))/N',4):- !.
arguments('((S[dcl]\NP)/S[dcl])/NP',  4):- !.
arguments('((S[dcl]\NP)/S[b])/NP',    4):- !.
arguments('((S[dcl]\NP)/S[pss])/NP',  4):- !.
arguments('((S[dcl]\NP)/S[ng])/NP',   4):- !.
arguments('((S[dcl]\NP)/S[for])/NP',  4):- !.
arguments('((S[dcl]\NP)/S[wq])/NP',   4):- !.
arguments('((S[dcl]\NP)/S[em])/NP',   4):- !.
arguments('((S[dcl]\NP)/S[qem])/NP',  4):- !.
arguments('((S[b]\NP)/S[qem])/NP',    4):- !.
arguments('((S[b]\NP)/S[em])/NP',     4):- !.
arguments('((S[b]\NP)/S[pss])/NP',    4):- !.
arguments('((S[b]\NP)/S[dcl])/NP',    4):- !.
arguments('((S[pss]\NP)/S[qem])/NP',  4):- !.
arguments('((S[pss]\NP)/S[dcl])/NP',  4):- !.
arguments('((S[pss]\NP)/S[em])/NP',   4):- !.
arguments('((S[pt]\NP)/S[em])/NP',    4):- !.
arguments('((S[pt]\NP)/S[qem])/NP',   4):- !.
arguments('((S[pt]\NP)/S[dcl])/NP',   4):- !.
arguments('((S[ng]\NP)/S[dcl])/NP',   4):- !.
arguments('((S[ng]\NP)/S[qem])/NP',   4):- !.
arguments('((S[ng]\NP)/S[em])/NP',    4):- !.
arguments('((S[ng]\NP)/S[pss])/NP',   4):- !.
arguments('((S[ng]\NP)/S[b])/NP',     4):- !.
arguments('((S[ng]\NP)/S[for])/NP',   4):- !.
arguments('((S[adj]\NP)/S[qem])/NP',  4):- !.
arguments('((S[adj]\NP)/S[for])/NP',  4):- !.
arguments('((S[adj]\NP)/S[em])/NP',   4):- !.
arguments('((S[adj]\NP)/S[dcl])/NP',  4):- !.
arguments('((S[dcl]\NP)/S[qem])/NP',  4):- !.
arguments('((S[dcl]\NP)/S[em])/NP',   4):- !.
arguments('((S[b]\NP)/S[dcl])/NP',    4):- !.
arguments('((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))',4):- !.
arguments('((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP))',4):- !.

arguments('(((S[dcl]\NP)/PP)/PP)/NP',5):- !.
arguments('(((S[dcl]/NP)/PP)/PP)/NP',5):- !.
arguments('(((S[frg]/NP)/PP)/PP)/NP',5):- !.
arguments('(((S[inv]/NP)/PP)/PP)/NP',5):- !.
arguments('(((S[b]\NP)/PP)/PP)/NP',5):- !.
arguments('(((S[pt]\NP)/PP)/PP)/NP',5):- !.
arguments('(((S[to]\NP)/PP)/PP)/NP',5):- !.
arguments('(((S[ng]\NP)/PP)/PP)/NP',5):- !.
arguments('(((S[pss]\NP)/PP)/PP)/NP',5):- !.
arguments('(((S[q]/NP)/PP)/PP)/NP',5):- !.
arguments('(((S[adj]/NP)/PP)/PP)/NP',5):- !.




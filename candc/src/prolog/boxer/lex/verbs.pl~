
:- module(verbs,
          [semlex_verb/5,    % +Cat, +Sym, +PoS, +Index, -Sem
           closing/1,
           aux_modal_verb/1
          ]).

:- use_module(boxer(slashes)).
:- use_module(semlib(options),[option/2]).
:- use_module(library(lists),[member/2]).
:- use_module(lex(tense),[tense/4,aspect/5]).
:- use_module(boxer(categories),[category/4,
                                 category/5,
                                 category_type/5]).


/* =========================================================================
   I n t r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   VP Ellipsis (... so does NP ...)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):- 
   option('--vpe',true),
   member(Cat,[(s:dcl\(s:adj\np))/np]),
   member(Sym,[do]), !,
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,98),
                    []:rel(E,X,agent,0)]),
               app(P,E)),
   tense(dcl,PoS,Index,TDRS),
   Sem = lam(NP,lam(_ADJ,app(TDRS,lam(P,app(NP,lam(X,DRS)))))).

/* -------------------------------------------------------------------------
   Intransitive (VP Ellipsis)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   option('--vpe',true),
   option('--modal',true),
   modal_verb(pos,Sym),
   category_type(Cat,_,npV,[Role],Mood), !, %%% VP Ellipsis
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,98),
                    []:rel(E,X,Role,0)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,drs([],[Index:pos(app(NP,lam(X,DRS)))])))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   option('--vpe',true),
   option('--modal',true),
   modal_verb(nec,Sym),
   category_type(Cat,_,npV,[Role],Mood), !, %%% VP Ellipsis
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,98),
                    []:rel(E,X,Role,0)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,drs([],[Index:nec(app(NP,lam(X,DRS)))])))).

semlex_verb(Cat,Sym,PoS,Index,Sem):- 
   option('--vpe',true),
   member(Sym,[do,have,be,to,
               shall,will,may,can,might,must,
               should,would,could]),
   category_type(Cat,_,npV,[Role],Mood), 
   ( Sym = do, \+ Mood = pss, \+ Mood = pt ; \+ Sym = do ), 
   ( Sym = can, \+ Mood = pss ; \+ Sym = can ), 
   !,   %%% VP Ellipsis
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,98),
                    []:rel(E,X,Role,0)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,app(NP,lam(X,DRS))))).

/* -------------------------------------------------------------------------
   Many (as in "there are many")
------------------------------------------------------------------------- */

semlex_verb(Cat,many,_Pos,Index,Sem):-
   category_type(Cat,_,npV,[],_Mood), !, 
   Sem = lam(NP,lam(P,app(NP,lam(X,merge(drs([[]:E],
                                             [[]:prop(E,drs([Index:Y],
                                                               [Index:eq(X,Y),
                                                                Index:pred(X,quantity,n,1)]))]),
                                         app(P,E)))))).

/* -------------------------------------------------------------------------
   Adjectival (possible, necessary)
------------------------------------------------------------------------- */

semlex_verb(s:adj\np,possible,_Pos,Index,Sem):- 
   option('--modal',true), !,
   Sem = lam(NP,lam(P,merge(drs([[]:E],
                                [Index:pos(app(NP,lam(X,drs([],[Index:rel(E,X,theme,0)]))))]),
                            app(P,E)))).

semlex_verb(s:adj\np,necessary,_Pos,Index,Sem):- 
   option('--modal',true), !,
   Sem = lam(NP,lam(P,merge(drs([[]:E],
                                [Index:nec(app(NP,lam(X,drs([],[Index:rel(E,X,theme,0)]))))]),
                            app(P,E)))).

/* -------------------------------------------------------------------------
   Adjectival (introduces no roles)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_PoS,Index,Sem):- 
   category_type(Cat,Sym,npV,_,adj), !,
   DRS = merge(drs([],[Index:pred(X,Sym,a,0)]),app(P,X)),
   Sem = lam(NP,lam(P,app(NP,lam(X,DRS)))).

/* -------------------------------------------------------------------------
   Intransitive (standard case)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):- 
   category_type(Cat,Sym,npV,[Role],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role,0)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,app(NP,lam(X,DRS))))).

/* -------------------------------------------------------------------------
   Phrasal
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_PoS,Index,Sem):-
   category_type(Cat,Sym,npVprep,[],_Mood), !, 
   Sem = lam(PP,lam(NP,lam(F,merge(drs([[]:E],
                                       [Index:pred(E,Sym,v,0)]),
                                   merge(app(app(PP,NP),E),
                                         app(F,E)))))).


/* =========================================================================
   T r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
    Copula 
------------------------------------------------------------------------- */

semlex_verb(Cat,be,PoS,Index,Sem):- 
   option('--copula',true),
   category_type(Cat,_,npVnp,_,Mood), !,
   DRS = merge(drs([[]:E],
                   [[]:prop(E,app(NP2,lam(Y,drs([],[Index:eq(X,Y)]))))]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,app(NP1,lam(X,DRS)))))).

semlex_verb(Cat,be,PoS,Index,Sem):- 
   option('--copula',false),
   category_type(Cat,_,npVnp,_,Mood), !,
   DRS = merge(drs([[]:E],
                   [Index:pred(E,be,v,1),
                    []:rel(E,X,agent,0),
                    Index:eq(X,Y)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,
                             lam(P,app(NP1,
                                       lam(X,app(NP2,
                                                 lam(Y,DRS)))))))).

semlex_verb(Cat,be,PoS,Index,Sem):-          %%% place holder for IS-A sense
    option('--copula',true),
    category_type(Cat,_,npVnp,_,Mood), !,
    DRS = merge(drs([[]:E],
                    [[]:prop(E,drs([],[Index:imp(merge(drs([Index:X],[]),app(NP1,lam(Y,drs([],[Index:eq(Y,X)])))),
                                                    app(NP2,lam(Y,drs([],[Index:eq(Y,X)]))))]))]),
                app(P,E)),
    tense(Mood,PoS,Index,TDRS),
    Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,DRS)))).

/* -------------------------------------------------------------------------
   Adjectival (np V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   category_type(Cat,_,npVnp,_,adj), !,
   DRS = drs([],[Index:pred(X,Sym,a,0),[]:rel(X,Y,rel,0)]),
   Sem = lam(NP1,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(DRS,app(P,X))))))))).


semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   category_type(Cat,_,npVnp,_,asup), !,
   DRS = drs([],[Index:pred(X,Sym,a,0),[]:rel(X,Y,rel,0)]),
   Sem = lam(NP1,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(DRS,app(P,X))))))))).


/* -------------------------------------------------------------------------
   Adjectival (np V pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   category_type(Cat,_,npVpp,[],_Mood), !, 
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(drs([[]:E],
                                                    [[]:prop(E,drs([],
                                                                      [Index:pred(X,Sym,a,0)]))]),
                                                merge(app(PP,E),
                                                      app(P,E)))))))).

/* -------------------------------------------------------------------------
   Transitive (np V np) intensional
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   member(Sym,[want,need]),
   category_type(Cat,Sym,npVnp,[Role,_],Mood), !, 
   closing(CC),
   semlex_verb((s:b\np)/np,have,'VB',[],TV),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role,0),
                    []:rel(E,A,theme,0),
                    []:prop(A,app(app(app(TV,NP2),lam(P,app(P,X))),CC))]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,app(NP1,lam(X,DRS)))))).


/* -------------------------------------------------------------------------
   Transitive (np V np) extensional
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVnp,[Role1,Role2],Mood), !, 
%  ADRS = alfa(def,drs([],[[]:not(drs([],[[]:eq(X,Y)]))]),DRS)  % presup!
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,Y,Role2,0)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,
                             lam(P,app(NP1,
                                       lam(X,app(NP2,
                                                 lam(Y,DRS)))))))).

/* -------------------------------------------------------------------------
   Transitive (there is NP)
------------------------------------------------------------------------- */

semlex_verb((s:Mood\np_thr)/np,_,PoS,Index,Sem):- !,
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,lam(_,app(TDRS,lam(F,app(NP,lam(X,merge(drs([[]:E],
                                                            [Index:pred(E,be,v,5), 
                                                             []:rel(E,X,agent,0)]),
                                                        app(F,E)))))))).


/* -------------------------------------------------------------------------
   Transitive (np V pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,be,PoS,Index,Sem):-
   option('--copula',true),
   category_type(Cat,_,npVpp,_,Mood), !, 
   DRS = merge(drs([[]:E],
                   [[]:prop(E,app(PP,Y))]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(NP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).

semlex_verb(Cat,be,PoS,Index,Sem):-
   option('--copula',false),
   category_type(Cat,_,npVpp,_,Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,be,v,1),
                    []:rel(E,Y,agent,0)]),
               merge(app(PP,Y),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(NP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVpp,[Role],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role,0)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(NP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Transitive (pp V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,ppVnp,[Role],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role,0)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,lam(PP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).


/* =========================================================================
   D i t r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Adjectival  (np V np pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   category_type(Cat,Sym,npVnppp,[],_Mood), !, 
   Sem = lam(NP1,lam(PP,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(drs([[]:E],
                                                                            [[]:prop(E,drs([],
                                                                                              [Index:pred(X,Sym,a,0)])),
                                                                             []:rel(E,Y,rel,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

/* -------------------------------------------------------------------------
   Adjectival (np V pp np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   category_type(Cat,Sym,npVppnp,[],_Mood), !, 
   Sem = lam(PP,lam(NP1,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(drs([[]:E],
                                                                            [[]:prop(E,drs([],
                                                                                              [Index:pred(X,Sym,a,0)])),
                                                                             []:rel(E,Y,rel,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

/* -------------------------------------------------------------------------
   Adjectival (np V pp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   category_type(Cat,Sym,npVpppp,[],_Mood), !, 
   Sem = lam(PP1,lam(PP2,lam(NP,lam(P,app(NP,lam(X,merge(drs([[]:E],
                                                             [[]:prop(E,drs([],
                                                                               [Index:pred(X,Sym,a,0)]))]),
                                                         merge(app(PP1,E),
                                                               merge(app(PP2,E),
                                                                     app(P,E)))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V np np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVnpnp,[Role1,Role2,Role3],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,Z,Role3,0)]),
               app(P,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP3,lam(NP2,lam(NP1,app(TDRS,
                                     lam(P,app(NP1,
                                               lam(X,app(NP2,
                                                         lam(Z,app(NP3,
                                                                   lam(Y,DRS))))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V np pp)   
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVnppp,[Role1,Role2],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,X,Role2,0)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP1,lam(PP,lam(NP2,app(TDRS,
                                    lam(P,app(NP2,
                                              lam(Y,app(NP1,
                                                        lam(X,DRS))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V pp np)   
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVppnp,[Role1,Role2],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,X,Role2,0)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(NP1,lam(NP2,app(TDRS,
                                    lam(P,app(NP2,
                                              lam(Y,app(NP1,
                                                        lam(X,DRS))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V pp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVpppp,[Role],Mood), !,
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role,0)]),
               merge(app(PP1,E),
                     merge(app(PP2,E),
                           app(P,E)))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP1,lam(PP2,lam(NP,app(TDRS,
                                    lam(P,app(NP,lam(Y,DRS))))))).

/* -------------------------------------------------------------------------
   Ditransitive (pp V np np)   
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,ppVnpnp,[Role1,Role2],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,X,Role2,0)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP1,lam(NP2,lam(PP,app(TDRS,
                                    lam(P,app(NP2,
                                              lam(Y,app(NP1,
                                                        lam(X,DRS))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (pp V np pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,ppVnppp,[Role],Mood), !, 
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role,0)]),
               merge(app(PP1,E),
                     merge(app(PP2,E),
                           app(P,E)))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,lam(PP1,lam(PP2,app(TDRS,
                                    lam(P,app(NP,
                                              lam(Y,DRS))))))).


/* =========================================================================
   T r i t r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Tritransitive (np V np pp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVnppppp,[Role1,Role2],Mood), !,
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,X,Role2,0)]),
               merge(merge(app(PP1,E),
                           app(PP2,E)),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP1,lam(PP1,lam(PP2,lam(NP2,app(TDRS,
                                             lam(P,app(NP2,
                                                       lam(Y,app(NP1,
                                                                 lam(X,DRS)))))))))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVppnppp,[Role1,Role2],Mood), !,
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,X,Role2,0)]),
               merge(merge(app(PP1,E),
                           app(PP2,E)),
                     app(P,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP1,lam(NP1,lam(PP2,lam(NP2,app(TDRS,
                                             lam(P,app(NP2,
                                                       lam(Y,app(NP1,
                                                                 lam(X,DRS)))))))))).

    
/* =========================================================================
   P r o p o s i t i o n a l   c o m p l e m e n t   v e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Intransitive (np V s)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVs,[Role1,Role2],Mood), !,  
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,A,Role2,0),
                    []:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(S,lam(Q,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Intransitive (np V pp s)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVpps,[Role1,Role2],Mood), !,  
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role1,0),
                    []:rel(E,A,Role2,0),
                    []:prop(A,app(S,CC))]),
               merge(app(PP,E),app(F,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(S,lam(Q,app(TDRS,lam(F,app(Q,lam(Y,DRS))))))).

/* -------------------------------------------------------------------------
   Intransitive (s V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   \+ PoS = 'IN',  %%% exclude prepositions!
   category_type(Cat,Sym,sVnp,[Role1,Role2],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,A,Role1,0),
                    []:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(Q,lam(S,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
    Copula (vp V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,be,PoS,Index,Sem):-
   option('--copula',true),
   category_type(Cat,_,vpVnp,_,Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [[]:prop(E,drs([],[Index:eq(Y,A)])),
                    []:prop(A,app(app(VP,lam(P,merge(drs([[]:X],[]),app(P,X)))),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(Q,lam(VP,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Intransitive (vp V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,vpVnp,[Role1,Role2],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,A,Role1,0),
                    []:prop(A,app(app(VP,lam(P,merge(drs([[]:X],[]),app(P,X)))),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(Q,lam(VP,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Transitive (s np V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,snpVnp,[Role1,Role2,Role3],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,Z,Role3,0),
                    []:rel(E,A,Role1,0),
                    []:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(R,lam(Q,lam(S,app(TDRS,lam(F,app(R,lam(Z,app(Q,lam(Y,DRS))))))))).


/* -------------------------------------------------------------------------
   Transitive (np V np s)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVnpq,[Role1,Role2,Role3],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,A,Role3,0),
                    []:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP2,lam(S,lam(NP1,app(TDRS,lam(F,app(NP1,lam(X,app(NP2,lam(Y,DRS))))))))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVnps,[Role1,Role2,Role3],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,A,Role3,0),
                    []:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP2,lam(S,lam(NP1,app(TDRS,lam(F,app(NP1,lam(X,app(NP2,lam(Y,DRS))))))))).

/* -------------------------------------------------------------------------
   Transitive (np V vp np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVvpnp,[Role1,Role2],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:Z],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,Z,Role2,0),
                    []:prop(Z,app(app(VP,NP2),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(NP2,lam(NP1,app(TDRS,lam(F,app(NP1,lam(X,DRS))))))).

/* -------------------------------------------------------------------------
   Transitive (np V vp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVvppp,[Role],Mood), !,
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(PP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,merge(drs([[]:E],
                                                                    [Index:pred(E,Sym,v,0),
                                                                     []:rel(E,X,Role,0)]),
                                                                merge(app(PP,E),
                                                                      merge(app(app(VP,lam(P,app(P,E))),F),
                                                                            app(F,E))))))))))).

/* -------------------------------------------------------------------------
   Transitive (np V pp vp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category_type(Cat,Sym,npVppvp,[Role],Mood), !,
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(AP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,merge(drs([[]:E],
                                                                    [Index:pred(E,Sym,v,0),
                                                                     []:rel(E,X,Role,0)]),
                                                                merge(app(PP,E),
                                                                      merge(app(app(AP,lam(P,app(P,E))),F),
                                                                            app(F,E))))))))))).


/* =========================================================================
   C o n t r o l   v e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Control Verbs (intransitive)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):- 
   Mood=adj,
   category(cv,Cat,[Role1,Role2],_,Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:B],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,B,Role2,0),
                    []:prop(B,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

semlex_verb(Cat,Sym,PoS,Index,Sem):- 
   category(cv,Cat,[Role1,Role2],PoS,Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:B],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,B,Role2,0),
                    []:prop(B,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).


/* -------------------------------------------------------------------------
   Subject Control Verbs
------------------------------------------------------------------------- */
                    
semlex_verb(Cat,Sym,PoS,Index,Sem):-
   member(Sym,[promise,offer]),
   category(socv,Cat,[Role1,Role2],Mood), !,
   closing(CC),
   Modal = drs([[]:A],
           [[]:rel(E,A,theme,0),
            []:prop(A,app(app(V,lam(R,app(R,X))),CC))]),
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,X,Role1,0)]),
               merge(Modal,app(F,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).


/* -------------------------------------------------------------------------
   ECM Verbs (exceptional case marking verbs, following GB)
------------------------------------------------------------------------- */
                    
semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category(socv,Cat,[Role1,_Role2],Mood), !,
   closing(CC),
   Modal = drs([[]:A],
           [[]:rel(E,A,theme,0),
            []:prop(A,app(app(V,Q2),CC))]),
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0)]),
               merge(Modal,app(F,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,DRS))))))).


/* -------------------------------------------------------------------------
   Object Control Verbs (default)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category(socv,Cat,[Role1,Role2],Mood), !,
   closing(CC),
   Modal = drs([[]:A],
           [[]:rel(E,A,theme,0),
            []:prop(A,app(app(V,lam(R,app(R,Y))),CC))]),
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,Y,Role2,0),
                    []:rel(E,X,Role1,0)]),
               merge(Modal,app(F,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).


/* -------------------------------------------------------------------------
   Object Control Verbs subcatting PP
------------------------------------------------------------------------- */
                    
semlex_verb(Cat,Sym,PoS,Index,Sem):-
   category(pscv,Cat,[Role1,Role2],Mood), !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:A],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,Role1,0),
                    []:rel(E,A,Role2,0),
                    []:prop(A,app(app(V,lam(R,app(R,X))),CC))]),
               merge(app(PP,E),
                     app(F,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(PP,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,DRS))))))).


/* =========================================================================
   A u x i l i a r y   a n d   M o d a l   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Standard case (NP aux VP)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   option('--modal',true), 
   modal_verb(pos,Sym),
   Cat = (s:Mood\np)/(s:Aspect\np), 
   \+ Mood = Aspect, !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,lam(E,drs([],[Index:pos(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   option('--modal',true),
   modal_verb(nec,Sym),
   Cat = (s:Mood\np)/(s:Aspect\np), 
   \+ Mood = Aspect, !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,lam(E,drs([],[Index:nec(app(app(TDRS,app(VP,NP)),E))])))).


semlex_verb(Cat,be,PoS,Index,Sem):-
   option('--copula',true),
   Cat = (s:Mood\np)/(s:adj\np), 
   \+ Mood = adj, !,
   closing(CC),
   DRS = merge(drs([[]:E],
                   [[]:prop(E,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   Cat = (s:Mood\np)/(s:adj\np), 
   \+ Mood = adj, !,
   closing(CC),
   DRS = merge(drs([[]:E],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,agent,0)]),
               merge(app(app(VP,lam(P,app(P,X))),CC),
                     app(F,E))),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   Cat = (s:Mood\np)/(s:ng\np), 
   \+ Sym = be, \+ Mood = ng, !,
   closing(CC),
   DRS = merge(drs([[]:E,[]:B],
                   [Index:pred(E,Sym,v,0),
                    []:rel(E,X,agent,0),
                    []:rel(E,B,theme,0),
                    []:prop(B,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   Cat = (s:Mood\np)/(s:Aspect\np), 
   \+ Mood = Aspect, !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,app(VP,NP)))).


/* -------------------------------------------------------------------------
   There (NP aux VP)
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   Cat = (s:Mood\np_thr)/(s:Aspect\np), 
   \+ Mood = Aspect, !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,app(VP,NP)))).

/* -------------------------------------------------------------------------
   Inversion case (Aux NP VP)
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   Cat = (s:Mood/(s:Aspect\np))/np, 
   \+ Mood = Aspect, !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(NP,lam(VP,app(TDRS,app(VP,NP)))).

/* -------------------------------------------------------------------------
   Funny transitive case (rare -- often in incorrect parses)
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   Cat = ((s:Mood\np)\np)/(s:Aspect\np), !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(_,lam(NP,app(TDRS,app(VP,NP))))).

/* -------------------------------------------------------------------------
   Copula "How ADJ be NP PP"
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   Cat = ((s:q/pp)/(s:Aspect\np))/np, !,
   aspect(Aspect,PoS,q,Index,TDRS),
   Sem = lam(NP,lam(VP,lam(PP,lam(F,app(app(TDRS,app(VP,NP)),lam(E,merge(app(PP,E),app(F,E)))))))).


/* =========================================================================
   E x p l e t i v e   v e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Cleft  "it was NP who VP"
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):- 
   member(Cat,[((s:Mood\np_exp)/(np\np))/np,
               ((s:Mood\np)/(np\np))/np]), !,
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,lam(M,lam(_,app(TDRS,lam(F,alfa(fac,merge(drs([Index:X],[]),
                                                          app(app(M,lam(P,app(P,X))),lam(U,merge(drs([[]:E],[[]:rel(E,U,theme,0)]),
                                                                                                 app(F,E))))),
                                                    app(NP,lam(U,drs([],[Index:eq(U,X)]))))))))).

/* -------------------------------------------------------------------------
   Copula  "it is ADJ to-VP"
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   Cat = ((s:Mood\np_exp)/(s:to\np))/(s:adj\np), !,
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(AP,lam(VP,lam(_,app(TDRS,lam(E,DRS))))),
   DRS = drs([],[Index:imp(merge(drs([[]:X,[]:Y],[]),
                                 app(app(VP,lam(P,app(P,X))),
                                     lam(F,drs([],[Index:eq(Y,F)])))),
                           app(app(AP,lam(P,app(P,Y))),E))]).

/* -------------------------------------------------------------------------
   Copula  "it is ADJ that S"
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   member(Cat,[((s:Mood\np_exp)/s:em)/(s:adj\np),
               ((s:Mood\np)/s:em)/(s:adj\np)]), !,
   tense(Mood,PoS,Index,TDRS),
   closing(CC),
   Sem = lam(AP,lam(S,lam(_,app(TDRS,lam(E,merge(drs([[]:K],[Index:prop(K,app(S,CC))]),
                                                 app(app(AP,lam(P,app(P,K))),E))))))).

/* -------------------------------------------------------------------------
   Copula  "it is ADJ whether S"
------------------------------------------------------------------------- */

semlex_verb(Cat,_,PoS,Index,Sem):-
   member(Cat,[((s:Mood\np_exp)/s:qem)/(s:adj\np),
               ((s:Mood\np)/s:qem)/(s:adj\np)]), !,
   tense(Mood,PoS,Index,TDRS),
   closing(CC),
   Sem = lam(AP,lam(S,lam(_,app(TDRS,lam(E,merge(drs([[]:K],[Index:prop(K,app(S,CC))]),
                                                 app(app(AP,lam(P,app(P,K))),E))))))).

/* -------------------------------------------------------------------------
   Copula  "it is ADJ for S (= NP to VP)"
------------------------------------------------------------------------- */

semlex_verb(Cat,_,PoS,Index,Sem):-
   member(Cat,[((s:Mood\np_exp)/s:for)/(s:adj\np), 
               ((s:Mood\np)/s:for)/(s:adj\np)]), !,
   tense(Mood,PoS,Index,TDRS),
   closing(CC),
   Sem = lam(AP,lam(S,lam(_,app(TDRS,lam(E,merge(drs([[]:K],[Index:prop(K,app(S,CC))]),
                                                 app(app(AP,lam(P,app(P,K))),E))))))).

/* -------------------------------------------------------------------------
   Copula  "it is PP that S"
------------------------------------------------------------------------- */

semlex_verb(Cat,_,_Pos,_Index,Sem):-
   Cat = ((s:dcl\np_exp)/s:em)/pp, !,
   Sem = lam(PP,lam(S,lam(_,lam(F,app(S,lam(E,merge(app(PP,E),app(F,E)))))))). 

/* -------------------------------------------------------------------------
   Copula  "it is NP that S"
------------------------------------------------------------------------- */

semlex_verb(Cat,_,PoS,Index,Sem):-
   Cat = ((s:Mood\np_exp)/s:em)/np, !,
   tense(Mood,PoS,Index,TDRS),
   closing(CC),
   Sem = lam(NP,lam(S,lam(_,app(TDRS,lam(E,merge(drs([Index:F,[]:K],
                                            [[]:rel(F,K,theme,0),
                                             []:prop(K,app(S,CC))]),
                                        merge(app(NP,lam(X,drs([],[[]:eq(X,K)]))),
                                              app(E,F)))))))).

/* -------------------------------------------------------------------------
   Control "it takes/makes/is NP to VP" 
------------------------------------------------------------------------- */

semlex_verb(Cat,_,PoS,Index,Sem):-
   Cat = ((s:Mood\np_exp)/(s:to\np))/np, !,
   closing(CC),
   tense(Mood,PoS,Index,TDRS),
   Sem = lam(NP,lam(VP,lam(_,app(TDRS,lam(F,merge(drs([[]:E],
                                             [Index:imp(merge(drs([[]:X],[]),
                                                              app(app(VP,lam(P,app(P,X))),CC)),
                                                        app(NP,lam(Y,drs([],[Index:eq(Y,X)]))))]),
                                         app(F,E))))))).

/* -------------------------------------------------------------------------
   Modal "it may/could/would VP"
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   option('--modal',true),
   modal_verb(pos,Sym),
   Cat = (s:Mood\np_exp)/(s:Aspect\np), !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,lam(E,drs([],[Index:pos(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,Sym,PoS,Index,Sem):-
   option('--modal',true),
   modal_verb(nec,Sym),
   Cat = (s:Mood\np_exp)/(s:Aspect\np), !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,lam(E,drs([],[Index:nec(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,_Sym,PoS,Index,Sem):-
   option('--modal',false),
   Cat = (s:Mood\np_exp)/(s:Aspect\np), !,
   aspect(Aspect,PoS,Mood,Index,TDRS),
   Sem = lam(VP,lam(NP,lam(E,app(app(TDRS,app(VP,NP)),E)))).


/* -------------------------------------------------------------------------
   Comparative "V it COMP (for) S"
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   Cat = (((s:_\np)/s:_)/(s:adj\np))/np_exp, !,
   closing(CC),
   Sem = lam(_,lam(AP,lam(S,lam(NP,lam(F,
         app(NP,lam(X,merge(drs([[]:K,[]:E],
                                [Index:pred(E,Sym,v,0),
                                 []:rel(E,X,agent,0),
                                 []:rel(E,K,theme,0),
                                 []:prop(K,app(S,CC))]),
                            merge(app(app(AP,lam(P,app(P,K))),lam(G,drs([],[[]:rel(E,G,result,0)]))),
                                  app(F,E)))))))))).

/* -------------------------------------------------------------------------
   Comparative "make/find it COMP to VP"
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,_Pos,Index,Sem):-
   Cat = (((s:_\np)/(s:to\np))/(s:adj\np))/np_exp, !,
   closing(CC),
   DRS = app(app(VP,lam(P,merge(drs([[]:X],[]),app(P,X)))),CC),
   Sem = lam(_,lam(AP,lam(VP,lam(NP,lam(F,
         app(NP,lam(X,merge(drs([[]:K,[]:E],
                                [Index:pred(E,Sym,v,0),
                                 []:rel(E,X,agent,0),
                                 []:rel(E,K,theme,0),
                                 []:prop(K,DRS)]),
                            merge(app(app(AP,lam(P,app(P,K))),lam(G,drs([],[[]:rel(E,G,result,0)]))),
                                  app(F,E)))))))))).


/* =========================================================================
   Closing
========================================================================= */

closing(lam(_,drs([],[]))).


/* =========================================================================
   Modal Verbs
========================================================================= */

aux_modal_verb(V):- aux_verb(V).
aux_modal_verb(V):- modal_verb(_,V).

aux_verb(be).
aux_verb(do).
aux_verb(have).
aux_verb(to).

modal_verb(pos, can).
modal_verb(pos, may).
modal_verb(pos, might).
modal_verb(pos, could).
modal_verb(nec, shall).
modal_verb(nec, will).
modal_verb(nec, must).
modal_verb(nec, should).
modal_verb(nec, would).


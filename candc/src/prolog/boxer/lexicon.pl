
:- module(lexicon,[semlex/5,semlex/6]).

/* -------------------------------------------------------------------------
   Notational conventions used in lambda expressions
     
        X, Y, Z       individuals
        G             groups
        P             properties (also event-types)
        NP            noun phrases
        N             nouns
        PP            prepositional phrases
        VP            verb phrases


   Senses used for standard predicates

       a 1           topic (not n, causes inconsistencies)
       n 12          thing
       n 1           person 
       n 1           event
       n 1           group
       n 2           reason
       n 2           manner
       n 1           proposition
       n 1           location


------------------------------------------------------------------------- */

:- use_module(string2digit,[string2digit/2]).
:- use_module(categories,[category/3]).
:- use_module(coordination,[coordsem/4]).
:- use_module(parse_ccgcat,[parse_ccgcat/2,gen_ccgcat/2]).

:- [nationality].


/* =========================================================================
   Wrapper to choose lemma or word
========================================================================= */

semlex(Cat,_,or,_Pos,Index,Sem):-
   parse_ccgcat(Cat,forward(backward(CCat,CCat),CCat)),
   gen_ccgcat(CCat,CCCat), !,
   coordsem(CCCat,or,Index,Sem).

semlex(Cat,_Word,Lemma,Pos,Index,Sem):-
   \+ Pos = 'NNP', \+ Pos = 'NNPS', !,
   semlex(Cat,Lemma,Pos,Index,Sem).

semlex(Cat,Word,_Lemma,Pos,Index,Sem):-
   (Pos = 'NNP'; Pos = 'NNPS'), !,
   semlex(Cat,Word,Pos,Index,Sem).



/* =========================================================================
   Quotes
========================================================================= */

semlex('Q',_,_,Index,Sem):- !,
   Sem = lam(X,drs([],[Index:pred(X,quotation,n,2)])).

semlex('(N/Q)/N',_,_,Index,Sem):- !,
   Sem = lam(N,lam(Q,lam(X,merge(drs([],[Index:pred(X,quotation,n,2)]),
                                 merge(app(N,X),app(Q,X)))))).

semlex('(NP/Q)/NP',_,_,Index,Sem):- !,
   Sem = lam(NP,lam(Q,lam(P,app(NP,lam(X,merge(drs([],[Index:pred(X,quotation,n,2)]),
                                               merge(app(Q,X),app(P,X)))))))).

semlex('(S[dcl]/Q)/S[dcl]',_,_,Index,Sem):- !,
   Sem = lam(S,lam(Q,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,quotation,n,2)]),
                                             merge(app(Q,E),app(F,E)))))))).


/* =========================================================================
   Noun Phrases
========================================================================= */

/* -------------------------------------------------------------------------
   Expletive 'there' and other "special" nouns
------------------------------------------------------------------------- */

semlex('N',there,'EX',Index,lam(X,drs([Index:Y],[Index:pred(Y,location,n,1),
                                                 Index:rel(Y,X,rel,0)]))):- !.

semlex('N',many,'NN',Index,lam(X,drs([],[Index:pred(X,quantity,n,1)]))):- !.
semlex('N',much,'NN',Index,lam(X,drs([],[Index:pred(X,amount,n,3)]))):- !.


/* -------------------------------------------------------------------------
   Nouns
------------------------------------------------------------------------- */

semlex('N[num]',Sym,_Pos,Index,Sem):- 
   string2digit(Sym,Digit), !,
   Sem = lam(X,drs([],[Index:card(X,Digit,ge)])).

semlex(Cat,Sym,Pos,Index,Sem):- 
   member(Cat,['N','N[num]']), !,
   (
       Pos = 'JJS', \+ Sym=most, !,   %%% Superlative, predicative
       symbol(Sym,NormSym),
       Sem = lam(X,drs([],[[-2|Index]:imp(drs([[]:Y],[[]:not(drs([],[[]:eq(Y,X)]))]),drs([],[Index:rel(X,Y,NormSym,0)]))]))
   ;
       Pos = 'CD',
       string2digit(Sym,Digit), !,
       Sem = lam(X,drs([],[Index:card(X,Digit,ge),Index:pred(X,thing,n,12)]))
   ;
       Pos = 'NNP', !,
       symbol(Sym,NormSym),
       Sem = lam(X,drs([],[Index:named(X,NormSym,nam,0)]))
   ;
       Pos = 'NNPS', !,
       symbol(Sym,NormSym),
       Sem = lam(X,drs([],[Index:named(X,NormSym,nam,0)]))
   ;
       symbol(Sym,NormSym),
       Sem = lam(X,drs([],[Index:pred(X,NormSym,n,0)]))
   ).


/* -------------------------------------------------------------------------
   Singular Determiners
   See for 'neither' Heim & Kratzer 1998 p. 154
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['a','an','one']), !,
   Sem = lam(N,lam(P,merge(merge(drs([Index:X],[]),app(N,X)),app(P,X)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,[each,either,every,whichever,whatever]), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),app(N,X)),app(P,X))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['neither']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),app(N,X)),drs([],[Index:not(app(P,X))]))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['that','this']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:X],[]),app(N,X)),app(P,X)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['another']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([[]:Y],[]),app(N,Y)),
                              merge(merge(drs([Index:X],[Index:not(drs([],[[]:eq(X,Y)]))]),app(N,X)),app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N',
   member(Lemma,[which,what]), !,
   Sem = lam(N,lam(P,drs([],[Index:whq([],
                                       merge(drs([Index:X],[]),app(N,X)),
                                       X,
                                       app(P,X))]))).


/* -------------------------------------------------------------------------
   Singular or Plural Determiners
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N',
   member(Lemma,['some']), !,
   Sem = lam(N,lam(P,merge(merge(drs([Index:X],[]),app(N,X)),app(P,X)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['some']), !,
   Sem = lam(N,lam(P,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                          Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                    app(P,X))]),
                           app(N,G)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N',
   member(Lemma,['no']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),app(N,X)),drs([],[Index:not(app(P,X))]))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['no']), !,
   Sem = lam(N,lam(P,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                          Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                    drs([],[Index:not(app(P,X))]))]),
                           app(N,G)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N',
   member(Lemma,['the']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:X],[]),app(N,X)),app(P,X)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['the']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                              drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))])))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N',
   member(Lemma,['any']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),app(N,X)),app(P,X))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['any']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))]))).

/* -------------------------------------------------------------------------
   Plural Determiners
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['few','several','many']), !,
   Sem = lam(N,lam(P,merge(merge(drs([Index:X],[]),app(N,X)),app(P,X)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['few','several','many']), !,
   Sem = lam(N,lam(P,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                        app(P,X))]),
                           app(N,G)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['all']), 
   !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),app(N,X)),app(P,X))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['all']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                                         drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N',
   member(Lemma,['these','those','both']),
   !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:X],[]),app(N,X)),app(P,X)))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['these','those']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                              drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))])))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   member(Cat,['NP[nb]/N','NP[nb]/N[pl]']),
   member(Lemma,['both']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                     Index:card(G,2,eq)]),app(N,G)),
                              drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))])))).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex(Cat,_Lemma,_Pos,Index,Sem):-
   Cat = '(NP[nb]/N)/(N/N)', !,
   Sem = lam(S,lam(P,lam(Q,merge(drs([Index:U],[]),
                                 merge(app(app(S,lam(X,merge(app(P,X),drs([Index:Y],[Index:rel(X,Y,of,0)])))),U),
                                       app(Q,U)))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['my','your']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['my','your']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['our']), 
   !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['our']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:imp(drs([Index:Y],[Index:rel(Y,G,member,0)]),
                                                                      drs([],[Index:pred(Y,person,n,1),
                                                                              Index:rel(X,Y,of,0)]))]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['our']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G1],[Index:pred(G1,group,n,1)]),
                              alfa(def,drs([Index:G2],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                                 merge(drs([Index:Y],[Index:pred(Y,person,n,1),
                                                                                      Index:rel(X,Y,of,0),
                                                                                      Index:rel(Y,G1,member,0)]),
                                                                       app(N,X)))]),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                         app(P,X))]))))).


semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['its']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,neuter,a,0)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['its']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,neuter,a,0)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['his']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,male,a,0)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['his']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,male,a,0)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['her']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,female,a,0)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['her']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,female,a,0)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['their']),
   !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,thing,n,12)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['their']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:imp(drs([Index:Y],[Index:rel(Y,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),
                                             app(N,X)),
                                       app(P,X))))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['their']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G1],[Index:pred(G1,group,n,1)]),
                              alfa(def,merge(drs([Index:G2],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                                       drs([],[Index:rel(X,G1,of,0)]))]),app(N,G2)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                         app(P,X))]))))).

/* -------------------------------------------------------------------------
   Misclassified determiners
------------------------------------------------------------------------- */

semlex(Cat,_Lemma,_Pos,Index,Sem):-
   Cat='NP[nb]/N', !,
   Sem = lam(N,lam(P,merge(merge(drs([Index:X],[]),app(N,X)),app(P,X)))).


/* -------------------------------------------------------------------------
   Many/Much
------------------------------------------------------------------------- */

semlex('NP',many,_,Index,Sem):- !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,quantity,n,1)]),app(P,X))).

semlex('NP',much,_,Index,Sem):- !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,amount,n,3)]),app(P,X))).


/* -------------------------------------------------------------------------
   Pronouns (non-reflexives)
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['I',i,me,mine]), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['we','us','ours']), 
   !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['we','us','ours']), !,
   Sem = lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                        drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                          merge(drs([],[Index:pred(X,person,n,1)]),       
                                                app(P,X)))]))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,[whom,'you','yours']), !,
   Sem = lam(P,alfa(dei,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['he','his','him']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,male,a,0)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['she','hers','her']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,female,a,0)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['it']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,neuter,a,0)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['they','them','theirs']), 
   !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['they','them','theirs']), !,
   Sem = lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                        drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                          app(P,X))]))).


/* -------------------------------------------------------------------------
   Reflexive Pronouns 
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['myself','yourself','ourselves']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['himself']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,male,a,0)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['herself']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,female,a,0)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['itself']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,neuter,a,0)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['themselves']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,group,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   Demonstratives and Quantificational Noun Phrases
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['none','neither',nothing]), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,thing,n,12)]),drs([],[Index:not(app(P,X))]))])).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,[something,some,'both','most','more','many','less','half','another']), !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['this','that','those','these']), !,
   Sem = lam(P,alfa(def,drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,['all','any','each','either',everything,anything]), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))])).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,[everybody,everyone,anybody,anyone]), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))])).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,[nobody,noone,'no-one']), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,person,n,1)]),drs([],[Index:not(app(P,X))]))])).

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   member(Lemma,[someone,somebody]), !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP Expletives
------------------------------------------------------------------------- */

semlex(Cat,_Lemma,_Pos,Index,Sem):-
   Cat = 'NP[expl]', !,
   Sem = lam(P,merge(drs([Index:X],[]),app(P,X))).

semlex(Cat,_Lemma,_Pos,Index,Sem):-
   Cat = 'NP[thr]', !,
   Sem = lam(P,merge(drs([Index:X],[]),app(P,X))).


/* -------------------------------------------------------------------------
   NP Why
------------------------------------------------------------------------- */

semlex(Cat,Lemma,_Pos,Index,Sem):-
   Cat = 'NP',
   Lemma = 'why', !,
   Sem = lam(P,drs([],[Index:whq([des:rea],
                                 drs([Index:X],[Index:pred(X,reason,n,2)]),
                                 X,
                                 app(P,X))])).


/* -------------------------------------------------------------------------
   NP (all others)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   Cat = 'NP', !,
   symbol(Sym,NormSym), 
   (
      member(Pos,['NNP','NNPS']), !,
      Sem = lam(P,alfa(nam,drs([Index:X],[Index:named(X,NormSym,nam,0)]),app(P,X)))
   ;
      Sem = lam(P,merge(drs([Index:X],[Index:pred(X,NormSym,n,0)]),app(P,X)))
   ).


/* -------------------------------------------------------------------------
   NP/PP
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = 'NP/PP', !,
   Sem = lam(PP,lam(P,merge(drs([Index:X],[Index:pred(X,Sym,n,0)]),merge(app(P,X),app(PP,X))))).


/* -------------------------------------------------------------------------
   Question words
------------------------------------------------------------------------- */

semlex(Cat,whose,_Pos,Index,Sem):-
   member(Cat,['(S[wq]/(S[dcl]\NP))/N',
               '(S[wq]\(S[dcl]/NP))/N']), !,  % which N + DCL
   Sem = lam(N,lam(VP,lam(F,drs([],[Index:whq([ins:hum],
                                              alfa(def,merge(drs([Index:Y],[]),app(N,Y)),
                                                       drs([Index:X],[Index:pred(X,person,n,1),Index:rel(Y,X,of,0)])),
                                              X, 
                                              app(app(VP,lam(P,app(P,Y))),lam(E,app(F,E))))])))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(S[wq]/(S[dcl]\NP))/N',
               '(S[wq]\(S[dcl]/NP))/N']), !,  % which N + DCL
   Sem = lam(N,lam(VP,lam(F,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),
                                              X, 
                                              app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))])))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   Cat = '(S[wq]/(S[q]/NP))/N', !,  % WH-DET N + YNQ
   Sem = lam(N,lam(VP,lam(F,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),          
                                              X,
                                              app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))])))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   Cat = '(S[wq]/(S[q]/PP))/N', !,  % WH-DET N + YNQ
   Sem = lam(N,lam(VP,lam(F,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),
                                              X,
                                              app(app(VP,lam(Y,drs([],[Index:rel(Y,X,rel,0)]))),lam(E,app(F,E))))])))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(S[wq]/(S[q]/NP))/NP',
               '(S[wq]/(S[dcl]\NP))/NP']), !,  % How much
   Sem = lam(NP,lam(VP,lam(F,drs([],[Index:whq([num:cou],
                                               merge(drs([Index:X],[]),
                                                     app(NP,lam(U,drs([],[Index:eq(U,X)])))),
                                               X,
                                               app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))])))).

% How many states were still united after the southern states seceded?
%
semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[wq]/(S[pss]\NP))/((S[q]/(S[pss]\NP))/NP))/N)/(NP/N)']),
   Sem = lam(M,lam(N,lam(_,lam(VP,lam(F,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[]),merge(app(app(M,Y),X),app(N,X))),
                                                    Y,
                                                    app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))])))))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['((S[wq]/(S[q]/NP))/N)/(NP/N)',        
               '((S[wq]/(S[dcl]\NP))/N)/(NP/N)']), !, % how much/many N + YNQ
   Sem = lam(M,lam(N,lam(VP,lam(F,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[]),merge(app(app(M,Y),X),app(N,X))),
                                                    Y,
                                                    app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))]))))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['((S[wq]/(S[q]/PP))/N)/(NP/N)',        
               '((S[wq]/(S[dcl]\PP))/N)/(NP/N)']), !, % how much/many N + YNQ
   Sem = lam(M,lam(N,lam(VP,lam(F,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[]),merge(app(app(M,Y),X),app(N,X))),
                                                    Y,
                                                    app(app(VP,lam(Y,drs([],[Index:rel(Y,X,rel,0)]))),lam(E,app(F,E))))]))))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[wq]/PP)/((S[q]/PP)/NP))/N)/(NP/N)']), !,
   Sem = lam(M,lam(N,lam(TV,lam(PP,lam(F,drs([],[Index:whq([num:cou],
                                                           merge(drs([Index:X,Index:Y],[]),merge(app(app(M,Y),X),app(N,X))),
                                                           Y,
                                                           app(app(app(TV,lam(P,app(P,X))),PP),lam(E,app(F,E))))])))))).

semlex('((S[wq]/PP)/N)/(NP/N)',_Sym,_Pos,Index,Sem):- !,  % American English dialect (How many feet in a mile?)
   Sem = lam(M,lam(N,lam(PP,lam(_,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[]),merge(app(app(M,Y),X),app(N,X))),
                                                    Y,
                                                    app(PP,X))]))))).
         
semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(S[qem]/(S[dcl]\NP))/N',
               '(S[qem]/(S[dcl]/NP))/N']), !, 
   Sem = lam(N,lam(VP,lam(F,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),
                                              X,
                                              app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))])))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(S[wq]/(S[q]/(S[adj]\NP)))/(S[adj]\NP)',
               '(S[qem]/(S[dcl]/(S[adj]NP)))/(S[adj]NP)']), !, % How ADJ
   Sem = lam(A,lam(U,app(U,lam(NP,lam(F,app(NP,lam(X,drs([],[Index:whq([mea:mis],
                                                                       merge(drs([Index:Y],[]),app(app(A,lam(P,app(P,Y))),lam(E,app(F,E)))),
                                                                       Y,
                                                                       drs([],[Index:rel(Y,X,of,0)]))])))))))).


semlex(Cat,_Sym,_Pos,Index,Sem):-
   Cat = '(S[wq]/(S[q]/PP))/(S[adj]NP)', !, % How often does...
   Sem = lam(A,lam(VP,lam(F,drs([],[Index:whq([mea:mis],
                                              merge(drs([Index:X],[]),
                                                    app(app(A,lam(P,app(P,X))),lam(E,drs([],[[]:pred(E,event,n,1)])))),
                                              X,
                                              app(app(VP,lam(Y,drs([],[Index:rel(Y,X,rel,0)]))),lam(E,app(F,E))))])))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = 'S[wq]/(S[q]/NP)',  
   ( Sym = what,  Pred = thing,        QType=[],        Sense=12;
     Sym = which, Pred = thing,        QType=[],        Sense=12;
     Sym = where, Pred = location,     QType=[loc:any], Sense=1;
     Sym = why,   Pred = reason,       QType=[des:rea], Sense=2;
     Sym = how,   Pred = manner,       QType=[des:man], Sense=2; 
     Sym = who,   Pred = person,       QType=[ins:hum], Sense=1;      
     Sym = whom,  Pred = person,       QType=[ins:hum], Sense=1;      
     Sym = when,  Pred = unit_of_time, QType=[tim:any], Sense=1 
   ), !,
   Sem = lam(VP,lam(F,drs([],[Index:whq(QType,   
                                        drs([Index:X],[Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(P,app(P,X))),lam(E,app(F,E))))]))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = 'S[wq]/(S[q]/PP)', 
   ( Sym=where, Pred=location,     Rel=loc_rel,  QType=[loc:any], Sense=1;
     Sym=why,   Pred=reason,       Rel=rel,      QType=[des:rea], Sense=2;
     Sym=how,   Pred=manner,       Rel=rel,      QType=[des:man], Sense=2;
     Sym=when,  Pred=unit_of_time, Rel=temp_rel, QType=[tim:any], Sense=1
   ), !, 
   Sem = lam(VP,lam(F,drs([],[Index:whq(QType,
                                        drs([Index:X],[Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(E,drs([],[Index:rel(E,X,Rel,0)]))),
                                            lam(E,app(F,E))))]))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['NP/(S[dcl]\NP)',
               'NP/(S[dcl]/NP)']), !,
   Sem = lam(VP,lam(P,drs([],[Index:whq([],
                                        drs([Index:X],[Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(VP,lam(R,app(R,X))),lam(E,drs([],[[]:pred(E,event,n,1)]))),app(P,X)))]))). 

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['NP/((S[to]\NP)/NP)']), !,
   Sem = lam(TV,lam(P,drs([],[Index:whq([],
                                        drs([Index:X],[Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(app(TV,lam(R,app(R,X))),lam(Q,merge(drs([[]:Z],[[]:pred(Z,thing,n,12)]),
                                                                                          app(Q,Z)))),
                                                  lam(E,drs([],[[]:pred(E,event,n,1)]))),app(P,X)))]))). 

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(NP/(S[dcl]\NP))/N','(NP/(S[dcl]/NP))/N']), !,
   Sem = lam(N,lam(VP,lam(P,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),
                                              X,
                                              merge(app(app(VP,lam(R,app(R,X))),lam(E,drs([],[[]:pred(E,event,n,1)]))),app(P,X)))])))). 

semlex('((NP\NP)/S[dcl])\((NP\NP)/NP)',_Sym,_Pos,_Index,Sem):- !,
   Sem = lam(Prep,lam(S,lam(NP,app(app(Prep,S),NP)))).

%semlex('((NP\NP)/S[dcl])\(NP/NP)',_Sym,_Pos,_Index,Sem):- !,
%   Sem = lam(M,lam(S,lam(NP,app(app(Prep,S),NP)))).

semlex('S[wq]/S[q]',Sym,_Pos,Index,Sem):- 
   ( Sym=how,   Pred=manner,       QType=[des:man], Sense=2 ;
     Sym=where, Pred=location,     QType=[loc:any], Sense=1 ;
     Sym=when,  Pred=unit_of_time, QType=[tim:any], Sense=1 ;
     Sym=why,   Pred=reason,       QType=[des:rea], Sense=2 ;
     Sym=what,  Pred=thing,        QType=[], Sense=12 
   ), !,
   Sem = lam(YNQ,lam(E,app(YNQ,lam(F,drs([],[Index:whq(QType,
                                                       drs([Index:X],[Index:pred(X,Pred,n,Sense)]),    
                                                       X,
                                                       merge(drs([],[Index:rel(F,X,rel,0)]),app(E,F)))]))))). 

semlex('S[qem]/(S[to]\NP)',_,_Pos,Index,Sem):- !,
   Sem = lam(VP,lam(E,app(app(VP,lam(P,merge(drs([Index:X],[]),app(P,X)))),lam(F,merge(drs([],[Index:pred(F,manner,n,2)]),app(E,F)))))). % how to

semlex(Cat,Sym,_Pos,Index,Sem):- 
   member(Cat,['S[wq]/(S[dcl]\NP)',  
               'S[wq]\(S[dcl]/NP)']), 
   ( Sym=what,  Pred=thing,  QType=[],        Sense=12 ;
     Sym=which, Pred=thing,  QType=[],        Sense=12 ;
     Sym=whom,  Pred=person, QType=[ins:hum], Sense=1 ;
     Sym=who,   Pred=person, QType=[ins:hum], Sense=1 
   ), !,
   Sem = lam(VP,lam(F,app(app(VP,lam(P,drs([],[Index:whq(QType,
                                                         drs([Index:X],[Index:pred(X,Pred,n,Sense)]),
                                                         X,
                                                         app(P,X))]))),lam(E,app(F,E))))).

semlex(Cat,_,_Pos,Index,Sem):- 
   member(Cat,['S[qem]/(S[dcl]\NP)',
               'S[qem]/(S[dcl]/NP)']), !,   
   Sem = lam(VP,app(VP,lam(P,drs([],[Index:whq([],
                                               drs([Index:X],[Index:pred(X,thing,n,12)]),
                                               X,
                                               app(P,X))])))).

% how
semlex('(S[qem]/S[dcl])/(S[adj]\NP)',_,_Pos,Index,Sem):- !,
   Sem = lam(VP,lam(S,app(VP,lam(P,drs([],[Index:whq([des:man],
                                                     merge(drs([Index:X],[Index:pred(X,manner,n,2)]),app(P,X)),
                                                     X,
                                                     app(S,lam(E,drs([],[Index:rel(E,X,rel,0)]))))]))))).

% how much energy was lost (??)
% how many years has GP been campaigning
semlex(Cat,_,_,Index,Sem):-
   member(Cat,['((S[qem]/(S[dcl]\NP))/N)/(S[adj]\NP)',
               '((S[qem]/(S[dcl]/NP))/N)/(S[adj]\NP)']), !,
   Sem = lam(VPADJ,lam(N,lam(VPDCL,lam(E,app(app(VPADJ,lam(P,drs([],[Index:whq([num:cou],
                                                                                merge(drs([Index:X],[]),
                                                                                      merge(app(N,X),
                                                                                            app(P,X))),
                                                                     X,
                                                                     app(app(VPDCL,lam(P,app(P,X))),E))]))),
                                                                         lam(E,drs([],[[]:pred(E,event,n,1)]))))))).

% why does he always wait
semlex('(S[X]\S[X])/S[q]',_,_,Index,Sem):- !,
   Sem = lam(W,lam(S,lam(F,app(S,lam(E,merge(drs([Index:Y],[Index:pred(Y,proposition,n,1),
                                                            Index:prop(Y,drs([],[Index:whq([des:rea],
                                                                                           drs([Index:X],[Index:pred(X,reason,n,2)]),
                                                                                           X,
                                                                                           app(W,lam(E,drs([],[Index:rel(E,X,rel,0)]))))])),
                                                            Index:rel(E,Y,rel,0)]),
                                             app(F,E))))))).



/* =========================================================================
   Verb Phrases
========================================================================= */

/* -------------------------------------------------------------------------
   Intransitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):- 
   category(iv,Cat,[Role]), !,
   Sem = lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                             [Index:pred(E,Sym,v,0),
                                              Index:rel(E,X,Role,0)]),
                                         app(P,E)))))).


/* -------------------------------------------------------------------------
    Copula
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):- 
    member(Sym,[be]),
    category(tv,Cat,_), !,
    Sem = lam(NP2,lam(NP1,lam(P,app(NP1,lam(X,app(NP2,lam(Y,merge(drs([Index:E],
                                                                      [Index:prop(E,drs([],[Index:eq(X,Y)]))]),
                                                                  app(P,E))))))))).


/* -------------------------------------------------------------------------
   NP-Transitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
    category(tv,Cat,[Role1,Role2]), !, 
    Sem = lam(NP2,lam(NP1,lam(P,app(NP1,lam(X,app(NP2,lam(Y,merge(drs([Index:E],
                                                                      [Index:pred(E,Sym,v,0),
                                                                       Index:rel(E,X,Role1,0),
                                                                       Index:rel(E,Y,Role2,0)]),
                                                                  app(P,E))))))))).


/* -------------------------------------------------------------------------
   "Adjectival verbs"
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   Sym=many, 
   category(iv,Cat,[]), !,
   Sem = lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                             [Index:prop(E,drs([Index:Y],
                                                               [Index:eq(X,Y),
                                                                Index:pred(X,quantity,n,1)]))]),
                                         app(P,E)))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(iv,Cat,[]), !,
   Sem = lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                             [Index:prop(E,drs([],[Index:pred(X,Sym,a,0)]))]),
                                         app(P,E)))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(S[adj]\NP)\NP',
               '(S[adj]\NP)/NP']), !,
   Sem = lam(NP1,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(drs([Index:E],
                                                                     [Index:prop(E,drs([],
                                                                     [Index:pred(X,Sym,a,0)])),
                                                                      Index:rel(E,Y,rel,0)]),
                                                                 app(P,E))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '(S[adj]\NP)/PP', !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                                    [Index:prop(E,drs([],
                                                                      [Index:pred(X,Sym,a,0)]))]),
                                                merge(app(PP,E),
                                                      app(P,E)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '((S[adj]\NP)/PP)/NP', !,
   Sem = lam(NP1,lam(PP,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(drs([Index:E],
                                                                            [Index:prop(E,drs([],
                                                                                              [Index:pred(X,Sym,a,0)])),
                                                                             Index:rel(E,Y,rel,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '((S[adj]\NP)\NP)/PP', !,
   Sem = lam(PP,lam(NP1,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(drs([Index:E],
                                                                            [Index:prop(E,drs([],
                                                                                              [Index:pred(X,Sym,a,0)])),
                                                                             Index:rel(E,Y,rel,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '((S[adj]\NP)/PP)/PP', !,
   Sem = lam(PP1,lam(PP2,lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                                             [Index:prop(E,drs([],
                                                                               [Index:pred(X,Sym,a,0)]))]),
                                                         merge(app(PP1,E),
                                                               merge(app(PP2,E),
                                                                     app(P,E)))))))))).


/* -------------------------------------------------------------------------
   Ditransitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(dtv,Cat,[Role1,Role2,Role3]), !,
   Sem = lam(NP3,lam(NP2,lam(NP1,lam(P,app(NP1,lam(Y,app(NP2,lam(X,app(NP3,lam(Z,merge(drs([Index:E],
                                                                                           [Index:pred(E,Sym,v,0),
                                                                                            Index:rel(E,Y,Role1,0),
                                                                                            Index:rel(E,X,Role2,0),
                                                                                            Index:rel(E,Z,Role3,0)]),
                                                                                       app(P,E)))))))))))).


/* -------------------------------------------------------------------------
   PP-Transitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(nppptv,Cat,[Role]), !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(Y,merge(drs([Index:E],
                                                    [Index:pred(E,Sym,v,0),
                                                     Index:rel(E,Y,Role,0)]),
                                                merge(app(PP,E),
                                                      app(P,E)))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   category(ppnptv,Cat,[Role]), !,
   Sem = lam(NP,lam(PP,lam(P,app(NP,lam(Y,merge(drs([Index:E],
                                                    [Index:pred(E,Sym,v,0),
                                                     Index:rel(E,Y,Role,0)]),
                                                merge(app(PP,E),
                                                      app(P,E)))))))).


/* -------------------------------------------------------------------------
   PP/NP-Transitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(npppnptv,Cat,[Role1,Role2]), !,
   Sem = lam(NP1,lam(PP,lam(NP2,lam(P,app(NP2,lam(Y,app(NP1,lam(X,merge(drs([Index:E],
                                                                            [Index:pred(E,Sym,v,0),
                                                                             Index:rel(E,Y,Role1,0),
                                                                             Index:rel(E,X,Role2,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(npnppptv,Cat,[Role1,Role2]), !,
   Sem = lam(PP,lam(NP1,lam(NP2,lam(P,app(NP2,lam(Y,app(NP1,lam(X,merge(drs([Index:E],
                                                                            [Index:pred(E,Sym,v,0),
                                                                             Index:rel(E,Y,Role1,0),
                                                                             Index:rel(E,X,Role2,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   category(ppnpnptv,Cat,[Role1,Role2]), !,
   Sem = lam(NP1,lam(NP2,lam(PP,lam(P,app(NP2,lam(Y,app(NP1,lam(X,merge(drs([Index:E],
                                                                            [Index:pred(E,Sym,v,0),
                                                                             Index:rel(E,Y,Role1,0),
                                                                             Index:rel(E,X,Role2,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).


/* -------------------------------------------------------------------------
   PP/PP-Transitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(nppppptv,Cat,[Role]), !,
   Sem = lam(PP1,lam(PP2,lam(NP,lam(P,app(NP,lam(Y,merge(drs([Index:E],
                                                             [Index:pred(E,Sym,v,0),
                                                              Index:rel(E,Y,Role,0)]),
                                                         merge(app(PP1,E),
                                                               merge(app(PP2,E),
                                                                     app(P,E)))))))))).


/* -------------------------------------------------------------------------
   NP/PP/PP-Transitive verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(npppppnptv,Cat,[Role1,Role2]), !,
   Sem = lam(NP1,lam(PP1,lam(PP2,lam(NP2,lam(P,app(NP2,lam(Y,app(NP1,lam(X,merge(drs([Index:E],
                                                                                     [Index:pred(E,Sym,v,0),
                                                                                      Index:rel(E,Y,Role1,0),
                                                                                      Index:rel(E,X,Role2,0)]),
                                                                                 merge(merge(app(PP1,E),
                                                                                             app(PP2,E)),
                                                                                       app(P,E)))))))))))).

    
/* -------------------------------------------------------------------------
   Propositional complement verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(invpcv,Cat,[Role1,Role2]), !,
   Sem = lam(Q,lam(S,lam(F,app(Q,lam(Y,merge(drs([Index:E,[]:A],[Index:pred(E,Sym,v,0),
                                                        Index:rel(E,Y,Role1,0),
                                                        Index:rel(E,A,Role2,0),
                                                        Index:pred(A,proposition,n,1),
                                                        Index:prop(A,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                             app(F,E))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '((S[dcl]\S[dcl])\NP)/NP', !,
   Sem = lam(R,lam(Q,lam(S,lam(F,app(R,lam(Z,app(Q,lam(Y,merge(drs([Index:E,[]:A],[Index:pred(E,Sym,v,0),
                                                                                  Index:rel(E,Y,agent,0),
                                                                                  Index:rel(E,Z,patient,0),
                                                                                  Index:rel(E,A,theme,0),
                                                                                  Index:pred(A,proposition,n,1),
                                                                                  Index:prop(A,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                             app(F,E)))))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   category(pcv,Cat,[Role1,Role2]), !,
   Sem = lam(S,lam(Q,lam(F,app(Q,lam(Y,merge(drs([Index:E,[]:A],[Index:pred(E,Sym,v,0),
                                                        Index:rel(E,Y,Role1,0),
                                                        Index:rel(E,A,Role2,0),
                                                        Index:pred(A,proposition,n,1),
                                                        Index:prop(A,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                             app(F,E))))))).


/* -------------------------------------------------------------------------
   Ditransitive Propositional complement verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(dpcv,Cat,[Role1,Role2,Role3]), !,
   Sem = lam(NP2,lam(S,lam(NP1,lam(F,app(NP1,lam(X,app(NP2,lam(Y,
             merge(drs([Index:E,[]:A],[Index:pred(E,Sym,v,0),
                              Index:rel(E,Y,Role2,0),
                              Index:rel(E,X,Role1,0),
                              Index:rel(E,A,Role3,0),
                              Index:pred(A,proposition,n,1),
                              Index:prop(A,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                   app(F,E)))))))))).


/* -------------------------------------------------------------------------
   Other Propositional complement verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[dcl]\NP)/NP)/(S[dcl]\NP)',
               '((S[dcl]\NP)/NP)/(S[adj]\NP)',
               '((S[ng]\NP)/NP)/(S[adj]\NP)',
               '((S[b]\NP)/NP)/(S[adj]\NP)']), !,
   Sem = lam(VP,lam(NP2,lam(NP1,lam(F,app(NP1,lam(X,merge(drs([Index:E,[]:Z],
                                                              [Index:pred(E,Sym,v,0),
                                                               Index:rel(E,X,agent,0),
                                                               Index:rel(E,Z,theme,0),
                                                               Index:pred(Z,proposition,n,1),
                                                               Index:prop(Z,app(app(VP,NP2),lam(G,drs([],[[]:pred(G,event,n,1)]))))]),
                                                          app(F,E)))))))). 


/* -------------------------------------------------------------------------
   Subject Control Verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):- 
   category(scv,Cat,[Role1,Role2]), !,
   Sem = lam(VP,lam(NP,lam(F,app(NP,lam(X,merge(drs([Index:E,[]:B],
                                                    [Index:pred(E,Sym,v,0),
                                                     Index:rel(E,X,Role1,0),
                                                     Index:rel(E,B,Role2,0),
                                                     Index:pred(B,proposition,n,1),
                                                     Index:prop(B,app(app(VP,lam(P,app(P,X))),lam(D,drs([],[[]:pred(D,event,n,1)]))))]),
                                                app(F,E))))))).


/* -------------------------------------------------------------------------
   Object Control Verbs
------------------------------------------------------------------------- */
                    
semlex(Cat,Sym,_Pos,Index,Sem):-
   category(ocv,Cat,[Role1,Role2]), !,
   Sem = lam(Q2,lam(V,lam(Q1,lam(F,app(Q1,lam(X,app(Q2,lam(Y,merge(drs([Index:E,[]:A],
                                                                       [Index:pred(E,Sym,v,0),
                                                                        Index:rel(E,Y,Role2,0),
                                                                        Index:rel(E,X,Role1,0),
                                                                        Index:pred(A,proposition,n,1),
                                                                        Index:prop(A,app(app(V,lam(R,app(R,Y))),lam(E,drs([],[[]:pred(E,event,n,1)]))))]),app(F,E)))))))))).


/* -------------------------------------------------------------------------
   Object Control Verbs subcat PP
------------------------------------------------------------------------- */
                    
semlex(Cat,Sym,_Pos,Index,Sem):-
   category(pscv,Cat,[Role1,Role2]), !,
   Sem = lam(PP,lam(V,lam(Q1,lam(F,app(Q1,lam(X,merge(drs([Index:E,[]:A],
                                                          [Index:pred(E,Sym,v,0),
                                                           Index:rel(E,X,Role1,0),
                                                           Index:rel(E,A,Role2,0),
                                                           Index:pred(A,proposition,n,1),
                                                           Index:prop(A,app(app(V,lam(R,app(R,X))),lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                                      merge(app(PP,E),app(F,E))))))))).


/* -------------------------------------------------------------------------
   Auxiliary Verbs
------------------------------------------------------------------------- */

/*

Commented out these 'result' analyses for now. Don't remember why they
were introduced, possibly for constructions like "come true".

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(av,Cat,pss), 
   \+ member(Sym,[be,become]), !,
   Sem = lam(ADJ,lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                                     [Index:pred(E,Sym,v,0),
                                                      Index:rel(E,X,patient,0)]),
                                                 merge(app(app(ADJ,lam(R,app(R,X))),lam(F,drs([],[Index:rel(E,F,result,0),
                                                                                                  []:pred(F,event,n,1)]))), 
                                                       app(P,E)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(av,Cat,_), 
   \+ member(Sym,[be,become]), !,
   Sem = lam(ADJ,lam(NP,lam(P,app(NP,lam(X,merge(drs([Index:E],
                                                     [Index:pred(E,Sym,v,0),
                                                      Index:rel(E,X,agent,0)]),
                                                 merge(app(app(ADJ,lam(R,app(R,X))),lam(F,drs([],[Index:rel(E,F,result,0),
                                                                                                  []:pred(F,event,n,1)]))), 
                                                       app(P,E)))))))).
*/

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   category(av,Cat,_), !,
	Sem = lam(U,U).


/* -------------------------------------------------------------------------
   Modal Verbs
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   category(mv,Cat,_), !,
   Sem = lam(NP,lam(VP,app(VP,NP))).


/* -------------------------------------------------------------------------
   Transitive Modal Verbs (ignoring object NP)
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['((S[dcl]\NP)\NP)/(S[b]\NP)']), !,
   Sem = lam(VP,lam(_,VP)).


% these need to be improved!
semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['((S[b]\NP)/S[for])/(S[adj]\NP)',
               '((S[dcl]\NP[expl])/S[for])/(S[adj]\NP)',      
               '((S[dcl]\NP[expl])/S[to])/(S[adj]\NP)',
               '((S[dcl]\NP[expl])/S[em])/(S[adj]\NP)',
               '((S[dcl]\NP[expl])/S[qem])/(S[adj]\NP)']), !,
   Sem = lam(A,lam(S,lam(NP,lam(E,merge(app(app(A,NP),E),app(S,lam(F,drs([],[[]:pred(F,event,n,1)])))))))).

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['((S[dcl]\NP[expl])/S[em])/PP']), !,
   Sem = lam(PP,lam(S,lam(_,lam(F,app(S,lam(E,merge(app(PP,E),app(F,E)))))))). 


% "to make it easier for S"
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[b]\NP)/S[for])/(S[adj]\NP))/NP[expl]',
               '(((S[dcl]\NP)/S[em])/(S[adj]\NP))/NP[expl]']), !,
   Sem = lam(NPE,lam(VP,lam(S,lam(NP,lam(F,
                      app(NP,lam(X,merge(drs([Index:E,[]:B],
                                             [Index:pred(E,Sym,v,0),
                                              Index:rel(E,X,agent,0),
                                              Index:rel(E,B,theme,0),
                                              Index:pred(B,proposition,n,1),
                                              Index:prop(B,app(app(VP,NPE),lam(G,app(S,lam(D,drs([],[[]:pred(D,event,n,1),
                                                                                                     []:pred(G,event,n,1),
                                                                                                     Index:rel(G,D,for,0)]))))))]),
                                         app(F,E))))))))).


% yes-no question 'do' (e.g. Does this corporation have a high-quality management team with a good track record ?)

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['(S[q]/(S[b]\NP))/NP',
               '(S[q]/(S[ng]\NP))/NP',
               '(S[q]/(S[pss]\NP))/NP',         % NEW Aug 2, 2004
               '(S[q]/(S[adj]\NP))/NP']), !,
   Sem = lam(NP,lam(VP,app(VP,NP))).


semlex('((S[q]/PP)/(S[adj]\NP))/NP',_Sym,_Pos,_Index,Sem):- !,
   Sem = lam(NP,lam(VP,lam(PP,lam(F,app(app(VP,NP),lam(E,merge(app(PP,E),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Even weirder verbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[dcl]\NP)/PP)/(S[adj]\NP)',
                   '((S[b]\NP)/PP)/(S[adj]\NP)',
                   '((S[ng]\NP)/PP)/(S[adj]\NP)',
                   '((S[pss]\NP)/PP)/(S[adj]\NP)']), !,
   Sem = lam(VP,lam(PP,lam(NP,lam(F,app(NP,lam(X,merge(drs([Index:E],
                                                           [Index:pred(E,Sym,v,0),
                                                            Index:rel(E,X,agent,0)]),
                                                       merge(app(PP,E),
                                                             merge(app(app(VP,NP),F),
                                                                   app(F,E)))))))))).


semlex('(S[pss]\NP)/(PP/NP)',Sym,_Pos,Index,Sem):-
   Sem = lam(PP,lam(NP,lam(F,app(NP,lam(X,merge(drs([Index:E],
                                                    [Index:pred(E,Sym,v,0),
                                                     Index:rel(E,X,patient,0)]),
                                                merge(app(app(PP,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),E),
                                                      app(F,E)))))))).


/* -------------------------------------------------------------------------
   Copula
------------------------------------------------------------------------- */

semlex(Cat,_,_Pos,_Index,Sem):- 
   member(Cat,['(S[dcl]\(S[adj]\NP))/NP',
               '(S[dcl]\(S[ng]NP))/NP',
               '(S[dcl]\(S[b]NP))/NP',
               '(S[dcl]\(S[pss]\NP))/NP']), !,
   Sem = lam(NP,lam(VP,app(VP,NP))).

semlex('(S[q]/(S[pss]\NP))/NP',_,_Pos,_Index,Sem):- !,
   Sem = lam(NP,lam(VP,app(VP,NP))).

semlex('((S[dcl]\NP[expl])/(NP\NP))/NP',_,_Pos,_Index,Sem):- !,
   Sem = lam(NP,lam(M,lam(_Expl,lam(F,app(app(M,NP),lam(X,merge(drs([[]:E],[[]:rel(E,X,rel,0)]),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Copula  "it is ADJ to-VP"
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['((S[dcl]\NP[expl])/(S[to]\NP))/(S[adj]\NP)']), !,
   Sem = lam(A,lam(VP,lam(NP,lam(E,app(NP,lam(X,merge(app(app(A,lam(P,app(P,X))),E),app(app(VP,lam(P,app(P,X))),lam(F,drs([],[Index:pred(F,event,n,1)])))))))))).


/* -------------------------------------------------------------------------
   Copula modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '((S[adj]\NP)/S[em])/(S[adj]\NP)',  % .. are so similar to themselves that ...
   Sem = lam(VP,lam(S,lam(Q,lam(F,app(app(VP,Q),lam(E,merge(drs([[]:A],[Index:rel(E,A,Sym,0),
                                                                       Index:pred(A,proposition,n,1),
                                                                       Index:prop(A,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),app(F,E)))))))).


/* =========================================================================
   Adjectives
========================================================================= */

/* -------------------------------------------------------------------------
   Wrongly Classified Adjectives + "own"
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,_Index,Sem):-
   member(Sym,[own,most,least,few,several,many]),
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,app(P,X))).

semlex(Cat,many,_Pos,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,quantity,n,1)]),app(P,X)))).

semlex(Cat,much,_Pos,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,amount,n,3)]),app(P,X)))).


/* -------------------------------------------------------------------------
   Presuppositional Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Sym,['other','previous']),
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,alfa(def,
                          merge(drs([[]:Y],[]),app(P,Y)),
                          merge(drs([],[Index:not(drs([],[[]:eq(X,Y)]))]),
                                app(P,X))))).


/* -------------------------------------------------------------------------
   Noun Noun Compounds
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Pos,['NN','NNS']),
   category(adj,Cat,_), !,
   symbol(Sym,NormSym),
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,NormSym,n,0)]),
                           app(P,X)))).

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   category(adj,Cat,_), !,
   symbol(Sym,NormSym),
   Sem = lam(P,lam(X,merge(drs([],[Index:named(X,NormSym,nam,0)]),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Singular Superlatives
------------------------------------------------------------------------- */

semlex(Cat,Sym,'JJS',Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(app(P,X),drs([],[[-1|Index]:imp(merge(drs([Index:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),app(P,Y)),
                                                      drs([],[Index:rel(X,Y,Sym,0)]))])))).


/* -------------------------------------------------------------------------
   Cardinal Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,'CD',Index,Sem):-
   category(adj,Cat,_), 
   string2digit(Sym,Digit), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:card(X,Digit,ge)]),app(P,X)))).


/* -------------------------------------------------------------------------
   Composite Adjectives:  Scottish-born, Chicago-based, 10-year-old
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   category(adj,Cat,_), 
   decompose(Sym,Prefix,Suffix),
   (
       Suffix=born,
       ( nationality(Prefix,Loc), !
       ; nationality(_,Prefix), Loc=Prefix ), 
       !,
       Sem = lam(P,lam(X,merge(drs([Index:E,Index:Y],
                               [Index:pred(E,bear,v,2),
                                Index:rel(E,X,patient,0),
                                Index:rel(E,Y,in,0),
                                Index:named(Y,Loc,loc,0)]),app(P,X))))
  ;
       Suffix=based, !,
       symbol(Prefix,LocSym), 
       Sem = lam(P,lam(X,merge(drs([Index:E,Index:Y],
                               [Index:pred(E,base,v,2),
                                Index:rel(E,X,patient,0),
                                Index:rel(E,Y,in,0),
                                Index:named(Y,LocSym,loc,0)]),app(P,X))))
  ;
       ( Suffix='year-old',  string2digit(Prefix,Number), !, Unit=year, Sense=1
       ; Suffix='month-old', string2digit(Prefix,Number), !, Unit=month, Sense=2 ), 
       !,
       Sem = lam(P,lam(X,merge(drs([Index:Y],
                               [Index:card(Y,Number,ge),
                                Index:pred(Y,Unit,n,Sense),
                                Index:rel(Y,X,of,0),
                                Index:pred(Y,age,n,1)]),app(P,X))))
  ;
       member(Suffix,[related,like]), !,
       symbol(Prefix,ThingSym), 
       Sem = lam(P,lam(X,merge(drs([Index:Y],
                               [Index:pred(Y,ThingSym,n,0),
                                Index:rel(Y,X,rel,0)]),app(P,X))))
  ;

       member(Suffix,[acre,year,yard,foot,pound,day,minute,page,point,man,inch,
	              degree,week,member,mile,week,km,dollar,kilometer,
                      'square-foot',seat,meter,story,hour,time,ton,month]),
       string2digit(Prefix,Number), !, 
       Sem = lam(P,lam(X,merge(drs([Index:Y],
                               [Index:card(Y,Number,ge),
                                Index:pred(Y,Suffix,n,0),
                                Index:rel(Y,X,nn,0)]),app(P,X))))
  ), !.



    

/* -------------------------------------------------------------------------
   Singular Intersective Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   category(adj,Cat,_), !,
   symbol(Sym,NormSym),
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,NormSym,a,0)]),app(P,X)))).

semlex(Cat,Sym,_,Index,Sem):-
   category(adjnum,Cat,_), !,
   symbol(Sym,NormSym),
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,NormSym,a,0)]),app(P,X)))).


/* -------------------------------------------------------------------------
   Adjectives introducing a degree
------------------------------------------------------------------------- */

semlex('D/N',Sym,_,Index,Sem):- !,
   symbol(Sym,NormSym),
%%   Sem = lam(P,lam(X,lam(D,merge(drs([],[Index:pred(D,degree,n,1),Index:rel(X,D,NormSym,0)]),app(P,X))))).
   Sem = lam(Y,lam(X,drs([],[Index:rel(X,Y,NormSym,0)]))).


/* =========================================================================
   Other Modifiers
========================================================================= */

/* -------------------------------------------------------------------------
   Comparative (more than, at least) -- can be improved...
------------------------------------------------------------------------- */

% more than
semlex(Cat,_,_Pos,_Index,Sem):- 
   member(Cat,['(N/N)\(S[adj]\NP)',
               '(N/N)/(S[adj]\NP)',
               '(N/N)\(S[asup]\NP)',
               '(N/N)/(S[asup]\NP)']), !,
   Sem = lam(VP,lam(N,lam(X,app(app(VP,lam(P,merge(app(P,X),app(N,X)))),lam(E,drs([],[[]:pred(E,event,n,1)])))))).

% more than
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(NP/NP)\(S[adj]\NP)',
               '(NP/NP)/(S[adj]\NP)']), !,
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(drs([],[Index:pred(X,Sym,a,0)]),app(P,X))))))).

% more than
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((NP/NP)/(NP/NP))\(S[adj]\NP)',
               '((NP\NP)\(NP\NP))/(S[dcl]\NP)']), !, 
   Sem = lam(VP,lam(NPNP,lam(NP,lam(P,app(app(VP,app(NPNP,NP)),lam(X,merge(drs([],[Index:pred(X,Sym,a,0)]),app(P,X)))))))).

% more than (provisional)
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))))\(S[adj]\NP)',  
               '((((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))))/(S[asup]\NP)']), !,
   Sem = lam(_VP,lam(_MM,lam(_M,lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([],[Index:pred(E,Sym,v,0)]),app(F,E)))))))))).      

% at least
semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '(NP/NP)/(S[asup]\NP)', !,
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(drs([],[Index:pred(X,Sym,a,0)]),app(P,X))))))).


/* -------------------------------------------------------------------------
   Superlatives: (the) most/least ... 
------------------------------------------------------------------------- */

semlex(Cat,most,_,Index,Sem):-  
   member(Cat,['(N/N)/(D/N)']), !,
   Sem = lam(R,lam(P,lam(X,merge(app(P,X),
                                 drs([],[[-1|Index]:imp(merge(drs([[]:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),app(P,Y)),
                                                        app(app(R,Y),X))]))))).

semlex(Cat,least,_,Index,Sem):-  
   member(Cat,['(N/N)/(D/N)']), !,
   Sem = lam(R,lam(P,lam(X,merge(app(P,X),
                                 drs([],[[-1|Index]:imp(merge(drs([[]:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),app(P,Y)),
                                                        app(app(R,X),Y))]))))).



/* -------------------------------------------------------------------------
   Intensifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-  
   member(Cat,['(N/N)/(N/N)',
               '(N\N)/(N\N)',
               '(N/N)\(N/N)']), 
   string2digit(Sym,Digit), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:card(X,Digit,ge)]),app(P,X)))),Y)))).

semlex(Cat,Sym,'JJS',Index,Sem):-        
   member(Cat,['(N/N)/(N/N)',        %%%% Example: ... fastest growing segment
               '(N/N)\(N/N)']), !,   %%%% Example: ... third largest bank (incorrect semantics!)
   symbol(Sym,NormSym),
   Sem = lam(Z,lam(P,lam(X,merge(app(app(Z,P),X),
                                 drs([],[[-1|Index]:imp(merge(drs([Index:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),
                                                              app(app(Z,P),Y)),
                                                        drs([],[Index:rel(X,Y,NormSym,0)]))]))))).

semlex(Cat,Sym,_,Index,Sem):-  
   member(Cat,['(N/N)/(N/N)',
               '(N\N)/(N\N)',
               '(N/N)\(N/N)']), !,
   symbol(Sym,NormSym),
%   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:pred(X,NormSym,a,0)]),app(P,X)))),Y)))).
   Sem = lam(Z,lam(P,lam(X,merge(app(app(Z,P),X),drs([],[Index:pred(X,NormSym,a,0)]))))).

semlex(Cat,_Sym,_Pos,_Index,Sem):-  
   member(Cat,['((N/N)/(N/N))/((N/N)/(N/N))']), !,
   Sem = lam(I,lam(Z,lam(P,lam(Y,app(app(app(I,Z),P),Y))))).  % place-holder only!

semlex(Cat,Sym,_Pos,Index,Sem):-  
   member(Cat,['((N/N)/(N/N))\(S[adj]\NP)',
               '((N/N)/(N/N))/(S[asup]\NP)']), !,
   Sem = lam(Q,lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:pred(X,Sym,a,0)]),
                                                       merge(app(app(Q,lam(P,app(P,X))),lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                                             app(P,X))))),Y))))).  


/* -------------------------------------------------------------------------
   Range constructions (e.g., "10 to 20") et al.
------------------------------------------------------------------------- */

semlex('(N\N)/N',_,_Pos,Index,Sem):- !,
   Sem = lam(Q,lam(P,lam(X,drs([],[Index:or(app(P,X),app(Q,X))])))).

semlex('(N/N)/N',_,_Pos,_Index,Sem):- !,
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),app(Q,X))))).

semlex('(N\N)/N[num]',_,_Pos,Index,Sem):- !,
   Sem = lam(Q,lam(P,lam(X,drs([],[Index:or(app(P,X),app(Q,X))])))).

semlex('(N/N)/N[num]',_,_Pos,_Index,Sem):- !,
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),app(Q,X))))).

semlex('((N/N)\(N/N))/(N/N)',_,_Pos,Index,Sem):- !,
   Sem = lam(M2,lam(M1,lam(P,lam(X,drs([],[Index:or(app(app(M1,P),X),app(app(M2,P),X))]))))).


/* -------------------------------------------------------------------------
   Complementizers
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['S[poss]/S[dcl]',
               'S[qem]/S[dcl]',
               'S[dcl]/S[dcl]',
               'S[dcl]/S[inv]',
               'S[bem]/S[b]',
               'S[em]/S[dcl]',
               'S[em]/S[b]',
               'S[adj]/S[adj]']), !,
   Sem = lam(U,U).

semlex(Cat,_,_,Index,Sem):-
   Cat = 'NP/S[dcl]',
   Sem = lam(S,lam(P,merge(drs([Index:X],[Index:pred(X,proposition,n,1),
                                          Index:prop(X,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),app(P,X)))).


/* -------------------------------------------------------------------------
   Locative Adverbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[somewhere]), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([Index:Z],[Index:pred(Z,location,n,1),
                                                                   Index:rel(E,Z,loc_rel,0)]),app(F,E))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[anywhere,everywhere]), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,drs([],[Index:imp(drs([Index:Z],[Index:pred(Z,location,n,1)]),
                                                                merge(drs([],[Index:rel(E,Z,loc_rel,0)]),app(F,E)))])))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[nowhere]), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,drs([],[Index:imp(drs([Index:Z],[Index:pred(Z,location,n,1)]),
                                                                drs([],[Index:not(merge(drs([],[Index:rel(E,Z,loc_rel,0)]),app(F,E)))]))])))))).


/* -------------------------------------------------------------------------
   Not 
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,['not','n\'t']), !,
   Sem = lam(X,lam(Q,lam(F,drs([],[Index:not(app(app(X,Q),F))])))).


/* -------------------------------------------------------------------------
   Cardinals that function as VP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   category(vpadv,Cat,_),
   member(Pos,['CD']), 
   string2digit(Sym,Digit), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([Index:X],[Index:card(X,Digit,ge),
                                                                   Index:rel(E,X,rel,0)]),app(F,E))))))).

/* -------------------------------------------------------------------------
   NPs that function as VP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   category(vpadv,Cat,_),
   member(Pos,['NN','NNP','NNS','NNPS']), !,
   symbol(Sym,NormSym),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([Index:X],[Index:pred(X,NormSym,n,0),
                                                                   Index:rel(E,X,rel,0)]),app(F,E))))))).

/* -------------------------------------------------------------------------
   Comparative (more)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   Sym = more,
   Cat = '(S[adj]\NP)/(S[adj]\NP)', !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(D1,merge(drs([Index:D2],[Index:rel(D1,D2,more,0)]),app(F,D1))))))).


/* -------------------------------------------------------------------------
   Adverbs (VP modifying)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(vpadv,Cat,_), !,
   symbol(Sym,NormSym),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([],[Index:pred(E,NormSym,a,0)]),app(F,E))))))).





%%% WHAT IS THIS DOING HERE???
semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '(S[pt]\NP)/S[em]', !,
   symbol(Sym,NormSym),
   Sem = lam(S,lam(Q,lam(F,app(Q,lam(Y,merge(drs([Index:E,[]:A],[Index:pred(E,NormSym,v,0),
                                                                 Index:rel(E,Y,agent,0),
                                                                 Index:rel(E,A,theme,0),
                                                                 Index:pred(A,proposition,n,1),
                                                                 Index:prop(A,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                              app(F,E))))))).

/* -------------------------------------------------------------------------
   "hard to take"
------------------------------------------------------------------------- */

semlex('(S[adj]\NP)/((S[to]\NP)/NP)',Sym,_Pos,Index,Sem):-
   Sem = lam(TV,lam(Q,lam(F,app(app(app(TV,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),Q),lam(E,merge(drs([],[Index:pred(E,Sym,v,0)]),app(F,E))))))).


/* -------------------------------------------------------------------------
   Prepositions
------------------------------------------------------------------------- */

semlex('PP',Sym,_Pos,Index,Sem):- !,
   Sem = lam(X,drs([Index:Y],[Index:pred(Y,thing,n,12),
                              Index:rel(X,Y,Sym,0)])).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(N\N)/NP','(N/N)/NP','(N/N)\NP','(N\N)\NP']), !,
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((N/N)\(N/N))/NP']), !,
   Sem = lam(Q,lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))),
                                                       app(P,X)))),Y))))).  

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((NP\NP)\(NP\NP))/NP',
               '((NP\NP)/(NP\NP))/NP']), !,
   Sem = lam(Q1,lam(R,lam(Q2,lam(P,merge(app(Q1,lam(X,app(Q2,lam(Y,drs([],[Index:rel(Y,X,Sym,0)]))))),app(app(R,Q2),P)))))).

% seven cents a share
semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(NP\NP)/N',
               '(NP\NP)/N[num]']), !,
   Sem = lam(N,lam(Q,lam(P,drs([],[Index:imp(merge(drs([Index:Y],[]),app(N,Y)),app(Q,lam(X,merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))))])))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(NP/NP)/N',
               '(NP/NP)/N[num]']), !,
   Sem = lam(N,lam(Q,lam(P,merge(merge(drs([Index:Y],[]),app(N,Y)),app(Q,lam(X,merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '(NP\NP)/PP', !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(drs([],[Index:pred(X,Sym,n,0)]),merge(app(PP,X),app(P,X)))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(NP\NP)/NP','(NP\NP)\NP','(NP/NP)/NP']),
   member(Sym,['-lrb-','-lcb-','-lsb-','(','[']), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(NP\NP)/NP','(NP\NP)\NP','(NP/NP)/NP']), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[Index:rel(X,Y,Sym,0)]),app(P,X))))))))).



semlex(Cat,Sym,_Pos,Index,Sem):- 
   member(Cat,['((NP\NP)/(S[to]\NP))/NP',
               '((NP\NP)/(S[ng]\NP))/NP']), !,
   Sem = lam(NP1,lam(VP,lam(NP2,lam(P,app(NP2,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,Sym,0),
                                                                         Index:pred(Y,proposition,n,1),
                                                                         Index:prop(Y,app(app(VP,NP1),lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                                          app(P,X)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):- 
   member(Cat,['((NP\NP)/PP)/NP']), !,
   Sem = lam(Q1,lam(PP,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[Index:rel(X,Y,Sym,0)]),
                                                                    merge(app(PP,X),app(P,X))))))))))).

semlex('(N/PP)/(S[adj]\NP)',Sym,_Pos,Index,Sem):-
   Sem = lam(VP,lam(PP,lam(X,app(app(VP,lam(P,app(P,X))),lam(E,merge(drs([],[Index:rel(X,E,Sym,0)]),app(PP,E))))))).

semlex('((S[wq]/S[q])\(S[wq]/S[q]))/NP',Sym,_Pos,Index,Sem):- !,
   Sem = lam(NP,lam(U,lam(YNQ,lam(F,app(app(U,YNQ),lam(E,merge(app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),app(F,E)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[adj]\NP)\(S[adj]\NP))/NP', % than
               '((S[adj]\NP)/(S[adj]\NP))/NP', 
               '((S[X]\NP)\(S[X]\NP))/NP', 
               '((S[X]\NP)/(S[X]\NP))/NP', 
               '((S[X]\NP)\(S[X]\NP))\NP', 
               '((S[X]\NP)/(S[X]\NP))\NP', 
               '((S\NP)\(S\NP))/NP',       
               '((S\NP)/(S\NP))/NP',       
               '((S\NP)/(S\NP))\NP']), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).
                    
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))\NP)/NP']), !,
   Sem = lam(Q3,lam(Q2,lam(V,lam(Q1,lam(F,app(app(V,Q1),lam(E,app(Q3,lam(X,app(Q2,lam(Y,merge(drs([],[Index:rel(X,Y,Sym,0),
                                                                                                      Index:rel(E,Y,tloc,0)]),app(F,E))))))))))))).
                    
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/NP',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/NP',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))\NP']), !,
   Sem = lam(Q,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(Q,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/N',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/N',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))\N',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/PP']), !,
   Sem = lam(N,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(merge(drs([Index:X],[Index:rel(E,X,rel,0)]),
                                                                                   app(N,X)),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/S[dcl]',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/S[dcl]',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))\S[dcl]']), !,
   Sem = lam(S,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(S,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/(S[asup]\NP)',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/(S[pss]\NP)',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/(S[adj]\NP)']), !,
   Sem = lam(VP1,lam(AV,lam(VP2,lam(NP,lam(F,app(app(app(AV,VP2),NP),lam(E,merge(app(app(VP1,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),
                                                                                                         app(P,X)))),lam(X,drs([],[Index:rel(E,X,Sym,0)]))),
                                                                                 app(F,E))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = 'PP/NP', !,
   Sem = lam(Q,lam(X,app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '(PP\NP)/NP', !,
   Sem = lam(Q1,lam(Q2,lam(X,merge(app(Q2,lam(Z,drs([],[Index:rel(X,Z,rel,0)]))),
                                   app(Q1,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['PP/(S[ng]\NP)','PP/(S[adj]\NP)','PP/(S[b]\NP)']), !,
   Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(drs([Index:Y],[Index:rel(X,Y,Sym,0)]),app(P,Y)))),lam(E,drs([],[[]:pred(E,event,n,1)]))))).


%%% suggested by Bastian Laubner
semlex(Cat,if,_Pos,Index,Sem):-
  member(Cat,['((S[X]\NP)\(S[X]\NP))/S[dcl]']), !,
  Sem = lam(S,lam(V,lam(Q,lam(F,merge(drs([Index:E],
                                          [Index:imp(app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                                     app(app(V,Q),lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                      app(F,E)))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/S[dcl]',
               '((S[X]\NP)\(S[X]\NP))/S',  
               '((S[X]\NP)/(S[X]\NP))/S[dcl]',
               '((S[X]\NP)\(S[X]\NP))/S[qem]',
               '((S[X]\NP)\(S[X]\NP))/S[ng]',
               '((S[X]\NP)\(S[X]\NP))/S[poss]',
               '((S[X]\NP)\(S[X]\NP))/S[em]',
               '((S[adj]\NP)\(S[adj]\NP))/S[dcl]',
               '((S[X]\NP)\(S[X]\NP))/S[inv]']), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Z],
                                                              [Index:pred(Z,proposition,n,1),
                                                               Index:rel(E,Z,Sym,0),
                                                               Index:prop(Z,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                                          app(F,E)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))\NP)/S[dcl]' ]), !,
   Sem = lam(S,lam(NP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Z],[Index:pred(Z,proposition,n,1),
                                                                      Index:rel(E,Z,Sym,0),
                                                                      Index:prop(Z,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                                                 merge(app(NP,lam(X,drs([],[[]:rel(E,X,rel,0)]))),
                                                                       app(F,E)))))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(NP\NP)/S[qem]',
               '(NP\NP)/S[inv]',
               '(NP\NP)/S',
               '(NP\NP)/S[dcl]']), !,
   Sem = lam(S,lam(Q,lam(P,app(Q,lam(X,merge(drs([Index:Z],[Index:pred(Z,proposition,n,1),
                                                      Index:rel(X,Z,Sym,0),
                                                      Index:prop(Z,app(S,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                             app(P,X))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((NP\NP)\(NP\NP))/S[dcl]']), !, 
   Sem = lam(S,lam(M,lam(NP,lam(Q,app(app(M,NP),
                                      lam(X,merge(app(S,lam(E,drs([],[[]:pred(E,event,n,1),
                                                                      Index:rel(E,X,Sym,0)]))),
                                                  app(Q,X)))))))).

%% need to check this: discourse referent is introduced without properties
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/(S[ng]\NP)',
                   '((S[X]\NP)\(S[X]\NP))/(S[adj]\NP)', 
                   '((S[X]\NP)\(S[X]\NP))/(S[dcl]\NP)', % which
                   '((S[X]\NP)\(S[X]\NP))/(S[dcl]/NP)', % who
                   '((S[X]\NP)\(S[X]\NP))/(S[asup]\NP)', 
                   '((S[X]\NP)\(S[X]\NP))/(S[pss]\NP)', 
                   '((S[X]\NP)\(S[X]\NP))/(S[pt]\NP)', 
                   '((S[X]\NP)\(S[X]\NP))/(S[b]\NP)',
                   '((S[X]\NP)\(S[X]\NP))/(S[to]\NP)', 

                   '((S[X]\NP)/(S[X]\NP))/(S[ng]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[adj]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[dcl]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[asup]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[pss]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[pt]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[b]\NP)', 
                   '((S[X]\NP)/(S[X]\NP))/(S[to]\NP)', 

                   '((S[X]\NP)/(S[X]\NP))\(S[adj]\NP)', 

                   '((S[adj]\NP)\(S[adj]\NP))/(S[pss]\NP)', 
                   '((S[adj]\NP)\(S[adj]\NP))/(S[adj]\NP)', 
                   '((S[adj]\NP)/(S[adj]\NP))/(S[adj]\NP)', 
                   '((S[adj]\NP)\(S[adj]\NP))/(S[pss]\NP)', 
                   '((S[adj]\NP)\(S[adj]\NP))/(S[to]\NP)', 
                   '((S[adj]\NP)/(S[to]\NP))/(S[adj]\NP)', 
                   '((S[adj]\NP)\(S[adj]\NP))/(S[ng]\NP)', 
                   '((S[adj]\NP)\(S[adj]\NP))/(S[dcl]\NP)', 
                   '((S[adj]\NP)/(S[adj]\NP))/(S[asup]\NP)', 
                   '((S[adj]\NP)/(S[for]\NP))/(S[adj]\NP)', 

                   '((S[b]\NP)/(S[to]\NP))/(S[adj]\NP)', 
                   '((S[b]\NP)/(S[ng]\NP))/(S[adj]\NP)', 
                   '((S[b]\NP)/(S[dcl]\NP))/(S[adj]\NP)', 
                   '((S[pt]\NP)/(S[to]\NP))/(S[adj]\NP)', 
                   '((S[pt]\NP)/(S[ng]\NP))/(S[adj]\NP)', 
                   '((S[dcl]\NP)/(S[b]\NP))/(S[adj]\NP)', 
                   '((S[dcl]\NP)/(S[to]\NP))/(S[adj]\NP)', 
                   '((S[dcl]\NP)/(S[adj]\NP))/(S[adj]\NP)']), !,
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(app(VP,lam(P,merge(drs([Index:X],[]),app(P,X)))),lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[dcl]\NP)/(S[to]\NP))/(S[adj]\NP))/NP[expl]',   %%% placeholder only!
               '(((S[b]\NP)/(S[to]\NP))/(S[adj]\NP))/NP[expl]',     %%% placeholder only!
                   '(((S[X]\NP)\(S[X]\NP))/(S[ng]\NP))/NP',
                   '(((S[X]\NP)\(S[X]\NP))/(S[pt]\NP))/NP',
                   '(((S[X]\NP)\(S[X]\NP))/(S[pss]\NP))/NP',
                   '(((S[X]\NP)\(S[X]\NP))/(S[b]\NP))/NP',
                   '(((S[X]\NP)\(S[X]\NP))/(S[to]\NP))/NP']), !,
   Sem =  lam(NP,lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(app(VP,lam(P,app(NP,lam(X,merge(drs([],[[]:pred(E,event,n,1),Index:rel(E,X,Sym,0)]),
                                                                                                 app(P,X)))))),F)))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/(S[ng]\NP))/N',
                   '(((S[X]\NP)\(S[X]\NP))/(S[pt]\NP))/N',
                   '(((S[X]\NP)\(S[X]\NP))/(S[pss]\NP))/N',
                   '(((S[X]\NP)\(S[X]\NP))/(S[b]\NP))/N',
                   '(((S[X]\NP)\(S[X]\NP))/(S[to]\NP))/N']), !,
   Sem =  lam(N,lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(app(VP,lam(P,merge(drs([Index:X],[[]:pred(E,event,n,1),Index:rel(E,X,Sym,0)]),
                                                                                   merge(app(N,X),app(P,X))))),F)))))))).



semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '(((S[X]\NP)\(S[X]\NP))/PP)/NP', !,
   Sem =  lam(NP,lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),
                                                                   merge(app(PP,E),app(F,E)))))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(S[wq]\S[wq])/NP',
               '(S[X]/S[X])\NP',
               '(S[X]/S[X])/NP',
               '(S[X]\S[X])/NP']), !,   
   Sem = lam(Q,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),app(F,E))))))).


semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(S[X]/S[X])/N','(S[X]\S[X])/N']), !,
   Sem = lam(P,lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:rel(E,X,rel,0)]),merge(app(P,X),app(F,E)))))))).


% With violence escalating in Kosovo, S
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]/S[X])/(S[ng]\NP))/NP',
               '((S[X]/S[X])/(S[pt]\NP))/NP',
               '((S[X]/S[X])/(S[b]\NP))/NP',
               '((S[X]/S[X])/(S[adj]\NP))/NP']), !,   
   Sem = lam(Q,lam(VP,lam(S,lam(F,app(S,lam(E,app(app(VP,lam(P,app(Q,lam(X,merge(drs([],[Index:rel(E,X,Sym,0)]),merge(app(P,X),app(F,E))))))),lam(D,drs([],[[]:pred(D,event,n,1)]))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]/S[X])\NP)/NP']), !,
   Sem = lam(Q1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(Q1,lam(X,merge(drs([],[Index:rel(Y,X,Sym,0)]),app(F,E)))))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]/S[X])\NP)/S[dcl]']), !,
   Sem = lam(S1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(S1,lam(E,merge(drs([],[[]:pred(E,event,n,1),
                                                                                       Index:rel(E,Y,Sym,0)]),app(F,E)))))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   Cat = '((S[wq]/(S[q]/PP))\(S[wq]/(S[q]/PP)))/NP',
   Sem = lam(NP,lam(Q,lam(VP,lam(F,app(app(Q,VP),lam(E,merge(app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),
                                                             app(F,E)))))))). 


/* -------------------------------------------------------------------------
   instead (of)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Sym,[instead]),
   member(Cat,['((S[X]\NP)\(S[X]\NP))/PP',
               '((S[X]\NP)/(S[X]\NP))/PP',
               '((S[adj]\NP)\(S[adj]\NP))/PP']), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,drs([],[Index:not(merge(app(PP,E),app(F,E)))]))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "out of", "together with"
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Pos,['VBG','VBN']),
   member(Cat,['((S[X]\NP)\(S[X]\NP))/PP',
               '((S[X]\NP)/(S[X]\NP))/PP',
               '((S[adj]\NP)\(S[adj]\NP))/PP']), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:X],[Index:pred(X,Sym,v,0),
                                                                          []:pred(X,event,n,1)]),
                                                           merge(app(PP,X),app(F,E))))))))). 

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/PP',
               '((S[X]\NP)/(S[X]\NP))/PP',
               '((S[adj]\NP)\(S[adj]\NP))/PP']), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(app(PP,E),app(F,E)))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "Cycling in the north of France, ..."
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Pos,['VBG','VBN']),
   member(Cat,['(S[X]/S[X])/PP',
               '(S[X]\S[X])/PP',
               '(S\S)/PP',
               '(S/S)/PP']), !, 
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:pred(X,Sym,v,0),
                                                             []:pred(X,event,n,1)]),
                                              merge(app(PP,X),
                                                    app(F,E)))))))). 

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(S[X]/S[X])/PP',
               '(S[X]\S[X])/PP',
               '(S\S)/PP',
               '(S/S)/PP']), !, 
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,Sym,a,0)]),
                                              merge(app(PP,E),
                                                    app(F,E)))))))). 



/* -------------------------------------------------------------------------
   VP adverb modifier (negation)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)/(S[X]\NP))/((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)/(S[X]\NP))\((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP))',
               '((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))']), 
   member(Sym,['not','n\'t']), !, 
   Sem = lam(AV,lam(VP,lam(NP,lam(F,drs([],[Index:not(app(app(app(AV,VP),NP),lam(E,app(F,E))))]))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (Cardinals that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)/(S[X]\NP))/((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)/(S[X]\NP))\((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP))',  
               '((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))']), 
   \+ member(Sym,['not','n\'t']),
   member(Pos,['CD']), 
   string2digit(Sym,Digit), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([Index:X],[Index:card(X,Digit,ge),
                                                                                      Index:rel(E,X,rel,0)]),
                                                                       app(F,E)))))))).

/* -------------------------------------------------------------------------
   VP adverb modifier (NPs that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)/(S[X]\NP))/((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)/(S[X]\NP))\((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP))',  
               '((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))']), 
   \+ member(Sym,['not','n\'t']),
   member(Pos,['NN','NNS','NNP','NNPS']), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([Index:X],[Index:pred(X,Sym,n,0),
                                                                                      Index:rel(E,X,rel,0)]),
                                                                       app(F,E)))))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (intersective)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)/(S[X]\NP))/((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)/(S[X]\NP))\((S[X]\NP)/(S[X]\NP))',
               '((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP))',  
               '((S[adj]\NP)\(S[adj]\NP))/((S[adj]\NP)\(S[adj]\NP))',  
               '((S[adj]\NP)/(S[adj]\NP))/((S[adj]\NP)/(S[adj]\NP))',
               '((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP))']), 
   \+ member(Sym,['not','n\'t']),  !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([],[Index:pred(E,Sym,a,0)]),app(F,E)))))))).



% VP adverb modifier (negation)
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))',
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))',
                   '(((S[adj]\NP)/(S[adj]\NP))/((S[adj]\NP)/(S[adj]\NP)))/(((S[adj]\NP)/(S[adj]\NP))/((S[adj]\NP)/(S[adj]\NP)))']), 
   member(Sym,['not','n\'t']), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,drs([],[Index:not(app(app(app(app(M,AV),VP),NP),lam(E,app(F,E))))])))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))\((S\NP)\(S\NP)))/((S\NP)(S\NP))']), !,
   Sem = lam(AV1,lam(AV2,lam(VP,lam(NP,lam(F,app(app(app(AV2,app(AV1,VP)),NP),
                                     lam(E,merge(drs([],[Index:pred(E,Sym,a,0)]),app(F,E))))))))).

% VP adverb modifier (intersective)
semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))/(((S[X]\NP)\(S[X]\NP))/((S[X]\NP)\(S[X]\NP)))', 
                   '(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))/(((S[X]\NP)\(S[X]\NP))\((S[X]\NP)\(S[X]\NP)))']),
   \+ member(Sym,['not','n\'t']), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(app(M,AV),VP),NP),lam(E,merge(drs([],[Index:pred(E,Sym,a,0)]),app(F,E))))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(S/S)/(S[pss]\NP)',
                   '(S[X]/S[X])/(S[pss]\NP)',
                   '(S/S)/(S[dcl]\NP)',
                   '(S[X]/S[X])/(S[dcl]\NP)',
                   '(S[X]\S[X])/(S[dcl]\NP)',
                   '(S/S)/(S[asup]\NP)',
                   '(S[X]/S[X])/(S[asup]\NP)',
                   '(S/S)/(S[ng]\NP)',
                   '(S[X]/S[X])/(S[ng]\NP)',
                   '(S\S)/(S[ng]\NP)',
                   '(S[X]\S[X])/(S[ng]\NP)',
                   '(S/S)/(S[adj]\NP)',
                   '(S[X]/S[X])/(S[adj]\NP)']), !,
   Sem = lam(VP,lam(S,lam(G,app(app(VP,lam(P,merge(drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X)))),
                                lam(F,merge(app(G,F),app(S,lam(E,drs([],[Index:rel(E,F,Sym,0)]))))))))).


/* -------------------------------------------------------------------------
   Preposition (in front of WH)
------------------------------------------------------------------------- */

semlex('(S[wq]/(S[q]/PP))/(S[wq]/(S[q]/NP))',Sym,_Pos,Index,Sem):-
   Sem = lam(U,lam(VP,lam(F,app(app(VP,lam(E,app(app(U,lam(NP,lam(G,app(NP,lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),
                                                                                   app(G,E))))))),F))),lam(E,drs([],[[]:pred(E,event,n,1)])))))).

semlex('(S[wq]/(S[q]/NP))/(S[wq]/(S[q]/NP))',_Sym,_Pos,_Index,Sem):-
   Sem = lam(X,X).


/* -------------------------------------------------------------------------
   Prep + NP + ING (... results in shareholders receiving ...)
------------------------------------------------------------------------- */

semlex('(PP/(S[ng]\NP))/NP',Sym,_Pos,Index,Sem):- !,
   Sem = lam(NP,lam(VP,lam(E,app(app(VP,NP),lam(F,drs([],[Index:rel(E,F,Sym,0),[]:pred(F,event,n,1)])))))).


/* -------------------------------------------------------------------------
   Possessive 's
------------------------------------------------------------------------- */

semlex('(NP[nb]/N)\NP',_,_Pos,Index,Sem):- !,
   Sem = lam(NP,lam(N,lam(P,app(NP,lam(Y,alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                                        app(N,X)),
                                                  app(P,X))))))).


semlex('((NP[nb]/N)/(N/N))\NP',_,_Pos,Index,Sem):- !,
   Sem = lam(N,lam(S,lam(P,lam(Q,merge(drs([Index:U],[]),
                                       merge(app(app(S,lam(X,merge(app(P,X),
                                                                   app(N,lam(Y,drs([],[Index:rel(X,Y,of,0)])))))),U),
                                             app(Q,U))))))).

semlex('(N/N)\N',_,_,Index,Sem):- !,
   Sem = lam(N1,lam(N2,lam(X,merge(drs([[]:Y],[]),merge(app(N1,Y),merge(app(N2,X),drs([],[Index:rel(X,Y,of,0)]))))))).

semlex(Cat,_,_,Index,Sem):- 
   member(Cat,['((S[wq]/(S[q]/NP))/N)\(S[wq]/(S[q]/NP))',
               '((S[wq]/(S[dcl]\NP))/N)\(S[wq]/(S[dcl]\NP))']), !,
   Sem = lam(U,lam(N,lam(VP,lam(F,app(app(VP,lam(P,
                 app(app(U,lam(NP,lam(_,app(NP,lam(Y,
                   alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                  app(N,X)),
                            app(P,X))))))),F))),lam(E,drs([],[[]:pred(E,event,n,1)]))))))).


/* -------------------------------------------------------------------------
   NP modifiers: vacuous reflexives
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,_Index,Sem):-
   member(Cat,['NP\NP']), 
   member(Sym,[myself,yourself,himself,herself,itself,ourselves,themselves]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,app(P,X))))).


/* -------------------------------------------------------------------------
   NP modifiers: floating quantifiers 
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['NP\NP','NP/NP']), 
   member(Sym,[all,each]), !,
   Sem = lam(Q,lam(P,drs([],[Index:not(app(Q,lam(X,drs([],[Index:not(app(P,X))]))))]))).


/* -------------------------------------------------------------------------
   NP modifiers: deictics (not implemented yet)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,_Index,Sem):-
   member(Cat,['NP\NP','NP/NP']), 
   member(Sym,[there,here,ago,such,now]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,app(P,X))))).


/* -------------------------------------------------------------------------
   NP modifiers: negation
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['NP\NP','NP/NP']), 
   member(Sym,[not]), !,
   Sem = lam(NP,lam(P,drs([],[Index:not(app(NP,P))]))).


/* -------------------------------------------------------------------------
   NP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   member(Cat,['NP\NP','NP/NP']), !,
   symbol(Sym,NormSym), 
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:named(X,NormSym,nam,0)]),
                                       app(P,X)))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['NP\NP','NP/NP']), !,
   symbol(Sym,NormSym), 
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([Index:Y],[Index:pred(Y,NormSym,n,0),
                                                      Index:rel(X,Y,rel,0)]),
                                       app(P,X)))))).

/* -------------------------------------------------------------------------
   NP modifiers (superlative contruction)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['D/NP']), !,
   symbol(Sym,NormSym), 
   Sem = lam(X,lam(Y,drs([],[Index:rel(Y,X,NormSym,0)]))).


/* -------------------------------------------------------------------------
   NP modifier modifiers: deitics
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,_Index,Sem):-
   member(Cat,['(NP\NP)/(NP\NP)',
               '(NP\NP)\(NP\NP)',
               '(NP/NP)/(NP/NP)']), 
   member(Sym,[there,here,ago,such,now]), !,
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),P)))). 


/* -------------------------------------------------------------------------
   NP modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   member(Cat,['(NP\NP)/(NP\NP)',
               '(NP\NP)\(NP\NP)',
               '(NP/NP)/(NP/NP)']), !, 
   symbol(Sym,NormSym),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(drs([],[Index:named(X,NormSym,nam,0)]),
                                                    app(P,X))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(NP\NP)/(NP\NP)',
               '(NP\NP)\(NP\NP)',
               '(NP/NP)/(NP/NP)']), !, 
   symbol(Sym,NormSym),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(drs([],[Index:pred(X,NormSym,n,0)]),
                                                    app(P,X))))))).

%   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(drs([Index:Y],[Index:pred(Y,NormSym,n,0),
%                                                                   Index:rel(X,Y,rel,0)]),
%                                                    app(P,X))))))).


/* -------------------------------------------------------------------------
   NP modifier modifiers, superlative ("most notably")
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Pos,Index,Sem):-
   member(Cat,['(NP/NP)/(D/NP)']), !, 
   Sem = lam(R,lam(Q,lam(P,app(Q,lam(X,merge(drs([],[[-3|Index]:imp(drs([[]:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),
                                                                    app(app(R,X),Y))]),
                                             app(P,X))))))).


/* -------------------------------------------------------------------------
   NPs that function as S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos,Index,Sem):-
   category(smod,Cat,Sym), 
   member(Pos,['NNP','NNPS']), !,
   symbol(Sym,NormSym),
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:named(X,NormSym,nam,0),
                                                      Index:rel(E,X,rel,0)]),app(F,E)))))).

semlex(Cat,Sym,Pos,Index,Sem):-
   category(smod,Cat,Sym), 
   member(Pos,['NN','NNS']), !,
   symbol(Sym,NormSym),
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:pred(X,NormSym,n,0),
                                                      Index:rel(E,X,rel,0)]),app(F,E)))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[everywhere,nowhere,anywhere,somewhere]),
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:pred(X,location,n,1),
                                                      Index:rel(E,X,loc_rel,0)]),app(F,E)))))).



/* -------------------------------------------------------------------------
   Punctuation
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,_Index,Sem):-
   category(punctuation,Cat,Sym), !,
   Sem = lam(X,X).


/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(smod,Cat,Sym), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,Sym,a,0)]),app(F,E)))))).


/* -------------------------------------------------------------------------
   S modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):- 
   member(Sym,['not','n\'t']),
   member(Cat,['(S[X]/S[X])/(S[X]/S[X])',
               '(S[X]/S[X])\(S[X]/S[X])',
               '(S[X]\S[X])/(S[X]\S[X])',
               '(S[X]\S[X])\(S[X]\S[X])']), !, 
   Sem = lam(M,lam(S,lam(F,drs([],[Index:not(app(app(M,S),F))])))).

semlex(Cat,Sym,_Pos,_Index,Sem):- 
   \+ member(Sym,['not','n\'t']), 
   member(Cat,['(S[X]/S[X])/(S[X]/S[X])',
               '(S[X]/S[X])\(S[X]/S[X])',
               '(S[X]\S[X])/(S[X]\S[X])',
               '(S[X]\S[X])\(S[X]\S[X])']), !, 
   Sem = lam(M,lam(S,lam(F,app(app(M,S),F)))).

semlex(Cat,Sym,_Pos,Index,Sem):- 
   member(Cat,['((S[X]/S[X])/(S[X]/S[X]))/NP',
               '((S[X]/S[X])\(S[X]/S[X]))/NP',
               '((S[X]\S[X])/(S[X]\S[X]))/NP',
               '((S[X]\S[X])\(S[X]\S[X]))/NP']), !, 
   Sem = lam(Q,lam(M,lam(S,lam(F,app(app(M,S),lam(E,merge(app(Q,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Funny modifiers: for
------------------------------------------------------------------------- */

semlex('(S[for]/(S[to]\NP))/NP',_,_Pos,_,lam(NP,lam(VP,app(VP,NP)))):- !.


/* -------------------------------------------------------------------------
   Mostly Temporal modifiers: "every month", "this week", "Nov. 29"
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/N',
               '((S[X]\NP)\(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N']), 
   member(Sym,[this,the,that,these,those]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(drs([Index:Y],[]),app(N,Y)),
                                                             merge(drs([],[Index:rel(E,Y,rel,0)]),app(F,E))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/N',
               '((S[X]\NP)\(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N']), 
   member(Sym,[every,each,all,any,either]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([],[Index:imp(merge(drs([Index:Y],[]),app(N,Y)),
                                                                            drs([],[Index:rel(E,Y,rel,0)]))]),
                                                          app(F,E)))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/N',
               '((S[X]\NP)\(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N']), 
   member(Sym,[a,an,some]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Y],[Index:rel(E,Y,rel,0)]),
                                                          merge(app(N,Y),app(F,E))))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/N',
               '((S[X]\NP)\(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N']), 
   member(Sym,[no]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,drs([],[Index:not(merge(drs([Index:Y],[Index:rel(E,Y,rel,0)]),
                                                                            merge(app(N,Y),app(F,E))))]))))))).


semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['((S[X]\NP)\(S[X]\NP))/N',
               '((S[X]\NP)\(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N[num]',
               '((S[X]\NP)/(S[X]\NP))/N']), !,
   symbol(Sym,NormSym),
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Y],[Index:pred(Y,NormSym,n,0),
                                                               Index:rel(E,Y,rel,0)]),
                                                          merge(app(P,Y),app(F,E))))))))).




/* -------------------------------------------------------------------------
   Sentence modifying noun
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['N/S[em]','N/S[dcl]','N/S[for]']), !, 
   Sem = lam(S,lam(X,merge(drs([],[Index:pred(X,Sym,n,0)]),
                           app(S,lam(E,drs([],[Index:rel(E,X,rel,0)])))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['PP/S[dcl]','PP/S[qem]','PP/S']), !, 
   Sem = lam(S,lam(E,app(S,lam(X,drs([],[Index:rel(E,X,Sym,0)]))))).


/* -------------------------------------------------------------------------
   NP modifying noun
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['N/NP']), !, 
   Sem = lam(NP,lam(X,merge(drs([],[Index:pred(X,Sym,n,0)]),
                            app(NP,lam(Y,drs([],[Index:rel(X,Y,rel,0)])))))).


/* -------------------------------------------------------------------------
   PP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['PP/PP','PP\PP']), !, 
   Sem = lam(P,lam(E,merge(app(P,E),drs([],[Index:pred(E,Sym,a,0)])))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(PP\PP)\NP','(PP\PP)/NP','(PP/PP)/NP']), !, 
   Sem = lam(NP,lam(P,lam(E,merge(app(P,E),app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,_Pos,Index,Sem):-  
   Sym = not,
   member(Cat,['(PP\PP)/(PP\PP)','(PP/PP)/(PP/PP)']), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,drs([],[Index:not(app(P,X))]))),Y)))).

semlex(Cat,Sym,_Pos,Index,Sem):-  
   member(Cat,['(PP\PP)/(PP\PP)','(PP/PP)/(PP/PP)']), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:pred(X,Sym,a,0)]),app(P,X)))),Y)))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Cat,['(PP\PP)/N']), !, 
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),alfa(def,merge(drs([[]:X],[]),app(N,X)),drs([],[Index:rel(E,X,Sym,0)])))))).


/* -------------------------------------------------------------------------
   Discourse connectors 
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   \+ member(Sym,[if]),
   member(Cat,['(S[X]/S[X])/S[dcl]',
               '(S/S)/S[dcl]',
               '(S[wq]/S[wq])/S[dcl]']), !, 
   Sem = lam(S1,lam(S2,lam(F,merge(drs([Index:E,Index:X,Index:Y],
                                       [Index:prop(E,drs([],[Index:rel(X,Y,Sym,0)])),
                                        Index:pred(X,proposition,n,1),
                                        Index:pred(Y,proposition,n,1),
                                        Index:prop(X,app(S1,lam(E,drs([],[[]:pred(E,event,n,1)])))),
                                        Index:prop(Y,app(S2,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                   app(F,E))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Sym,[if]),
   member(Cat,['(S[X]/S[X])/S[dcl]',
               '(S/S)/S[dcl]',
               '(S[wq]/S[wq])/S[dcl]']), !, 
   Sem = lam(S1,lam(S2,lam(F,merge(drs([Index:E],[Index:imp(app(S1,lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                                      app(S2,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                   app(F,E))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   \+ member(Sym,[if]),
   member(Cat,['(S[X]\S[X])/S[dcl]',
               '(S\S)/S[dcl]',
               '(S[wq]\S[wq])/S[dcl]']), !, 
   Sem = lam(S2,lam(S1,lam(F,merge(drs([Index:E,Index:X,Index:Y],
                                       [Index:prop(E,drs([],[Index:rel(X,Y,Sym,0)])),
                                        Index:pred(X,proposition,n,1),
                                        Index:pred(Y,proposition,n,1),
                                        Index:prop(X,app(S1,lam(E,drs([],[[]:pred(E,event,n,1)])))),
                                        Index:prop(Y,app(S2,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                   app(F,E))))).

semlex(Cat,Sym,_Pos,Index,Sem):-
   member(Sym,[if]),
   member(Cat,['(S[X]\S[X])/S[dcl]',
               '(S\S)/S[dcl]',
               '(S[wq]\S[wq])/S[dcl]']), !, 
   Sem = lam(S2,lam(S1,lam(F,merge(drs([Index:E],[Index:imp(app(S1,lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                                            app(S2,lam(E,drs([],[[]:pred(E,event,n,1)]))))]),
                                   app(F,E))))).


/* -------------------------------------------------------------------------
   Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   member(Cat,['(NP\NP)/(S[dcl]\NP)',
               '(NP\NP)/(S[dcl]/NP)',
               '(NP\NP)/(S[b]\NP)',
               '(NP\NP)/(S[ng]\NP)',
               '(NP\NP)/(S[pss]\NP)',
               '(NP\NP)/(S[to]\NP)',
               '(NP\NP)/(S[adj]\NP)']), !, 
   Sem = lam(S,lam(Q,lam(P,app(Q,lam(X,merge(app(app(S,lam(P,app(P,X))),lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                             app(P,X))))))).

semlex(Cat,_Sym,_Pos,_Index,Sem):-
   Cat='((NP\NP)/(S[dcl]\NP))\(NP/NP)',
   Sem = lam(M,lam(S,lam(Q,lam(P,app(app(M,Q),lam(X,merge(app(app(S,lam(P,app(P,X))),lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                             app(P,X)))))))).


/* -------------------------------------------------------------------------
   whose
------------------------------------------------------------------------- */

semlex('((NP\NP)/(S[dcl]\NP))/N',_,_Pos,Index,Sem):- !,
   Sem = lam(N,lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,lam(P,merge(drs([Index:Y],[Index:rel(Y,X,of,0)]),
                                                                           merge(app(N,Y),app(P,Y))))),lam(E,drs([],[[]:pred(E,event,n,1)]))),
                                                    app(P,X)))))))). 


/* -------------------------------------------------------------------------
   Coordination modifiers (e.g. "as well as", "rather than", "not just")
------------------------------------------------------------------------- */

semlex('conj/conj',_,_Pos,_,lam(U,U)):- !.
semlex('conj\conj',_,_Pos,_,lam(U,U)):- !.



/* -------------------------------------------------------------------------
   Interjections/Sentential Categories
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Pos,Index,Sem):-
   category(s,Cat,_), !,
   Sem = lam(E,merge(drs([Index:X],[[]:pred(X,event,n,1),
                                    Index:pred(X,Sym,n,0)]),app(E,X))).



/* -------------------------------------------------------------------------
   Many/Much
------------------------------------------------------------------------- */

semlex('NP/N',many,_,Index,Sem):- !,
   Sem = lam(Y,lam(X,drs([],[Index:card(X,Y,eq),Index:pred(Y,quantity,n,1)]))).

semlex('NP/N',much,_,Index,Sem):- !,
   Sem = lam(Y,lam(X,drs([],[Index:card(X,Y,eq),Index:pred(Y,amount,n,3)]))).



/*========================================================================
   Decompose Compunt Symbols
========================================================================*/

decompose(Sym,One,Two):-
   atom(Sym),
   atom_codes(Sym,Codes),
   append(PartOne,[45|PartTwo],Codes),
   atom_codes(One,PartOne),
   atom_codes(Two,PartTwo), !.


/* =========================================================================
   Normalize Symbols
========================================================================= */

symbol(F1,F2):- 
   name(F1,A1),
   sym(A1,[A|A2]),
   name(F2,[A|A2]), !.

symbol(F,F).


sym([],[]).

%%% full stop
%%%
sym([46|L1],L2):- !,
   sym(L1,L2).

%%% lowercase characters
%%%
sym([X|L1],[Y|L2]):- 
   X > 64, X < 91, !,
   Y is X + 32,
  sym(L1,L2).

sym([X|L1],[X|L2]):- 
  sym(L1,L2).

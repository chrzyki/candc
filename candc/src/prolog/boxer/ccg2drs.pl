
:- module(ccg2drs,[ccg2drs/3]).

:- use_module(betaConversionDRT,[betaConvert/2]).
:- use_module(presupDRT,[resolveDrs/3]).
:- use_module(vpe,[resolveVPE/2]).
:- use_module(ppDrs,[ppDrs/3]).
:- use_module(library(lists),[append/3,member/2]).
:- use_module(parse_ccgcat,[parse_ccgcat/2,gen_ccgcat/2]).
:- use_module(transform,[trans/2]).
:- use_module(closure,[closure/3]).
:- use_module(lexicon,[semlex/6]).
:- use_module(coordination,[coordsem/4]).

:- [typechange].



/* =========================================================================
   Main Predicate
========================================================================= */

ccg2drs([C|L],XDRS,Context):-
   build(C,UDRS), 
   addinfo([C],Words1,Pos1,NE1),
   ppDrs(xdrs(Words1,Pos1,NE1,UDRS),Context,xdrs(Words2,Pos2,NE2,PUDRS)),   
   (
      user:option('--resolve',true),
      resolve(PUDRS,Pos2,NewDRS,_Links)
   ;
      user:option('--resolve',false),
      NewDRS = PUDRS
   ), 
   !,
   ccg2drss(L,Words2,Pos2,NE2,NewDRS,Context,XDRS). 

ccg2drs([_|L],XDRS,Context):-
   ccg2drs(L,XDRS,Context).


/* =========================================================================
   Build rest of underspecified Semantic Representations
========================================================================= */

ccg2drss([],PW,PP,PN,smerge(drs([],[]),PDRS),_,xdrs(PW,PP,PN,PDRS)):- !.

ccg2drss([],PW,PP,PN,PDRS,_,xdrs(PW,PP,PN,PDRS)):- !.

ccg2drss([C|L],PrevWords,PrevPos,PrevNE,PrevDRS,Context,XDRS):-
   build(C,UDRS), 
   addinfo([C],Words1,Pos1,NE1),
   ppDrs(xdrs(Words1,Pos1,NE1,UDRS),Context,xdrs(Words2,Pos2,NE2,PUDRS)),   
   (
      user:option('--resolve',true),
      (  
         PrevDRS = smerge(GlobalDRS,DRS), 
         resolve(smerge(GlobalDRS,smerge(DRS,PUDRS)),Pos2,NewDRS,Links)
      ;
         \+ PrevDRS = smerge(_,_),
         resolve(smerge(PrevDRS,PUDRS),Pos2,NewDRS,Links)
      )
   ;
      user:option('--resolve',false),
      NewDRS = smerge(PrevDRS,PUDRS)
   ), !,
   append(PrevWords,Words2,Words),
   append(PrevPos,Pos2,Pos),
   append(PrevNE,NE2,NE),
   ccg2drss(L,Words,Pos,NE,NewDRS,Context,XDRS). 

ccg2drss([_|L],PW,PP,PN,PDRS,Context,XDRS):-
   ccg2drss(L,PW,PP,PN,PDRS,Context,XDRS). 


/* =========================================================================
   Build one underspecified UDRS for derivation N
========================================================================= */

build(N,UDRS):-
   user:ccg(N,CCG0),
   trans(CCG0,CCG1),
   traverse(CCG1,Sem,Cat,_,_Ind,_Pos),
   closure(Cat,Sem,Closed),
   betaConvert(Closed,UDRS), !.

build(N,_):-
   user:option('--warnings',true),
   user:ccg(N,_), !,
   format(user_error,'WARNING: no compositional semantics for sentence ~p.~n',[N]), 
   fail.   

build(N,_):-
   user:option('--warnings',true),
   format(user_error,'WARNING: no parse for sentence ~p.~n',[N]), 
   fail.   


/* =========================================================================
   Resolve Semantic Representation
========================================================================= */

resolve(X,_Pos,Z,Links):-
   user:option('--vpe',true), 
   user:option('--resolve',true), !,
   resolveDrs(X,Y,Links),
   resolveVPE(Y,Z).

resolve(X,_Pos,Z,Links):-
   user:option('--vpe',false), 
   user:option('--resolve',true), !,
   resolveDrs(X,Z,Links).

resolve(X,_,X,[]).


/* =========================================================================
   Add POS and Index information
========================================================================= */

addinfo(List,Words,Pos,NE):-
    setof(word(Index,W),N^I^( member(N,List),
                              accessWord(N,I,W),
                              Index is 1000*N+I ), Words),
    findall(ne(Index,E),( member(N,List),
                          accessNE(N,I,E),
                          \+ E='O',
                          Index is 1000*N+I ), NE),
    setof(pos(Index,P),N^I^( member(N,List),
                             accessPOS(N,I,P),
                             Index is 1000*N+I), Pos).
   

/* =========================================================================
   Traverse the CCG tree for semantic composition

   traverse(+Node,-Sem,-Cat,+CoordinatingCat,-Index,-Pos)

   -> Node: the current position in the CCG tree 
   -> Sem: the computed semantic representation for Node
   -> Cat: the CCG category for Node (used for coordination)
   -> CoordinatingCat: used for coordination
   -> Index: list of indexes used for Sem (used in lexical rules)
   -> Pos: Pos-Tag of head category (CCG functor)
========================================================================= */


/* -------------------------------------------------------------------------
   Forward Application
------------------------------------------------------------------------- */

traverse(fa(Cat,F1,A1),Sem,Cat,CCat,Ind3,Pos):- !,
   (
      traverse(F1,F2,_,CCat,Ind1,Pos),
      traverse(A1,A2,_,CCat,Ind2,_), !,
      Sem=app(F2,A2),
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Backward Application
------------------------------------------------------------------------- */

traverse(ba(Cat,A1,F1),Sem,Cat,CCat,Ind3,Pos):- !,
   ( 
      traverse(A1,A2,_,CCat,Ind1,_),
      traverse(F1,F2,_,CCat,Ind2,Pos), !,
      Sem=app(F2,A2),
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Forward Composition
------------------------------------------------------------------------- */

traverse(fc(Cat,F1,A1),Sem,Cat,CCat,Ind3,Pos):- !,
   ( 
      traverse(F1,F2,_,CCat,Ind1,Pos),
      traverse(A1,A2,_,CCat,Ind2,_), !,
      Sem=lam(X,app(F2,app(A2,X))),
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).

/* -------------------------------------------------------------------------
   Backward Composition
------------------------------------------------------------------------- */

traverse(bc(Cat,A1,F1),Sem,Cat,CCat,Ind3,Pos):- !,
   ( 
      traverse(F1,F2,_,CCat,Ind1,Pos),
      traverse(A1,A2,_,CCat,Ind2,_), !,
      Sem=lam(X,app(F2,app(A2,X))),
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Backward Cross Composition
------------------------------------------------------------------------- */

traverse(bx(Cat,A1,F1),Sem,Cat,CCat,Ind3,Pos):- !,
   (
      traverse(F1,F2,_,CCat,Ind1,Pos),
      traverse(A1,A2,_,CCat,Ind2,_), !, 
      Sem=lam(X,app(F2,app(A2,X))),
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Generalised Forward Composition
------------------------------------------------------------------------- */

traverse(gfc(Cat,F1,A1),Sem,Cat,CCat,Ind3,Pos):- !,
   ( 
      traverse(F1,F2,FCat,CCat,Ind1,Pos),
      traverse(A1,A2,ACat,CCat,Ind2,_), 
      fcomp(FCat,ACat,F2,A2,Sem), !,
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Generalised Backward Composition
------------------------------------------------------------------------- */

traverse(gbc(Cat,A1,F1),Sem,Cat,CCat,Ind3,Pos):- !,
   ( 
      traverse(F1,F2,FCat,CCat,Ind1,Pos),
      traverse(A1,A2,ACat,CCat,Ind2,_), 
      bcomp(FCat,ACat,F2,A2,Sem), !,
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Generalised Backward Cross Composition
------------------------------------------------------------------------- */

traverse(gbx(Cat,F1,A1),Sem,Cat,CCat,Ind3,Pos):- !,
   (
      traverse(F1,F2,FCat,CCat,Ind1,Pos),
      traverse(A1,A2,ACat,CCat,Ind2,_), 
      bcross(FCat,ACat,F2,A2,Sem), !,
      append(Ind1,Ind2,Ind3)
   ;
      robust(Cat,Sem),
      Ind3=[]
   ).


/* -------------------------------------------------------------------------
   Lexical Nodes
------------------------------------------------------------------------- */

traverse(lf(I,J,Cat),Sem,Cat,_,Ind,Pos):-
   \+ member(Cat,[conj, ';', ',', ':',apposition]),
   (
      accessWord(I,J,Word),
      accessLemma(I,J,Lemma),
      accessPOS(I,J,Pos),
      Index is 1000*I+J,
      semlex(Cat,Word,Lemma,Pos,[Index],Sem), !,
      Ind=[Index]
   ;
      robust(Cat,Sem),
      Ind=[]
   ), !.

traverse(lf(I,J,Cat),Sem,Cat,CCat,Ind,Pos):-
   member(Cat,[conj, ';', ',', ':']),
   accessLemma(I,J,Lemma),
   accessPOS(I,J,Pos),
   Index is 1000*I+J,
   Ind=[Index],
   coordsem(CCat,Lemma,[Index],Sem), !.

traverse(lf(I,J,Cat),Sem,Cat,CCat,Ind,Pos):-
   Cat=apposition,
   accessPOS(I,J,Pos),
   Index is 1000*I+J,
   Ind=[Index],
   coordsem(CCat,apposition,[Index],Sem), !.


/* -------------------------------------------------------------------------
   Type Changing Rules (Lexical Rules)
------------------------------------------------------------------------- */

traverse(lex(Old,New,A1),A3,New,CCat,Ind,Pos):- !,
   traverse(A1,A2,_,CCat,Ind,Pos),
   typechange(Old:A2,New:A3,Ind,Pos).


/* -------------------------------------------------------------------------
   Type Raising
------------------------------------------------------------------------- */

traverse(tr(Cat,A1),lam(X,app(X,A2)),Cat,CCat,Ind,Pos):- !,
   traverse(A1,A2,_Old,CCat,Ind,Pos).


/* -------------------------------------------------------------------------
   Vacuous and General Coordination
------------------------------------------------------------------------- */

traverse(conj(_,Cat,Cat,_,A1),A2,Cat,CCat,Ind,Pos):- !,
   traverse(A1,A2,Cat,CCat,Ind,Pos).

traverse(conj(_,CoordCat,Cat,F1,A1),app(F2,A2),Cat,_,Ind3,Pos):-
   \+ Cat=CoordCat, !,
   traverse(F1,F2,_,CoordCat,Ind1,Pos),
   traverse(A1,A2,_,CoordCat,Ind2,_), 
   append(Ind1,Ind2,Ind3).


/* -------------------------------------------------------------------------
   Warning Messages
------------------------------------------------------------------------- */

traverse(Input,_,_,CCat,_,_):-
   user:option('--warnings',true),
   Input = lf(I,J,C), !,
   accessLemma(I,J,Lemma),
   format(user_error,'WARNING: no lexical semantics for cat ~p (~p) [CoordCat: ~p] leaf(~p,~p)~n',[C,Lemma,CCat,I,J]), 
   fail.




/* =========================================================================
   Semantics for Generalised Backward Cross Composition
========================================================================= */

bcross(FCat,ACat,F,A,lam(X,app(A,app(F,X)))):-
   parse_ccgcat(FCat,forward(Term1,_)),
   parse_ccgcat(ACat,backward(_,Term2)), 
   Term1=Term2, !.

bcross(FCat,ACat,F,A,lam(X,lam(Y,app(A,app(app(F,X),Y))))):-
   parse_ccgcat(FCat,forward(forward(Term1,_),_)),
   parse_ccgcat(ACat,backward(_,Term2)),
   Term1=Term2, !.


/* -------------------------------------------------------------------------
   Semantics for Generalised Forward Composition
------------------------------------------------------------------------- */

fcomp(FCat,ACat,F,A,lam(X,app(F,app(A,X)))):-
   parse_ccgcat(FCat,forward(_,Term1)),
   parse_ccgcat(ACat,forward(Term2,_)),
   Term1=Term2, !.

fcomp(FCat,ACat,F,A,lam(X,lam(Y,app(F,app(app(A,X),Y))))):-
   parse_ccgcat(FCat,forward(_,Term1)),
   parse_ccgcat(ACat,forward(forward(Term2,_),_)),
   Term1=Term2, !.


/* -------------------------------------------------------------------------
   Semantics for Generalised Backward Composition
------------------------------------------------------------------------- */

bcomp(FCat,ACat,F,A,lam(X,app(A,app(F,X)))):-
   parse_ccgcat(FCat,backward(Term1,_)),
   parse_ccgcat(ACat,backward(_,Term2)),
   Term1=Term2, !.

bcomp(FCat,ACat,F,A,lam(X,lam(Y,app(A,app(app(F,X),Y))))):-
   parse_ccgcat(FCat,backward(backward(Term1,_),_)),
   parse_ccgcat(ACat,backward(Term2,_)),
   Term1=Term2, !.


/* -------------------------------------------------------------------------
   Robust Recovery Rules
------------------------------------------------------------------------- */

robust(_,_):- user:option('--robust',false), !, fail.
robust('N',lam(X,drs([],[[]:pred(X,thing,n,12)]))):- !.
robust('N/N',lam(P,lam(X,app(P,X)))):- !.
robust('NP[nb]/N',lam(P,lam(Q,merge(drs([[]:X],[]),merge(app(P,X),app(Q,X)))))):- !.
robust('(NP\NP)/NP',lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[[]:rel(X,Y,rel,0)]),app(P,X)))))))))):- !.
robust('((S[X]\NP)\(S[X]\NP))/NP',lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(drs([],[[]:rel(E,Y,rel,0)]),app(F,E))))))))))):- !.
robust('NP',lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))):- !.
robust('PP/NP',lam(Q,lam(X,app(Q,lam(Y,drs([],[[]:rel(X,Y,rel,0)])))))):- !.
robust('(S[X]\NP)\(S[X]\NP)',lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([],[[]:pred(E,rel,a,0)]),app(F,E)))))))):- !.
robust('(S[X]\NP)/(S[X]\NP)',lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([],[[]:pred(E,rel,a,0)]),app(F,E)))))))):- !.
robust('S[dcl]\NP',lam(Q,lam(F,app(Q,lam(X,merge(drs([[]:E],[[]:pred(E,event,n,1),[]:rel(E,X,agent,0)]),app(F,E))))))):- !.
robust('(S[dcl]\NP)/NP',lam(Q2,lam(Q1,lam(F,app(Q1,lam(Y,app(Q2,lam(X,merge(drs([[]:E],[[]:pred(E,event,n,1),
                                                                                     []:rel(E,Y,agent,0),
                                                                                     []:rel(E,X,patient,0)]),app(F,E)))))))))):- !.
robust('(S[b]\NP)/NP',lam(Q2,lam(Q1,lam(F,app(Q1,lam(Y,app(Q2,lam(X,merge(drs([[]:E],[[]:pred(E,event,n,1),
                                                                                   []:rel(E,Y,agent,0),
                                                                                   []:rel(E,X,patient,0)]),app(F,E)))))))))):- !.



    
/* =========================================================================
   Access Information in CCG input file
========================================================================= */

accessWord(S,I,X):-  user:w(S,I,X,_,_,_,_,_).
accessLemma(S,I,X):- user:w(S,I,_,X,_,_,_,_).
accessPOS(S,I,X):-   user:w(S,I,_,_,X,_,_,_).
accessChunk(S,I,X):- user:w(S,I,_,_,_,X,_,_).
accessNE(S,I,X):-    user:w(S,I,_,_,_,_,X,_).
accessCat(S,I,X):-   user:w(S,I,_,_,_,_,_,X).

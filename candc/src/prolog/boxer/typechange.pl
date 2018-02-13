
/* -------------------------------------------------------------------------

This file contains the rules for type changing. These are called
"lexical rules" in CCGbank. They are unary type changing rules that
change the type of the category, or derived from binary type changing
rules represented as unary type chaning rule.  As they are not regular
like type shifting rules, it is impossible to give a general semantic
pattern for them. Instead each pair of categories (consisting of the
old and the new type of the category, will get its own semantic
interpretation.

The predicate typechange/4 has four arguments:

  - the old category and interpretation, O:OI
  - the new category and interpretation, N:NI
  - the word indices
  - the part of speech tag corresponding to the category

The POS information is needed to distinguish proper names from bare
nouns.

------------------------------------------------------------------------- */

:- use_module(categories,[category/3]).


/* -------------------------------------------------------------------------
   Type Change 
------------------------------------------------------------------------- */

typechange('NP':Old,'NP/NP':New,Index,_):- !,
   change(np,appo,Old,Index,New).

typechange('NP[nb]':Sem,'NP':Sem,_,_):- !.

typechange('NP[nb]':Old,'S[X]/S[X]':New,Index,_):- !,
   change(np,smod,Old,Index,New).

typechange('NP':Old,'S[X]/S[X]':New,Index,_):- !,
   change(np,smod,Old,Index,New).

typechange('NP[nb]':Old,'(S[X]\NP)\(S[X]\NP)':New,Index,_):- !,
   change(np,vpmod,Old,Index,New).

typechange('NP':Old,'(S[X]\NP)\(S[X]\NP)':New,Index,_):- !,
   change(np,vpmod,Old,Index,New).

typechange('S[dcl]/S[dcl]':Old,'(S[X]\NP)\(S[X]\NP)':New,Index,_):- !,
   change(smod,vpmod,Old,Index,New).

typechange('S[dcl]/S[dcl]':Old,'(S[X]\NP)/(S[X]\NP)':New,Index,_):- !,
   change(smod,vpmod,Old,Index,New).

typechange('S[dcl]/S[dcl]':Sem,'S[X]/S[X]':Sem,_,_):- !.
typechange('S[dcl]\S[dcl]':Sem,'S[X]/S[X]':Sem,_,_):- !.
typechange('S[dcl]/S[dcl]':Sem,'S[X]\S[X]':Sem,_,_):- !.
typechange('S[dcl]\S[dcl]':Sem,'S[X]\S[X]':Sem,_,_):- !.


typechange('N':Old,'NP':New,Index,Pos):-
   member(Pos,['NNP','NNPS']), !,
   change(n,pn,Old,Index,New).

typechange('N':Old,'NP':New,Index,_Pos):-
   change(n,np,Old,Index,New), !.

typechange('N[num]':Old,'NP':New,Index,_Pos):-
   change(n,np,Old,Index,New), !.


typechange('NP':Old,'NP/(NP\NP)':New,Index,_Pos):-
   change(np,npnpmod,Old,Index,New), !.

typechange('NP[nb]':Old,'NP/(NP\NP)':New,Index,_Pos):-
   change(np,npnpmod,Old,Index,New), !.


typechange(IV:Old,Mod:New,Index,_Pos):- 
   category(iv,IV,_),
   member(Mod,['N\N','N/N']), !, 
   change(iv,adj,Old,Index,New).

typechange(IV:Old,Mod:New,Index,_Pos):- 
   category(iv,IV,_),
   member(Mod,['NP\NP','NP/NP']), !,
   change(iv,npmod,Old,Index,New).

typechange(IV:Old,Mod:New,Index,_Pos):- 
   category(iv,IV,_),
   member(Mod,['(S[X]\NP)\(S[X]\NP)','(S[X]\NP)/(S[X]\NP)']), !,
   change(iv,vpmod,Old,Index,New).

typechange(IV:Old,Mod:New,Index,_Pos):- 
   category(iv,IV,_),
   member(Mod,['S[X]\S[X]','S[X]/S[X]']), !,
   change(iv,smod,Old,Index,New).


typechange(TV:Old,Mod:New,Index,_Pos):- 
   category(tv,TV,_),
   member(Mod,['NP\NP','NP/NP']), !,
   change(tv,npmod,Old,Index,New).


typechange('S[dcl]':Old,Mod:New,Index,_Pos):- 
   member(Mod,['N\N','N/N']), !, 
   change(s,adj,Old,Index,New).

typechange('S[dcl]':Old,Mod:New,Index,_Pos):- 
   member(Mod,['NP\NP','NP/NP']), !,
   change(s,npmod,Old,Index,New).

typechange('S[dcl]':Old,Mod:New,Index,_Pos):- 
   member(Mod,['S[X]\S[X]','S[X]/S[X]','S[dcl]/S[dcl]']), !,
   change(s,smod,Old,Index,New).


typechange('S[ng]\NP':Old,'NP':New,Index,_Pos):- !,
   change(iv,np,Old,Index,New).


typechange(Cat1:_,Cat2:_,_,_):-
   user:option('--warnings',true), !,
   format(user_error,'WARNING: no type changing rule for ~p --> ~p~n',[Cat1,Cat2]),
   fail.
    

/* -------------------------------------------------------------------------
   Semantics
------------------------------------------------------------------------- */

change(s,      adj, Old, _, lam(P,lam(X,merge(app(P,X),app(Old,lam(E,drs([],[[]:pred(E,event,n,1),[]:rel(E,X,rel,0)]))))))).
change(s,    npmod, Old, _, lam(Q,lam(P,app(Q,lam(X,merge(app(P,X),app(Old,lam(E,drs([],[[]:pred(E,event,n,1),[]:rel(E,X,rel,0)]))))))))).
change(s,     smod, Old, _, lam(S,lam(E,merge(app(Old,lam(E,drs([],[[]:pred(E,event,n,1)]))),app(S,E))))).

change(iv,     adj, Old, _, lam(P,lam(X,app(app(Old,lam(Q,merge(app(Q,X),app(P,X)))),lam(E,drs([],[[]:pred(E,event,n,1)])))))).
change(iv,   npmod, Old, _, lam(Q,lam(P,app(Q,lam(X,app(app(Old,lam(R,merge(app(R,X),app(P,X)))),lam(E,drs([],[[]:pred(E,event,n,1)])))))))).
change(iv,   vpmod, Old, I, lam(X,lam(Q,lam(E,merge(app(app(X,Q),E),app(app(Old,lam(P,merge(drs([I:Y],[]),app(P,Y)))),lam(E,drs([],[[]:pred(E,event,n,1)])))))))).
change(iv,    smod, Old, I, lam(S,lam(E,merge(app(app(Old,lam(P,merge(drs([I:X],[]),app(P,X)))),E),app(S,E))))).
change(iv,      np, Old, _, app(Old,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X))))).

change(n,       np, Old, I, lam(P,merge(merge(drs([I:X],[]),app(Old,X)),app(P,X)))).
change(n,       pn, Old, I, lam(P,alfa(nam,merge(drs([I:X],[]),app(Old,X)),app(P,X)))).

change(np,    smod, Old, _, lam(S,lam(E,app(S,lam(X,merge(app(Old,lam(Y,drs([],[[]:rel(X,Y,rel,0)]))),app(E,X))))))).
change(np,   vpmod, Old, _, lam(V,lam(N,lam(E,app(app(V,N),lam(X,merge(app(Old,lam(Y,drs([],[[]:rel(X,Y,rel,0)]))),app(E,X)))))))).

change(np, npnpmod, Old, _, lam(M,app(M,Old))).

change(np,    appo, Old, _, lam(M,lam(P,app(Old,lam(X,app(M,lam(Y,merge(drs([],[[]:eq(X,Y)]),app(P,X))))))))).

change(smod, vpmod, Old, _, lam(V,lam(N,app(Old,app(V,N))))).

change(tv,     Cat, Old, I, New):- change(iv,Cat,app(Old,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),I,New).




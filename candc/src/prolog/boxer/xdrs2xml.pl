
:- module(xdrs2xml,[drs2xml/2,
                    xfdrs2xml/2,
                    xdrs2xml/2]).

/*========================================================================
   Converting DRSs to XML
========================================================================*/

drs2xml(DRS,Stream):-
   drs2xml(DRS,Stream,1).

xdrs2xml(XDRS,Stream):-
   XDRS=xdrs(Words,POStags,NEtags,DRS),
   write(Stream,' <words>'), nl(Stream),
   words2xml(Words,Stream),
   write(Stream,' </words>'), nl(Stream),
   write(Stream,' <postags>'), nl(Stream),
   tags2xml(POStags,Stream),
   write(Stream,' </postags>'), nl(Stream),
   write(Stream,' <netags>'), nl(Stream),
   tags2xml(NEtags,Stream),
   write(Stream,' </netags>'), nl(Stream),
   drs2xml(DRS,Stream).

xfdrs2xml(XDRS,Stream):-
   XDRS=xdrs(Words,POStags,NEtags,Cons),
   write(Stream,' <words>'), nl(Stream),
   words2xml(Words,Stream),
   write(Stream,' </words>'), nl(Stream),
   write(Stream,' <postags>'), nl(Stream),
   tags2xml(POStags,Stream),
   write(Stream,' </postags>'), nl(Stream),
   write(Stream,' <netags>'), nl(Stream),
   tags2xml(NEtags,Stream),
   write(Stream,' </netags>'), nl(Stream),
   write(Stream,' <cons>'), nl(Stream),
   cons2xml(Cons,Stream),
   write(Stream,' </cons>'), nl(Stream).


/*========================================================================
   Converting DRSs to XML (with tab insertion)
========================================================================*/

drs2xml(drs(D,C),Stream,Tab):-
   tab(Stream,Tab),
   format(Stream,'<drs>~n',[]),
   dom2xml(D,Stream,Tab),
   conds2xml(C,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</drs>~n',[]).

drs2xml(alfa(Type,B1,B2),Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<alfa type="~p">~n',[Type]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab),
   drs2xml(B2,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</alfa>~n',[]).

drs2xml(merge(B1,B2),Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<merge>~n',[]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab),
   drs2xml(B2,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</merge>~n',[]).

drs2xml(smerge(B1,B2),Stream,Tab):- !,
   drs2xml(merge(B1,B2),Stream,Tab).


/*========================================================================
   Converting DRS-domains to XML (with tab insertion)
========================================================================*/

dom2xml([],_,_).

dom2xml([Index:X|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<dr name="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</dr>~n',[]),
   dom2xml(L,Stream,Tab).

dom2xml([X|L],Stream,Tab):-
   user:option('--warnings',true), !,
   format(user_error,'WARNING: cannot print DR ~p~n',[X]),
   dom2xml(L,Stream,Tab).

dom2xml([_|L],Stream,Tab):-
   dom2xml(L,Stream,Tab).


/*========================================================================
   Converting DRS-conditions to XML (with tab insertion)
========================================================================*/

conds2xml([],_,_).

conds2xml([Index:not(B)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<not>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</not>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:prop(X,B)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<prop argument="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</prop>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:or(B1,B2)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<or>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab),
   drs2xml(B2,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</or>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:imp(B1,B2)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<imp>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab),
   drs2xml(B2,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</imp>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:whq(_,B1,_,B2)|L],Stream,Tab):- !,
   conds2xml([Index:whq(B1,B2)|L],Stream,Tab).

conds2xml([Index:whq(B1,B2)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<whq>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab),
   drs2xml(B2,Stream,NewTab),
   tab(Stream,Tab),   
   format(Stream,'</whq>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:pred(Arg,X,Type,Sense)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'<pred arg="~p" symbol="~w" type="~p" sense="~p">~n',[Arg,Y,Type,Sense]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</pred>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:rel(Arg1,Arg2,X,Sense)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'<rel arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Arg1,Arg2,Y,Sense]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</rel>~n',[]),   
   conds2xml(L,Stream,Tab).

conds2xml([Index:named(Arg,X,Type,_)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'<named arg="~p" symbol="~w" type="~p">~n',[Arg,Y,Type]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</named>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:card(X,Y,Type)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<card arg="~p" value="~p" type="~p">~n',[X,Y,Type]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</card>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:timex(X,Y)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<timex arg="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   timex2xml(Y,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</timex>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:eq(X,Y)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<rel arg1="~p" arg2="~p" symbol="eq" sense="1">~n',[X,Y]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</rel>~n',[]),  
   conds2xml(L,Stream,Tab).

conds2xml([X|L],Stream,Tab):-
   user:option('--warnings',true), !,
   format(user_error,'WARNING: cannot print DR-Condition ~p~n',[X]),
   conds2xml(L,Stream,Tab).

conds2xml([_|L],Stream,Tab):-
   conds2xml(L,Stream,Tab).


/*========================================================================
   Timex
========================================================================*/

timex2xml(date(_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<date>~w~w~w</date>~n',[A,B,C]).

timex2xml(date(_:_,_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<date>~w~w~w</date>~n',[A,B,C]).

timex2xml(time(_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<time>~w~w~w</time>~n',[A,B,C]).

timex2xml(X,Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<unknown>~p</unknown>~n',[X]).


/*========================================================================
   Words
========================================================================*/

words2xml([],_).

words2xml([word(Index,Word)|L],Stream):-
   symbol(Word,Word1),
   format(Stream,'  <word xml:id="i~p">~w</word>~n',[Index,Word1]),
   words2xml(L,Stream).


/*========================================================================
   POS tags
========================================================================*/

tags2xml([],_).

tags2xml([pos(Index,POS)|L],Stream):-
   format(Stream,'  <postag index="i~p">~w</postag>~n',[Index,POS]),
   tags2xml(L,Stream).

tags2xml([ne(Index,NE)|L],Stream):-
   format(Stream,'  <netag index="i~p">~w</netag>~n',[Index,NE]),
   tags2xml(L,Stream).


/*========================================================================
   Flat DRSs
========================================================================*/

cons2xml([],_).

cons2xml([Label:alfa(Type,L1,L2)|Cons],Stream):- !,
   format(Stream,'  <alfa label="~p" type="~p"><label>~p</label><label>~p</label></alfa>~n',[Label,Type,L1,L2]),
   cons2xml(Cons,Stream).

cons2xml([Label:merge(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <merge label="~p"><label>~p</label><label>~p</label></merge>~n',[Label,L1,L2]),
   cons2xml(Cons,Stream).

cons2xml([Label:smerge(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <merge label="~p"><label>~p</label><label>~p</label></merge>~n',[Label,L1,L2]),
   cons2xml(Cons,Stream).

cons2xml([Label:drs(D,Labels)|Cons],Stream):- !,
   format(Stream,'  <drs label="~p">~n',[Label]),
   dom2xml(D,Stream,3),
   labels2xml(Labels,Stream),
   format(Stream,'  </drs>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:named(Arg,X,Type,_)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <named label="~p" arg="~p" symbol="~w" type="~p">~n',[Label,Arg,Y,Type]),
   index2xml(Index,Stream,2),
   format(Stream,'  </named>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:pred(Arg,X,Type,Sense)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <pred label="~p" arg="~p" symbol="~w" type="~p" sense="~p">~n',[Label,Arg,Y,Type,Sense]),
   index2xml(Index,Stream,2),
   format(Stream,'  </pred>~n',[]), 
   cons2xml(Cons,Stream).

cons2xml([Label:Index:rel(Arg1,Arg2,X,Sense)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <rel label="~p" arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Label,Arg1,Arg2,Y,Sense]),
   index2xml(Index,Stream,2),
   format(Stream,'  </rel>~n',[]), 
   cons2xml(Cons,Stream).

cons2xml([Label:Index:card(X,Y,Type)|Cons],Stream):- !,
   format(Stream,'  <card label="~p" arg="~p" value="~p" type="~p">~n',[Label,X,Y,Type]),
   index2xml(Index,Stream,2),
   format(Stream,'  </card>~n',[]), 
   cons2xml(Cons,Stream).

cons2xml([Label:Index:timex(X,Y)|Cons],Stream):- !,
   format(Stream,'  <timex label="~p" arg="~p">~n',[Label,X]),
   timex2xml(Y,Stream,2),
   index2xml(Index,Stream,2),
   format(Stream,'  </timex>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:eq(X,Y)|Cons],Stream):- !,
   format(Stream,'  <rel label="~p" arg1="~p" arg2="~p" symbol="eq" sense="1">~n',[Label,X,Y]),
   index2xml(Index,Stream,2),
   format(Stream,'  </rel>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:not(L)|Cons],Stream):- !,
   format(Stream,'  <not label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2xml(Index,Stream,2),
   format(Stream,'  </not>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:prop(X,L)|Cons],Stream):- !,
   format(Stream,'  <prop label="~p" argument="~p">~n',[Label,X]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2xml(Index,Stream,2),
   format(Stream,'  </prop>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:or(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <or label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2xml(Index,Stream,2),
   format(Stream,'  </or>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:imp(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <imp label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2xml(Index,Stream,2),
   format(Stream,'  </imp>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:whq(_,L1,_,L2)|Cons],Stream):- !,
   cons2xml([Label:Index:whq(L1,L2)|Cons],Stream).

cons2xml([Label:Index:whq(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <whq label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2xml(Index,Stream,2),
   format(Stream,'  </whq>~n',[]),
   cons2xml(Cons,Stream).


/*========================================================================
   Labels
========================================================================*/

labels2xml([],_).

labels2xml([Label|L],Stream):-
   format(Stream,'   <label>~w</label>~n',[Label]),
   labels2xml(L,Stream).


/*========================================================================
   Indexes
========================================================================*/

index2xml([],_,_):- !.

index2xml([X|L],Stream,Tab):-
   number(X), !,
   Pos is mod(X,1000),
   tab(Stream,Tab), 
   format(Stream,'<index pos="~p">i~p</index>~n',[Pos,X]),
   index2xml(L,Stream,Tab).

index2xml([_|L],Stream,Tab):-
   index2xml(L,Stream,Tab).


/*========================================================================
   Deal with special symbols
========================================================================*/

symbol(S1,S2):-
   name(S1,C1),
   check(C1,C2),
   name(S2,C2).

check([],[]).

%%% Special character &
%%%
check([38|L1],[38,97,109,112,59|L2]):- !,
   check(L1,L2).

%%% Special character <
%%%
check([60|L1],[38,108,116,59|L2]):- !,
   check(L1,L2).

%%% Special character >
%%%
check([62|L1],[38,103,116,59|L2]):- !,
   check(L1,L2).

%%% Special character '
%%%
check([62|L1],[38,97,112,111,115,59|L2]):- !,
   check(L1,L2).

%%% Special character "
%%%
check([62|L1],[38,113,117,111,116,59|L2]):- !,
   check(L1,L2).

check([X|L1],[X|L2]):-
   check(L1,L2).

   



:- module(transform,[trans/2]).

:- use_module(library(lists),[append/3,member/2]).


/* =========================================================================
   Transformations of input CCG tree:
   (1) N/N -> D/N  for adjectives modified by most/least
   (2) PP attachments to NP become to PP attachments to N 
   (3) possessives and superlatives
========================================================================= */


% case (1)
trans(fa('N/N',lf(X,I1,'(N/N)/(N/N)'),lf(X,I2,'N/N')),
      fa('N/N',lf(X,I1,'(N/N)/(D/N)'),lf(X,I2,'D/N'))):-
   member(Sym,[most,least]),
   accessWord(X,I1,Sym),
   accessLemma(X,I1,Sym),
   accessPOS(X,I2,'JJ'), !.

% case (1)

trans(fa('NP/NP',lf(X,I1,'(NP/NP)/(NP/NP)'),lf(X,I2,'NP/NP')),
      fa('NP/NP',lf(X,I1,'(NP/NP)/(D/NP)'),lf(X,I2,'D/NP'))):-
   member(Sym,[most,least]),
   accessWord(X,I1,Sym),
   accessLemma(X,I1,Sym),
   accessPOS(X,I2,'RB'), !.

% case (2)

trans(ba('NP',NP1,Mod1),Repaired2):-
   trans(NP1,NP2),
   makeNN(Mod1,Mod2),
   addNN(NP2,Mod2,Repaired1),
   trans(Repaired1,Repaired2), !.


% case (3)

trans(fa('NP[nb]',
           ba('NP[nb]/N',
	        NP,
                lf(I1,J1,'(NP[nb]/N)\NP')),
           fa('N',
                Superlative1,
                N)),
      fa('NP[nb]',
           fa('NP[nb]/N',
                ba('(NP[nb]/N)/(N/N)',
                     NP,
                     lf(I1,J1,'((NP[nb]/N)/(N/N))\NP')),
                Superlative2),
           N)):-
   superlative(Superlative1),
   trans(Superlative1,Superlative2), !.

trans(fa('NP[nb]',
           lf(I1,J1,'NP[nb]/N'),
           fa('N',
                Superlative1,
                N)),
      fa('NP[nb]',
	   fa('NP[nb]/N',
                lf(I1,J1,'(NP[nb]/N)/(N/N)'),
                Superlative2),
           N)):- 
   accessPOS(I1,J1,'PRP$'),
   superlative(Superlative1),
   trans(Superlative1,Superlative2), !.


trans(funny(_,_,X1),X2):- !,
   trans(X1,X2).

trans(appo(_,X1,Y1),fa('NP',lex('NP','NP/NP',X2),Y2)):- 
   trans(X1,X2),
   topcat(X2,NP1),
   member(NP1,['NP[nb]','NP']),
   trans(Y1,Y2),
   topcat(Y2,NP2),
   member(NP2,['NP[nb]','NP']), !.

trans(appo(_,X1,Y1),fa('S[dcl]',lex('S[dcl]','S[dcl]/S[dcl]',X2),Y2)):- 
   trans(X1,X2),
   topcat(X2,'S[dcl]'),
   trans(Y1,Y2),
   topcat(Y2,'S[dcl]'), !.

trans(rtc(NewCat,X1,_),lex(OldCat,NewCat,X2)):- !,
   trans(X1,X2),
   topcat(X2,OldCat).

trans(ltc(NewCat,_,X1),lex(OldCat,NewCat,X2)):- !,
   trans(X1,X2),
   topcat(X2,OldCat).

trans(rp(_,X1,_),X2):- !,
   trans(X1,X2).

trans(lp(_,_,X1),X2):- !,
   trans(X1,X2).

trans(ba(C,X1,Y1),ba(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(fa(C,X1,Y1),fa(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(bc(C,X1,Y1),bc(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(fc(C,X1,Y1),fc(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(bx(C,X1,Y1),bx(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(gfc(C,X1,Y1),gfc(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(gbc(C,X1,Y1),gbc(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(gbx(C,X1,Y1),gbx(C,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(lex(C1,C2,X1),lex(C1,C2,X2)):- !,
   trans(X1,X2).

trans(tr(C,X1),tr(C,X2)):- !,
   trans(X1,X2).
    
trans(other(Name,Cat,X1,Y1),other(Name,Cat,X2,Y2)):- !,
   trans(X1,X2), 
   trans(Y1,Y2).

trans(conj(',','NP','NP\NP',C1,C2),conj(app,'NP','NP\NP',D1,D2)):-
   C1=lf(I,J,','),
   BeforeJ is J - 1,
   AfterJ is J + 1,  
   \+ ( accessNE(I,BeforeJ,'I-LOCACTION'),
        accessNE(I,AfterJ,'I-LOCATION') ),
   \+ hasConjunction(C2), !,
   D1=lf(I,J,apposition),
   trans(C2,D2).

trans(conj(A,B,C,F1,A1),conj(A,B,C,F2,A2)):- !,
   trans(A1,A2),
   trans(F1,F2).

%% rule 1 (superlative detection)

trans(lf(I,J,'N/N'),lf(I,J,'N/N')):-
   accessLemma(I,J,Lemma),
   accessPOS(I,J,'JJ'),
   atom(Lemma),
   atom_chars(Lemma,Chars),
   append([m,o,s,t,'-'],[_|_],Chars),
   !,
   retract(user:w(I,J,Word,Lemma,'JJ',CHU,NE,CAT)),
   assert(user:w(I,J,Word,Lemma,'JJS',CHU,NE,CAT)).

trans(lf(I,J,'N/N'),lf(I,J,'N/N')):-
   accessLemma(I,J,Lemma),
   accessPOS(I,J,'JJ'),
   member(Lemma,[foremost,uttermost]),  %% unsurpassed
   !,
   retract(user:w(I,J,Word,Lemma,'JJ',CHU,NE,CAT)),
   assert(user:w(I,J,Word,Lemma,'JJS',CHU,NE,CAT)).


%% rule 2 (superlative detection)
trans(lf(I,J,'N/N'),lf(I,J,'N/N')):-
   accessLemma(I,J,Lemma),
   accessPOS(I,J,'JJ'),
   atom(Lemma),
   atom_chars(Lemma,Chars),
   append(FirstPart,[e,s,t],Chars),
   append(OrdinalChars,['-'|_],FirstPart),
   atom_chars(Ordinal,OrdinalChars),
   member(Ordinal,[second,third,fourth,fifth,sixth,seventh,eighth,ninth,tenth,
                   eleventh,twelfth,thirteenth,fourteenth,fifteenth,sixteenth,seventeenth,eighteenth,nineteenth,twentieth,
                   '2nd','3rd','4th','5th','6th','7th','8th','9th','10th',
                   '11th','12th','13th','14th','15th','16th','17th','18th','19th','20th']),  !,
   retract(user:w(I,J,Word,Lemma,'JJ',CHU,NE,CAT)),
   assert(user:w(I,J,Word,Lemma,'JJS',CHU,NE,CAT)).

trans(lf(I,J,Cat),lf(I,J,Cat)):- !.


/* =========================================================================
   Checking for superlative
========================================================================= */

superlative(lf(I,J,'N/N')):-
   trans(lf(I,J,'N/N'),lf(I,J,'N/N')),
   accessPOS(I,J,'JJS'), !.

superlative(ba('N/N',lf(_,_,'N/N'),lf(I,J,'(N/N)\(N/N)'))):-
   accessPOS(I,J,'JJS'), !.

superlative(fa('N/N',lf(I,J,'(N/N)/(N/N)'),lf(_,_,'N/N'))):-
   accessWord(I,J,Sym),
   accessLemma(I,J,Sym),
   member(Sym,[most,least]), !.


/* =========================================================================
   Tree subsumes conjunction (used to identify apposition)
========================================================================= */

hasConjunction(conj(conj,'NP','NP\NP',_,_)):- !.

hasConjunction(ba('NP',_,C)):- 
   hasConjunction(C).


/* =========================================================================
   Change the modifier from NP\NP to N\N
========================================================================= */

makeNN(fa('NP\NP',lf(X,Y,'(NP\NP)/NP'),Z),
       fa('N\N',lf(X,Y,'(N\N)/NP'),Z)).

makeNN(fa('NP\NP',fc('(NP\NP)/N',lf(X,Y,'(NP\NP)/NP'),U),Z),
       fa('N\N',fc('(N\N)/N',lf(X,Y,'(N\N)/NP'),U),Z)).

makeNN(lex('S[to]\NP','NP\NP',Z),
       lex('S[to]\NP','N\N',Z)).

makeNN(lex('S[pss]\NP','NP\NP',Z),
       lex('S[pss]\NP','N\N',Z)).

makeNN(lex('S[adj]\NP','NP\NP',Z),
       lex('S[adj]\NP','N\N',Z)).

makeNN(lex('S[ng]\NP','NP\NP',Z),
       lex('S[ng]\NP','N\N',Z)).
      

/* =========================================================================
   Include the N\N modifier into the main NP
========================================================================= */

addNN(ba(C,X,Y1),Mod,ba(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2). 

addNN(fa(C,X,Y1),Mod,fa(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(bc(C,X,Y1),Mod,bc(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(fc(C,X,Y1),Mod,fc(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(bx(C,X,Y1),Mod,bx(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(gfc(C,X,Y1),Mod,gfx(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(gbc(C,X,Y1),Mod,gbc(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(gbx(C,X,Y1),Mod,gbx(C,X,Y2)):- !,
   addNN(Y1,Mod,Y2).

addNN(lex(C1,C2,X1),Mod,lex(C1,C2,X2)):- !,
   addNN(X1,Mod,X2).

addNN(tr(C,X1),Mod,tr(C,X2)):- !,
   addNN(X1,Mod,X2).
    
addNN(lf(X,Y,'N'),Mod,ba('N',lf(X,Y,'N'),Mod)):- !.


/* =========================================================================
   Access Information in CCG input file
========================================================================= */

accessWord(S,I,X):- user:w(S,I,X,_,_,_,_,_).
accessLemma(S,I,X):- user:w(S,I,_,X,_,_,_,_).
accessPOS(S,I,X):- user:w(S,I,_,_,X,_,_,_).
accessChunk(S,I,X):- user:w(S,I,_,_,_,X,_,_).
accessNE(S,I,X):- user:w(S,I,_,_,_,_,X,_).
accessCat(S,I,X):- user:w(S,I,_,_,_,_,_,X).


/* =========================================================================
   Top cat
========================================================================= */

topcat(fa(C,_,_),C).
topcat(ba(C,_,_),C).
topcat(fc(C,_,_),C).
topcat(bc(C,_,_),C).
topcat(bx(C,_,_),C).
topcat(gfc(C,_,_),C).
topcat(gbc(C,_,_),C).
topcat(gbx(C,_,_),C).
topcat(lf(_,_,C),C).
topcat(lex(_,C,_),C).
topcat(tr(C,_),C).

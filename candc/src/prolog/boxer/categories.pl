
:- module(categories,[category/3]).


/* -------------------------------------------------------------------------
    Sentence
------------------------------------------------------------------------- */

category(s, 'S',       _):- !.   
category(s, 'S[X]',    _):- !.   
category(s, 'S[intj]', _):- !.    % interjection
category(s, 'S[frg]',  _):- !.    % fragment
category(s, 'S[inv]',  _):- !.    % elliptical inversion
category(s, 'S[dcl]',  _):- !.    % declarative
category(s, 'S[em]',   _):- !.    % embedded declarative
category(s, 'S[bem]',  _):- !.    % embedded sentence in subjuntive mood 
category(s, 'S[for]',  _):- !.    % small clause headed by "for"
category(s, 'S[b]',    _):- !.    % bare infinitive
category(s, 'S[ng]',   _):- !.    % present part
category(s, 'S[pss]',  _):- !.    % past part passive
category(s, 'S[wq]',   _):- !.    % wh questions
category(s, 'S[q]',    _):- !.    % yes-no questions
category(s, 'S[qem]',  _):- !.    % embedded question


/* -------------------------------------------------------------------------
    Intransitive verbs
------------------------------------------------------------------------- */

category(iv, 'S\NP',           [agent]):- !.       % declarative
category(iv, 'S[dcl]\NP[thr]', [rel]):- !.         % declarative
category(iv, 'S[dcl]\NP[expl]',[rel]):- !.         % declarative
category(iv, 'S[dcl]/NP[thr]', [rel]):- !.         % declarative
category(iv, 'S[dcl]\NP',      [agent]):- !.       % declarative
category(iv, 'S[dcl]/NP',      [agent]):- !.       % declarative
category(iv, 'S[frg]/NP',      [role]):- !.        % sentence fragments
category(iv, 'S[inv]/NP',      [agent]):- !.       % elliptical inversion
category(iv, 'S[b]\NP',        [agent]):- !.       % bare infinitive
category(iv, 'S[pt]\NP',       [agent]):- !.       % past participle (active mode)
category(iv, 'S[to]\NP',       [agent]):- !.       % to-infinitive      
category(iv, 'S[ng]\NP',       [agent]):- !.       % present participle
category(iv, 'S[bem]\NP',      [agent]):- !.       % subjunctive mood
category(iv, 'S[pss]\NP',      [patient]):- !.     % past participle (passive mode)
category(iv, 'S[adj]\NP',      []):- !.       
category(iv, 'S[asup]\NP',     []):- !.      


/* -------------------------------------------------------------------------
    Adjectives
------------------------------------------------------------------------- */

category(adj, 'N/N', _):- !.      
category(adj, 'N\N', _):- !.       
category(adjnum, 'N/N[num]', _):- !.      


/* -------------------------------------------------------------------------
    Transitive verbs
------------------------------------------------------------------------- */

category(tv, '(S[dcl]\NP)/NP',      [agent,patient]):- !.       % declarative
category(tv, '(S[dcl]/NP)/NP',      [agent,patient]):- !.       % declarative
category(tv, '(S[frg]/NP)/NP',      [agent,patient]):- !.       % sentence fragments
category(tv, '(S[inv]/NP)/NP',      [agent,patient]):- !.       % elliptical inversion
category(tv, '(S[b]\NP)/NP',        [agent,patient]):- !.       % bare infinitive
category(tv, '(S[pt]\NP)/NP',       [agent,patient]):- !.       % past participle (active mode)
category(tv, '(S[to]\NP)/NP',       [agent,patient]):- !.       % to-infinitive      
category(tv, '(S[ng]\NP)/NP',       [agent,patient]):- !.       % present participle
category(tv, '(S[pss]\NP)/NP',      [patient,theme]):- !.       % past participle (passive mode)
category(tv, '(S[dcl]\NP[thr])/NP', [rel,patient]):- !.         % there
category(tv, '(S[q]/NP)/NP',        [patient,agent]):- !.       % yes-no question


/* -------------------------------------------------------------------------
    Di-Transitive verbs
------------------------------------------------------------------------- */

category(dtv, '((S[dcl]\NP)/NP)/NP', [agent,patient,theme]):- !.       % declarative
category(dtv, '((S[dcl]/NP)/NP)/NP', [agent,patient,theme]):- !.       % declarative
category(dtv, '((S[frg]/NP)/NP)/NP', [agent,patient,theme]):- !.       % sentence fragments
category(dtv, '((S[inv]/NP)/NP)/NP', [agent,patient,theme]):- !.       % elliptical inversion
category(dtv, '((S[b]\NP)/NP)/NP',   [agent,patient,theme]):- !.       % bare infinitive
category(dtv, '((S[pt]\NP)/NP)/NP',  [agent,patient,theme]):- !.       % past participle (active mode)
category(dtv, '((S[to]\NP)/NP)/NP',  [agent,patient,theme]):- !.       % to-infinitive      
category(dtv, '((S[ng]\NP)/NP)/NP',  [agent,patient,theme]):- !.       % present participle
category(dtv, '((S[pss]\NP)/NP)/NP', [agent,patient,theme]):- !.       % past participle (passive mode)
category(dtv, '((S[q]/NP)/NP)/NP',   [patient,agent,theme]):- !.       % yes-no question

/* -------------------------------------------------------------------------
    NP/PP-Transitive verbs
------------------------------------------------------------------------- */

category(nppptv, '(S[dcl]\NP)/PP', [agent]):- !.       % declarative
category(nppptv, '(S[dcl]/NP)/PP', [agent]):- !.       % declarative
category(nppptv, '(S[frg]/NP)/PP', [agent]):- !.       % sentence fragments
category(nppptv, '(S[inv]/NP)/PP', [agent]):- !.       % elliptical inversion
category(nppptv, '(S[b]\NP)/PP',   [agent]):- !.       % bare infinitive
category(nppptv, '(S[pt]\NP)/PP',  [agent]):- !.       % past participle (active mode)
category(nppptv, '(S[to]\NP)/PP',  [agent]):- !.       % to-infinitive      
category(nppptv, '(S[ng]\NP)/PP',  [agent]):- !.       % present participle
category(nppptv, '(S[pss]\NP)/PP', [patient]):- !.       % past participle (passive mode)
category(nppptv, '(S[q]/NP)/PP',   [agent]):- !.       % yes-no question


/* -------------------------------------------------------------------------
    PP/NP-Transitive verbs
------------------------------------------------------------------------- */

category(ppnptv, '(S[dcl]\PP)/NP', [agent]):- !.       % declarative
category(ppnptv, '(S[dcl]/PP)/NP', [agent]):- !.       % declarative
category(ppnptv, '(S[frg]/PP)/NP', [agent]):- !.       % sentence fragments
category(ppnptv, '(S[inv]/PP)/NP', [agent]):- !.       % elliptical inversion
category(ppnptv, '(S[b]\PP)/NP',   [agent]):- !.       % bare infinitive
category(ppnptv, '(S[pt]\PP)/NP',  [agent]):- !.       % past participle (active mode)
category(ppnptv, '(S[to]\PP)/NP',  [agent]):- !.       % to-infinitive      
category(ppnptv, '(S[ng]\PP)/NP',  [agent]):- !.       % present participle
category(ppnptv, '(S[pss]\PP)/NP', [patient]):- !.     % past participle (passive mode)
category(ppnptv, '(S[q]/PP)/NP',   [agent]):- !.       % yes-no question


/* -------------------------------------------------------------------------
    NP/PP/NP-Transitive verbs
------------------------------------------------------------------------- */

category(npppnptv, '((S[dcl]\NP)/PP)/NP', [agent,patient]):- !.       % declarative
category(npppnptv, '((S[dcl]/NP)/PP)/NP', [agent,patient]):- !.       % declarative
category(npppnptv, '((S[frg]/NP)/PP)/NP', [agent,patient]):- !.       % sentence fragments
category(npppnptv, '((S[inv]/NP)/PP)/NP', [agent,patient]):- !.       % elliptical inversion
category(npppnptv, '((S[b]\NP)/PP)/NP',   [agent,patient]):- !.       % bare infinitive
category(npppnptv, '((S[pt]\NP)/PP)/NP',  [agent,patient]):- !.       % past participle (active mode)
category(npppnptv, '((S[to]\NP)/PP)/NP',  [agent,patient]):- !.       % to-infinitive      
category(npppnptv, '((S[ng]\NP)/PP)/NP',  [agent,patient]):- !.       % present participle
category(npppnptv, '((S[pss]\NP)/PP)/NP', [agent,patient]):- !.       % past participle (passive mode)
category(npppnptv, '((S[q]/NP)/PP)/NP',   [patient,agent]):- !.       % yes-no question


/* -------------------------------------------------------------------------
    NP/NP/PP-Transitive verbs
------------------------------------------------------------------------- */

category(npnppptv, '((S[dcl]\NP)/NP)/PP', [agent,patient]):- !.       % declarative
category(npnppptv, '((S[dcl]/NP)/NP)/PP', [agent,patient]):- !.       % declarative
category(npnppptv, '((S[frg]/NP)/NP)/PP', [agent,patient]):- !.       % sentence fragments
category(npnppptv, '((S[inv]/NP)/NP)/PP', [agent,patient]):- !.       % elliptical inversion
category(npnppptv, '((S[b]\NP)/NP)/PP',   [agent,patient]):- !.       % bare infinitive
category(npnppptv, '((S[pt]\NP)/NP)/PP',  [agent,patient]):- !.       % past participle (active mode)
category(npnppptv, '((S[to]\NP)/NP)/PP',  [agent,patient]):- !.       % to-infinitive      
category(npnppptv, '((S[ng]\NP)/NP)/PP',  [agent,patient]):- !.       % present participle
category(npnppptv, '((S[pss]\NP)/NP)/PP', [agent,patient]):- !.       % past participle (passive mode)
category(npnppptv, '((S[q]/NP)/NP)/PP',   [patient,agent]):- !.       % yes-no question


/* -------------------------------------------------------------------------
    PP/NP/NP-Transitive verbs
------------------------------------------------------------------------- */

category(ppnpnptv, '((S[dcl]\PP)/NP)/NP', [agent,patient]):- !.       % declarative
category(ppnpnptv, '((S[dcl]/PP)/NP)/NP', [agent,patient]):- !.       % declarative
category(ppnpnptv, '((S[frg]/PP)/NP)/NP', [agent,patient]):- !.       % sentence fragments
category(ppnpnptv, '((S[inv]/PP)/NP)/NP', [agent,patient]):- !.       % elliptical inversion
category(ppnpnptv, '((S[b]\PP)/NP)/NP',   [agent,patient]):- !.       % bare infinitive
category(ppnpnptv, '((S[pt]\PP)/NP)/NP',  [agent,patient]):- !.       % past participle (active mode)
category(ppnpnptv, '((S[to]\PP)/NP)/NP',  [agent,patient]):- !.       % to-infinitive      
category(ppnpnptv, '((S[ng]\PP)/NP)/NP',  [agent,patient]):- !.       % present participle
category(ppnpnptv, '((S[pss]\PP)/NP)/NP', [agent,patient]):- !.       % past participle (passive mode)
category(ppnpnptv, '((S[q]/PP)/NP)/NP',   [patient,agent]):- !.       % yes-no question
 

/* -------------------------------------------------------------------------
    NP/PP/PP-Transitive verbs
------------------------------------------------------------------------- */

category(nppppptv, '((S[dcl]\NP)/PP)/PP', [agent]):- !.       % declarative
category(nppppptv, '((S[dcl]/NP)/PP)/PP', [agent]):- !.       % declarative
category(nppppptv, '((S[frg]/NP)/PP)/PP', [agent]):- !.       % sentence fragments
category(nppppptv, '((S[inv]/NP)/PP)/PP', [agent]):- !.       % elliptical inversion
category(nppppptv, '((S[b]\NP)/PP)/PP',   [agent]):- !.       % bare infinitive
category(nppppptv, '((S[pt]\NP)/PP)/PP',  [agent]):- !.       % past participle (active mode)
category(nppppptv, '((S[to]\NP)/PP)/PP',  [agent]):- !.       % to-infinitive      
category(nppppptv, '((S[ng]\NP)/PP)/PP',  [agent]):- !.       % present participle
category(nppppptv, '((S[pss]\NP)/PP)/PP', [agent]):- !.       % past participle (passive mode)
category(nppppptv, '((S[q]/NP)/PP)/PP',   [agent]):- !.       % yes-no question
category(nppppptv, '((S[adj]/NP)/PP)/PP', [agent]):- !.       % adjectival 
 

/* -------------------------------------------------------------------------
    NP/PP/PP/NP-Transitive verbs
------------------------------------------------------------------------- */

category(npppppnptv, '(((S[dcl]\NP)/PP)/PP)/NP', [agent,patient]):- !.       % declarative
category(npppppnptv, '(((S[dcl]/NP)/PP)/PP)/NP', [agent,patient]):- !.       % declarative
category(npppppnptv, '(((S[frg]/NP)/PP)/PP)/NP', [agent,patient]):- !.       % sentence fragments
category(npppppnptv, '(((S[inv]/NP)/PP)/PP)/NP', [agent,patient]):- !.       % elliptical inversion
category(npppppnptv, '(((S[b]\NP)/PP)/PP)/NP',   [agent,patient]):- !.       % bare infinitive
category(npppppnptv, '(((S[pt]\NP)/PP)/PP)/NP',  [agent,patient]):- !.       % past participle (active mode)
category(npppppnptv, '(((S[to]\NP)/PP)/PP)/NP',  [agent,patient]):- !.       % to-infinitive      
category(npppppnptv, '(((S[ng]\NP)/PP)/PP)/NP',  [agent,patient]):- !.       % present participle
category(npppppnptv, '(((S[pss]\NP)/PP)/PP)/NP', [agent,patient]):- !.       % past participle (passive mode)
category(npppppnptv, '(((S[q]/NP)/PP)/PP)/NP',   [agent,patient]):- !.       % yes-no question
category(npppppnptv, '(((S[adj]/NP)/PP)/PP)/NP', [agent,patient]):- !.       % adjectival 


/* -------------------------------------------------------------------------
    Propositional Complement verbs
------------------------------------------------------------------------- */

category(invpcv, '(S[dcl]\S[dcl])\NP', [agent,theme]):- !.       % declarative
category(invpcv, '(S[dcl]\S[dcl])/NP', [agent,theme]):- !.       % declarative
category(invpcv, '(S[dcl]/S[dcl])/NP', [agent,theme]):- !.       % declarative
category(invpcv, '(S[dcl]/S[dcl])\NP', [agent,theme]):- !.       % declarative
category(invpcv, '(S[dcl]\S[wq])/NP',  [agent,theme]):- !.       % q


/* -------------------------------------------------------------------------
    Propositional Complement verbs
------------------------------------------------------------------------- */

category(pcv, '(S[dcl]\NP)/S[bem]',  [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[bem]',   [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[dcl]',  [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S',       [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[b]',    [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[pss]',  [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[ng]',   [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[for]',  [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[wq]',   [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[em]',   [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[qem]',  [agent,theme]):- !.
category(pcv, '(S[b]\NP)/S[qem]',    [agent,theme]):- !.
category(pcv, '(S[b]\NP)/S[em]',     [agent,theme]):- !.
category(pcv, '(S[b]\NP)/S[pss]',    [agent,theme]):- !.
category(pcv, '(S[b]\NP)/S[dcl]',    [agent,theme]):- !.
category(pcv, '(S[pss]\NP)/S[qem]',  [agent,theme]):- !.
category(pcv, '(S[pss]\NP)/S[dcl]',  [agent,theme]):- !.
category(pcv, '(S[pss]\NP)/S[em]',   [agent,theme]):- !.
category(pcv, '(S[pt]\NP)/S[em]',    [agent,theme]):- !.
category(pcv, '(S[pt]\NP)/S[qem]',   [agent,theme]):- !.
category(pcv, '(S[pt]\NP)/S[dcl]',   [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[dcl]',   [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[qem]',   [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[em]',    [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[pss]',   [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[b]',     [agent,theme]):- !.
category(pcv, '(S[ng]\NP)/S[for]',   [agent,theme]):- !.
category(pcv, '(S[adj]\NP)/S[qem]',  [agent,theme]):- !.
category(pcv, '(S[adj]\NP)/S[for]',  [agent,theme]):- !.
category(pcv, '(S[adj]\NP)/S[em]',   [agent,theme]):- !.
category(pcv, '(S[adj]\NP)/S[dcl]',  [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[qem]',  [agent,theme]):- !.
category(pcv, '(S[dcl]\NP)/S[em]',   [agent,theme]):- !.
category(pcv, '(S[b]\NP)/S[dcl]',    [agent,theme]):- !.


/* -------------------------------------------------------------------------
    Ditransitive Propositional Complement verbs
------------------------------------------------------------------------- */

category(dpcv, '((S[dcl]\NP)/S[dcl])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[b])/NP',    [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[pss])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[ng])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[for])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[wq])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[em])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[qem])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[b]\NP)/S[qem])/NP',    [agent,patient,theme]):- !.
category(dpcv, '((S[b]\NP)/S[em])/NP',     [agent,patient,theme]):- !.
category(dpcv, '((S[b]\NP)/S[pss])/NP',    [agent,patient,theme]):- !.
category(dpcv, '((S[b]\NP)/S[dcl])/NP',    [agent,patient,theme]):- !.
category(dpcv, '((S[pss]\NP)/S[qem])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[pss]\NP)/S[dcl])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[pss]\NP)/S[em])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[pt]\NP)/S[em])/NP',    [agent,patient,theme]):- !.
category(dpcv, '((S[pt]\NP)/S[qem])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[pt]\NP)/S[dcl])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[ng]\NP)/S[dcl])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[ng]\NP)/S[qem])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[ng]\NP)/S[em])/NP',    [agent,patient,theme]):- !.
category(dpcv, '((S[ng]\NP)/S[pss])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[ng]\NP)/S[b])/NP',     [agent,patient,theme]):- !.
category(dpcv, '((S[ng]\NP)/S[for])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[adj]\NP)/S[qem])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[adj]\NP)/S[for])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[adj]\NP)/S[em])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[adj]\NP)/S[dcl])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[qem])/NP',  [agent,patient,theme]):- !.
category(dpcv, '((S[dcl]\NP)/S[em])/NP',   [agent,patient,theme]):- !.
category(dpcv, '((S[b]\NP)/S[dcl])/NP',    [agent,patient,theme]):- !.


/* -------------------------------------------------------------------------
   Subject Control Verbs
------------------------------------------------------------------------- */

category(scv, '(S[dcl]\NP)/(S[to]\NP)',        [agent,theme]):- !.
category(scv, '(S[adj]\NP)/(S[to]\NP)',        [agent,theme]):- !.
category(scv, '(S[pt]\NP)/(S[to]\NP)',         [agent,theme]):- !.
category(scv, '(S[pss]\NP)/(S[to]\NP)',        [agent,theme]):- !.
category(scv, '(S[ng]\NP)/(S[to]\NP)',         [agent,theme]):- !.
category(scv, '(S[b]\NP)/(S[to]\NP)',          [agent,theme]):- !.

category(scv, '(S[adj]\NP)/(S[ng]\NP)',        [agent,theme]):- !.
category(scv, '(S[ng]\NP)/(S[ng]\NP)',         [agent,theme]):- !.
category(scv, '(S[pt]\NP)/(S[ng]\NP)',         [agent,theme]):- !.
category(scv, '(S[b]\NP)/(S[ng]\NP)',          [agent,theme]):- !.
category(scv, '(S[pss]\NP)/(S[ng]\NP)',        [agent,theme]):- !.

category(scv, '(S[ng]\NP)/(S[dcl]\NP)',        [agent,theme]):- !.
category(scv, '(S[pss]\NP)/(S[dcl]\NP)',       [agent,theme]):- !.

category(scv, '(S[pss]\NP)/(S[b]\NP)',         [agent,theme]):- !.
category(scv, '(S[ng]\NP)/(S[b]\NP)',          [agent,theme]):- !.
category(scv, '(S[pt]\NP)/(S[b]\NP)',          [agent,theme]):- !.
category(scv, '(S[b]\NP)/(S[b]\NP)',           [agent,theme]):- !.

category(scv, '(S[ng]\NP)\(S[adj]\NP)',        [agent,theme]):- !.

category(scv, '(S[pss]\NP)/(S[pt]\NP)',        [agent,theme]):- !.

category(scv, '(S[dcl]\NP[expl])/(S[b]\NP)',   [agent,theme]):- !.
category(scv, '(S[dcl]\NP[expl])/(S[pss]\NP)', [agent,theme]):- !.
category(scv, '(S[dcl]\NP[expl])/(S[pt]\NP)',  [agent,theme]):- !.

category(pscv, '((S[dcl]\NP)/(S[to]\NP))/PP', [agent,theme]):- !.
category(pscv, '((S[ng]\NP)/(S[to]\NP))/PP',  [agent,theme]):- !.
category(pscv, '((S[pss]\NP)/(S[to]\NP))/PP', [agent,theme]):- !.
category(pscv, '((S[b]\NP)/(S[to]\NP))/PP',   [agent,theme]):- !.
category(pscv, '((S[pt]\NP)/(S[to]\NP))/PP',  [agent,theme]):- !.


/* -------------------------------------------------------------------------
   Object Control Verbs
------------------------------------------------------------------------- */

category(ocv, '((S[dcl]\NP)/(S[to]\NP))/NP',       [agent,patient]):- !.
category(ocv, '((S[dcl]\NP[expl])/(S[to]\NP))/NP', [agent,patient]):- !. 
category(ocv, '((S[ng]\NP)/(S[to]\NP))/NP',        [agent,patient]):- !. 
category(ocv, '((S[b]\NP)/(S[to]\NP))/NP',         [agent,patient]):- !. 
category(ocv, '((S[pt]\NP)/(S[to]\NP))/NP',        [agent,patient]):- !. 
category(ocv, '((S[dcl]\NP)/(S[b]\NP))/NP',        [agent,patient]):- !. 
category(ocv, '((S[ng]\NP)/(S[b]\NP))/NP',         [agent,patient]):- !. 
category(ocv, '((S[b]\NP)/(S[b]\NP))/NP',          [agent,patient]):- !. 
category(ocv, '((S[dcl]\NP)/(S[ng]\NP))/NP',       [agent,patient]):- !. 
category(ocv, '((S[b]\NP)/(S[ng]\NP))/NP',         [agent,patient]):- !. 
category(ocv, '((S[ng]\NP)/(S[ng]\NP))/NP',        [agent,patient]):- !. 
category(ocv, '((S[dcl]\NP)/(S[adj]\NP))/NP',      [agent,patient]):- !. 
category(ocv, '((S[b]\NP)/(S[adj]\NP))/NP',        [agent,patient]):- !.  
category(ocv, '((S[ng]\NP)/(S[adj]\NP))/NP',       [agent,patient]):- !. 
category(ocv, '((S[pt]\NP)/(S[adj]\NP))/NP',       [agent,patient]):- !. 
category(ocv, '((S[dcl]\NP)/(S[pss]\NP))/NP',      [agent,patient]):- !. 
category(ocv, '((S[b]\NP)/(S[pss]\NP))/NP',        [agent,patient]):- !. 
category(ocv, '((S[dcl]\NP)/(S[pt]\NP))/NP',       [agent,patient]):- !. 
category(ocv, '((S[b]\NP)/(S[pt]\NP))/NP',         [agent,patient]):- !. 
category(ocv, '((S[ng]\NP)/(S[pss]\NP))/NP',       [agent,patient]):- !. 
category(ocv, '((S[ng]\NP)/(S[pt]\NP))/NP',        [agent,patient]):- !. 
category(ocv, '((S[pt]\NP)/(S[b]\NP))/NP',         [agent,patient]):- !. 
category(ocv, '((S[pt]\NP)/(S[ng]\NP))/NP',        [agent,patient]):- !.



/* -------------------------------------------------------------------------
   Auxiliary Verbs
------------------------------------------------------------------------- */

category(av, '(S\NP)/(S[b]\NP)',        b  ):- !.
category(av, '(S[dcl]\NP)/(S[dcl]\NP)', dcl):- !.     % have
category(av, '(S[dcl]\NP[thr])/(S[dcl]\NP)', dcl):- !.     % have
category(av, '(S[dcl]\NP)/(S[b]\NP)',   b  ):- !.     % modal verb (do, will, may)
category(av, '(S[dcl]\NP[thr])/(S[b]\NP)',   b  ):- !.     % modal verb (do, will, may)
category(av, '(S[dcl]\NP)/(S[pt]\NP)',  pt ):- !.     % auxiliary verb (has)
category(av, '(S[dcl]\NP[thr])/(S[pt]\NP)',  pt ):- !.     % auxiliary verb (has)
category(av, '(S[dcl]\NP)/(S[ng]\NP)',  ng ):- !.     % to be + ing (not always to be!)
category(av, '(S[dcl]\NP[thr])/(S[ng]\NP)',  ng ):- !.     % to be + ing (not always to be!)
category(av, '(S[dcl]\NP)/(S[pss]\NP)', pss):- !.     % was, were
category(av, '(S[dcl]\NP[thr])/(S[pss]\NP)', pss):- !.     % was, were
category(av, '(S[dcl]\NP)/(S[adj]\NP)', adj):- !.     % to be + adj (not always to be!)
category(av, '(S[dcl]\NP[thr])/(S[adj]\NP)', adj):- !.     % to be + adj (not always to be!)
category(av, '(S[to]\NP)/(S[b]\NP)',    b  ):- !.     % to
category(av, '(S[to]\NP)/(S[pt]\NP)',   pt ):- !.     % to
category(av, '(S[to]\NP)/(S[ng]\NP)',   ng ):- !.     % to
category(av, '(S[b]\NP)/(S[dcl]\NP)',   dcl):- !.   
category(av, '(S[b]\NP)/(S[pt]\NP)',    pt ):- !.     % have
category(av, '(S[b]\NP)/(S[pss]\NP)',   pss):- !.     % be
category(av, '(S[b]\NP)/(S[adj]\NP)',   adj):- !.     % to be + adj
category(av, '(S[pt]\NP)/(S[pss]\NP)',  pss):- !.     % been
category(av, '(S[pt]\NP)/(S[adj]\NP)',  adj):- !.  
category(av, '(S[ng]\NP)/(S[pss]\NP)',  pss):- !. 
category(av, '(S[ng]\NP)/(S[adj]\NP)',  adj):- !.
category(av, '(S[ng]\NP)/(S[pt]\NP)',   pt ):- !.     % having
category(av, '(S[pss]\NP)/(S[pss]\NP)', pss):- !.
category(av, '(S[pss]\NP)/(S[adj]\NP)', adj):- !. 
category(av, '(S[em]\NP)/(S[b]\NP)',    b  ):- !.


/* -------------------------------------------------------------------------
   Modal Verbs
------------------------------------------------------------------------- */

category(mv, '(S[dcl]/(S[b]\NP))/NP',    _):- !.
category(mv, '(S[dcl]\(S[adj]\NP))/NP',  _):- !.
category(mv, '(S[q]/(S[pt]\NP))/NP',     _):- !.
category(mv, '(S[q]/(S[dcl]\NP))/NP',    _):- !.
category(mv, '(S[dcl]/(S[pt]\NP))/NP',   _):- !. 


/* -------------------------------------------------------------------------
   VP adverbials
------------------------------------------------------------------------- */

category(vpadv, '(S\NP)/(S\NP)',           _):- !. 
category(vpadv, '(S\NP)\(S\NP)',           _):- !. 
category(vpadv, '(S[X]\NP)\(S[X]\NP)',     _):- !.
category(vpadv, '(S[X]\NP)/(S[X]\NP)',     _):- !.
category(vpadv, '(S[adj]\NP)/(S[adj]\NP)', _):- !.
category(vpadv, '(S[adj]\NP)\(S[adj]\NP)', _):- !.


/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

category(smod, 'S/S[dcl]',   _):- !.
category(smod, 'S[wq]\S[wq]',_):- !.
category(smod, 'S[X]/S[X]',  _):- !.
category(smod, 'S[X]\S[X]',  _):- !.

 
/* -------------------------------------------------------------------------
   Punctuation
------------------------------------------------------------------------- */

category(punctuation, 'S[wq]\S[wq]','?'):- !.
category(punctuation, 'S[wq]\S[wq]','.'):- !.
category(punctuation, 'S[wq]\S[wq]','!'):- !.
category(punctuation, 'S[dcl]\S[dcl]','?'):- !.
category(punctuation, 'S[dcl]\S[dcl]','.'):- !.
category(punctuation, 'S[dcl]\S[dcl]','!'):- !.
category(punctuation, 'S[X]\S[X]','?'):- !.
category(punctuation, 'S[X]\S[X]','.'):- !.
category(punctuation, 'S[X]\S[X]','!'):- !.


:- module(ne,[netype/2]).

netype('I-LOC',loc).
netype('B-LOC',loc).
netype('E-LOC',loc).

netype('I-ORG',org).
netype('B-ORG',org).
netype('E-ORG',org).

netype('I-PER',per).
netype('B-PER',per).
netype('E-PER',per).

netype('Person',per).
netype('Organisation',org).
netype('Location',loc).
netype('Artifact',art).
netype('Event',eve).
netype('Natural_Object',nat).
netype('Timex',tim).

netype(_,nam).

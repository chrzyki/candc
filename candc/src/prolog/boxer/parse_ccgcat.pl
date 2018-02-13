
:- module(parse_ccgcat,[parse_ccgcat/2, gen_ccgcat/2]).

parse_ccgcat(Cat,Term):-  
   atom(Cat),
   atom_codes(Cat,Codes),
   mcat(Term,Codes,[]), !.

gen_ccgcat(Term,Cat):-  
   mcat(Term,Codes,[]), !,
   atom_codes(Cat,Codes).


mcat(backward(Cat1,Cat2)) --> cat(Cat1), cat(Cat2). 
mcat(backward(Cat1,Cat2)) --> cat(Cat1), [92], cat(Cat2). 
mcat(forward(Cat1,Cat2)) --> cat(Cat1), [47], cat(Cat2). 
mcat(X) --> cat(X).

cat(backward(Cat1,Cat2)) --> [40], cat(Cat1), cat(Cat2), [41]. 
cat(backward(Cat1,Cat2)) --> [40], cat(Cat1), [92], cat(Cat2), [41]. 
cat(forward(Cat1,Cat2)) --> [40], cat(Cat1), [47], cat(Cat2), [41]. 

cat(q([])) --> [81].
cat(s([])) --> [83].
cat(s([F])) --> [83,91], feature(F), [93].
cat(n([])) --> [78].
cat(punct([])) --> [46].
cat(conj([])) --> [99,111,110,106].
cat(np([])) --> [78,80].
cat(np([])) --> [78,80,91], feature(_), [93].
cat(pp([])) --> [80,80].

feature(X) --> {var(X)}, [88].
feature(X) --> {f(X), atom_codes(X,Codes)}, Codes.


f(adj).
f(as).
f(asup).
f(b).
f(bem).
f(dcl).
f(em).
f(expl).
f(for).
f(frg).
f(intj).
f(inv).
f(nb).
f(ng).
f(num).
f(poss).
f(pss).
f(pt).
f(q).
f(qem).
f(tpc).
f(thr).
f(to).
f(wq).

   

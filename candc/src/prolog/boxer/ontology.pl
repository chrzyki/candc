

/*************************************************************************

                               thing
                   /           /   \     \       \
         proposition     entity   event  time   abstraction
                         /    \                 /        \
                     object  organism    organisation     
                              /     \
                         animal    person
                                   /    \
                                man    woman

*************************************************************************/

:- module(ontology,[isa/2,isnota/2]).

isa(event,thing).
isa(event,neuter).

isa(time,thing).
isa(time,neuter).

isa(entity,thing).

isa(abstraction,thing).
isa(abstraction,neuter).

isa(organisation,abstraction).
isa(proposition,thing).


isa(object,entity).
isa(object,neuter).

isa(organism,entity).

isa(animal,organism).

isa(person,organism).

isa(plant,organism).
isa(plant,neuter).

isa(man,person).
isa(woman,person).
isa(governer,person).
isa(lawyer,person).
isa(director,person).
isa(chairman,person).
isa('dr.',person).




isa('mr.',man).
isa(spokesman,man).

isa('mrs.',woman).
isa('ms.',woman).
isa(spokeswoman,womam).

isa(woman,female).
isa(man,male).

isa('n.v.',organisation).
isa('inc.',organisation).
isa('corp.',organisation).
isa('co.',organisation).
isa('ltd.',organisation).
isa(company,organisation).


isa(year,time).
isa(today,time).
isa(july,time).
isa(monday,time).



isnota(man,woman).

isnota(male,female).
isnota(neuter,female).
isnota(neuter,male).


isnota(abstraction,time).
isnota(abstraction,event).
isnota(abstraction,entity).
isnota(entity,event).
isnota(time,event).
isnota(entity,time).

isnota(object,organism).

isnota(plant,person).
isnota(plant,animal).
isnota(animal,person).

isnota(single,plural).
isnota(single,event).
isnota(plural,event).

isnota(person,nonhuman).

isnota(proposition,entity).
isnota(proposition,event).
isnota(proposition,time).
isnota(proposition,abstraction).
isnota(proposition,neuter).
isnota(proposition,male).
isnota(proposition,female).

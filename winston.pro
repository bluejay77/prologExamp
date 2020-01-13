% -*- Mode: Prolog -*-
% The animals identification knowledge base from Ivan Bratko's book.
%
% AJY 2012-06-27.

:- op(100, xfx, [has, gives, 'does not', eats, lays, isa]).
:- op(100, xf, [swims, flies]).


rule1 :: if
    Animal has hair
    or
    Animal gives milk
    then
    Animal isa mammal.

rule2 :: if
    Animal has feathers
    or
    Animal flies and
    Animal lays eggs
    then
    Animal isa bird.

rule3 :: if
    Animal isa mammal and
    (Animal eats meat
    or
    Animal has 'pointed teeth' and
    Animal has claws and
    Animal has 'forward pointing eyes')
    then
    Animal isa carnivore.

rule4 :: if
    Animal isa carnivore and
    Animal has 'tawny colour' and
    Animal has 'dark spots'
    then
    Animal isa cheetah.

rule5 :: if
    Animal isa carnivore and
    Animal has 'tawny colour' and
    Animal has 'black stripes'
    then
    Animal isa tiger.

rule6 :: if
    Animal isa bird and
    Animal 'does not' fly and
    Animal swims
    then
    Animal isa penguin.

rule7 :: if
    Animal isa bird and
    Animal isa 'good flyer'
    then
    Animal isa albatross.


fact :: X isa animal :-
    member(X, [cheetah, tiger, penguin, albatross]).


askable(_ gives _, 'Animal' gives 'What').
askable(_ flies, 'Animal' flies).
askable(_ lays eggs, 'Animal' lays eggs).
askable(_ eats _, 'Animal' eats 'What').
askable(_ has _, 'Animal' has 'Something').
askable(_ 'does not' _, 'Animal' 'does not' 'DoSomething').
askable(_ swims, 'Animal' swims).
askable(_ isa 'good flier', 'Animal' isa 'good flier').


    

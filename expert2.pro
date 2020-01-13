% -*- Mode: Prolog -*-
% Expert system shell from Ivan Bratkos book.
%
% AJY 2012-06-26.

% The SWI Prolog
% https://www.swi-prolog.org/download/stable





%----------------------------------------------------------------------

:- op(900, xfx, ::).
:- op(800, xfx, was).
:- op(870, fx, if).
:- op(880, xfx, then).
:- op(550, xfy, or).
:- op(540, xfy, and).
:- op(300, fx, 'derived by').
:- op(600, xfx, from).
:- op(600, xfx, by).

%----------------------------------------------------------------------

% Program assumes priorities of predefined operators:
% op(700, xfx, is) and op(900, fx, \+)

%----------------------------------------------------------------------

% This has been hard-coded into the program: The knowledge base.

% The animals identification knowledge base from Ivan Bratkos book.
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

fact :: antti isa bluejay.

askable(_ gives _, 'Animal' gives 'What').
askable(_ flies, 'Animal' flies).
askable(_ lays eggs, 'Animal' lays eggs).
askable(_ eats _, 'Animal' eats 'What').
askable(_ has _, 'Animal' has 'Something').
askable(_ 'does not' _, 'Animal' 'does not' 'DoSomething').
askable(_ swims, 'Animal' swims).
askable(_ isa 'good flier', 'Animal' isa 'good flier').


%----------------------------------------------------------------------

% And some facts about animals.  This is missing in Bratkos book.

fact :: antti isa bird.
fact :: antti isa 'good flyer'.

fact :: willie isa bird.
fact :: willie 'does not' fly.
fact :: willie swims.

fact :: marsu has hair.
fact :: marsu gives milk.

fact :: marsu eats meat.

fact :: marsu has 'tawny colour'.
fact :: marsu has 'black stripes'.


%----------------------------------------------------------------------


% Case #1.  Goal is immediately asserted by a fact in the KB.
explore(Goal, Trace, Goal is true was 'found as a fact') :-
    fact :: Goal.

% Case #2.  There exists a rule from which Goal can be derived.
explore(Goal,
	Trace,
	Goal is TruthValue was 'derived by' Rule from Answer) :-
    Rule :: if Condition then Goal, % Rule is relevant to Goal
    explore(Condition, [Goal by Rule | Trace], Answer),
    truth(Answer, TruthValue).

% Case #3.  Conjuction of two goals to be solved.
explore(Goal1 and Goal2, Trace, Answer) :-
    !,
    explore(Goal1, Trace, Answer1),
    continue(Answer1, Goal1 and Goal2, Trace, Answer).

% Case #4.  Disjunction of two goals to be solved.
explore(Goal1 or Goal2, Trace, Answer) :-
    exploreyes(Goal1, Trace, Answer)  % Positive answer to Goal1
    ;
    exploreyes(Goal2, Trace, Answer). % Positive answer to Goal2

% Case 5#.  We have an OR and both goals give a negative answer:
% the net answer must be negative.
explore(Goal1 or Goal2, Trace, Answer1 and Answer2) :-
    !,
    \+ exploreyes(Goal1, Trace, _),
    \+ exploreyes(Goal2, Trace, _),  % no positive answer
    explore(Goal1, Trace, Answer1),
    explore(Goal2, Trace, Answer2).

explore(Goal, Trace, Goal is Answer was 'told') :-
    useranswer(Goal, Trace, Answer). % user-supplied answer

% Case #6 was missing from Bratkos book.  There is no rule from which
% the answer could be derived. Added by AJY 2012-06-27.

explore(Goal, Trace, Goal is false was 'not found as a fact') :-
    !.


% ----------------------------------------------------------------------

exploreyes(Goal, Trace, Answer) :-
    explore(Goal, Trace, Answer),
    positive(Answer).

%----------------------------------------------------------------------

continue(Answer1, Goal1 and Goal2, Trace, Answer) :-
    positive(Answer1),
    explore(Goal2, Trace, Answer2),
    ( positive(Answer2),  % both answers were 'true'
      Answer = Answer1 and Answer2
    ;
      negative(Answer2),  % Answer2 was 'false' -- conjuction is false
      Answer = Answer2
    ).

continue(Answer1, Goal1 and Goal2, _, Answer2) :-
    negative(Answer1).

%----------------------------------------------------------------------

truth(Question is TruthValue was Found, TruthValue) :-
    !.

truth(Answer1 and Answer2, TruthValue) :-
    truth(Answer1, true),
    truth(Answer2, true),
    !,
    TruthValue = true
    ;
    TruthValue = false.

%----------------------------------------------------------------------

positive(Answer) :-
    truth(Answer, true).

negative(Answer) :-
    truth(Answer, false).

%----------------------------------------------------------------------

getreply(Reply) :-
    read(Answer),
    means(Answer, Reply), !  % Answer means something?
    ;
    nl, write('Answer unknown, try again please.'), nl, % No it does not.
    getreply(Reply).

means(yes, yes).
means(y, yes).
means(no, no).
means(n, no).
means(why, why).
means(w, why).

%----------------------------------------------------------------------

% This goal generates, through backtracking, user-supplied solutions
% to Goal.

useranswer(Goal, Trace, Answer) :-
    askable(Goal, _),  % Is the goal askable from the user?
    freshcopy(Goal, Copy), % Variables in Goal renamed.
    useranswer(Goal, Copy, Trace, Answer, 1).


% If the goal is instantiated, do not ask again.

useranswer(Goal, _, _, _, N) :-
    N > 1,
    instantiated(Goal), !,
    fail.

% Is the Goal implied to be true or false for all instantiations?

useranswer(Goal, Copy, _, Answer, _) :-
    wastold(Copy, Answer, _),
    instance_of(Copy, Goal), !.  % Answer to Goal implied

% Retrieve known solutions, indexed from N on, for Goal.

useranswer(Goal, _, _, true, N) :-
    wastold(Goal, true, M),
    M >= N.

% Has everything already been said about Goal?

useranswer(Goal, Copy, _, Answer, _) :-
    end_answers(Copy),
    instance_of(Copy, Goal), !,  % EVerything already was said about Goal.
    fail.

% Ask the user for (more) solutions.

useranswer(Goal, _, Trace, Answer, N) :-
    askuser(Goal, Trace, Answer, N).

%----------------------------------------------------------------------

askuser(Goal, Trace, Answer, N) :-
    askable(Goal, ExternFormat),
    format(Goal, ExternFormat, Question, [], Variables), % Question format.
    ask(Goal, Question, Variables, Trace, Answer, N).

ask(Goal, Question, Variables, Trace, Answer, N) :-
    nl,
    ( Variables = [], !,
      write('Is it true: ')
    ;
      write('Any (more) solution(s) to: ')
    ),
    write(Question), write('? '),
    getreply(Reply), !,  % Reply = yes/no/why
    process(Reply, Goal, Question, Variables, Trace, Answer, N).

%----------------------------------------------------------------------

process(why, Goal, Question, Variables, Trace, Answer, N) :-
    showtrace(Trace),
    ask(Goal, Question, Variables, Trace, Answer, N).

process(yes, Goal, _, Variables, Trace, true, N) :-
    nextindex(Next), % New free index for 'wastold'
    Next1 is Next + 1,
    ( askvars(Variables),
      assertz(wastold(Goal, true, Next)) % Record solution
    ;
      freshcopy(Goal, Copy), % Copy of Goal
      useranswer(Goal, Copy, Trace, Answer, Next1)
    ).

process(no, Goal, _, _, _, false, N) :-
    freshcopy(Goal, Copy),
    wastold(Copy, true, _), !, % 'no' means: no more solutions
    assertz(end_answers(Goal)),
    fail
    ;
    nextindex(Next), % Next free index for 'wastold'
    assertz(wastold(Goal, false, Next)).

%----------------------------------------------------------------------

format(Var, Name, Name, Vars, [Var/Name | Vars]) :-
    var(Var), !.

format(Atom, Name, Atom, Vars, Vars) :-
    atomic(Atom),
    atomic(Name).

format(Goal, Form, Question, Vars0, Vars) :-
    Goal =.. [Functor | Args1],
    Form =.. [Functor | Forms],
    formatall(Args1, Forms, Args2, Vars0, Vars),
    Question =.. [Functor | Args2].

formatall([], [], [], Vars, Vars).

formatall([X|XL], [F|FL], [Q|QL], Vars0, Vars) :-
    formatall(XL, FL, QL, Vars0, Vars1),
    format(X, F, Q, Vars1, Vars).
    
%----------------------------------------------------------------------

askvars([]).

askvars([Variable/Name | Variables]) :-
    nl, write(Name), write(' = '),
    read(Variable),
    askvars(Variables).

%----------------------------------------------------------------------

showtrace([]) :-
    nl, write('This was your question.'), nl.

showtrace([Goal by Rule | Trace]) :-
    nl, write('To investigate, by '),
    write(Rule), write(', '),
    write(Goal),
    showtrace(Trace).

%----------------------------------------------------------------------

instantiated(Term) :-
    numbervars(Term, 0, 0).  % No variables in Term

%----------------------------------------------------------------------

% instance_of(T1, T2), instance of T1 is T2; that is,
% T1 is equally or more general than T2.

instance_of(Term, Term1) :-
    freshcopy(Term1, Term2),
    numbervars(Term2, 0, _), !,
    Term = Term2.

freshcopy(Term, FreshTerm) :- % Make a copy of Term with variables renamed.
    asserta(copy(Term)),
    (retract(copy(FreshTerm)) ; true), !. % added AJY 2012-06-27.

nextindex(Next) :-
    retract(lastindex(Last)), !,
    Next is Last + 1,
    assert(lastindex(Next)).

%----------------------------------------------------------------------

% Initialize dynamic procedures lastindex/1, wastold/3, end_answers/1

:- assertz(lastindex(0)).
:- assertz(wastold(dummy, false, 0)).
:- assertz(end_answers(dummy)).

%----------------------------------------------------------------------


% Displaying the conclusion of a consultation and 'how' explanation

present(Answer) :-
    nl, showconclusion(Answer),
    nl, nl, write('Would you like to see how? '),
    getreply(Reply),
    ( Reply = yes, !,
      show(Answer)  % Show it.
      ;
      true           % Don't show it.
    ).


showconclusion(Answer1 and Answer2) :- !,
    showconclusion(Answer1),
    write(' AND '),
    showconclusion(Answer2).

showconclusion(Conclusion was Found) :-
    write(Conclusion).

%----------------------------------------------------------------------

% The predicate 'show' displays a complete solution tree.

show(Solution) :-
    nl, show(Solution, 0), !.   % indent by zero spaces.

show(Answer1 and Answer2, H) :- !, % indent by H spaces.
    show(Answer1, H),
    tab(H), write(' AND '), nl,
    show(Answer2, H).

show(Answer was Found, H) :-
    tab(H), writeans(Answer),
    nl, tab(H),
    write(' was '),
    show1(Found, H).

show1(Derived from Answer, H) :- !,
    write(Derived), write(' from '), % show rule name
    nl,
    H1 is H + 4,
    show(Answer, H1). % show antecedent

show1(Found, _) :- % Found == 'told' or 'found as fact')
    write(Found), nl.

writeans( Goal is true) :- !,
    write(Goal).  % Omit the words 'is true' on output

writeans(Answer) :- % We land here at a negative answer.
    write(Answer).

%----------------------------------------------------------------------

% The top-level driving procedure.

system :- % This should be made to work.  Backtrack to the ; .
    repeat,
    expert,
    nl, write('Continue? '),
    getreply(RPL),
    RPL = no.


expert :-
    getquestion(Q), % input the user's question
    ( answeryes(Q)  % Attempt to find a positive answer
    ;
      answerno(Q)   % If no pos answer then try to find a neg one
    ).

answeryes(Q) :- % search for positive answers to Q
    markstatus(negative), % No positive answer(s) yet
    explore(Q, [], A),    % The trace is empty
    positive(A),
    markstatus(positive), % Positive answer found
    present(A), nl,
    write('Want more solution(s)? '),
    getreply(Reply),
    Reply = no.

answerno(Q) :- % search for a negative answer(s) to Q
    (retract(no_positive_answer_yet) ; true), !, % added by AJY 2012-06-27
    explore(Q, [], A),    % The trace is empty
    negative(A),
    present(A), nl,
    write('Want more negative solution(s)? '),
    getreply(Reply),
    Reply = no.


markstatus(negative) :-
    assert(no_positive_answer_yet).

markstatus(positive) :-
    retract(no_positive_answer_yet), !
    ;
    true.

getquestion(Q) :-
    nl,
    write('Query to the knowledge base: '),
    nl,
    read(Q).

%----------------------------------------------------------------------



man(gustaf-vi-adolf).
man(carl-johan).
man(prins-bertil).
man(fredrik-ix-av-danmark).
man(sigvard).
man(prins-gustaf-adolf).
man(john-ambler).
man(johann-georg-av-hohenzollern).
man(niclas-silfverschiold).
man(tord-magnuson).
man(carl-xvi-gustaf).
man(prins-daniel).
man(prins-oscar).
man(prins-carl-philip).
man(prins-alexander).
man(christopher-o-neill).
man(prins-nicolas).

woman(margareta-av-storbritannien).
woman(louise-f-mountbatten).
woman(lilian-f-davies).
woman(prinsessan-ingrid).
woman(sibylla-av-sachsen-coburg-gotha).
woman(prinsessan-margaretha).
woman(prinsessan-birgitta).
woman(prinsessan-desiree).
woman(prinsessan-christina).
woman(silvia-f-sommerlath).
woman(kronprinsessan-victoria).
woman(prinsessan-estelle).
woman(prinsessan-sofia).
woman(prinsessan-madeleine).
woman(prinsessan-leonore).

husband(gustaf-vi-adolf, margareta-av-storbritannien).
husband(gustaf-vi-adolf, louise-f-mountbatten).
husband(prins-gustaf-adolf, sibylla-av-sachsen-coburg-gotha).
husband(fredrik-ix-av-danmark, prinsessan-ingrid).
husband(prins-bertil, lilian-f-davies).
husband(john-ambler, prinsessan-margaretha).
husband(johann-georg-av-hohenzollern, prinsessan-birgitta).
husband(niclas-silfverschiold, prinsessan-desiree).
husband(tord-magnuson, prinsessan-christina).
husband(carl-xvi-gustaf, silvia-f-sommerlath).
husband(prins-daniel, kronprinsessan-victoria).
husband(prins-carl-philip, prinsessan-sofia).
husband(christopher-o-neill, prinsessan-madeleine).

wife(margareta-av-storbritannien, gustaf-vi-adolf).
wife(louise-f-mountbatten, gustaf-vi-adolf).
wife(sibylla-av-sachsen-coburg-gotha, prins-gustaf-adolf).
wife(prinsessan-ingrid, fredrik-ix-av-danmark).
wife(lilian-f-davies, prins-bertil).
wife(prinsessan-margaretha, john-ambler).
wife(prinsessan-birgitta, johann-georg-av-hohenzollern).
wife(prinsessan-desiree, niclas-silfverschiold).
wife(prinsessan-christina, tord-magnuson).
wife(silvia-f-sommerlath, carl-xvi-gustaf).
wife(kronprinsessan-victoria, prins-daniel).
wife(prinsessan-sofia, prins-carl-philip).
wife(prinsessan-madeleine, christopher-o-neill).

father(gustaf-vi-adolf, prins-gustaf-adolf).
father(gustaf-vi-adolf, sigvard).
father(gustaf-vi-adolf, prinsessan-ingrid).
father(gustaf-vi-adolf, prins-bertil).
father(gustaf-vi-adolf, carl-johan).
father(prins-gustaf-adolf, prinsessan-margaretha).
father(prins-gustaf-adolf, prinsessan-birgitta).
father(prins-gustaf-adolf, prinsessan-desiree).
father(prins-gustaf-adolf, prinsessan-christina).
father(prins-gustaf-adolf, carl-xvi-gustaf).
father(carl-xvi-gustaf, kronprinsessan-victoria).
father(carl-xvi-gustaf, prinsessan-madeleine).
father(carl-xvi-gustaf, prins-carl-philip).
father(prins-daniel, prinsessan-estelle).
father(prins-daniel, prins-oscar).
father(prins-carl-philip, prins-alexander).
father(christopher-o-neill, prinsessan-leonore).
father(christopher-o-neill, prins-nicolas).

mother(margareta-av-storbritannien, prins-gustaf-adolf).
mother(margareta-av-storbritannien, sigvard).
mother(margareta-av-storbritannien, prinsessan-ingrid).
mother(margareta-av-storbritannien, prins-bertil).
mother(margareta-av-storbritannien, carl-johan).
mother(sibylla-av-sachsen-coburg-gotha, prinsessan-margaretha).
mother(sibylla-av-sachsen-coburg-gotha, prinsessan-birgitta).
mother(sibylla-av-sachsen-coburg-gotha, prinsessan-desiree).
mother(sibylla-av-sachsen-coburg-gotha, prinsessan-christina).
mother(sibylla-av-sachsen-coburg-gotha, carl-xvi-gustaf).
mother(silvia-f-sommerlath, kronprinsessan-victoria).
mother(silvia-f-sommerlath, prinsessan-madeleine).
mother(silvia-f-sommerlath, prins-carl-philip).
mother(kronprinsessan-victoria, prinsessan-estelle).
mother(kronprinsessan-victoria, prins-oscar).
mother(prinsessan-sofia, prins-alexander).
mother(prinsessan-madeleine, prinsessan-leonore).
mother(prinsessan-madeleine, prins-nicolas).

%% BEGINNING OF YOUR CODE

married(M,F):- wife(F,M), husband(M,F).
married(F,M):- wife(F,M), husband(M,F).

father_in_law(FiL, P):- married(P, Sp), father(FiL, Sp).
mother_in_law(MiL, P):- married(P, Sp), mother(MiL, Sp).

parent(P,C):- father(P,C); mother(P,C).
son(S, P):- man(S), parent(P, S).
daughter(D, P):- woman(D), parent(P, D).

grandfather(GP, GC):- parent(P, GC), father(GP, P).
grandmother(GM, GC):- parent(P, GC), mother(GM, P).

brother(B, C):- parent(P, C), son(B, P), C\=B.
sister(S, C):- parent(P, C), daughter(S, P), C\=S.

brother_in_law(BiL, P):- 
	married(P, Sp), brother(BiL, Sp); 
	sister(S, P), married(BiL, S); 
	married(P, Sp), sister(S, Sp), married(S, BiL).

sister_in_law(SiL, P):- 
	married(P, Sp), sister(SiL, Sp); 
	brother(B, P), married(SiL, B); 
	married(P, Sp), brother(B, Sp), married(B, SiL).

uncle(U, N):- 
	parent(P, N), brother(U, P); 
	parent(P,N), sister(S, P), husband(U, S).
aunt(A, N):- 
	parent(P, N), sister(A, P); 
	parent(P, N), brother(B, P), wife(A, B).

cousin(C, P):- uncle(U, P), father(U, C).
cousin(C, P):- aunt(U, P), mother(U, C).

nephew(N, M):- 
	brother(P, M), son(N, P); 
	sister(P, M), son(N, P);
	brother_in_law(P, M), son(N, P); 
	sister_in_law(P, M), son(N, P).

niece(N, M):- 
	brother(P, M), daughter(N, P); 
	sister(P, M), daughter(N, P);
	brother_in_law(P, M), daughter(N, P); 
	sister_in_law(P, M), daughter(N, P).

ancestor(A, Nu):- parent(A, Nu).
ancestor(A, Nu):- parent(A, X), ancestor(X, Nu).

%% END OF YOUR CODE



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%           DO NOT MODIFY THE FILE FROM HERE TO THE END            %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptest(P, A):-
	write('Exists '), write(P), write('/'), write(A), write('?'),
	(
	 current_predicate(P/A), !, write(' [YES]'), nl;
	 write(' [PREDICATE MISSING]'), nl
	).

stest(Term1, A, Term2):-
	current_predicate(Term1/A),
	write('Correct sex in '), write(Term1), write('/'), write(A),
	write('?'),
	(
	 functor(F1, Term1, 2),
	 call(F1),
	 arg(1, F1, A1),
	 functor(F2, Term2, 1),
	 arg(1, F2, A1),
	 \+ call(F2), write(' [NO]'), nl;
	 write(' [YES]'), nl
	).
stest(_, _, _).

test_([[P, A]|Ps]):-
	ptest(P, A),
	test_(Ps).
test_([[P, A, S]|Ps]):-
	A == 2,
	ptest(P, A),
	stest(P, A, S),
	test_(Ps).
test_([]).	

test_case(P, 1, Argument, ExpectedSet):-
	current_predicate(P/2),
	(write('Test case: '), write(P),
	 write('('), write(Argument), write(', X)'),
	 functor(F1, P, 2),
	 arg(1, F1, Argument),
	 findall(X, (call(F1), arg(2, F1, X)), Xs),
	 list_to_set2(Xs, Ys),
	 sort(Ys, Ys1), sort(ExpectedSet, ExpectedSet1),
	 Ys1 = ExpectedSet1, write(' [OK]'), nl;
	 write(' [INCORRECT]'), nl).
test_case(P, 2, Argument, ExpectedSet):-
	current_predicate(P/2),
	(write('Test case: '), write(P),
	 write('(X, '), write(Argument), write(')'),
	 functor(F1, P, 2),
	 arg(2, F1, Argument),
	 findall(X, (call(F1), arg(1, F1, X)), Xs),
	 list_to_set2(Xs, Ys),
	 sort(Ys, Ys1), sort(ExpectedSet, ExpectedSet1),
	 Ys1 = ExpectedSet1, write(' [OK]'), nl;
	 write(' [INCORRECT]'), nl).
test_case(_, _, _, _).

test_cases([[P, N, A, R]|Cs]):-
	test_case(P, N, A, R),
	test_cases(Cs).
test_cases([]).
	
test:-
	test_([[man, 1], [woman, 1], [husband, 2, man], [wife, 2, woman], [father, 2, man], [mother, 2, woman], [married, 2], [father_in_law, 2, man], [mother_in_law, 2, woman], [parent, 2], [son, 2, man], [daughter, 2, woman], [grandfather, 2, man], [grandmother, 2, woman], [brother, 2, man], [sister, 2, woman], [uncle, 2, man], [aunt, 2, woman], [cousin, 2], [brother_in_law, 2, man], [sister_in_law, 2, woman], [nephew, 2, man], [niece, 2, woman], [ancestor, 2]]),
	test_cases([
		    [uncle, 1, prins-daniel, [prinsessan-leonore, prins-nicolas, prins-alexander]],
		    [uncle, 2, prins-alexander, [prins-daniel, christopher-o-neill]],
		    [cousin, 2, prins-alexander, [prinsessan-estelle, prins-oscar, prinsessan-leonore, prins-nicolas]],
		    [son, 2, gustaf-vi-adolf, [prins-gustaf-adolf, sigvard, prins-bertil, carl-johan]],
		    [father_in_law, 1, gustaf-vi-adolf, [sibylla-av-sachsen-coburg-gotha, fredrik-ix-av-danmark, lilian-f-davies]],
		    [mother_in_law, 1, silvia-f-sommerlath, [prins-daniel, christopher-o-neill, prinsessan-sofia]],
		    [parent, 2, prins-bertil, [margareta-av-storbritannien, gustaf-vi-adolf]],
		    [grandfather, 1, carl-xvi-gustaf, [prinsessan-estelle, prins-oscar, prinsessan-leonore, prins-nicolas, prins-alexander]],
		    [grandmother, 1, silvia-f-sommerlath, [prinsessan-estelle, prins-oscar, prinsessan-leonore, prins-nicolas, prins-alexander]],
		    [brother, 1, prins-bertil, [prins-gustaf-adolf, sigvard, prinsessan-ingrid, carl-johan]],
		    [sister, 2, carl-xvi-gustaf, [prinsessan-margaretha, prinsessan-birgitta, prinsessan-desiree, prinsessan-christina]],
		    [aunt, 2, prins-carl-philip, [prinsessan-margaretha, prinsessan-birgitta, prinsessan-desiree, prinsessan-christina]],
		    [brother_in_law, 1, prins-daniel, [prinsessan-madeleine, prins-carl-philip, christopher-o-neill, prinsessan-sofia]],
		    [brother_in_law, 2, prins-daniel, [prins-carl-philip, christopher-o-neill]],
		    [sister_in_law, 2, prins-daniel, [prinsessan-madeleine, prinsessan-sofia]],
		    [sister_in_law, 1, silvia-f-sommerlath, [prinsessan-margaretha, prinsessan-birgitta, prinsessan-desiree, prinsessan-christina, john-ambler, johann-georg-av-hohenzollern, niclas-silfverschiold, tord-magnuson]],
		    [nephew, 2, prinsessan-sofia, [prins-oscar, prins-nicolas]],
		    [niece, 2, prinsessan-sofia, [prinsessan-estelle, prinsessan-leonore]],
		    [ancestor, 2, prins-oscar, [kronprinsessan-victoria, prins-daniel, margareta-av-storbritannien, sibylla-av-sachsen-coburg-gotha, silvia-f-sommerlath, gustaf-vi-adolf, prins-gustaf-adolf, carl-xvi-gustaf]]
		   ]), !.

list_to_set2([], Acc, Acc).
list_to_set2([X|Xs], Acc, Set):-
	member(X, Acc),
	list_to_set2(Xs, Acc, Set), !;
	list_to_set2(Xs, [X|Acc], Set).
list_to_set2([X|Xs], Set):-
	list_to_set2(Xs, [X], Set).
list_to_set2([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% END OF TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

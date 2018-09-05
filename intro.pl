% Add Constraint Logic Programming over Integers 
:- use_module(library(clpfd)).

% Imperative Programming: Programs are sequences of instructions. You tell the machine, step by 
% step, *how* to get what you want. Lots of control statements, mutation everywhere, etc.  These 
% make it harder to understand what the code is really doing (try it without a single-step 
% debugger). Can you reason about it without simulating the machine in your head?
%
% Functional Programming: Programs are functions (in the sense of mathematics). You tell the 
% machine which functions to compose to get what you want. No mutation, but you still own the
% control flow (if/then, case expressions, etc.). Easier to reason about as long as you can 
% understand the control flow. Functions satisfy equations, e.g. fib(n+2) = fib(n+1) + fib(n), 
% enabling *equational reasoning*.
%
% Logic Programming: Programs are queries against relations (in the sense of mathematics). You tell
% the machine 1) facts about these relations and 2) rules to infer new facts. Composition is by
% conjunciton and disjunction of predicates. No mutation, and no explicit control flow. Similar 
% to querying relations in SQL, but more powerful. 
% 
% Declarative Programming: Say *what you want*, not *how to get it*. The details of how to get it 
% are left to the computer to figure out. All programming languages support declarative methods to 
% some extent, but functional and logic languages take the lead. In logic programming, you
% query relations, but you don't specify *how* to query them.

% Q: What's a relation?
% A: It's a subset of a product. The underlying relation of a function f : a -> b is the subset 
% of the product (a, b) which picks out the pairs (x, y) such that f x == y.
%    
% In logic programming, we generalize to subsets of larger products (a_1, ..., a_n).

% Example of a fact about the list_length relation: the empty list has length 0
list_length([], 0).

% Example of a rule about the list_length relation: 
list_length([_|Ls], Len) :- % The length of a list [_|Ls] is Len whenever:
  Len0 #>= 0,               % Len0 is an integer greater than or equal to 0; and
  Len  #= Len0 + 1,         % Len is 1 greater than Len0; and
  list_length(Ls, Len0).    % Len0 is the length of Ls  

% The above defines a *relation* (not just a function) between lists and their lengths
% Standard function usage: list_length([1,2,3], N).
% N = 3.
%
% But because this is a full relation, we can also query it "backwards": 
% list_length(Xs, 3).   
% Xs = [_600, _942, _1284]
% This gives a list of three unknowns, as expected. Unlike functional programming which always 
% requires variables to be bound to values, logic programming allows logic variables to be 
% uninitialized
%
% We can also query the entire relation for values:
% list_length(Xs, N).
% Xs = [],
% N = 0 ;
% Xs = [_876],
% N = 1 ;
% Xs = [_876, _1708],
% N = 2 ;
% [...]
% In a sense, we have just taught the computer everything it needs to know about list lengths!


% Integer successor as a relation 
integer_succ(X, Y) :- Y #= X + 1.
% integer_succ(3, N).
% N = 4.
% 
% integer_succ(N, 3).
% N = 2. 
%
% integer_succ(N, M).
% N + 1 #= M.


% Relate lists Xs to pairs (X, Ys) where Xs can be obtained by inserting X somewhere into Ys
select_first(X, [Y|Xs], Xs)     :- X = Y.
select_first(X, [Y|Xs], [Y|Ys]) :- select_first(X, Xs, Ys).

% Use it to delete a specified element from a list
% select_first(3, [1,2,3,4], Ys).
% Ys = [1, 2, 4].
%
% Use it to determine which element was deleted from a list:
% select_first(X, [1,2,3,4], [1,2,4]).
% X = 3.
%
% Use it to determine all lists that could result in [1,2,4] if 3 was deleted:
% select_first(3, Xs, [1,2,4]).
% Xs = [3, 1, 2, 4] ;
% Xs = [1, 3, 2, 4] ;
% Xs = [1, 2, 3, 4] ;
% Xs = [1, 2, 4, 3] ;
% false.
% 
% This shows how the control flow works automatically: Prolog will backtrack through all possible
% answers without any additional effort. 


% Dot products of integer vectors  
dot([], [], 0).
dot([X|XS], [Y|YS], Result) :- Result #= (X * Y) + R, dot(XS, YS, R). 
% Vectors without the same length are rejected with no solution: 
% dot([1,2,3], [1,2], R).
% false.
%
% Correct forward mode usage:
% dot([1,2,3], [4,5,6], R).
% R = 32.
%
% Example usage in backwards mode to solve equations:
% Ys = [A,B,C], Ys ins 4..6, all_distinct(Ys), label(Ys), dot([1,2,3], Ys, 32).
% Ys = [4, 5, 6],
% A = 4,
% B = 5,
% C = 6 ; 


% A simple refinement type of integers: the natural numbers
nat(X) :- X #>= 0.
% nat(X).
% X in 0..sup.
% Ask for some examples of natural numbers:
% nat(X), X in 0..10, indomain(X).
% X = 0 ;
% X = 1 ;
% X = 2 ;
% X = 3 ;
% X = 4 ;
% X = 5 ;
% X = 6 ;
% X = 7 ;
% X = 8 ;
% X = 9 ;
% X = 10.


% Ranges of natural numbers
range(N, N, [N]).
range(M, N, [M|LS]) :- nat(M), nat(N), M #< N, M1 #= M + 1, range(M1, N, LS). 
% Get range from 3 to 8
% range(3, 8, Ns).
% Ns = [3, 4, 5, 6, 7, 8] .
%
% Get min and max for given range:
% range(Min, Max, [3, 4, 5, 6, 7, 8]).


% Typical if-then-else contortions in business logic:
%
% if(animal has wings) {
%   if(animal flies) {
%     hawk(); 
%   } else if(animal swims) {
%     penguin();
%   } else {
%     deadbird();
%   }
% } else {
%   if(animal has claws) {
%     if(eats burritos) {
%       racoon();
%     } else {
%       rat();
%     }
%   }
% }

animal_attr(hawk,     has_wings).
animal_attr(hawk,     flies).
animal_attr(penguin,  has_wings).
animal_attr(penguin,  swims).
animal_attr(deadbird, has_wings).
animal_attr(racoon,   has_claws).
animal_attr(racoon,   eats_burritos).
animal_attr(rat,      has_claws).

% An animal A matches a list of traits Ts whenever it has every such trait as an attribute
is_animal(_, []).
is_animal(A, [T|Ts]) :-
  animal_attr(A, T),
  is_animal(A, Ts).

% Forward mode usage:
% is_animal(A, [has_wings, flies]).
% A = hawk .
%
% Provide multiple answers in case of ambiguity:
% is_animal(A, [has_wings]).       
% A = hawk ;
% A = penguin ;
% A = deadbird .
%
% Rule out some possibilities:
% is_animal(A, [has_wings]), dif(A, hawk), dif(A, penguin).
% A = deadbird .
%
% Ask for all traits associated with an animal:
% setof(T, animal_attr(hawk, T), Ts).
%
% Do it for all animals:
% setof(T, animal_attr(A, T), Ts).


% map_list/3: Given a relation a ~ b, lift it to a relation on lists [a] ~ [b]:
map_list(_, [], []).
map_list(P, [X|Xs], [Y|Ys]) :-
  call(P, X, Y),
  map_list(P, Xs, Ys).
% Forward mode:
% map_list(integer_succ, [1,2,3], Ys).
% Ys = [2, 3, 4] .
%
% Backwards mode:
% map_list(integer_succ, Xs, [2,3,4]).
% Xs = [1, 2, 3] .

% map_list/2: Given a predicate P, succeed only if P succeeds for every element of the given list
map_list(_, []).
map_list(P, [X|Xs]) :-
  call(P, X),
  map_list(P, Xs).

% Find an element that is different from every element of a given list:
% map_list(dif(E), [1,2,3]), E in 1..4, indomain(E).
% E = 4 .


% Datatype example: Binary Trees
bintree(leaf(_)).
bintree(node(_, Left, Right)) :- bintree(Left), bintree(Right).
% Ask for some examples of binary trees:
% bintree(X).
% X = leaf(_9392) ;
% X = node(_9392, leaf(_9400), leaf(_9404)) ;
% X = node(_9392, leaf(_9400), node(_9404, leaf(_9412), leaf(_9416))) .

% Relate binary trees to their depths
bintree_depth(leaf(_), 0).
bintree_depth(node(_, Left, Right), N) :-
  N #>= 1,
  N #= max(NL, NR) + 1,
  bintree_depth(Left, NL),
  bintree_depth(Right, NR).
% Example usage in forward mode:
% bintree_depth(leaf(1), N).
% N = 0.
% bintree_depth(node(1, leaf(2), leaf(3)), N).
% N = 1.
% bintree_depth(node(0, node(1, leaf(2), leaf(3)), leaf(4)), N).
% N = 2.
%
% Backward mode: ask for all trees of a given depth:
% bintree_depth(Tree, 0). 
% Tree = leaf(_882).
%
% bintree_depth(Tree, 1).
% Tree = node(_864, leaf(_2890), leaf(_3160)) .
%
% bintree_depth(Tree, 2).
% Tree = node(_872, leaf(_2898), node(_3052, leaf(_5078), leaf(_5348))) ;
% Tree = node(_872, node(_2898, leaf(_5294), leaf(_5564)), leaf(_5594)) ;
% Tree = node(_872, node(_2898, leaf(_5294), leaf(_5564)), node(_5594, leaf(_7750), leaf(_8020))) .
%
% and so on... you can imagine the possibilities for test case generation!



% There is a street with three neighbouring houses that all have a different colour, namely red, 
% blue, and green. People of different nationalities live in the different houses and they all 
% have a different pet and sport.  Here are some more facts about them:
%
% 1. The Brazilian does not live in house two.
% 2. The person with the Dogs plays Basketball.
% 3. There is one house between the house of the person who plays Football and the Red house on 
%    the right.
% 4. The person with the Fishes lives directly to the left of the person with the Cats.
% 5. The person with the Dogs lives directly to the right of the Green house.
% 6. The German lives in house three.

% Q: Who keeps the Cats? Which Color house do they live in and which Sport do they play?
zebra(HouseNum, ColorName, NatName, PetName, SportName) :-
  % Define variables whose values will match a house number in 1..3
  Colors        = [Red, Green, Blue],
  Nationalities = [Australian, Brazilian, German],
  Pets          = [Cats, Dogs, Fishes],
  Sports        = [Basketball, Football, Soccer],
  Attributes    = [Colors, Nationalities, Pets, Sports],

  % Provide names for every variable 
  pairs_keys_values(ColorPairs, Colors, [red, green, blue]),
  pairs_keys_values(NatPairs, Nationalities, [australian, brazilian, german]),
  pairs_keys_values(PetPairs, Pets, [cats, dogs, fishes]),
  pairs_keys_values(SportPairs, Sports, [basketball, football, soccer]),

  append(Attributes, Vs),             % Flatten list of all variables
  Vs ins 1..3,                        % Say each variable has a value in 1..3: a house num
  maplist(all_distinct, Attributes),  % Each attribute consists of distinct values
  Brazilian #\= 2,                    % Hint 1 (in terms of identifying house numbers)
  Dogs      #= Basketball,            % Hint 2
  Football  #= 1,                     % Hint 3
  Red       #= 3,                     % Hint 3 (cont'd)
  Fishes    #= Cats - 1,              % Hint 4
  Dogs      #= Green + 1,             % Hint 5
  German    #= 3,                     % Hint 6

  member(HouseNum-NatName, NatPairs), % Pull solutions for each house number
  member(HouseNum-PetName, PetPairs),
  member(HouseNum-ColorName, ColorPairs),
  member(HouseNum-SportName, SportPairs).

% zebra(HouseNum, ColorName, NatName, PetName, SportName).

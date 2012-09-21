/*
Michal_Klein_Zapoctak.pl

Input assumptions:
    Input is always correct.
    RegExp assumption:
        - we don't have a sub regexp (). Meaning we don't have redundant () containing empty string
          at input
        - and we don't allow "+"^n, (n + in a row) bracketed subregexp or the whole regular expression equal to it.
            --it's also unnecessary. 
    Both RegExp assumptions are for convenience.

Definitions:
    Deterministic Finite Automaton (DFA):
        (Edges, Starting_State, Terminal_States) ,States and Alphabet
        where
            States are already listed in Edges.
            Alphabet is also already listed in Edges (DFA requairs explicit transition for every letter)
            Edges is a list of Edge.
            Terminal_States is a subset of States. 
            Starting_State is a starting state.
    Edge:
        State_A -> State_B/Character_List
        where 
            State_A and State_B are states.
            -> and / are operators
            Character_List is a transition from State_A to State_B.
                It's in a form of characters list, e.g. "a".
                Its length is 1.

    Regular Expression (RegExp):
        null is a RegExp of empty language L ( L == empty set)
        "" is a RegExp of empty string
        ( and ) are special characters
        *    is a Kleene star
        +    is an alternating operator.
        Everything else is a letter of the alphabet.
        For example RegExp (b+(a+)) takes language L = {a,}. (a and empty string)

    Equations:
        It's a list of Equation. Also, due to the manner in which we 
        construct them, head of Equations is the main equation. 
        The main equation corresponds to the Starting_State.
    Equation:
        Each corresponds to a state of DFA.

        Logic behind an equation:
        Corresponding_State = Transition_1.State_j_1 + .. + Transition_n.State_j_n + Transition_n+1.null + Transition_n+2.terminal
        State_j_i are states and Transition_i is a transition which we can use to get to State_j_i
        from Corresponding_State.
        Transition_n+1 is created after removing recursion and when Corresponding_State is not a terminal state.
            It can still play a role when Corresponding_State gets substituted into another one.
        null is here just to help us distinguish that it's not a regular state.
        Transition_n+2 is at first created at every Corresponding_State that is termial (with empty string value). It can get substituetd into non-terminal 
            states as well. 
        terminal here is to distinguish that we can stop calculating and get a word from this state (if it's reachable from Starting_State).
        
        Implementation:
        (Corresponding_State, Paths_And_States)
        where 
            Paths_And_States is a list of (Transition, State_Or_null_Or_terminal).

        Note: States here are also in a sense variable. And we're trying to solve the value of Starting_State.    

    Non-deterministic finite automaton (NFA):
        (States, NFA_Edges, Starting_State, Terminal_States)
        Starting_State, Terminal_States are same as in DFA.
        States here are mentioned explicitely. Here it's no extra work inputing them and it can be more clear.
        NFA_Edges:
            List of NFA_Edge
        NFA_Edge:
            (State_A->State_B/Character_Transition)
            where
                Character_Transition is a String    

Author Michal Klein
Version 1.1                    */


/*
    Here starts a block of code with test date.
*/


%% graph(Graph_ID, Edges, Startin_State, Terminal_States)
%
%  Test data - DFAs
graph(dfa_0, [0->1/"a", 0->2/"b", 1->3/"a", 1->4/"b", 2->3/"b", 2->4/"a", 3->4/"a", 3->4/"b", 4->4/"a", 4->4/"b"], 0, [3,0]).
graph(dfa_1, [0->1/"a", 1->0/"a"], 0, [0]).
graph(dfa_2, [0->0/"a"], 0, []).
graph(dfa_3, [0->1/"a", 1->1/"a"], 0, [0]).
graph(dfa_4, [0->1/"a", 1->2/"a", 2->3/"a", 3->3/"a"], 0, [0,1,2,3]).


%% regExp(+RegExp_ID, -RegExp)
%
%  Test data - RegExps
regExp(re_0, "abc").
regExp(re_1, "a*(b)+cd").
regExp(re_2, "a+").
regExp(re_3, "((aa)*+(bb)*)+c*+(d)*").
regExp(re_4, null).
regExp(re_5, "").
regExp(re_6, "+a"). 
regExp(re_7, "+++++++++a").
regExp(re_8, "(ab+cd)*").
regExp(re_9, "((a+b)+(b+c)+dd*)+").
regExp(re_10, "ab*c").
regExp(re_11, "a(+b)c").


/*
    Here begins the code for converting DFA to a regular expression.
*/


%  graph(Graph_ID, States, Edges, Starting_State, Terminal_States)
%  Definition of opperators for a graph


%  I commented out the first operator definition because it was messing with built-in if then else.
%  After commenting it out graphs are still working.
%  However, NFA output edges are enclosed in brackets.
%  :-op(500,xfy,->). 


:-op(450,xfy,/). 


%% convert_DFA_To_Equations(+[Starting_State], +Edges, +Terminal_States, +Visited_States, -Equations)   
%
%  We use graph search to traverse the graph. We visit all reachable states from Starting_State.
%  For each state we create an equation.
%  Visited_States input must be [].
%  EXAMPLE
%    We know of state 0: 0->1/"a" 0->2/"b" 0->0/"c"
%    Equation corresponding to sate 0 would look like this :
%    0 = "a"1 + "b"2 + "c"0
%    We format it in this way:
%    (0, [("a", 1), ("b", 2), ("c", 0)])


%  If we've visited all states we can get to from Starting_State, we've finnished creating equations necessary to compute regular expression.
%  Thus, we're stopping recursion.
convert_DFA_To_Equations([],_,_,_,[]).


%  Visits all reachable states from Starting_State. Creates equation for each state.
convert_DFA_To_Equations([State_in|States_in], Edges, Terminal_States, Visited_States_in, [Equation|Equations]):-
    append([State_in], Visited_States_in, Visited_States_out),
    findall(
            (Connecting_Path, Neighbour),
            edge(Edges, State_in, Neighbour, Connecting_Path),
            Paths_And_States_raw
    ),
    create_Equation(State_in, Paths_And_States_raw, [], Equation_tmp), 
    add_Terminal(Equation_tmp, Terminal_States, Equation),
    findall( 
            State,
            (Equation_tmp = (State_in, Paths_And_States), states(Paths_And_States, State), \+member(State, Visited_States_out), \+member(State, States_in) ),
            New_States
    ),
    append(New_States, States_in, States_out),
    convert_DFA_To_Equations(States_out, Edges, Terminal_States, Visited_States_out, Equations).


%% edge(+Edges, +State, -Neighbour, -Connecting_Path)
%
%  Finds an edge (Neighbour,Connecting_Path) from State 
edge(Edges, State, Neighbour, Connecting_Path) :-
    member(State->Neighbour/Connecting_Path, Edges).


%% states(+Paths_And_States, -Return_State)
%
%  Returns a state from Paths_And_States
states([(_, State)|_], State).
states([_|Paths_And_States],State):-
    states(Paths_And_States,State).

%% create_Equation(+State, +Paths_And_States_raw, +Expression_Accumulator, -Equation)
%
%  Expression_Accumulator must be input as [].
%  Creates equation where left side is State and right side is compressed Paths_And_States_raw
%  Compressed means that there can't be multiple states a,b that a==b and both are on the right side.


create_Equation(State, [], Expression, (State, Expression)).


create_Equation(State, [(Transition, State_in)|Paths_And_States_raw], Expression_in, Equation):-
    add_State_To_Equation(State_in, Transition, (State, Expression_in), (State, Expression_out)),
    create_Equation(State, Paths_And_States_raw, Expression_out, Equation).


%% add_Terminal(+Equation_in, +Terminal_States, -Equation_out)
%
%  Adds [] transition from State to "terminal" into Equation_in if State is a terminal state
%  [] is an empty transition, length 0
add_Terminal((State, Expression), Terminal_States, Equation_out):-
    member(State, Terminal_States), 
    !,
    add_State_To_Equation(terminal, [], (State, Expression), Equation_out).


%  Cut guarantees State is not a terminal state
add_Terminal(Equation, _, Equation).


%% add_State_To_Equation(+State, +Transition, +Equation_in, -Equation_out)
%
%  We're adding (Transition, State) to Equations_in.


%  State is already in Equation_in. 
%  Therefore, we have to change (Transition_old,State) into ((Transition+Transition_old), State)
add_State_To_Equation(State, Transition, Equation_in, Equation_out):-
    (Left_Side, Right_Side_old) = Equation_in,
    contains_State(Right_Side_old, State),
    !,
    update_State_Transition(State, Transition, Right_Side_old, Right_Side_out),
    Equation_out = (Left_Side, Right_Side_out).


%  If State is not in Equation_in, simply append (Transiion,State) and Equation_in
add_State_To_Equation(State, Transition, Equation_in, Equation_out):-
    (Left_Side, Right_Side_old) = Equation_in,
    Right_Side_out = [(Transition, State)|Right_Side_old],                
    Equation_out = (Left_Side, Right_Side_out).


%% update_State_Transition(+State, +New_Transition, +Right_Side_in, -Right_Side_out)
%
%  State is already on the right side of the equation and we need to add New_Transition.
%  We need to find (Old_Transition, State) and change it to ( (New_Transition+Old_Transition) ,State)


%  We found the State that needs adjustement to its Transition
update_State_Transition(State, New_Transition, [(Transition, State)|Right_Side], [(Adjusted_Transition, State)|Right_Side]):-
    append("(", New_Transition, New_Transition_tmp),
    append(Transition, ")", Transition_tmp0),
    append("+", Transition_tmp0, Transition_tmp1),
    append(New_Transition_tmp, Transition_tmp1, Adjusted_Transition),
    !.
    

%  We haven't found the state we're looking for yet. 
update_State_Transition(State_in, New_Transition, [(Transition, State)|Right_Side_in], [(Transition, State)|Right_Side_out]):-
    update_State_Transition(State_in, New_Transition, Right_Side_in, Right_Side_out).


%% solve_Equations(+Equations, -RegularExpression)
%
%  head of Equations is the main equation. It corresponds to the starting state of the DFA
%  these equations describe. 
%
%  First we elimite recursion from all equations.
%  Then, if the main equation has a state variable,
%  we remove equation corresponding to the state variable and substitute it
%  into all equations the state variable is in. 
%  If the main equation is not solved yet, meaning it still has a state variable, we
%  repeate the whole process.
%  Else, we retrieve the solution. Either a null or a string version of RegExp


%  This means calculation is over as we've substituted all variables.
solve_Equations([Main_Equation|_], Solution):-
    no_State_Variable(Main_Equation), 
    retrieve_Solution(Main_Equation, Solution).


%  Only equation remaining is the main equation. We make sure there's no recursion and retrieve the solution.
solve_Equations([Main_Equation], Solution):-
    remove_All_Recursions([Main_Equation], [Main_Equation_tmp]),
    retrieve_Solution(Main_Equation_tmp, Solution).


%  We remove recursion from all equations. Then we pick the first state variable
%  from the right side of the main equation, remove its corresponding equation and
%  remove substitute it into all equations containing that variable.
solve_Equations(Equations_in, Solution):-
    remove_All_Recursions(Equations_in, Equations_tmp0),
    head(Equations_tmp0, Main_Equation),                                 
    first_Variable(Main_Equation, State_Variable),
    remove_Equation(Equations_tmp0, State_Variable, Removed_Equation, Equations_tmp1),
    substitute(Removed_Equation, Equations_tmp1, Equations_out),
    solve_Equations(Equations_out, Solution).


%% retrieve_Solution(+Equation, -Solution)
%
%  Return regexp from a pair (RegExp, terminal) on the right side of Equation.
%  Note: terminal is only in terminal states at first, however we've substituted all 
%  reachable terminal states into the main equation by now.


retrieve_Solution((_, Paths_And_States), Solution):-
    member((Solution, terminal), Paths_And_States),
    !.


%  This is a special case. This happens when we're retrieving
%  the solution but there's no (_, terminal) in equation. 
%  That means DFA's langugae is an empty set.
retrieve_Solution(_, null).


%% remove_All_Recurions(+Equations, -Equations_Without_Recursion)
%
%  Transforms equations from Equations if they contain recursion
%  to their equivalent and non-recursive form.
%  S = aS + bQ  into S = a*bQ


remove_All_Recursions([], []).


%  Current equation has recursion. We remove it and continue processing the rest.
remove_All_Recursions([Equation_in|Equations_in], [Equation_out|Equations_out]):-
    has_Recursion(Equation_in),
    !,                
    remove_Recursion(Equation_in, Equation_out),
    remove_All_Recursions(Equations_in, Equations_out).


%  Current equation doesn't have recursion.
remove_All_Recursions([Equation|Equations_in], [Equation|Equations_out]):-
    remove_All_Recursions(Equations_in, Equations_out).


%% has_Recursion(+Equation)
%
%  Checks if the state variable on the left side of the equation is present on the right side.
has_Recursion((Left_Side_State, Right_Side)):-
    member((_, Left_Side_State), Right_Side).


%% remove_Recursion(+Equation_With_Recursion, -Equation_Without_Recursion)
%
%  We know equation on the input has recursion. 
%  We remove it by removing the (Transition, Recursive_State) and appending Transition*
%  to all other transitions in the expression.


%  Due to the fact that we remove the State from expression first, we need to
%  check special case - it being the only state in expression
remove_Recursion((State, [(Transition, State)]), (State, [(Adjusted_Transition, null)])):-
    append(Transition, ")*", Adjusted_Transition_tmp),
    append("(", Adjusted_Transition_tmp, Adjusted_Transition).
    

remove_Recursion((Left_Side, Right_Side_in) , (Left_Side, Right_Side_out)):-
    remove_State(Left_Side, Right_Side_in, Transition, Right_Side_tmp),
    append(Transition, ")*", Transition_tmp0),
    append("(", Transition_tmp0, Transition_tmp1),
    append_Transition(Transition_tmp1, Right_Side_tmp, Right_Side_out).


%% remove_State(+State, +Paths_And_States_in, -Transition, -Paths_And_States_out)
%
%  Removes (Transition, State) from Paths_And_States_in and returns the result of the deletion
%  and the removed transition.


remove_State(State, [(Transition, State)|Transitions_And_States_in], Transition, Transitions_And_States_in):-
    !.    


remove_State(State, [(Transition_in, State_in)|Transitions_And_States_in], Transition, [(Transition_in, State_in)|Transitions_And_States_out]):-
    remove_State(State, Transitions_And_States_in, Transition, Transitions_And_States_out).


%% head(+List, -Head_Of_The_List)
%
%  Returns head of the list.
head([Head|_], Head).


%% first_Variable(+Equation, -State_Variable)
%
%  Returns first state variable from the rigt side of the equation
%  Is true if state variable is in the expression on the right side.


first_Variable((_, [(_, State)|_]), State):-
    State \== null, State \== terminal,
    !.


first_Variable((_, [_|Right_Side]), State):-
    first_Variable((_, Right_Side), State).


%% remove_Equation(+Equations_in, +Equation_State, -Removed_Equation, -Equations_out):-
%
%  Removes Removed_Equation from Equations_in by finding Equation_State on the left side of Removed_Equation. 
%  Returns the rest of the equations and Removed_Equation.


remove_Equation([Equation|Equations_in], Equation_State, Equation, Equations_in):-
    (Equation_State, _) = Equation,
    !.

remove_Equation([Equation|Equation_in], Equation_State, Removed_Equation, [Equation|Equations_out]):-
    remove_Equation(Equation_in, Equation_State, Removed_Equation, Equations_out).
    

%% no_State_Variable(Equation)
%
%  True, if in the expression on the right side of the equation there are no state variables.
%  Only thing remaining on the right side must be a null tag or terminal tag.
no_State_Variable((_, Expression)):-
    findall(
            Variable,
            (
                member((_,Variable), Expression),        % equivalent to contains_State(Right_Side, Variable)
                Variable \== null,
                Variable \== terminal
            ),
            Vars
    ),
    Vars == [].


%%    substitute(+Equation, +Equations_in, -Equations_out)
%
%  Substitue state variables corresponding to Equation
%  to all equations from Equations_in.
%  Equation isn't in Equations_in.


substitute(_, [], []).


%  We substitute in for the state variable.
substitute((State, Substitution), [(Left_Side, Right_Side_in)|Equations_in], [(Left_Side, Right_Side_out)|Equations_out]):-
    contains_State(Right_Side_in, State),
    !,
    remove_State(State, Right_Side_in, Transition, Right_Side_tmp),
    append_Transition(Transition, Substitution, Adjusted_Substitution),
    substitute_State(Adjusted_Substitution, Right_Side_tmp, Right_Side_out),
    substitute((State, Substitution), Equations_in, Equations_out).


%  Doesn't contain the state variable that needs to be substituted so we just continue.
substitute(Substituting_Equation, [Equation_in|Equations_in], [Equation_in|Equations_out]):-
    substitute(Substituting_Equation, Equations_in, Equations_out).


%% substitute_State(+Substitution, +Expression_in, -Expression_out)
%
%  Substitution is an expression which we need to add to Expression_in 
%  We do that by adding each (Transition, State) pair to the Expression_in
%  one by one. 


substitute_State([], Expression, Expression).


substitute_State([(Transition, State)|Substitution], Expression_in, Expression_out):-
    add_State_To_Equation(State, Transition, (_, Expression_in), (_, Expression_tmp)),
    substitute_State(Substitution, Expression_tmp, Expression_out).


%% append_Transition(+Adjusting_Transition, +Paths_And_States, -Adjusted_Paths_And_States)
%
%  Appends Adjusting_Transition to each Path of Path_And_States.
%  (P,S) -> (T.P,S)    where . is concat, T is Adjustring_Transition, (P,S) is from Paths_And_States


append_Transition(_, [], []).


append_Transition(Transition, [(Path, State)|Paths_And_States_in], [(Adjusted_Path, State)|Paths_And_States_out]):-
    append(Transition, Path, Adjusted_Path),
    append_Transition(Transition, Paths_And_States_in, Paths_And_States_out).


%% contains_State(+Paths_And_States, +State)
%
%  Checks if Paths_And_States contains a (path,state) pair where state is State.
%  True only if (_, State) is a member of Paths_And_States    
contains_State(Paths_And_States, State):-
    member((_, State), Paths_And_States).


%% convert_DFA_To_RegExp(+DFA_Graph, -RegularExpression)
%
%  First, we convert DFA to a set of equations.
%  We solve those and return a regexp in string or null.
%  null means empty set.


%  Input method 1 - graoh(Edges, Starting_State, Terminal_States)
convert_DFA_To_RegExp(Graph, Str_RegExp):-
    graph(Edges, Starting_State, Terminal_States) = Graph,
    !,
    convert_DFA_To_Equations([Starting_State], Edges, Terminal_States, [], Equations),
    solve_Equations(Equations, RegExp),
    ( RegExp == null ->
        Str_RegExp = null
    ;
        name(Str_RegExp, RegExp)
    ).


%  Input method 2 - ID of a graph from samples.
convert_DFA_To_RegExp(Graph_ID, Str_RegExp):-
    graph(Graph_ID, Edges, Starting_State, Terminal_States),
    convert_DFA_To_Equations([Starting_State], Edges, Terminal_States, [], Equations),
    solve_Equations(Equations, RegExp),
    ( RegExp == null ->
        Str_RegExp = null
    ;
        name(Str_RegExp, RegExp)
    ).


/*
 Here starts the code for transforming regular expression to NFA (non-deterministic finite automaton)
*/

%% special_Character(+Character).
%
%  Checks if Character is special in regular expressions
%  Here are predefined special characters.
special_Character(0'+).
special_Character(0'*).
special_Character(0'().
special_Character(0')).



%% enumerate_Letters(+RegExp, +Starting_Index, -Enumerated_RegExp)
%
%  We enumarate each character that is not special.
%  Starting index is a number we start numbering letters from.


enumerate_Letters([], _, []).


%  We leave special characters as they were.
enumerate_Letters([Char|RegExp], Index, [Char|Enumerated_RegExp]):-
    special_Character(Char),
    !,
    enumerate_Letters(RegExp, Index, Enumerated_RegExp).


%  Letter L is enumerated with in pair (L, Index). 
enumerate_Letters([Char|RegExp], Index_in, [Enumerated_Letter|Enumerated_RegExp]):-
    Enumerated_Letter = (Char, Index_in),
    Index_out is Index_in + 1,
    enumerate_Letters(RegExp, Index_out, Enumerated_RegExp).



%% find_Starting_Letters(+Enumerated_RegExp, +Lambda_Flag, -Starting_Letters, -Empty_Word)
%
%  Finds all possible starting letters (Starting_Letters) for words in a regular expression.
%  If empty word is part of the language, we set Empty_Word flag true.
%  Empty_Word is set to false at the beggining.
%  Lambda_Flag is true at the beggining.


%  Lambda_Flag is true. That means empty word is part of the language of the regexp.
%  Thus, we set Empty_Word to true.
find_Starting_Letters([], true, [], Empty_Word):-
    Empty_Word = yes.


%  Lambda_Flag is false. But, since we process regexp divided by +,
%  some previous subregexp could have contained empty word and changed Empty_Word to true.
%  Empty_Word is false when procedure is first called, so we let it be.
find_Starting_Letters([], false, [], Empty_Word):-
    ( (Empty_Word == yes) ->
        true
    ;
        Empty_Word = no
    ).


%  Lambda_Flag is true and we found a letter (not special character).
%  That means it can be starting letter. We continue with Lambda_Flag false.
find_Starting_Letters([Char|RegExp], true, [Char|Starting_Letters], Empty_Word):-
    \+ special_Character(Char),
    !,
    find_Starting_Letters(RegExp, false, Starting_Letters, Empty_Word).


%  Lambda_Flag is true and we found a left parenthesis.
%  Since all starting letters from subregexp from within the brackets are also starting
%  letters of our regexp, we recursively calculate them and append them.
%  If subregexp contained empty word, we continue with Lambda_Flag true, else false.
find_Starting_Letters([Left_Parenthesis|RegExp], true, Starting_Letters, Empty_Word):-
    Left_Parenthesis ==  0'(,        
    !,
    %  We call getRegExp predicate with constant -1, meaning we're on layer -1,
    %  which is a subregexp inside of brackets
    getRegExp(RegExp, -1, Sub_RegExp, RegExp_rest),
    find_Starting_Letters(Sub_RegExp, true, Starting_Letters_tmp, Empty_Word_tmp),
    ( (Empty_Word_tmp == yes) ->    
        Lambda_Flag = true
    ;    
        Lambda_Flag = false
    ),
    append(Starting_Letters_tmp, Starting_Letters_rest, Starting_Letters),
    find_Starting_Letters(RegExp_rest, Lambda_Flag, Starting_Letters_rest, Empty_Word).


%  Lambda_Flag is true and we found a plus, which means we've processed one subregexp.
%  We know this subregexp contains empty word, therefore the whole regexp contains empty word as well.
%  Thus, we set Empty_Word flag true.
%  We continue processing another subregexp because we need its starting letters and reset Lambda_Flag.
find_Starting_Letters([Plus|RegExp], true, Starting_Letters, Empty_Word):-
    Plus == 0'+,
    !,
    Empty_Word = yes,
    find_Starting_Letters(RegExp, true, Starting_Letters, Empty_Word).


%  Lambda_Flag is true and we know _ is *. That means we've just processed
%  (Sub_RegExp) which contained empty word and it set lambda flag to true.
%  We skip * and continue processing regexp.
find_Starting_Letters([_|RegExp], true, Starting_Letters, Empty_Word):-
    find_Starting_Letters(RegExp, true, Starting_Letters, Empty_Word).


%  Lambda_Flag is false and we found *.
%  We can set Lambda_Flag true and continue processing regexp, because
%  if we have false flag and hit a letter, we skip everything until
%  we find a new relevant subregexp.
find_Starting_Letters([Star|RegExp], false, Starting_Letters, Empty_Word):-
    Star == 0'*,
    !,
    find_Starting_Letters(RegExp, true, Starting_Letters, Empty_Word).


%  Lambda_Flag is false and we found +.
%  This means, that we've just finnished processing this subregexp so we begin
%  processing another subregexp with reset Lambda_Flag to true.
find_Starting_Letters([Plus|RegExp], false, Starting_Letters, Empty_Word):-
    Plus == 0'+,
    !,
    find_Starting_Letters(RegExp, true, Starting_Letters, Empty_Word).


%  can be letter OR (, which means we've found all starting letters.
%  Lambda_Flag is false. We found a letter or (. 
%  We know we found all starting letters of this subregexp.
%  Remember, there are no empty brackets () containing no letters inside them in our regexps.
%  So we skip until we find another relevant subregexp and we continue processing it.
find_Starting_Letters([Letter_LeftPar|RegExp], false, Starting_Letters, Empty_Word):-
    ( Letter_LeftPar == 0'(  ->    
        skip_Irrelevant_Characters(RegExp, -1, RegExp_rest)
    ;
        skip_Irrelevant_Characters(RegExp, 0, RegExp_rest)
    ),
    find_Starting_Letters(RegExp_rest, false, Starting_Letters, Empty_Word).


%% skip_Irrelevant_Characters(+RegExp, +Layer, -Rest_Of_The_RegExp)
%
%  This procedures skips until the end of the regular expression
%  OR until + character separating regular expressions on the upper level.


%  We hit the end of regexp.
%  0 can be replaced with _. I kept it there for error testing just in case.
skip_Irrelevant_Characters([], 0, []).


%  We hit the plus character separating regexps.
%  We return plus as well, because find_Starting_Letters will use it 
%  to set a Lambda_Flag to true.
skip_Irrelevant_Characters([Plus|RegExp_rest], 0, [Plus|RegExp_rest]):-
    Plus == 0'+.


skip_Irrelevant_Characters([Left_Parenthesis|RegExp], Layer, RegExp_rest):-
    Left_Parenthesis == 0'(,
    !,
    Layer_new is Layer - 1,
    skip_Irrelevant_Characters(RegExp, Layer_new, RegExp_rest).


skip_Irrelevant_Characters([Right_Parenthesis|RegExp], Layer, RegExp_rest):-
    Right_Parenthesis == 0'),
    !,
    Layer_new is Layer + 1,
    skip_Irrelevant_Characters(RegExp, Layer_new, RegExp_rest).


%  _ can only be a letter, * or +. We just skip these.
skip_Irrelevant_Characters([_|RegExp], Layer, RegExp_rest):-
    skip_Irrelevant_Characters(RegExp, Layer, RegExp_rest).


%% find_Ending_Letters(+RegExp, +Ending_Letters)
%
%  Finds all possible ending letters in a language described by a RegExp.
%  We reverse RegExp and find all possible starting letters.
find_Ending_Letters(RegExp, Ending_Letters):-
    reverse(RegExp, RegExp_tmp),
    relocate_Stars_And_Switch_Brackets(RegExp_tmp, false, RegExp_rev),
    find_Starting_Letters(RegExp_rev, true, Ending_Letters, _).        


%% getRegExp(+RegExp, +Layer, -Sub_RegExp, -RegExp_rest)
%
%  Extracts Sub_RegExp from within brackets and returns rest of the RegExp.
%  It assumes we're already inside the brackets.
%  We add all characters until we find enclosing bracket for the whole Sub_RegExp.
%  Other brackets are used for adjusting Layer, so we can know when we've reached the end.


%  We just found the end of subexpression and it's enclosing parenthesis.
getRegExp([Right_Parenthesis|RegExp_rest], -1, [], RegExp_rest):-
    Right_Parenthesis == 0'),
    !.
    

%  This is a special case of enclosing parenthesis for reversed brackts
getRegExp([Left_Parenthesis|RegExp_rest], 1, [], RegExp_rest):-
    Left_Parenthesis == 0'(,
    !.


getRegExp([Left_Parenthesis|RegExp], Layer, [Left_Parenthesis|Sub_RegExp], RegExp_rest):-
    Left_Parenthesis == 0'(,
    !,
    Layer_new is Layer - 1,
    getRegExp(RegExp, Layer_new, Sub_RegExp, RegExp_rest).


getRegExp([Right_Parenthesis|RegExp], Layer, [Right_Parenthesis|Sub_RegExp], RegExp_rest):-
    Right_Parenthesis == 0'),
    !,
    Layer_new is Layer + 1,
    getRegExp(RegExp, Layer_new, Sub_RegExp, RegExp_rest).


getRegExp([Letter_Star_Plus|RegExp], Layer, [Letter_Star_Plus|Sub_RegExp], RegExp_rest):-
    getRegExp(RegExp, Layer, Sub_RegExp, RegExp_rest).


%% relocate_Stars_And_Switch_Brackets(+Simply_Reversed_RegExp, -Valid_Reversed_RegExp)
%
%  For every Letter l, Switches ) to (, ( to ) and *l to l*, and *)subregexp( to (subregexp)*

relocate_Stars_And_Switch_Brackets(Simply_Reversed_RegExp, Valid_Reversed_RegExp):-
    relocate_Stars_And_Switch_Brackets(Simply_Reversed_RegExp, false, Valid_Reversed_RegExp).


%% relocate_Stars_And_Switch_Brackets(+Simply_Reversed_RegExp, +Star_Flag, -Valid_Reversed_RegExp)
%
%  Star_Flag is false at input.
%  When Star_Flag is true, we don't check for + or *, because if one of those characters was there
%  it'd mean we're processing invalid regular expression.


%  Recursion end. false constant is for error testing. 
%  _ is just as good. true cannot happen, that'd would mean that
%  the original regexp started with *.
relocate_Stars_And_Switch_Brackets([], false, []).



%  Star_Flag is true and we've found the beginnigh of a subexpr.
%  Here we recursively process it and add parenthesis' and * accordingly.
%  Then we continue processing the rest of the regexp.
relocate_Stars_And_Switch_Brackets([Right_Parenthesis|RegExp], true, Valid_RegExp):-
    Right_Parenthesis == 0'),
    !,
    % We call getRegExp predicate with constant 1 because we're getting into brackets and brackets are reversed
    getRegExp(RegExp, 1, SubRegExp, RegExp_rest),
    relocate_Stars_And_Switch_Brackets(SubRegExp, false, Valid_Sub_RegExp),
    append([0'(|Valid_Sub_RegExp], [0'),0'*|Valid_RegExp_rest], Valid_RegExp),
    relocate_Stars_And_Switch_Brackets(RegExp_rest, false, Valid_RegExp_rest).


% Star_Flag is true and we found a letter. We merely place * after it and continue.
relocate_Stars_And_Switch_Brackets([Letter|RegExp], true, [Letter, 0'*|Valid_RegExp]):-
    relocate_Stars_And_Switch_Brackets(RegExp, false, Valid_RegExp).
    

%  Star_Flag is false. We just replace ) with ( and continue.
relocate_Stars_And_Switch_Brackets([Right_Parenthesis|RegExp], false, [0'(|Valid_RegExp]):-
    Right_Parenthesis == 0'),
    !,
    relocate_Stars_And_Switch_Brackets(RegExp, false, Valid_RegExp).


%  Star_Flag is false. We replace ( with ) and continue.
relocate_Stars_And_Switch_Brackets([Left_Parenthesis|RegExp], false, [0')|Valid_RegExp]):-
    Left_Parenthesis == 0'(,
    !,
    relocate_Stars_And_Switch_Brackets(RegExp, false, Valid_RegExp).


%  Star_Flag is false. We found  a* so we set Star_Flag true and continue.
relocate_Stars_And_Switch_Brackets([Star|RegExp], false, Valid_RegExp):-
    Star == 0'*,
    !,
    relocate_Stars_And_Switch_Brackets(RegExp, true, Valid_RegExp).


%  Star_Flag is false. We copy + and continue.
relocate_Stars_And_Switch_Brackets([Plus|RegExp], false, [0'+|Valid_RegExp]):-
    Plus == 0'+,
    !,
    relocate_Stars_And_Switch_Brackets(RegExp, false, Valid_RegExp).


%  Star_Flag is false. We copy the letter and continue.
relocate_Stars_And_Switch_Brackets([Letter|RegExp], false, [Letter|Valid_RegExp]):-
    relocate_Stars_And_Switch_Brackets(RegExp, false, Valid_RegExp).


%% find_Neighbouring_Letter_Pairs(+RegExp, +Append_Flag, +Continuous_Endings, +Last_RegExp_Empty_Word_Flag, +Beginnings_Of_Last_RegExp, +Endings_Of_Last_RegExp, -Letter_Pairs)
%
%  Finds all possible neighbouring Letter_Pairs in all possible words from RegExp's language.
%
%  Beginnings/Endings_Of_Last_RegExp and Continuous_Endings are [] at input. 
%  Append_Flag, if true, is telling us that we're appending previous char with current.
%  If false, it means we haven't processed any letter yet OR we are after separator +.
%  Append_Flag input must be false.
%  Last_RegExp_Empty_Word_Flag, if true, means we continue append new Endings to Continuous_Endings.
%  If false, we check input.
%    If it's a start, we set it to true and continue appending.
%    Else, we reset Continuous_Endings to current Endings_Of_Last_RegExp,
%      set the flag to true and continue processing.
%  Last_RegExp_Empty_Word_Flag must be input as true.
%
%  We always keep track of ending and starting letters of last subregexp processed
%  and all continuous ending letters (in case some previous contained empty word).
%  Example
%    In ab*c  a and c should be a letter pair, because they can be neighours.
%    That's why we use Continuous_Endings and Last_RegExp_Empty_Word_Flag
%
%  Then, if we find * we know possible pairs are ending_letters x starting_letters of last subregexp.
%  If we find + then we make no new pairs ans continue processing another subregexp.
%  If we find a letter or a (, we use this new subregexp and make pairs 
%  endings_of_continuous_previous_subregexps x starting_letters_of_new_subregexp.
%  Note: x stands for cartesian product.


find_Neighbouring_Letter_Pairs([], _, _, _, _, _, []).


%  Append_Flag is true and we found +. We set the flag to false and continue.
%  We must do this as + divides regexp into sub expressions.
find_Neighbouring_Letter_Pairs([Plus|RegExp], true, _, _, _, _, Letter_Pairs):-
    Plus == 0'+,
    !,
    find_Neighbouring_Letter_Pairs(RegExp, false, [], true, [], [], Letter_Pairs).


%  Append_Flag is true and we found *.
%  (RegExp)* has pairs in RegExp which are calculated elsewhere and from ()*, 
%  which adds pairs (Ending_Letters, Starting_Letters) of RegExp.
%  Note: Last_RegExp_Empty_Word_Flag must be false if we're at *, because we must have processed a letter or (subregexp) before it. 
%  We reset Last_RegExp_Empty_Word_Flag to true and keep Continuous_Endings.
find_Neighbouring_Letter_Pairs([Star|RegExp], true, Continuous_Endings, false, Beg_Last_RegExp, End_Last_RegExp, Letter_Pairs):-
    Star == 0'*,
    !,
    make_Pairs(End_Last_RegExp, Beg_Last_RegExp, Letter_Pairs_tmp),
    append(Letter_Pairs_tmp, Letter_Pairs_rest, Letter_Pairs),
    find_Neighbouring_Letter_Pairs(RegExp, true, Continuous_Endings, true, Beg_Last_RegExp, End_Last_RegExp, Letter_Pairs_rest).


%  Append_Flag is true and Last_RegExp_Empty_Word_Flag is false.
%  The cut in previous predicate makes sure * is not at input.
%  We set Last_RegExp_Empty_Word_Flag true and set Continuous_Endings to only last RegExp's endings, because
%  we know it didn't contain an empty word.
find_Neighbouring_Letter_Pairs(RegExp, true, _, false, Beg_Last_RegExp, End_Last_RegExp, Letter_Pairs):-
	find_Neighbouring_Letter_Pairs(RegExp, true, End_Last_RegExp, true, Beg_Last_RegExp, End_Last_RegExp, Letter_Pairs).


%  Append_Flag is true and we found (. We calculate sub expression recursively,
%  make pairs with previous SubRegExp and append them accordingly.
find_Neighbouring_Letter_Pairs([Left_Parenthesis|RegExp], true, Continuous_Endings_in, true, _, _, Letter_Pairs):-
    Left_Parenthesis == 0'(,
    !,
	% We process subregexp
    getRegExp(RegExp, -1, Sub_RegExp, RegExp_rest),
    find_Starting_Letters(Sub_RegExp, true, Beg_New_RegExp, Empty_Word),
    find_Ending_Letters(Sub_RegExp, End_New_RegExp),
    make_Pairs(Continuous_Endings_in, Beg_New_RegExp, Letter_Pairs_current),
    find_Neighbouring_Letter_Pairs(Sub_RegExp, false, [], true, [], [], Letter_Pairs_Sub_RegExp),
	append(Continuous_Endings_in, End_New_RegExp, Continuous_Endings_out),
    append(Letter_Pairs_current, Letter_Pairs_Sub_RegExp, Letter_Pairs_tmp),
    append(Letter_Pairs_tmp, Letter_Pairs_rest, Letter_Pairs),
	% We check if Sub_RegExp contained an empty word and set a flag.
	( Empty_Word == yes ->
		Last_RegExp_Empty_Word_Flag = true
	;
		Last_RegExp_Empty_Word_Flag = false
	),
    find_Neighbouring_Letter_Pairs(RegExp_rest, true, Continuous_Endings_out, Last_RegExp_Empty_Word_Flag, Beg_New_RegExp, End_New_RegExp, Letter_Pairs_rest). 


%  Append_Flag is true and we found a letter (not a special character).
%  Also, Last_RegExp_Empty_Word_Flag is true, so we append Letter, new ending, to Continuous_Endings.
%  We set it to false, make pairs with previos SubRegExps and continue processing.
find_Neighbouring_Letter_Pairs([Letter|RegExp], true, Continuous_Endings, true, _, _, Letter_Pairs):-
    make_Pairs(Continuous_Endings, [Letter], Letter_Pairs_current),
    append(Letter_Pairs_current, Letter_Pairs_rest, Letter_Pairs),
    find_Neighbouring_Letter_Pairs(RegExp, true, [Letter|Continuous_Endings], false, [Letter], [Letter], Letter_Pairs_rest).


%  Append_Flag is false. That means we're not making pairs with previous regexp.
%  We found (, so we calculate subexpr recursively and append pairs accordingly.
%  Also, Last_RegExp_Empty_Word_Flag is true. If Sub_RegExp contained an empty word 
%  we set it true, else we set it false.
%  Because Append_Flag input was false we set Continuous_Endings to Sub_RegExp's endings,
%  End_New_RegExp and continue processing.
find_Neighbouring_Letter_Pairs([Left_Parenthesis|RegExp], false, _, true, _, _, Letter_Pairs):-
    Left_Parenthesis = 0'(,
    !,
    getRegExp(RegExp, -1, Sub_RegExp, RegExp_rest),
    % we set Lambda_Flag true at the start of find_Starting_Letters predicate
    find_Starting_Letters(Sub_RegExp, true, Beg_New_RegExp, Empty_Word),
    find_Ending_Letters(Sub_RegExp, End_New_RegExp),
    find_Neighbouring_Letter_Pairs(Sub_RegExp, false, [], true, [], [], Letter_Pairs_Sub_RegExp),
    append(Letter_Pairs_Sub_RegExp, Letter_Pairs_rest, Letter_Pairs),
	( Empty_Word == yes ->
		Last_RegExp_Empty_Word_Flag = true
	;
		Last_RegExp_Empty_Word_Flag = false
	),
    find_Neighbouring_Letter_Pairs(RegExp_rest, true, End_New_RegExp, Last_RegExp_Empty_Word_Flag, Beg_New_RegExp, End_New_RegExp, Letter_Pairs_rest). 


%  Append_Flag is false. That means we're not making pairs with previous regexp.
%  We found a letter (not a special character). We set new beginnigs and ends.
%  Also, Last_RegExp_Empty_Word_Flag is true. Since Append_Flag was false, we 
%  set [Letter] as Continuous_Endings and set Empty_Word flag false.  
find_Neighbouring_Letter_Pairs([Letter|RegExp], false, _, true, _, _, Letter_Pairs):-
    find_Neighbouring_Letter_Pairs(RegExp, true, [Letter], false, [Letter], [Letter], Letter_Pairs).


%% make_Pairs(+ListA, +ListB, -Cartesian_Product_Of_List_A_And_B)
make_Pairs(ListA, ListB, Cartesian_Product):-
    findall(
        (A,B),
        (member(A, ListA), member(B, ListB)),
        Cartesian_Product).
        

%% convert_RegExp_To_NFA(+Regular_Expression, -Graph_Of_Nondeterministic_Finite_Automaton)
%
%  Covnerts Regular_Expression to NFA. 
%
%  At first it enumerates all letters in regular expression.
%  Then it finds all possible starting and ending letters in it.
%  We also fout out whether empty word is part of the language.
%  It determines which letter can possibly be next to each other in a word of regexp's language.
%  
%  Now begind construction of NFA.
%  
%  Creaing states.
%  We create a Starting_State 0 and set it up as a starting state.
%  Then we creates the rest of the states, each corresponding to one enumerated letter.
%  (We can have ("a", 2), ("a", 5) and they would be different states).
%
%  Creating Edges
%  We connect Starting_State to each starting letter Letter, with transition value Letter.
%  Then, for each possible neighbouring letters L_1.L_2 we add an edge from
%  the state L_1 to the state L_2 with transition value L_2.
%
%  Marking states as Terminal_States
%  All states corresponding to ending letters are marked as terminal states.
%  If an empty word is part of the language, we also mark Starting_State as a terminal state.


%  Input RegExp is empty string, meaning empty word is part of the language
convert_RegExp_To_NFA([], Graph):-
    Graph = graph([0], [], 0, [0]).


%  Input RegExp is null. That describes empty language.    
convert_RegExp_To_NFA(null, Graph):-
    Graph = graph([0], [], 0, []).


%  We use this to input sample data with their IDs.
%  Others are for RegExp inputs.
convert_RegExp_To_NFA(RegExp_ID, NFA_Graph):-
    regExp(RegExp_ID, RegExp),
    !,
    convert_RegExp_To_NFA(RegExp, NFA_Graph).


convert_RegExp_To_NFA(RegExp, NFA_Graph):-
    enumerate_Letters(RegExp, 1, Enumerated_RegExp),
    find_Starting_Letters(Enumerated_RegExp, true, Starting_Letters, Empty_Word_Flag),
    find_Ending_Letters(Enumerated_RegExp, Ending_Letters),
    find_Neighbouring_Letter_Pairs(Enumerated_RegExp, false, [], true, [], [], Neighbouring_Letter_Pairs),
    highest_State_Number(Ending_Letters, Highest_State_Number),
    create_NFA_Graph(Highest_State_Number, Empty_Word_Flag, Starting_Letters, Ending_Letters, Neighbouring_Letter_Pairs, NFA_Graph).


%% highest_State_Number(+Ending_Letters, -Highest_State_Number)
%
%  head(Ending_Letters, Head) is the letter with highest number.
%  Since we use numbers of letters' for states of automaton, it's also highest
%  state number.
highest_State_Number([Last_State|_], Highest_State_Number):-
    (_, Highest_State_Number) = Last_State.


%% create_NFA_Graph(+Highest_State_Number, +Empty_Word_Flag, +Starting_Letters, +Ending_Letters, +Neighbouring_Letter_Pairs, -NFA_Graph)
%
%  Creates States and Edges for a NFA graph. Adds starting state 0 and connetcts it with states corresponding to Starting_Letters.
%  Terminals are states coresponding to Ending_Letters
create_NFA_Graph(Highest_State_Number, Empty_Word, Starting_Letters, Ending_Letters, Letter_Pairs, NFA_Graph):-
    n_To_Zero(Highest_State_Number, States),
    findall(State_Number,
            member((_, State_Number), Ending_Letters),
            Terminal_States_tmp),
    ( (Empty_Word == yes) ->    
        Terminal_States = [0|Terminal_States_tmp]
    ;
        Terminal_States = Terminal_States_tmp
    ),
    Starting_State = 0,
    findall((0->I/Str_Transition),
            (member((Transition, I), Starting_Letters), name(Str_Transition, [Transition])),
            Edges_From_Starting_State),
    create_Edges(Letter_Pairs, Inner_Edges),
    append(Edges_From_Starting_State, Inner_Edges, Edges),
    NFA_Graph = graph(States, Edges, Starting_State, Terminal_States).

    
%% n_To_Zero(+N, -List_Of_Number_From_N_To_Zero)
%
%  N is >= 0


n_To_Zero(0, [0]):-
    !.


n_To_Zero(N, [N|List]):-
    New_N is N - 1,
    n_To_Zero(New_N, List).
    

%% create_Edges(+Letter_Pairs, -Edges)
%
%  Transforms every pair ((a,i),(b,j)) to edge i->j/b
create_Edges(Letter_Pairs, Edges):-
    findall( I->J/Str_Transition,
            (member(((_, I), (Transition, J)), Letter_Pairs), name(Str_Transition, [Transition])),
            Edges).

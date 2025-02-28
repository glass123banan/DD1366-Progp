% Terminal input: Total dart score
% Terminal output: A possible combo of dart scores


% Helper predicate to find if something exists
exists(Condition, Goal) :-  
    call(Condition),        % Call condition 
    call(Goal).             % If condition is true, eval goal

% input: total dart score, possible numbers (1-20)
findDartscore(totalDartscore, possibleNumbers) :-
    % checka för alla värden 1-20 om tryScores funkar
    forall(
        member(possibleNumbers),
        tryScores(totalDartscore)
    ).

tryScores(totalDartscore) :-
    


main :- 
    write('Enter a total dart score: '),
    read_line_to_string(user_input, Input),
    write('You entered: '), write(Input), nl.
% RUN FILE:
%           erl
%           c(bernoulli).
%           bernoulli:start().

-module(bernoulli).
-export([start/0, b/1, b_loop/2, binom/2]). % makes funcs public

% variables must start with cap-letter
% returns Nth bernoulli number
b(N) -> 
    Blst = b_loop(N, [1.0]), % Calls recursive loop with Blst[0]=1.0
    Bn = lists:nth(N+1,Blst), % retrieves (N+1)th bernoulli number since erl is 1-indexed

    % If |Bn| < 1e-10, treat it as zero (to handle floating-point noise)
    case abs(Bn) < 0.0000000001 of
        true  -> 0.0;
        false -> Bn
    end.

% helper func recursive loop - builds list of bernoulli nr
% If the list Blst has more than N elements, return list
b_loop(N,Blst) when length(Blst) > N ->
    Blst;
% otherwise, continue building Blst list
b_loop(N,Blst) ->
    M = length(Blst), % length of list with bernoulli numbers
    Klst = lists:seq(0,M-1), % set Klst to sequence 0 to (M-1)

    % fold from left, accumulate a sum
    Sum = lists:foldl(
        % lambdafunc with K (elem from Klst) and Acc=0 initially
        fun(K, Acc) -> 
            Bk = lists:nth(K+1,Blst), % first, retrieve Blst[K+1] from list 
            Acc - binom(M+1,K) * Bk % second, call binom and multiply with Bk and add to sum (acc)
        end, 
        0, % start with sum=0 
        Klst % apply fun on every elem in Klst
    ), 
    Bm = Sum / (M+1), % divide with M+1 
    b_loop(N, Blst ++ [Bm]). % call on itself again with Bm appended

% binom function
binom(_,K) when K =:= 0 -> 1; % if k=0 return 1 
binom(N,K) when K > N -> 0;  % return 0 if k>n

% otherwise, calculate with foldl
binom(N,K) -> 
    % lambdafunc tar in I och R och räknar ut
    % accumulatorn startar på 1
    % applicera fun på alla elem i listan med siffrorna 1 till K (tänk forloop 1 till k)
    lists:foldl(fun(I,R) -> R * (N - I + 1) div I end, 1, lists:seq(1,K)). % måste använda fun som lambdafunc

% "main" function
start() ->
    % ~p prints binom(5,2)
    % ~n prints newline
    % io:format("~p~n", [b(4)]). 
    
     % Print first 10 Bernoulli numbers
    lists:foreach(fun(N) -> 
        io:format("B(~p) = ~.6f~n", [N, b(N)]) % display with 6 decimals
    end, lists:seq(0, 9)). % Loop through 0 to 9 and print each Bernoulli number

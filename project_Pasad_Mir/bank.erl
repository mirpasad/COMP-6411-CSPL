-module(bank).
-export([start/1]).

start({Name, Resources}) ->
    register(Name, self()),
    Master = whereis(money),
    loop(Name, Resources, Master).

loop(Name, Resources, Master) ->
    receive
        {request_loan, CustomerName, LoanAmount} ->
            timer:sleep(rand:uniform(91) + 10), % Sleep between 10 and 100 milliseconds
            if Resources >= LoanAmount ->
                ResourcesLeft = Resources - LoanAmount,
                Master ! {request, CustomerName, Name, LoanAmount},
                CustomerName ! {loan_approved, LoanAmount},
                Master ! {response, Name, CustomerName, LoanAmount},
                loop(Name, ResourcesLeft, Master);
            true ->
                Master ! {request, CustomerName, Name, LoanAmount},
                CustomerName ! {loan_rejected, LoanAmount},
                Master ! {reject, Name, CustomerName, LoanAmount},
                loop(Name, Resources, Master)
            end,
            check_resources(Resources)
    end.

check_resources(Resources) ->
    if Resources =< 0 ->
        io:format("$ The bank has run out of cash.~n"),
        exit(bank_out_of_cash);
    true ->
        ok
    end.

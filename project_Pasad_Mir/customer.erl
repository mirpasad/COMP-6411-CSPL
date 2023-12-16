-module(customer).
-export([start/2]).

start({Name, LoanAmount}, Banks) ->
    register(Name, self()),
    Master = whereis(money),
    request_loan(Name, LoanAmount, Banks, Master),
    Master ! {done, Name}.

request_loan(Name, LoanAmount, Banks, Master) when LoanAmount > 0 ->
    case Banks of
        [] ->
            {done, Name};
        _ ->
            timer:sleep(200), % Sleep between 10 and 100 milliseconds
            RequestAmount = rand:uniform(min(LoanAmount, 50)),
            ChosenBank = rand:uniform(length(Banks)),
            Bank = lists:nth(ChosenBank, Banks),
            Bank ! {request_loan, Name, RequestAmount},
            receive
                {loan_approved, ApprovedAmount} ->
                    request_loan(Name, LoanAmount - ApprovedAmount, Banks, Master);
                {loan_rejected, _LoanAmount} ->
                    NewBanks = lists:delete(Bank, Banks),
                    request_loan(Name, LoanAmount, NewBanks, Master)
            end
    end;

request_loan(_, _, _, _) ->
    done.

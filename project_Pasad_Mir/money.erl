-module(money).
-export([start/1, get_customer_info/0]).

start(Args) ->
    register(money, self()),
    CustomerFile = lists:nth(1, Args),
    BankFile = lists:nth(2, Args),
    {ok, CustomerInfo} = file:consult(CustomerFile),
    {ok, BankInfo} = file:consult(BankFile),
    put(customer_info, CustomerInfo),
    put(bank_info, BankInfo),
    put(customer_info_new, CustomerInfo),
    put(bank_info_new, BankInfo),
    io:format("~n** The financial market is opening for the day **~n~n"),
    io:format("Starting transaction log...~n~n"),
    BankPids = spawn_banks(BankInfo),
    spawn_customers(CustomerInfo, BankPids),
    loop([], [], 0, 0, [], []).

spawn_banks(BankInfo) ->
    [spawn_link(bank, start, [Bank]) || Bank <- BankInfo].

spawn_customers(CustomerInfo, BankPids) ->
    [spawn_link(customer, start, [Customer, BankPids]) || Customer <- CustomerInfo].

get_customer_info() ->
    process_info(self(), dictionary),
    case get(customer_info) of
        undefined -> undefined;
        Value -> Value
    end.

get_customer_info_after_process() ->
    process_info(self(), dictionary),
    case get(customer_info_new) of
        undefined -> undefined;
        Value -> Value
    end.

get_bank_info_after_process() ->
    process_info(self(), dictionary),
    case get(bank_info_new) of
        undefined -> undefined;
        Value -> Value
    end.

get_bank_info() ->
    process_info(self(), dictionary),
    case get(bank_info) of
        undefined -> undefined;
        Value -> Value
    end.


loop(Customers, Banks, TotalReceived, TotalLoaned, UpdatedCX, UpdatedBanks) ->
    receive
        {request, Customer, Bank, Amount} ->
            io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank~n", [Customer, Amount, Bank]),
            loop(Customers, Banks, TotalReceived, TotalLoaned, UpdatedCX, UpdatedBanks);
        {response, Bank, Customer, Amount} ->
            io:format("$ The ~p bank approves a loan of ~p dollar(s) to ~p~n", [Bank, Amount, Customer]),
            NewCustomers = update_customers(Customers, Customer, Amount),
            NewUpdatedCX = update_customers(UpdatedCX, Customer, Amount),
            NewBanks = update_banks(Banks, Bank, Amount),
            NewUpdatedBanks = update_banks(UpdatedBanks, Bank, Amount),
            NewTotalReceived = TotalReceived + Amount,
            NewTotalLoaned = TotalLoaned + Amount,
            loop(NewCustomers, NewBanks, NewTotalReceived, NewTotalLoaned, NewUpdatedCX, NewUpdatedBanks);
        {reject, Bank, Customer, Amount} ->
            io:format("$ The ~p bank denies a loan of ~p dollar(s) to ~p~n", [Bank, Amount, Customer]),
            NewCustomers = remove_customer(Customers, Customer),
            loop(NewCustomers, Banks, TotalReceived, TotalLoaned, UpdatedCX, UpdatedBanks);
        {bank_out_of_cash, Bank} ->
            io:format("$ The ~p bank has run out of cash. No more loans will be processed.~n", [Bank]),
            NewBanks = remove_bank(Banks, Bank),
            loop(Customers, NewBanks, TotalReceived, TotalLoaned, UpdatedCX, UpdatedBanks);
        {done, Customer} ->
            NewCustomers = remove_customer(Customers, Customer),
            case NewCustomers of
                [] ->
                    io:format("~n~n** Banking Report **~n"),
%%                    io:format("~n~n** UpdatedCX ~p  UpdatedBanks ~p**~n", [UpdatedCX, UpdatedBanks]),
                    display_customers(UpdatedCX),
                    display_banks(UpdatedBanks);
%%                    io:format("~n~n** NewTotalReceived ~p  NewTotalLoaned ~p **~n", [TotalReceived, TotalLoaned]);
                _ ->
                    loop(NewCustomers, Banks, TotalReceived, TotalLoaned, UpdatedCX, UpdatedBanks)
            end
    end.

update_customers(Customers, Customer, Amount) ->
    case lists:keyfind(Customer, 1, Customers) of
        false ->
            [{Customer, Amount} | Customers];
        {Customer, Received} ->
            lists:keyreplace(Customer, 1, Customers, {Customer, Received + Amount})
    end.

update_banks(Banks, Bank, Amount) ->
    case lists:keyfind(Bank, 1, Banks) of
        false ->
            [{Bank, Amount} | Banks];
        {Bank, Balance} ->
            lists:keyreplace(Bank, 1, Banks, {Bank, Balance + Amount})
    end.

remove_customer(Customers, Customer) ->
    lists:keydelete(Customer, 1, Customers).

remove_bank(Banks, Bank) ->
    lists:keydelete(Bank, 1, Banks).

display_customers(Customers) ->
    io:format("~nCustomers:~n"),
    {TotalObjective, TotalReceived} = lists:foldl(
        fun({Customer, Amount}, {AccObj, AccRec}) ->
            case lists:keyfind(Customer, 1, Customers) of
                false ->
                    io:format(" ~p: objective ~p, received ~p~n", [Customer, Amount, Amount]);
                {Customer, Received} ->
                    io:format(" ~p: objective ~p, received ~p~n", [Customer, Amount, Received]),
                    {AccObj + Amount, AccRec + Received}
            end
        end,
        {0, 0},
        get_customer_info()
    ),
    io:format(" -----~n"),
    io:format(" Total: objective ~p, received ~p~n", [TotalObjective, TotalReceived]).

display_banks(Banks) ->
    io:format("~nBanks:~n"),
    {TotalLoaned, Total} = lists:foldl(
        fun({Bank, Amount}, {AccObj, AccRec}) ->
            case lists:keyfind(Bank, 1, Banks) of
                false ->
                    io:format(" ~p: original ~p, balance ~p~n", [Bank, Amount, Amount]);
                {Bank, Balance} ->
                    io:format(" ~p: original ~p, balance ~p~n", [Bank, Amount, Amount - Balance]),
                    {AccObj + Amount, AccRec + Balance}
            end
        end,
        {0, 0},
        get_bank_info()
    ),
    io:format(" -----~n"),
    io:format(" Total: original ~p, loaned ~p~n~n", [TotalLoaned, Total]),
    io:format("~n** The financial market is closing for the day **~n~n").

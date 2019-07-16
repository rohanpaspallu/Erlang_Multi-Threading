%% @author
%% @doc This bank process will get request from customer and according to that it will send
%% response of reject or granted

-module(bank).
-export([start/0, bank/1, writeBankUpdatedList/1, displayRemainingBankMoney/0, printAvailableBankDetails/0]).


%% ------------------------------
%% @doc This function will print the initial bank details at starting of program
%% @params None
%% ------------------------------
printAvailableBankDetails() ->
    {ok, C} = file:consult("./bank.txt"),
    io:format(" ******** Banks and financial resources ********* ~n"),
    [io:format("~p: ~p ~n", [element(1,X),element(2,X)]) || X <- C].

%% ------------------------------
%% @doc This will talk with money process to print data
%% @params None
%% ------------------------------
displayMessageProcess() ->
    spawn(money, displayMessages, []).

%% ------------------------------
%% @doc This will talk with money process to handle requests
%% @params None
%% ------------------------------
handleMoneyProcess () ->
    spawn(money, handleProcessRequest, []).

%% ------------------------------
%% @doc At the end when all customers have their money the banks will show remaining money
%% @params None
%% ------------------------------
displayRemainingBankMoney() ->
    timer:sleep(500),
    case filelib:is_regular("./bank_tmp.txt") of
         true ->
            {ok, C} = file:consult("./bank_tmp.txt"),
            [displayMessageProcess() ! {bank, element(1,X), element(2,X)} || X <- C];
        false ->
            do_nothing
    end.

%% ------------------------------
%% @doc Update list of bank in temp
%% @params List
%% ------------------------------
writeBankUpdatedList(List) ->
    file:delete("./bank_tmp.txt"),
    [writeBankTemp(X) || X <- List].

%% ------------------------------
%% @doc write elements of bank list to temp file
%% @params Element
%% ------------------------------
writeBankTemp(Element) ->
    file:write_file("./bank_tmp.txt", io_lib:format("~p.~n", [Element]), [append]).

%% ------------------------------
%% @doc modify the list
%% @params List, Name, Diff
%% ------------------------------
modifyList(List,BankName,Difference) ->
    maps:to_list(maps:update(BankName,Difference,maps:from_list(List))).

modifyBankProcess(List,CustomerName, BankProcesses) ->
    maps:to_list(maps:update(CustomerName,BankProcesses,maps:from_list(List))).

%% ------------------------------
%% @doc Delete from list
%% @params Name, Amount, List
%% ------------------------------
deleteFromList(BankName,AmountBankHolds,List) ->
    lists:delete({BankName, AmountBankHolds}, List).

deleteBankProcess(BankProcess, List) ->
    lists:delete(BankProcess, List).

%% ------------------------------
%% @doc bank process which will resond to request from customer
%% @params Name
%% ------------------------------
bank(BankName) ->
    receive
        {requestParse, CustomerName, AmountAsked, Lcb, CustomerProcess, Ecb, BankProcess} ->
        {ok, C} = file:consult("./bank_tmp.txt"),
        case maps:is_key(BankName, maps:from_list(C)) of
            true ->
                displayMessageProcess() ! {ask, CustomerName, AmountAsked, BankName},
                AmountBankHolds = maps:get(BankName, maps:from_list(C)),
                LoanAbleAmountRemaining = AmountBankHolds - AmountAsked,
                if
                    LoanAbleAmountRemaining > 0 ->
                    writeBankUpdatedList(modifyList(C, BankName, LoanAbleAmountRemaining)),
                    displayMessageProcess() ! {grant, CustomerName, AmountAsked, BankName},
                    CustomerProcess ! {granted, CustomerName, AmountAsked, Lcb, Ecb};
                LoanAbleAmountRemaining < 0 ->
                    displayMessageProcess() ! {reject, CustomerName, AmountAsked, BankName},
                    {Customer, BankProcesses} = {lists:nth(1, Ecb), lists:nth(2, Ecb)},
                    BankRemainingProcesses = deleteBankProcess(BankProcess, BankProcesses),
                    if
                        length(BankRemainingProcesses) < 1 ->
                            handleMoneyProcess() ! {delCustomerProcess, Lcb, Ecb};
                        true ->
                            Temp = modifyBankProcess(Lcb, Customer, BankRemainingProcesses),
                            handleMoneyProcess() ! {newRequest, Temp}
                    end,
                    end_of_if;
                LoanAbleAmountRemaining == 0 ->
                    displayMessageProcess() ! {grant, CustomerName, AmountAsked, BankName},
                    writeBankUpdatedList(deleteFromList(BankName, AmountBankHolds, C)),
                    {Customer, BankProcesses} = {lists:nth(1, Ecb), lists:nth(2, Ecb)},
                    BankRemainingProcesses = deleteBankProcess(BankProcess, BankProcesses),
                    if
                        length(BankRemainingProcesses) < 1 ->
                            handleMoneyProcess() ! {delCustomerProcess, Lcb, Ecb};
                        true ->
                            Temp = modifyBankProcess(Lcb, Customer, BankRemainingProcesses),
                            handleMoneyProcess() ! {newRequest, Temp}
                    end,
                    end_of_if;
                true ->
                    not_ok
            end,
            ok;
        false ->
                {Customer, BankProcesses} = {lists:nth(1, Ecb), lists:nth(2, Ecb)},
                BankRemainingProcesses = deleteBankProcess(BankProcess, BankProcesses),
                if
                    length(BankRemainingProcesses) < 1 ->
                        handleMoneyProcess() ! {delCustomerProcess, Lcb, Ecb};
                    true ->
                        Temp = modifyBankProcess(Lcb, Customer, BankRemainingProcesses),
                        handleMoneyProcess() ! {newRequest, Temp}
                end,
                ok
        end,
        timer:sleep(500),
        bank(BankName)
    end.
%% ------------------------------
%% @doc starting point
%% @params None
%% ------------------------------
start() ->
    io:format("starting bank process ~n").
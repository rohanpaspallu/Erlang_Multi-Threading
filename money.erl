%% @author
%% @doc This is the main process of the program which will spawn the individual bank and customer processes which
%% are given inside the txt files. Will listen for the the messages to be displayed on display.

-module(money).
-export([start/0, money/1, displayMessages/0, handleProcessRequest/0]).

%% ------------------------------
%% @doc This function will spawn the individual process give in params
%% @params Module, Process Name, Arguments
%% ------------------------------
startProcess(Module,ProcessName,Name) ->
    spawn(Module, ProcessName, [Name]).

%% ------------------------------
%% @doc This function will pick a random element from given list
%% @params List
%% ------------------------------
pickRandomElement(List) ->
    lists:nth(rand:uniform(length(List)), List).

%% ------------------------------
%% @doc This function will delete the element from given list
%% @params List, Element
%% ------------------------------
deleteFromList(Element,List) ->
    lists:delete(Element,List).

%% ------------------------------
%% @doc The recursive function choosing random customer and bank to communicate
%% @params Lc -> List of processes from customer
%%         Lb -> List of processes from bank
%% ------------------------------
money(Lcb) ->
    if
        length(Lcb) < 1 ->
            bank:displayRemainingBankMoney(),
            customer:displayRemainingCustomerMoney();
        true ->
            ElementD = erlang:tuple_to_list(pickRandomElement(Lcb)),
            {Customer, Bank} = {lists:nth(1, ElementD), pickRandomElement(lists:nth(2, ElementD))},
            Customer ! {request, Bank, Lcb, Customer, ElementD}
    end.

%% ------------------------------
%% @doc starting point of the function
%% @params None
%% ------------------------------
start() ->
    {ok, C} = file:consult("./customer.txt"),
    {ok, B} = file:consult("./bank.txt"),
    customer:writeNewList(C),
    bank:writeBankUpdatedList(B),
    customer:printAvailableCustomerDetails(),
    bank:printAvailableBankDetails(),
    Lc = [startProcess(customer, customer, element(1,X)) || X <- C],
    Lb = [startProcess(bank, bank, element(1,Y)) || Y <- B],
    Lcb = [{X,Lb} || X <- Lc],
    money(Lcb).

%% ------------------------------
%% @doc This will listen for the process request for bank and customer
%% @params None
%% ------------------------------
handleProcessRequest() ->
    receive
        {newRequest, Lcb} ->
            money(Lcb);
        {delCustomerProcess, Lcb, Element} ->
            Temp = deleteFromList(erlang:list_to_tuple(Element), Lcb),
            money(Temp)
    end.

%% ------------------------------
%% @doc This will display all the messages from bank and customer process
%% @params None
%% ------------------------------
displayMessages() ->
    timer:sleep(rand:uniform(100)),
    receive
        {ask, Name, Amount, Bank} ->
            io:format("~p requests a loan of ~p dollars(s) from ~p ~n", [Name, Amount, Bank]),
            displayMessages();
        {grant, Name, Amount, Bank} ->
            io:format("~p approves a loan of ~p dollars(s) from ~p ~n", [Bank, Amount, Name]),
            displayMessages();
        {reject, Name, Amount, Bank} ->
            io:format("~p denies loan of ~p dollars(s) from ~p ~n", [Bank, Amount, Name]),
            displayMessages();
        {bank, BankName, AmountLeft} ->
            timer:sleep(200),
            io:format("~p has ~p dollars(s) remaining. ~n", [BankName, AmountLeft]),
            displayMessages();
        {objectiveReached, Name, Amount} ->
            io:format("~p has reached the objective of ~p dollar(s). Woo Hoo ! ~n", [Name, Amount]),
            displayMessages();
        {objectiveNotReached, Name, Amount} ->
            io:format("~p was only able to borrow ~p dollars(s). Boo Hoo ! ~n", [Name, Amount]),
            displayMessages();
        {finish} ->
            timer:sleep(500),
            file:delete("./customer_tmp.txt"),
            file:delete("./bank_tmp.txt"),
            io:format("~n Program ended successfully ~n"),
            erlang:halt()
    end.
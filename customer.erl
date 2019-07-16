%% @author
%% @doc This customer process will send request to bank and wait for response
%% It will not send another request untill it gets response.

-module(customer).
-export([start/0, customer/1, writeNewList/1, displayRemainingCustomerMoney/0, printAvailableCustomerDetails/0]).


%% ------------------------------
%% @doc print all the customer details at initial stage
%% @params None
%% ------------------------------
printAvailableCustomerDetails() ->
    {ok, List} = file:consult("./customer.txt"),
    io:format(" ******** Customers and loan objectives ********* ~n"),
    [io:format("~p: ~p ~n", [element(1,X),element(2,X)]) || X <- List],
    timer:sleep(1000).

%% ------------------------------
%% @doc To save the temporarily modified data of customers
%% @params List
%% ------------------------------
writeNewList(List) ->
    file:delete("./customer_tmp.txt"),
    [writeTemp(X) || X <- List].

%% ------------------------------
%% @doc To talk with main process
%% @params None
%% ------------------------------
handleMoneyProcess () ->
    spawn(money, handleProcessRequest, []).

%% ------------------------------
%% @doc To talk with money process to display messages
%% @params None
%% ------------------------------
displayMessageProcess() ->
    spawn(money, displayMessages, []).

%% ------------------------------
%% @doc At the end when banks are empty we will print the remaining money which every customer needs
%% @params None
%% ------------------------------
displayRemainingCustomerMoney() ->
    timer:sleep(1000),
    case filelib:is_regular("./customer_tmp.txt") of
        true ->
            {ok, C} = file:consult("./customer_tmp.txt"),
            {ok, F} = file:consult("./customer.txt"),
            [displayMessageProcess() ! {objectiveNotReached, element(1,X), element(2,Y) - element(2,X)} || X <- C ,Y <- F, element(1,X) =:= element(1,Y)],
            displayMessageProcess() ! {finish};
        false ->
            displayMessageProcess() ! {finish}
    end.
%% ------------------------------
%% @doc To write element to temp file
%% @params Element
%% ------------------------------
writeTemp(Element) ->
    file:write_file("./customer_tmp.txt", io_lib:format("~p.~n", [Element]), [append]).

%% ------------------------------
%% @doc Generate Random number
%% @params Int
%% ------------------------------
generateRandomNumber(N) when N > 50 ->
    rand:uniform(50);

%% ------------------------------
%% @doc generate random number
%% @params Int
%% ------------------------------
generateRandomNumber(N) when N < 51 ->
    rand:uniform(N).

%% ------------------------------
%% @doc Modify list upon posirive response from bank
%% @params List, Name, Diff
%% ------------------------------
modifyList(List,CustomerName,Difference) ->
    if
        Difference > 0 ->
            maps:to_list(maps:update(CustomerName,Difference,maps:from_list(List)))
        end.

%% ------------------------------
%% @doc This will delete element from list
%% @params Name, Amount, List
%% ------------------------------
deleteFromList(CustomerName,AmountCustomerHolds,List) ->
    lists:delete({CustomerName, AmountCustomerHolds}, List).

%% ------------------------------
%% @doc To get the goal of customer
%% @params Name
%% ------------------------------
getObjective(CustomerName) ->
    {ok, List} = file:consult("./customer.txt"),
    Objective = maps:get(CustomerName,maps:from_list(List)),
    displayMessageProcess() ! {objectiveReached, CustomerName, Objective}.

%% ------------------------------
%% @doc customer sends the request to bank
%% @params Name
%% ------------------------------
customer(Name) ->
    receive
        {request, B, Lcb, Ec, Ecb} ->
            {ok, C} = file:consult("./customer_tmp.txt"),
            AmountToAsk = generateRandomNumber(maps:get(Name, maps:from_list(C))),
            B ! {requestParse, Name, AmountToAsk, Lcb, Ec, Ecb, B},
            customer(Name);
        {granted, CustomerName, AmountAsked, Lcb, Ecb} ->
            {ok, C} = file:consult("./customer_tmp.txt"),
            CustomerRemainingLoanMoney = maps:get(CustomerName, maps:from_list(C)),
            CustomerMoneyDifference = CustomerRemainingLoanMoney - AmountAsked,
            if
                CustomerMoneyDifference > 0 ->
                    writeNewList(modifyList(C, CustomerName, CustomerMoneyDifference)),
                    handleMoneyProcess() ! {newRequest, Lcb};
                CustomerMoneyDifference < 1 ->
                    getObjective(CustomerName),
                    writeNewList(deleteFromList(CustomerName, CustomerRemainingLoanMoney,C)),
                    handleMoneyProcess() ! {delCustomerProcess, Lcb, Ecb}
            end,
            customer(Name)
    end.

%% ------------------------------
%% @doc start point of customer
%% @params None
%% ------------------------------
start() ->
    io:format("starting process ~n").
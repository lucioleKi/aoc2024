-module(day01).
-export([diff1/2, diff2/2, task1/0, task2/0, test/0]).

%% Calculate the difference of 2 sorted lists.
diff1(L1, L2) ->
    Sorted1 = lists:sort(L1),
    Sorted2 = lists:sort(L2),
    Diff = [abs(X - Y) || X <- Sorted1 && Y <- Sorted2],
    lists:sum(Diff).

%% Calculate the weighted difference of 2 sorted lists.
diff2(L1, L2) ->
    Counts = make_map(L2, #{}),
    lists:foldl(fun(X, Sum) -> X * maps:get(X, Counts, 0) + Sum end, 0, L1).

%% Main function for task1.
task1() ->
    FileName = "input01.txt",
    case file:read_file(FileName) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            {L1, L2} = parse_line(Lines, [], []),
            diff1(L1, L2);
        {error, Reason} ->
            {error, Reason}
    end.

%% Main function for task2.
task2() ->
    FileName = "input01.txt",
    case file:read_file(FileName) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            {L1, L2} = parse_line(Lines, [], []),
            diff2(L1, L2);
        {error, Reason} ->
            {error, Reason}
    end.

%% Test for diff1/2 and diff2/2 using a toy example
test() ->
    L1 = [3,4,2,1,3,3],
    L2 = [4,3,5,3,9,3],
    11 = diff1(L1, L2),
    31 = diff2(L1, L2),
    ok.

%% Helper functions
parse_line([<<>>], L1, L2) ->
    {L1, L2};
parse_line([Line|Lines], L1, L2) ->
    [N1, N2] = binary:split(Line, <<"   ">>),
    parse_line(Lines, [binary_to_integer(N1)|L1], [binary_to_integer(N2)|L2]).

make_map([], Map) ->
    Map;
make_map([H|T], Map0) ->
    Map1 = maps:update_with(H, fun(X) -> X + 1 end, 1, Map0),
    make_map(T, Map1).
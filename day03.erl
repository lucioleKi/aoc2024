-module(day03).
-export([task/1]).

%% Find all mul(X, Y) and sum the multiplication.
match(L) ->
    Pattern = <<".*mul\\((\\d{1,3}),(\\d{1,3})\\).*">>,
    case re:run(L, Pattern, [global,ungreedy,{capture, [1,2], list}]) of
        {match, Matches} ->
            lists:sum([list_to_integer(X) * list_to_integer(Y) || [X, Y] <- Matches]);
        nomatch ->
            0
    end.

match2(L, do, Acc0) ->
    %% Find the next don't(). Call match/1 for the segment in between.
    Pattern = <<"don\\'t\\(\\)">>,
    case re:run(L, Pattern, [{capture,first}]) of
        {match, [{Position,_}]} ->
            NewStart = Position + byte_size(<<"don't()">>),
            L1 = binary:part(L, 0, Position),
            L2 = binary:part(L, NewStart, byte_size(L) - NewStart),
            match2(L2, dont, Acc0 + match(L1));
        nomatch ->
            match(L) + Acc0
    end;
match2(L, dont, Acc) ->
    %% Find the next do().
    Pattern = <<"do\\(\\)">>,
    case re:run(L, Pattern, [{capture,first}]) of
        {match, [{Position,_}]} ->
            NewStart = Position + byte_size(<<"do()">>),
            L1 = binary:part(L, NewStart, byte_size(L) - NewStart),
            match2(L1, do, Acc);
        nomatch ->
            Acc
    end.

%% Main function for task 1 and 2. Call task(1) and task(2) respectively.
task(Number) ->
    FileName = "input03.txt",
    case file:read_file(FileName) of
        {ok, Content} ->
            case Number of
                1 -> match(Content);
                2 -> match2(Content, do, 0)
            end;
        {error, Reason} ->
            {error, Reason}
    end.
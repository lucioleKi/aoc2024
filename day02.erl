-module(day02).
-export([task/1]).

%% Check if a list is safe according to task 1's rules
safe([E1,E2|T]) ->
    case E1 < E2 andalso E1 + 4 > E2 of
        true -> safe([E2|T]);
        false -> false
    end;
safe([_E1]) ->
    true.

%% Check if a list is safe according to task 2's rules
safe2([E1,E2|T], []) ->
    case E1 < E2 andalso E1 + 4 > E2 of
        true -> safe2([E2|T], [E1]);
        false -> safe([E1|T]) orelse safe([E2|T])
    end;
safe2([E1,E2|T1], [H|_T2]=L2) ->
    case E1 < E2 andalso E1 + 4 > E2 of
        true -> safe2([E2|T1], [E1|L2]);
        false -> safe([E2|T1]) orelse safe([H,E2|T1])
    end;
safe2([_E1,_E2], _) ->
    true;
safe2([_E1], _) ->
    true.

%% Reverse decreasing lists
to_inc([E1, E2|_T] = L, 1) ->
    case E1 < E2 of
        true -> safe(L);
        false -> safe(lists:reverse(L))
    end;
to_inc([E1, E2|_T] = L, 2) ->
    case E1 < E2 of
        true -> safe2(L,[]);
        false -> safe2(lists:reverse(L),[])
    end.

%% Main function for task 1 and 2. Call task(1) and task(2) respectively.
task(Number) ->
    FileName = "input02.txt",
    case file:read_file(FileName) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            count_safe(Lines, 0, Number);
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper function
count_safe([<<>>], Acc, _Task) ->
    Acc;
count_safe([Line|Lines], Acc0, Task) ->
    L0 = binary:split(Line, <<" ">>, [global, trim]),
    L1 = [binary_to_integer(N) || N <- L0],
    Acc1 = case to_inc(L1,Task) of
        true -> Acc0 + 1;
        false -> Acc0
    end,
    count_safe(Lines, Acc1, Task).


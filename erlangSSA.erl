-module(erlangSSA).
-export([run/2, unique/1, defined/1, translate_expr/1])
%Definitions of SSA statements, as well as the items contained in them, programs that contain them, 
%and environments that result when they are run.
-type item() :: {'num',integer()}
            |  {'var',atom()}.
			
-type statement() :: item()
			|  {'add',item(),item()}
			|  {'mul',item(),item()}.


-type program() :: [{atom(), statement()}].

-type environment() :: [{atom(), integer()}].


%Runs an SSA program in a given environment, returning a new environmet. 
-spec run(program(), environment()) ->  integer().
run([{A, S}|Continue], E) -> 
	run(Continue, [{A, eval(E, S)}|E]);
run([], E) ->
	E.
	
%A lookup function to lookup an element in an environment.
-spec lookup(atom(), environment()) -> integer().
lookup(_A, []) ->
	lookup_error;
lookup(A,[{A,V}|_]) ->
    V;
lookup(A,[_|Continue]) ->
    lookup(A,Continue).
	
%Evaluates a statement, returning an integer value.
-spec eval(environment(),statement()) -> integer().
eval(_,{num,N}) ->
    N;
eval(E,{var,A}) ->
    lookup(A,E);
eval(E,{add,E1,E2}) ->
    eval(E,E1) + eval(E,E2);
eval(E,{mul,E1,E2}) ->
    eval(E,E1) * eval(E,E2).

%Checks that all the variables in an SSA statement are assigned to once by creating a list of the variables, then comparing it to
%a set of the variables, and checking if their sizes are equal.
%Returns true if the statement is valid.
-spec unique(program()) -> boolean().
unique(P) ->
	Variables = listVars(P),
	erlang:length(Variables) == sets:size(sets:from_list(Variables)).

%Lists the variables assigned to by the program.
-spec listVars(program()) -> [atom()].
listVars([]) -> [];
listVars([{A, _S}|Continue])	-> 
	[A|listVars(Continue)].

	
%Checks that every variable in an SSA program has a value before it's used on the right hand side of the assignment.
%This assumes that no environment is given.
%Returns true if the statement is valid. 
-spec defined(program()) -> boolean().
defined(P) ->
	defined(P, []).

-spec defined(program(), [atom()]) -> boolean().
defined([], _) -> true;
defined([{A, {num, _V}}|Ps], As) ->
	defined(Ps, [A|As]);
defined([{A, {_Operation, {num, _V1}, {num, _V2}}}|Ps], As) ->
	defined(Ps, [A|As]);
defined([{A, {var, V}}|Ps], As) ->
	case lists:member(V, As) of
		false -> false;
		true -> defined(Ps, [A|As])
		end;
defined([{A, {_Operation, {var, V1}, {'var', V2}}}|Ps], As) ->
	case lists:member(V1, As) of
		false -> false;
		true -> 
			case lists:member(V2, As) of
			false -> false;
			true -> defined(Ps, [A|As])
			end end;
defined([{A, {_Operation, {num, _V1}, {var, V2}}}|Ps], As) ->
	case lists:member(V2, As) of
		false -> false;
		true -> defined(Ps, [A|As])
		end;
defined([{A, {_Operation, {var, V1}, {num, _V2}}}|Ps], As) ->
	case lists:member(V1, As) of
		false -> false;
		true -> defined(Ps, [A|As])
		end.

%Takes an expression and translates it to an SSA program.
%The translated expression, when ran, gives the final value of the expression in a variable labled 0. 
-type expr() :: {'num',integer()}
             |  {'var',atom()}
             |  {'add',expr(),expr()}
             |  {'mul',expr(),expr()}.
-spec translate_expr(expr()) -> program().
translate_expr(Expr) ->
	translate_expr(Expr, 0).
	
translate_expr({O, {Type, V}, {Type2, V2}}, N) ->
	[{N, {O, {Type, V}, {Type2, V2}}}];
translate_expr({O, {O2, T1, T2}, {Type, V}}, N) ->
	translate_expr({O2, T1, T2}, N+1) ++ [{N, {O, {var, N+1}, {Type, V}}}];
translate_expr({O, {Type, V}, {O2, T1, T2}}, N) ->
	translate_expr({O2, T1, T2}, N+1) ++ [{N, {O, {Type, V}, {var, N+1}}}];
translate_expr({O, {O2, T1, T2}, {O3, T3, T4}}, N) ->
	translate_expr({O2, T1, T2}, N+1) ++ translate_expr({O3, T3, T4}, N+2) ++ [{N, {O, {var, N+1}, {var, N+2}}}].
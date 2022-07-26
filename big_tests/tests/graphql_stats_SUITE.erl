-module(graphql_stats_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_command/4, get_ok_value/2]).
-import(mongooseimctl_helper, [mongooseimctl/3, rpc_call/3]).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_stats_http},
     {group, admin_stats_cli}].

groups() ->
    [{admin_stats_http, [], admin_stats_tests()},
     {admin_stats_cli, [], admin_stats_tests()}].

admin_stats_tests() ->
    [admin_stats_global_test,
     admin_stats_global_with_users_test,
     admin_stats_domain_test,
     admin_stats_domain_with_users_test].

init_per_suite(Config) ->
    Config1 = ejabberd_node_utils:init(mim(), Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(admin_stats_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_stats_cli, Config) ->
    graphql_helper:init_admin_cli(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus_fresh:clean(),
    escalus:end_per_testcase(CaseName, Config).

% Admin test cases

admin_stats_global_test(Config) ->
    Result = get_ok_value([data, stat, globalStats], get_stats(Config)),
    #{<<"uptimeSeconds">> := UptimeSeconds, <<"registeredUsers">> := RegisteredUsers,
      <<"onlineUsersNode">> := OnlineUsersNode, <<"onlineUsers">> := OnlineUsers,
      <<"incomingS2S">> := IncomingS2S, <<"outgoingS2S">> := OutgoingS2S} = Result,
    ?assert(is_not_negative_integer(UptimeSeconds)),
    ?assertEqual(0, RegisteredUsers),
    ?assertEqual(0, OnlineUsersNode),
    ?assertEqual(0, OnlineUsers),
    ?assert(is_not_negative_integer(IncomingS2S)),
    ?assert(is_not_negative_integer(OutgoingS2S)).

admin_stats_global_with_users_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_stats_global_with_users_test/2).

admin_stats_global_with_users_test(Config, _Alice) ->
    Result = get_ok_value([data, stat, globalStats], get_stats(Config)),
    #{<<"uptimeSeconds">> := UptimeSeconds, <<"registeredUsers">> := RegisteredUsers,
      <<"onlineUsersNode">> := OnlineUsersNode, <<"onlineUsers">> := OnlineUsers,
      <<"incomingS2S">> := IncomingS2S, <<"outgoingS2S">> := OutgoingS2S} = Result,
    ?assert(is_not_negative_integer(UptimeSeconds)),
    ?assert(RegisteredUsers > 0),
    ?assertEqual(1, OnlineUsersNode),
    ?assertEqual(1, OnlineUsers),
    ?assert(is_not_negative_integer(IncomingS2S)),
    ?assert(is_not_negative_integer(OutgoingS2S)).

admin_stats_domain_test(Config) ->
    Result = get_ok_value([data, stat, domainStats], get_domain_stats(domain(), Config)),
    #{<<"registeredUsers">> := RegisteredUsers, <<"onlineUsers">> := OnlineUsers} = Result,
    ?assertEqual(0, RegisteredUsers),
    ?assertEqual(0, OnlineUsers).

admin_stats_domain_with_users_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_stats_domain_with_users_test/2).

admin_stats_domain_with_users_test(Config, _Alice) ->
    Result = get_ok_value([data, stat, domainStats], get_domain_stats(domain(), Config)),
    #{<<"registeredUsers">> := RegisteredUsers, <<"onlineUsers">> := OnlineUsers} = Result,
    ?assertEqual(1, RegisteredUsers),
    ?assertEqual(1, OnlineUsers).

% Commands

get_stats(Config) ->
    execute_command(<<"stat">>, <<"globalStats">>, #{}, Config).

get_domain_stats(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"stat">>, <<"domainStats">>, Vars, Config).

% Helpers

is_not_negative_integer(Number) when is_integer(Number), Number >= 0 ->
    true;
is_not_negative_integer(_) ->
    false.

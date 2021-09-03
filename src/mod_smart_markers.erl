%%%----------------------------------------------------------------------------
%%% @copyright (C) 2020, Erlang Solutions Ltd.
%%% @doc
%%%   This module implements storage of the latest chat markers
%%%   sent by the users. This can be used to optimize mod_offline
%%%   functionality, or to implement custom fetching protocol and
%%%   avoid storage of chat markers in MAM.
%%%
%%%   Please be aware of the next implementation details:
%%%
%%%    1) Current implementation is based on user_send_packet hook.
%%%       It doesn't work for s2s connections, but usage of another
%%%       hook (e.g. filter_local_packet) makes implementation harder
%%%       and results in multiple processing of one and the same
%%%       chat marker notification (sent to different users by MUC).
%%%       However that is the only possible way to deal with group
%%%       chat messages sent from the room to the user over s2s.
%%%
%%%       ```
%%%                                            S2S
%%%                                             +
%%%                                             |
%%%                +--------------------+       |
%%%                |                    |       |   filter
%%%                |                    +--------------->
%%%       send     |                    |       |   filter
%%%       +------->+       ROOM         +--------------->
%%%                |                    |       |   filter
%%%                |                    +--------------->
%%%                |                    |       |
%%%                +--------------------+       |
%%%                                             |
%%%                                             +
%%%    '''
%%%
%%%    2) DB backend requires us to provide host information, and
%%%       the host is always the recipient's server in case one2one
%%%       messages, and a master domain of the MUC service in case
%%%       of groupchat.
%%%
%%%    3) It is the client application's responsibility to ensure that
%%%       chat markers move only forward. There is no verification of
%%%       chat markers in this module, it just stores the latest chat
%%%       marker information sent by the user.
%%%
%%%    4) MUC light doesn't have message serialization! So it doesn't
%%%       guaranty one and the same message order for different users.
%%%       This can result in a race condition situation when different
%%%       users track (and mark) different messages as the last in a
%%%       chat history. However, this is a rare situation, and it self
%%%       recovers on the next message in the room. Anyway storing chat
%%%       markers in MAM doesn't fix this problem.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(mod_smart_markers).

-include("jlib.hrl").

-xep([{xep, 333}, {version, "0.3"}]).
-behaviour(gen_mod).

%% gen_mod API
-export([start/2]).
-export([stop/1]).
-export([supported_features/0]).

%% gen_mod API
-export([get_chat_markers/3]).

%% Hook handlers
-export([user_send_packet/4]).

-define(MOD_SMART_MARKERS_BACKEND, mod_smart_markers_backend).
-ignore_xref([
    {?MOD_SMART_MARKERS_BACKEND, get_chat_markers, 4},
    {?MOD_SMART_MARKERS_BACKEND, init, 2},
    {?MOD_SMART_MARKERS_BACKEND, update_chat_marker, 2},
    behaviour_info/1, user_send_packet/4
]).

%%--------------------------------------------------------------------
%% Type declarations
%%--------------------------------------------------------------------
-type maybe_thread() :: undefined | binary().
-type chat_type() :: one2one | groupchat.

-type chat_marker() :: #{from := jid:jid(),
                         to := jid:jid(),
                         thread := maybe_thread(), % it is not optional!
                         type := mongoose_chat_markers:chat_marker_type(),
                         timestamp := integer(), % microsecond
                         id := binary()}.

-export_type([chat_marker/0]).

%%--------------------------------------------------------------------
%% DB backend behaviour definition
%%--------------------------------------------------------------------
-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

%%% 'from', 'to', 'thread' and 'type' keys of the ChatMarker map serve
%%% as a composite database key. If key is not available in the database,
%%% then chat marker must be added. Otherwise this function must update
%%% chat marker record for that composite key.
-callback update_chat_marker(mongooseim:host_type(), chat_marker()) -> ok.

%%% This function must return the latest chat markers sent to the
%%% user/room (with or w/o thread) later than provided timestamp.
-callback get_chat_markers(HostType :: mongooseim:host_type(),
                           To :: jid:jid(),
                           Thread :: maybe_thread(),
                           Timestamp :: integer()) ->
    [chat_marker()].

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    gen_mod:start_backend_module(?MODULE, add_default_backend(Opts),
                                 [get_chat_markers, update_chat_marker]),
    mod_smart_markers_backend:init(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(hooks(HostType)).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%--------------------------------------------------------------------
%% Hook handlers
%%--------------------------------------------------------------------
-spec hooks(mongooseim:host_type()) -> [ejabberd_hooks:hook()].
hooks(HostType) ->
    [{user_send_packet, HostType, ?MODULE, user_send_packet, 90}].

-spec user_send_packet(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
	mongoose_acc:t().
user_send_packet(Acc, From, To, Packet = #xmlel{name = <<"message">>}) ->
    case has_valid_markers(Acc, From, To, Packet) of
        {true, HostType, Markers} ->
            update_chat_markers(Acc, HostType, Markers);
        false -> Acc
    end;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------
-spec get_chat_markers(jid:jid(), maybe_thread(), integer()) ->
    [chat_marker()].
get_chat_markers(#jid{lserver = LServer} = To, Thread, TS) ->
    %% internal API, no room access rights verification here!
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mod_smart_markers_backend:get_chat_markers(HostType, To, Thread, TS).

%%--------------------------------------------------------------------
%% Local functions
%%--------------------------------------------------------------------
-spec update_chat_markers(mongoose_acc:t(), mongooseim:host_type(), [chat_marker()]) ->
    mongoose_acc:t().
update_chat_markers(Acc, HostType, Markers) ->
    TS = mongoose_acc:timestamp(Acc),
    [mod_smart_markers_backend:update_chat_marker(HostType, CM) || CM <- Markers],
    mongoose_acc:set_permanent(?MODULE, timestamp, TS, Acc).

-spec has_valid_markers(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
    false | {true, mongooseim:host_type(), Markers :: [chat_marker()]}.
has_valid_markers(Acc, From, To, Packet) ->
    case is_valid_markers(Acc, From, To, Packet) of
        false -> false;
        {true, Markers} ->
            case is_valid_host(Acc, From, To) of
                false -> false;
                {true, HostType} -> {true, HostType, Markers}
            end
    end.

-spec is_valid_host(mongoose_acc:t(), jid:jid(), jid:jid()) ->
    false | {true, mongooseim:host_type()}.
is_valid_host(Acc, From, To) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"groupchat">> -> get_host(groupchat, From, To);
        _ -> get_host(one2one, From, To)
    end.

-spec is_valid_markers(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
    false | {true, [chat_marker()]}.
is_valid_markers(Acc, From, To, Packet) ->
    case extract_chat_markers(Acc, From, To, Packet) of
        [] -> false;
        ChatMarkers ->
            {true, ChatMarkers}
    end.

-spec extract_chat_markers(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
	[chat_marker()].
extract_chat_markers(Acc, From, To, Packet) ->
    TS = mongoose_acc:timestamp(Acc),
    case mongoose_chat_markers:list_chat_markers(Packet) of
        [] -> [];
        ChatMarkers ->
            CM = #{from => From, to => To, thread => get_thread(Packet), timestamp => TS},
            [CM#{type => Type, id => Id} || {Type, Id} <- ChatMarkers]
    end.

-spec get_thread(exml:element()) -> maybe_thread().
get_thread(El) ->
    case exml_query:path(El, [{element, <<"thread">>}, cdata]) of
        Thread when Thread =/= <<>> -> Thread;
        _ -> undefined
    end.

-spec get_host(chat_type(), jid:jid(), jid:jid()) ->
    false | {true, mongooseim:host_type()}.
get_host(groupchat, From, To) ->
    HostType = mod_muc_light_utils:room_jid_to_host_type(To),
    can_access_room(HostType, From, To) andalso {true, HostType};
get_host(one2one, _From, To) ->
    LServer = To#jid.lserver,
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} -> {true, HostType};
        {error, not_found} -> false
    end.

-spec can_access_room(HostType :: mongooseim:host_type(),
                      User :: jid:jid(), Room :: jid:jid()) -> boolean().
can_access_room(HostType, User, Room) ->
    mongoose_hooks:can_access_room(HostType, Room, User).

add_default_backend(Opts) ->
    case lists:keyfind(backend, 2, Opts) of
        false ->
            [{backend, rdbms} | Opts];
        _ ->
            Opts
    end.

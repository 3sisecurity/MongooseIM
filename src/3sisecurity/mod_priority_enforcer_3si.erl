%%%----------------------------------------------------------------------
%%% File    : mod_priority_enforcer_3si.erl
%%% Author  : Sacha Bernstein <sacha_bernstein@3sisecurity.com>
%%% Purpose : Force a priority of 17 on all connections
%%% Created : 15 Aug 2024 by Sacha Bernstein
%%%----------------------------------------------------------------------

-module(mod_priority_enforcer_3si).
-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("session.hrl").

-export([start/2, stop/1, set_presence/3, user_send_presence/3, hooks/1]).

start(_HostType, _Opts) ->
    ok.

stop(_HostType) ->
    ok.

hooks(HostType) ->
    [
        {set_presence, HostType, fun ?MODULE:set_presence/3, #{}, 100},
        {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 10}
    ].

-type priority() :: -128..127.

-spec get_priority_from_presence(exml:element()) -> priority().
get_priority_from_presence(PresencePacket) ->
    MaybePriority = exml_query:path(PresencePacket, [{element, <<"priority">>}, cdata], undefined),
    case catch binary_to_integer(MaybePriority) of
        P when is_integer(P), -128 =< P, P =< 127 -> P;
        _ -> 0
    end.

replace_priority_in_presence(Presence, NewPriority) ->
    OldPriority = get_priority_from_presence(Presence),
    ?LOG_INFO(#{
        what => presence_update,
        text => <<"Presence Update - Priorities">>,
        old_priority => OldPriority,
        new_priority => NewPriority
    }),
    case Presence of
        #xmlel{name = <<"presence">>, attrs = Attrs, children = Children} ->
            ?LOG_DEBUG(#{
                what => presence_update,
                text => <<"Presence Update Match">>,
                attrs => Attrs,
                children => Children
            }),
            NewPriorityBinary = integer_to_binary(NewPriority),
            NewInnerPriority = #xmlel{
                name = <<"priority">>,
                attrs = [],
                children = [#xmlcdata{content = NewPriorityBinary}]
            },
            NewPresence = xml:replace_subelement(Presence, NewInnerPriority),

            ?LOG_DEBUG(#{
                what => new_presence,
                text => <<"New Presence">>,
                new_presence => NewPresence
            }),
            NewPresence;
        _ ->
            ?LOG_INFO(#{
                what => presence_no_match,
                text => <<"Presence Update No Match">>,
                presence => Presence
            }),
            Presence
    end.

-spec set_presence(Acc, JID, Presence) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Presence :: any(),
    Result :: mongoose_acc:t().
set_presence(Acc, _JID, Presence) ->
    NewPriority = 17,

    case ejabberd_sm:get_session(mongoose_acc:from_jid(Acc)) of
        #session{
            priority = Priority,
            info = Info,
            sid = SID
        } ->
            case Priority of
                NewPriority ->
                    ?LOG_INFO(#{
                        what => priority_already_set,
                        text => <<"Priotity Already Set">>,
                        priority => NewPriority,
                        element => mongoose_acc:element(Acc)
                    });
                _ ->
                    ?LOG_INFO(#{
                        what => changing_priority,
                        text => <<"Priotity Change">>,
                        new_priority => NewPriority,
                        old_priority => Priority,
                        element => mongoose_acc:element(Acc)
                    }),

                    ejabberd_sm:set_presence(
                        Acc,
                        SID,
                        mongoose_acc:from_jid(Acc),
                        NewPriority,
                        Presence,
                        Info
                    )
            end;
        _ ->
            ?LOG_INFO(#{
                what => no_session_found,
                text => <<"No session found for priority update">>
            })
    end,

    NewPresence = replace_priority_in_presence(mongoose_acc:element(Acc), NewPriority),

    Acc1 = mongoose_acc:update_stanza(
        #{
            to_jid => mongoose_acc:to_jid(Acc),
            from_jid => mongoose_acc:from_jid(Acc),
            element => NewPresence
        },
        Acc
    ),

    {ok, Acc1}.

get_presence_type(Acc) ->
    case mongoose_acc:stanza_type(Acc) of
        %% Note that according to https://www.rfc-editor.org/rfc/rfc6121.html#section-4.7.1
        %% there is no default value nor "available" is considered a valid type.
        %% However, we keep accepting this for compatibility reasons.
        undefined -> available;
        <<"available">> -> available;
        <<"error">> -> error;
        <<"probe">> -> probe;
        <<"subscribe">> -> subscribe;
        <<"subscribed">> -> subscribed;
        <<"unsubscribe">> -> unsubscribe;
        <<"unsubscribed">> -> unsubscribed;
        <<"unavailable">> -> unavailable;
        _ -> {error, invalid_presence}
    end.

% We get called before mod_presence.  We need to adjust the packet in the accumulator, and mod_presence will see what we set :)
-spec user_send_presence(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_presence(Acc, #{c2s_data := _StateData}, _Extra) ->
    {FromJid, ToJid, Packet} = mongoose_acc:packet(Acc),
    NewPriority = 17,

    case {get_presence_type(Acc), jid:are_bare_equal(FromJid, ToJid)} of
        {{error, invalid_presence}, _} ->
            ?LOG_INFO(#{
                what => invalid_presence_type,
                text => <<"Invalid Presence Type">>,
                from => FromJid,
                to => ToJid
            }),
            {ok, Acc};
        {Type, true} ->
            ?LOG_INFO(#{
                what => presence_received,
                text => <<"Presence Received">>,
                from => FromJid,
                to => ToJid,
                type => Type
            }),

            NewPacket = replace_priority_in_presence(Packet, NewPriority),
            Acc1 = mongoose_acc:update_stanza(
                #{
                    to_jid => ToJid,
                    from_jid => FromJid,
                    element => NewPacket
                },
                Acc
            ),

            {ok, Acc1};
        {Type, false} ->
            ?LOG_INFO(#{
                what => presence_user_mismatch,
                text => <<"Presence User Mismatch">>,
                from => FromJid,
                to => ToJid,
                type => Type
            }),
            {ok, Acc}
    end.

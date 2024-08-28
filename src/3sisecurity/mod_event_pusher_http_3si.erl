%%%----------------------------------------------------------------------
%%% File    : mod_event_pusher_http_3si.erl
%%% Author  : Sacha Bernstein <sacha_bernstein@3sisecurity.com>
%%% Purpose : Message passing via http
%%% Created : 15 Aug 2024 by Sacha Bernstein
%%%----------------------------------------------------------------------

-module(mod_event_pusher_http_3si).
-author("sacha_bernstein@3sisecurity.com").

-behaviour(mod_event_pusher_http).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%% API
-export([should_make_req/6, prepare_body/7, prepare_headers/7]).

%% @doc This function determines whether to send http notification or not.
%% Can be reconfigured by creating a custom module implementing should_make_req/3
%% and adding it to mod_event_pusher_http settings as {callback_module}
%% Default behaviour is to send all chat messages with non-empty body.
should_make_req(_Acc, out, _Packet, _From, _To, _Opts) ->
    false;
should_make_req(Acc, in, Packet, From, To, Opts) ->
    Type = exml_query:attr(Packet, <<"type">>, <<>>),
    Body = exml_query:path(Packet, [{element, <<"body">>}, cdata], <<>>),
    should_make_req_type(Acc, Type, Body, From, To).

should_make_req_type(_Acc, <<"chat">>, Body, _From, _To) when Body /= <<"">> ->
    true;
should_make_req_type(_Acc, _, _, _, _) ->
    false.

standardise_jid(<<"@", Server/binary>>) ->
    <<"@", (stringprep:nodeprep(Server))/binary>>;
standardise_jid(B) when is_binary(B) ->
    jid:to_binary(jid:to_lower(jid:from_binary(B)));
standardise_jid(S) ->
    standardise_jid(list_to_binary(S)).

prepare_body(Acc, _Dir, _Host, Message, _Sender, _Receiver, _Opts) ->
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    M = #{
        author => #{
            username => From#jid.luser,
            server => From#jid.lserver,
            resource => From#jid.lresource
        },
        receiver => #{
            username => To#jid.luser,
            server => To#jid.lserver,
            resource => To#jid.lresource
        },
        message => Message
    },
    jiffy:encode(M).

prepare_headers(_Acc, _Dir, _Host, _Sender, _Receiver, _Message, _Opts) ->
    [{<<"Content-Type">>, <<"application/json">>}].

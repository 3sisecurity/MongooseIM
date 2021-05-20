%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_jwt.erl
%%% Author  : Astro <astro@spaceboyz.net>
%%% Purpose : Authentification with JSON Web Tokens
%%% Created : 02 Aug 2016 by Stephan Maka <stephan@spaceboyz.net>
%%%
%%%
%%% MongooseIM, Copyright (C) 2016   CostaDigital
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_jwt).
-author('astro@spaceboyz.net').

%% External exports
-behaviour(mongoose_gen_auth).

-export([start/1,
         stop/1,
         authorize/1,
         check_password/4,
         check_password/6,
         does_user_exist/3,
         supports_sasl_module/2,
         supported_features/0
        ]).


-include("mongoose.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type()) -> ok.
start(HostType) ->
    UsernameKey = ejabberd_auth:get_opt(HostType, jwt_username_key),
    true = is_atom(UsernameKey) andalso UsernameKey /= undefined,

    JWTSecret = get_jwt_secret(HostType),
    JWTAlgorithm = ejabberd_auth:get_opt(HostType, jwt_algorithm),
    ejabberd_auth:set_opts(HostType,
                           [{jwt_secret, JWTSecret},
                           {jwt_algorithm, list_to_binary(JWTAlgorithm)}]),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec supports_sasl_module(binary(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_plain.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(HostType, LUser, LServer, Password) ->
    Key = case ejabberd_auth:get_opt(HostType, jwt_secret) of
              Key1 when is_binary(Key1) -> Key1;
              {env, Var} -> list_to_binary(os:getenv(Var))
          end,
    BinAlg = ejabberd_auth:get_opt(HostType, jwt_algorithm),
    Alg = binary_to_atom(jid:str_tolower(BinAlg), utf8),
    case jwerl:verify(Password, Alg, Key) of
        {ok, TokenData} ->
            UserKey = ejabberd_auth:get_opt(HostType, jwt_username_key),
            case maps:find(UserKey, TokenData) of
                {ok, LUser} ->
                    %% Login username matches $token_user_key in TokenData
                    ?LOG_INFO(#{what => jwt_success_auth,
                                text => <<"Successfully authenticated with JWT">>,
                                user => LUser, server => LServer,
                                token => TokenData}),
                    true;
                {ok, ExpectedUser} ->
                    ?LOG_WARNING(#{what => wrong_jwt_user,
                                   text => <<"JWT contains wrond user">>,
                                   expected_user => ExpectedUser,
                                   user => LUser, server => LServer}),
                    false;
                error ->
                    ?LOG_WARNING(#{what => missing_jwt_key,
                                   text => <<"Missing key {user_key} in JWT data">>,
                                   user_key => UserKey, token => TokenData,
                                   user => LUser, server => LServer}),
                    false
            end;
        {error, Reason} ->
            ?LOG_WARNING(#{what => jwt_verification_failed,
                           text => <<"Cannot verify JWT for user">>,
                           reason => Reason,
                           user => LUser, server => LServer}),
            false
    end.


-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(HostType, LUser, LServer, Password, _Digest, _DigestGen) ->
    check_password(HostType, LUser, LServer, Password).

-spec does_user_exist(HostType :: mongooseim:host_type(),
                      LUser :: jid:luser(),
                      LServer :: jid:lserver()) -> boolean() | {error, atom()}.
does_user_exist(_HostType, _LUser, _LServer) ->
    true.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%----------------------------------------------------------------------
%%% Internal helpers
%%%----------------------------------------------------------------------

% A direct path to a file is read only once during startup,
% a path in environment variable is read on every auth request.
get_jwt_secret(HostType) ->
   JWTSource = ejabberd_auth:get_opt(HostType, jwt_secret_source),
   JWTSecret = ejabberd_auth:get_opt(HostType, jwt_secret),

   case {JWTSource, JWTSecret} of
       {undefined, JWTSecret0} when is_list(JWTSecret0) ->
           list_to_binary(JWTSecret0);
       {undefined, JWTSecret0} when is_binary(JWTSecret0) ->
           JWTSecret0;
       {{env, _} = Env, _} ->
           Env;
       {JWTSecretPath, _} when is_list(JWTSecretPath) ->
           {ok, JWTSecretBin} = file:read_file(JWTSecretPath),
           JWTSecretBin
   end.

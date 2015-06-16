%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(pal_vk_user).
-behaviour(pal_authentication).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0
]).

%% Authentication callbacks
-export([
	authenticate/4,
	info/2
]).

%% Definitions
-define(VKONTAKTE_URI, <<"https://vk.com">>).
-define(VKONTAKTE_API_URI, <<"https://api.vk.com">>).
-define(VKONTAKTE_API_VERSION, <<"5.8">>).
-define(FIELDS, <<"first_name,last_name,nickaname,domain,bdate,photo_50,sex,contacts">>).

-define(RESPONSE, <<"response">>).
-define(FIRST_NAME, <<"first_name">>).
-define(LAST_NAME, <<"last_name">>).
-define(NICKNAME, <<"nickname">>).
-define(DOMAIN, <<"domain">>).
-define(BDATE, <<"bdate">>).
-define(PHOTO_50, <<"photo_50">>).
-define(SEX, <<"sex">>).
-define(MALE, <<"male">>).
-define(FEMALE, <<"female">>).
-define(OTHER, <<"other">>).
-define(CONTACTS, <<"contacts">>).
-define(HOME_PHONE, <<"home_phone">>).

%% Types
-type data() :: #{uid => binary()}.

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{request_options => [{follow_redirect, true}]},

	{pal_authentication, ?MODULE, Opts}.

%% ============================================================================
%% Authentication callbacks
%% ============================================================================

-spec authenticate(list(module()), data(), map(), map()) -> pal_authentication:result().
authenticate(Hs, #{uid := UID} = Data, Meta, State) ->
	#{request_options := ReqOpts} = State,

	Uri =
		<<?VKONTAKTE_API_URI/binary, "/method/users.get",
				$?, "v=", ?VKONTAKTE_API_VERSION/binary,
				$&, "https=1",
				$&, "user_ids=", UID/binary,
				$&, "fields=", ?FIELDS/binary>>,

	case hackney:get(Uri, [], <<>>, ReqOpts) of
		{ok, 200, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{ok, jsx:decode(Body)};
		{ok, _, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{error, {vkontakte, Body}};
		{error, Reason} ->
			exit({Reason, {?MODULE, authenticate, [Hs, Data, Meta, State]}})
	end.

-spec info(pal_authentication:rawdata(), map()) -> map().
info([{?RESPONSE, [Data]}], M) ->
	#{first_name := FirstName,
		last_name := LastName} = M2 = info_(Data, M),

	Name = <<FirstName/binary, $\s, LastName/binary>>,
	M2#{name => Name}.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec info_(pal_authentication:rawdata(), map()) -> map().
info_([{?FIRST_NAME, Val}|T], M)  -> info_(T, M#{first_name => Val});
info_([{?LAST_NAME, Val}|T], M)   -> info_(T, M#{last_name => Val});
info_([{?DOMAIN, Val}|T], M)      -> info_(T, M#{uri => uri(Val)});
info_([{?NICKNAME, <<>>}|T], M)   -> info_(T, M);
info_([{?NICKNAME, Val}|T], M)    -> info_(T, M#{nickaname => Val});
info_([{?HOME_PHONE, <<>>}|T], M) -> info_(T, M);
info_([{?HOME_PHONE, Val}|T], M)  -> info_(T, M#{phone => Val});
info_([{?PHOTO_50, <<>>}|T], M)   -> info_(T, M);
info_([{?PHOTO_50, Val}|T], M)    -> info_(T, M#{image => Val});
info_([{?SEX, <<>>}|T], M)        -> info_(T, M);
info_([{?SEX, Val}|T], M)         -> info_(T, M#{gender => parse_sex(Val)});
info_([_|T], M)                   -> info_(T, M);
info_([], M)                      -> M.

-spec uri(binary()) -> binary().
uri(Domain) ->
	<<?VKONTAKTE_URI/binary, $/, Domain/binary>>.

-spec parse_sex(non_neg_integer()) -> binary().
parse_sex(1) -> ?FEMALE;
parse_sex(2) -> ?MALE;
parse_sex(_) -> ?OTHER.


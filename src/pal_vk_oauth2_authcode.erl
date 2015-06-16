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

-module(pal_vk_oauth2_authcode).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0
]).

%% Authentication workflow callbacks
-export([
	uid/1,
	info/2,
	credentials/2
]).

% Definitions
-define(ACCESS_TOKEN, <<"access_token">>).
-define(EXPIRES_IN, <<"expires_in">>).
-define(USER_ID, <<"user_id">>).
-define(EMAIL, <<"email">>).

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{authorization_uri => <<"https://oauth.vk.com/authorize">>,
			access_token_uri  => <<"https://oauth.vk.com/access_token">>,
			scope             => [?EMAIL]},

	{pal_oauth2_authcode, ?MODULE, Opts}.

%% ============================================================================
%% Authentication workflow callbacks
%% ============================================================================

-spec uid(pal_authentication:rawdata()) -> binary().
uid(Data) ->
	integer_to_binary(pt_kvlist:get(?USER_ID, Data)).

-spec credentials(pal_authentication:rawdata(), map()) -> map().
credentials([{?ACCESS_TOKEN, Val}|T], M) -> credentials(T, M#{access_token => Val});
credentials([{?EXPIRES_IN, Val}|T], M)   -> credentials(T, M#{expires_in => Val});
credentials([_|T], M)                    -> credentials(T, M);
credentials([], M)                       -> M.

-spec info(pal_authentication:rawdata(), map()) -> map().
info([{?EMAIL, Val}|T], M) -> info(T, M#{email => Val});
info([_|T], M)             -> info(T, M);
info([], M)                -> M.


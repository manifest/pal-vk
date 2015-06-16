# Pragmatic Authentication Library: VK (VKontakte) workflows

Collection of VK workflows for [PAL][pal].

### 1. VK Login (OAuth2 Authorization Code Grant) workflow

For details, read the VK [documentation][vk-oauth2-doc].

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `client_id` (required) -
		The client ID obtained from the [App dashboard][vk-app-dashboard].
- `client_secret` (required) -
		The client secret obtained from the [App Dashboard][vk-app-dashboard].
- `redirect_uri` (required) -
		The client redirection endpoint.
		After completing its interaction with the resource owner,
		the authorization server directs the resource owner's user-agent to this uri.
- `scope` (optional) -
		A list of requested [permissions][vk-oauth2-scope].
- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by this workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `code` -
		The authorization code.
- `state` -
		The state previously passed to the authentication provider.
- `error`
		When request fails due to a missing, invalid, or mismatching
		redirection URI, or if the client identifier is missing or invalid.

#### Authentication Schema

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

```erlang
#{access_token => <<"...">>, 
  expires_in => 86396,
  uid => <<"...">>,
  info =>
    #{email => <<"john@example.com">>}}
```

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 2. VK User (user's profile data) workflow

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by this workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `uid` -
		A user identifier obtained using the `pal_vk_oauth2_authcode` workflow.

#### Authentication Schema

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

```erlang
#{info =>
    #{name => <<"John Doe">>,
      first_name => <<"John">>,
      last_name => <<"Doe">>,
      gender => <<"male">>,
      email => <<"john@example.com">>,
      image => <<"https://cs625824.vk.me/...">>,
      uri => <<"https://vk.com/...">>}}
```

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[cowboy]:https://github.com/extend/cowboy
[vk-oauth2-doc]:https://vk.com/dev/auth_sites
[vk-oauth2-scope]:https://vk.com/dev/permissions
[vk-app-dashboard]:https://vk.com/apps?act=manage
[hackney]:https://github.com/benoitc/hackney
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example


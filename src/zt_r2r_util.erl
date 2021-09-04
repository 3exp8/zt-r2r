-module(zt_r2r_util).

-export([
			send_sms/3,
			send_sms/4,
			call/2,
			send_email/5,
			get_reminder_token/0,
			buildPath/0
		]).

%% echo -n username:password | md5sum

-define(Protocol,application:get_env(zt_r2r, reminder_protocol, http)).
-define(Host,application:get_env(zt_r2r, reminder_host, "reminder-api")).
-define(Port, application:get_env(zt_r2r, reminder_port, 80)).
-define(Base64Secret, zt_util:to_bin(application:get_env(zt_r2r, reminder_secret_key, ""))).
-define(CampaignId, zt_util:to_bin(application:get_env(zt_r2r, reminder_campaign_id, ""))).
-define(Caller, zt_util:to_bin(application:get_env(zt_r2r, caller, ""))).
-define(CB_FUNC,application:get_env(zt_push, cb_fun, [])).

-define(REMINDER_SMS_API, "/api/v1/smss").
-define(REMINDER_EMAIL_API, "/api/v1/emails").
-define(REMINDER_ACCESS_TOKEN_API, "/api/v1/users/generate_access_token").
-define(REMINDER_CALL_START_API, "/api/v2/campaigns/").
-define(REMINDER_CALL_END_API, "/otp").

start_conn(Host, Port)->
	start_conn(http, Host, Port).

start_conn(http, Host, Port)->
	{ok, Conn} = shotgun:open(Host, Port),
	Conn;

start_conn(https, Host, Port)->
	{ok, Conn} = shotgun:open(Host, Port, https),
	Conn.

stop_conn(Conn)->
	shotgun:close(Conn),
	ok.

send_sms(From, To, Text)->
	send_sms(From, To, Text, false).

send_sms(From, To, Text, Confirm)->
	Conn = start_conn(?Protocol, ?Host, ?Port),
	Token = get_token(Conn),
	{ok, Resp} = send_sms(Conn,Token, From, To, Text, Confirm),
	lager:info("SMS Resp ~p ~n", [Resp]),
	stop_conn(Conn).

send_email(FromEmail, FromName,ToEmail, Subject, Content)->
	Conn = start_conn(?Protocol, ?Host, ?Port),
	Token = get_token(Conn),
	{ok, Resp} = send_email(Conn,Token, FromEmail, FromName, ToEmail, Subject, Content),
	lager:info("Email Resp ~p ~n", [Resp]),
	stop_conn(Conn).

get_token(Conn) ->
	{Body, Url} = zt_rb_api_helper:get_token_request(),
	Header = zt_rb_api_helper:create_header(?Base64Secret),
	{ok, Response} = shotgun:post(Conn, Url, Header, Body, #{}),
	zt_rb_api_helper:get_response_value(Response,<<"auth_token">>).

send_sms(Conn,Token, From, To, Text, Confirm) when is_binary(To) ->
	{Header,Body} = zt_rb_api_helper:get_sms_request(Token, From, To, Text, Confirm),
	Url = "/api/v1/smss",
	shotgun:post(Conn, Url, Header, Body, #{});

send_sms(Conn,Token, From, To, Text, Confirm) when is_list(To) ->
	{Header,Body} = zt_rb_api_helper:get_sms_request(Token, From, To, Text, Confirm),
	Path = ?REMINDER_SMS_API,
	shotgun:post(Conn, Path, Header, Body, #{}).

send_email(Conn,Token, FromEmail, FromName, ToEmail, Subject, Content)->
	OptionalsData = #{
		from_name => FromName
	},
	{Header,Body} = zt_rb_api_helper:get_email_request(Token, FromEmail, ToEmail, Subject, Content, OptionalsData),
	Path = ?REMINDER_EMAIL_API,
	shotgun:post(Conn, Path, Header, Body, #{}).

call(To, CallInfo) ->
	Conn = start_conn(?Protocol, ?Host, ?Port),
	Token = get_reminder_token(),
	{ok, Resp} = call(Conn, Token, To, CallInfo),
	lager:info("AutoCall Response: ~p ~n", [Resp]),
	stop_conn(Conn).

call(Conn, Token, To, CallInfo) when is_binary(To) ->
	{Header, Body} = zt_rb_api_helper:get_call_request(Token, ?Caller, To, CallInfo),
	Path = buildPath(),
	shotgun:post(Conn, Path, Header, Body, #{});

call(Conn, Token, To, CallInfo) when is_list(To) ->
	{Header, Body} = zt_rb_api_helper:get_call_request(Token, ?Caller, To, CallInfo),
	Path = buildPath(),
	shotgun:post(Conn, Path, Header, Body, #{}).

get_reminder_token() ->
	Conn = start_conn(?Protocol, ?Host, ?Port),
	Path = ?REMINDER_ACCESS_TOKEN_API,
	{Header, Body} = zt_rb_api_helper:get_call_access_token_request(?Base64Secret),
	{ok, Response} = shotgun:post(Conn, Path, Header, Body, #{}),
	Token = zt_rb_api_helper:get_call_access_token_response_value(Response),
	lager:info("AutoCall Token: ~p ~n", [Token]),
	stop_conn(Conn),
	Token.

buildPath() ->
	Path = ?REMINDER_CALL_START_API ++ binary_to_list(?CampaignId) ++ ?REMINDER_CALL_END_API,
	Path.
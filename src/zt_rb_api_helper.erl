-module(zt_rb_api_helper).

-export([
	create_header/0,
	create_header/1
]).
-export([get_token_request/0]).
-export([
	get_response_value/2,
	get_response_value/3
]).
-export([
			get_sms_request/4
			,get_sms_request/5
			,get_email_request/5
			,get_email_request/6
			,get_call_access_token_request/1
			,get_call_access_token_response_value/1
			,get_call_request/4
		]).

create_header() ->
	#{<<"Accept">> => <<"application/json">>}.

create_header(Base64Secret) ->
	HeaderMap = #{<<"Content-Type">> => <<"application/x-www-form-urlencoded">>},
	maps:put(<<"Authorization">>, list_to_binary("Basic "++Base64Secret), HeaderMap).

get_token_request() ->
	Body = <<"grant_type=client_credentials&scope=yourbase">>,
	Url = "/api/v1/auth",
	{Body,Url}.

get_sms_request(Token, From, To, Text) ->
	Header = #{
		<<"Accept">> => <<"application/json">>,
		<<"Content-Type">> => <<"application/json">>,
		<<"Authorization">> => list_to_binary("Bearer "++Token)
	},
	Body = jsx:encode([
		{<<"type">>,<<"sms">>},
		{<<"from">>,From},
		{<<"text">>,Text},
		{<<"to">>,To},
		{<<"callback_url">>,<<"">>}
	]),
	{Header,Body}.

get_sms_request(Token, From, To, Text, Confirm) ->
	Header = #{
		<<"Accept">> => <<"application/json">>,
		<<"Content-Type">> => <<"application/json">>,
		<<"Authorization">> => list_to_binary("Bearer "++Token)
	},
	Body = jsx:encode([
		{<<"type">>,<<"sms">>},
		{<<"from">>,From},
		{<<"text">>,Text},
		{<<"to">>,To},
		{<<"callback_url">>,<<"">>},
		{<<"confirm">>, Confirm}
	]),
	{Header,Body}.

get_email_request(Token, From, To, Subject, Content) ->
	get_email_request(Token, From, To, Subject, Content, #{}).

get_email_request(Token, From, To, Subject, Content, OptionsData) when is_map(OptionsData) ->
	Header = #{
					<<"Accept">> => <<"application/json">>,
					<<"Content-Type">> => <<"application/json">>,
					<<"Authorization">> => list_to_binary("Bearer "++Token)
			},
	BodyBaseData = #{
		type => <<"email">>,
		from => From,
		to => To,
		subject => Subject,
		content => Content,
		confirm => <<"false">>,
		confirm_url_template => <<"http://reminder.zentech.io/email_reminder/{request_id}/{secret_code}">>
	},

	BodyData = maps:merge(BodyBaseData, OptionsData),
	Body = jsx:encode(maps:to_list(BodyData)),
	{Header,Body}.

get_response_value(Response,Key) ->
	ResBody = maps:get(body,Response),
	ResDecode = jsx:decode(ResBody),
	proplists:get_value(Key,ResDecode,<<>>).

get_response_value(Response, ParentKey, ChildKey) ->
	ParentValue = get_response_value(Response,ParentKey),
	proplists:get_value(ChildKey,ParentValue,<<>>).

get_call_access_token_request(SecretKey) ->
	Header = 	[
		{<<"Accept">>,<<"application/json">>},
		{<<"Content-Type">>,<<"application/json">>}
	],
	PrePayload = #{
		secret_key => SecretKey
	},
	Payload = maps:to_list(PrePayload),
	Body = jsx:encode(Payload),
	{Header, Body}.

get_call_access_token_response_value(Response) ->
	ResBody = maps:get(body,Response),
	ResDecode = jsx:decode(ResBody),
	Content = proplists:get_value(<<"data">>, ResDecode),
	ContentMap = maps:from_list(Content),
	Token = maps:get(<<"access_token">>, ContentMap),
	Token.

get_call_request(Token, Caller, To, CallInfo) ->
	Header =[
		{<<"Accept">>,<<"application/json">>},
		{<<"Content-Type">>,<<"application/json">>},
		{<<"Authorization">>,Token}
	],
	PrePayload = #{
		caller => Caller,
		callees => [To],
		params => #{
			template_name => 'covidai_template_otp_1',
			code_otp => CallInfo
		}
	},
	Payload = maps:to_list(PrePayload),
	Body = jsx:encode(Payload),
	{Header, Body}.

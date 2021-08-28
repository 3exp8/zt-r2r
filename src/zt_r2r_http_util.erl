-module(zt_r2r_http_util).

-export([do_http_get/3,
		do_http_post/4]).

start_conn(Host, Port)->
	{ok, Conn} = shotgun:open(Host, Port),
	Conn.

stop_conn(Conn)->
	shotgun:close(Conn),
	ok.

-spec do_http_get(string(),integer(),string()) -> binary().
do_http_get(Host, Port, Url)->	
	Conn = start_conn(Host, Port),
	{ok, Response} = shotgun:get(Conn, Url, #{}),
	io:format("Response: ~p~n", [Response]),
	stop_conn(Conn),
	Response.

do_http_post(Host, Port, Path, Body) ->
	Conn = start_conn(Host, Port),
	GunRes = shotgun:post(Conn, Path, #{<<"Content-Type">> => <<"application/json">> }, Body,#{}), 
	stop_conn(Conn),
	GunRes.
-module(zt_r2r_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(term(), term()) -> ok.
start(_StartType, _StartArgs) ->
    zt_r2r_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
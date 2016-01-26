-module(sctp_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, accept/1, accept_connection/5,
         setup/5, close/1, select/1, is_node_name/1]).

%% internal exports

-export([accept_loop/2, do_accept/6, do_setup/6, getstat/1, tick/1]).

-import(error_logger,[error_msg/2]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

-define(to_port(Socket, Assoc, Data, _Opts),
    case gen_sctp:send(Socket, Assoc, 0, Data) of
        {error, closed} ->
        self() ! {sctp_closed, Socket},
            {error, closed};
        R ->
            R
        end).


-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

%% Causion:
%%	After Erlang R6, version Number = 5
%%	We rule that for using our tools, Erlang version should be no lower than R6.
-define(DIST_VERSION, 5).

-define(LOCALHOST, {127, 0, 0, 1}).

-define(MAX_CREATION, 3).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    gen_select(inet, Node).

gen_select(Driver, Node) ->
    case split_node(atom_to_list(Node), $@, []) of
        [_, Host] ->
            case inet:getaddr(Host, Driver) of
                {ok,_} -> true;
                _ -> false
            end;
        _ -> false
    end.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name) ->
    case do_listen(gen_sctp, [{active, false}, {reuseaddr, true}]) of
        {ok, Socket} ->
            SctpAddress = get_sctp_address(Socket),
            {_, Port} = SctpAddress#net_address.address,
            case erl_epmd:register_node(Name, Port) of
                {ok, Creation} ->
                    {ok, {Socket, SctpAddress, Creation}};
                Error ->
                    Error
            end
    end.

do_listen(Driver, Options) ->
    {First,Last} = case application:get_env(kernel,inet_dist_listen_min) of
                       {ok,N} when is_integer(N) ->
                           case application:get_env(kernel,
                                                    inet_dist_listen_max) of
                               {ok,M} when is_integer(M) ->
                                   {N,M};
                               _ ->
                                   {N,N}
                           end;
                       _ ->
                           {30000,30500}
                   end,
    do_listen(Driver, First, Last, listen_options(Options)).

do_listen(_Driver, First,Last,_) when First > Last ->
    {error,eaddrinuse};
do_listen(Driver, First,Last,Options) ->
    case gen_sctp:open(First, [{mode,list} | Options]) of
        {ok, Socket} ->
            case Driver:listen(Socket, true) of
                {error, eaddrinuse} ->
                    do_listen(Driver, First+1, Last, Options);
                ok ->
                    {ok, Socket}
            end;
        _ ->
            do_listen(Driver, First+1, Last, Options)
    end.

listen_options(Opts0) ->
    Opts1 =
        case application:get_env(kernel, inet_dist_use_interface) of
            {ok, Ip} ->
                [{ip, Ip} | Opts0];
            _ ->
                Opts0
        end,
    case application:get_env(kernel, inet_dist_listen_options) of
        {ok,ListenOpts} ->
            ListenOpts ++ Opts1;
        _ ->
            Opts1
    end.

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    spawn_opt(?MODULE, accept_loop, [self(), Listen], [link, {priority, max}]).

accept_loop(Kernel, Listen) ->
    case gen_sctp:recv(Listen) of
        {ok, {_, _, [], NewAssoc=#sctp_assoc_change{}}} ->
            case gen_sctp:peeloff(Listen, NewAssoc) of
                {ok, Socket} ->
                    Kernel ! {accept, self(), {Socket, NewAssoc}, inet, sctp},
                    _ = controller(Kernel, Socket),
                    accept_loop(Kernel, Listen);
                Error ->
                    exit(Error)
            end;
        {ok, _} ->
            timer:sleep(100),
            accept_loop(Kernel, Listen)
    end.

controller(Kernel, Socket) ->
    receive
        {Kernel, controller, Pid} ->
            flush_controller(Pid, Socket),
            gen_sctp:controlling_process(Socket, Pid),
            flush_controller(Pid, Socket),
            Pid ! {self(), controller};
        {Kernel, unsupported_protocol} ->
            exit(unsupported_protocol)
    end.

flush_controller(Pid, Socket) ->
    receive
        {sctp, Socket, Data} ->
            Pid ! {sctp, Socket, Data},
            flush_controller(Pid, Socket);
        {sctp_closed, Socket} ->
            Pid ! {sctp_closed, Socket},
            flush_controller(Pid, Socket)
    after 0 ->
            ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, do_accept,
              [self(), AcceptPid, Socket, MyNode, Allowed, SetupTime],
              [link, {priority, max}]).

do_accept(Kernel, AcceptPid, {Socket, Assoc}, MyNode, Allowed, SetupTime) ->
    receive
        {AcceptPid, controller} ->
            Timer = dist_util:start_timer(SetupTime),
            case check_ip(Socket) of
                true ->
                    HSData = #hs_data{
                      kernel_pid = Kernel,
                      this_node = MyNode,
                      socket = {Socket, Assoc},
                      timer = Timer,
                      this_flags = 0,
                      allowed = Allowed,
                      f_send = fun({S,A},D) -> gen_sctp:send(S,A,0,D) end,
                      f_recv = fun({S,_},_,_T) ->
                                   {ok, {_FromIP, _FromPort, _AncData, Data}} = gen_sctp:recv(S),
                                   {ok, Data}
                               end,
                      f_setopts_pre_nodeup =
                          fun({S,_}) ->
                              inet:setopts(S,
                                           [{mode,list},
                                            {active, false},
                                            nodelay()])
                          end,
                      f_setopts_post_nodeup =
                          fun({S,_}) ->
                              inet:setopts(S,
                                           [{mode,list},
                                            {active, true},
                                            nodelay()])
                          end,
                      f_getll = fun({S,_}) -> inet:getll(S) end,
                      f_address = fun get_remote_id/2,
                      mf_tick = fun ?MODULE:tick/1,
                      mf_getstat = fun ?MODULE:getstat/1
                     },
                    dist_util:handshake_other_started(HSData);
                {false,IP} ->
                    error_msg("** Connection attempt from "
                             "disallowed IP ~w ** ~n", [IP]),
                    ?shutdown(no_node)
            end
    end.


%% we may not always want the nodelay behaviour
%% for performance reasons

nodelay() ->
    case application:get_env(kernel, dist_nodelay) of
        undefined ->
            {sctp_nodelay, true};
        {ok, true} ->
            {sctp_nodelay, true};
        {ok, false} ->
            {sctp_nodelay, false};
        _ ->
            {sctp_nodelay, true}
    end.


%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------
get_remote_id({Socket,_}, Node) ->
    case inet:peername(Socket) of
    {ok, Address} ->
        case split_node(atom_to_list(Node), $@, []) of
        [_, Host] ->
            #net_address{address=Address, host=Host,
                 protocol=sctp, family=inet};
        _ ->
            %% No '@' or more than one '@' in node name.
            ?shutdown(no_node)
        end;
    {error, _Reason} ->
        ?shutdown(no_node)
    end.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    spawn_opt(?MODULE, do_setup,
          [self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
          [link, {priority, max}]).

do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?trace("~p~n",[{sctp_dist, self(), setup, Node}]),
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
        {ok, Ip} ->
            Timer = dist_util:start_timer(SetupTime),
            case erl_epmd:port_please(Name, Ip) of
                {port, SctpPort, _} ->
                    Version = ?DIST_VERSION,
                    ?trace("node_to_port(~p) -> version ~p~n",
                           [Node, Version]),
                    dist_util:reset_timer(Timer),
                    {ok, Socket} = gen_sctp:open([{mode,list}]),
                    case gen_sctp:connect(Socket, {127,0,0,1}, SctpPort,
                                          [{active, false},{mode,list}]) of
                        {ok, Assoc} ->
                            HSData = #hs_data{
                              kernel_pid = Kernel,
                              other_node = Node,
                              this_node = MyNode,
                              socket = {Socket, Assoc},
                              timer = Timer,
                              this_flags = 0,
                              other_version = Version,
                              f_send = fun({S, A}, D) -> gen_sctp:send(S,A,0,D) end,
                              f_recv = fun({S, _}, _, _T) ->
                                           {ok, {_FromIP, _FromPort, _AncData, Data}} = gen_sctp:recv(S),
                                           {ok, Data}
                                          end,
                              f_setopts_pre_nodeup =
                                  fun({S, _}) ->
                                      inet:setopts
                                        (S,
                                         [{mode,list},
                                          {active, false},
                                          nodelay()])
                                  end,
                              f_setopts_post_nodeup =
                                  fun({S, _}) ->
                                      inet:setopts
                                        (S,
                                         [{mode,list},
                                          {active, true},
                                          {deliver, port},
                                          nodelay()])
                                  end,
                              f_getll = fun({S,_}) -> inet:getll(S) end,
                              f_address =
                                  fun(_, _) ->
                                      #net_address{
                                       address = {{127,0,0,1}, SctpPort},
                                       host = Address,
                                       protocol = sctp,
                                       family = inet}
                                  end,
                              mf_tick = fun ?MODULE:tick/1,
                              mf_getstat = fun ?MODULE:getstat/1,
                              request_type = Type
                             },
                            dist_util:handshake_we_started(HSData);
                        _ ->
                            %% Other Node may have closed since
                            %% node_to_port!
                            ?trace("other node (~p) "
                                  "closed since node_to_port.~n",
                                  [Node]),
                            ?shutdown(Node)
                    end;
                _ ->
                    ?trace("node_to_port (~p) "
                          "failed.~n", [Node]),
                    ?shutdown(Node)
            end;
        _ ->
            ?trace("inet_getaddr(~p) "
                  "failed.~n", [Node]),
            ?shutdown(Node)
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    gen_sctp:close(Socket).

%% New node name rule 'CID@IP' is almost the same
%% with the original node name rule, except that Name is a Container ID. So the judgement
%% rule here is the same.
%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
        [Name|Tail] when Tail =/= [] ->
            Host = lists:append(Tail),
            case split_node(Host, $., []) of
                [_] when LongOrShortNames =:= longnames ->
                    error_msg("** System running to use "
                             "fully qualified "
                             "hostnames **~n"
                             "** Hostname ~s is illegal **~n",
                             [Host]),
                    ?shutdown(Node);
                L when length(L) > 1, LongOrShortNames =:= shortnames ->
                    error_msg("** System NOT running to use fully qualified "
                             "hostnames **~n"
                             "** Hostname ~s is illegal **~n",
                             [Host]),
                    ?shutdown(Node);
                _ ->
                    [Name, Host]
            end;
        [_] ->
            error_msg("** Nodename ~p illegal, no '@' character **~n",
                      [Node]),
            ?shutdown(Node);
        _ ->
            error_msg("** Nodename ~p illegal **~n", [Node]),
            ?shutdown(Node)
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

%% ------------------------------------------------------------
%% Fetch local information about a Socket.
%% ------------------------------------------------------------
get_sctp_address(Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
                   address = Address,
                   host = Host,
                   protocol = sctp,
                   family = inet
                 }.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Socket) ->
    case application:get_env(check_ip) of
        {ok, true} ->
            case get_ifs(Socket) of
                {ok, IFs, IP} ->
                    check_ip(IFs, IP);
                _ ->
                    ?shutdown(no_node)
            end;
        _ ->
            true
    end.

get_ifs(Socket) ->
    case inet:peername(Socket) of
        {ok, {IP, _}} ->
            case inet:getif(Socket) of
                {ok, IFs} -> {ok, IFs, IP};
                Error     -> Error
            end;
        Error ->
            Error
    end.

check_ip([{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {mask(Netmask, PeerIP), mask(Netmask, OwnIP)} of
    {M, M} -> true;
    _      -> check_ip(IFs, PeerIP)
    end;
check_ip([], PeerIP) ->
    {false, PeerIP}.

mask({M1,M2,M3,M4}, {IP1,IP2,IP3,IP4}) ->
    {M1 band IP1,
     M2 band IP2,
     M3 band IP3,
     M4 band IP4}.

is_node_name(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
        [_, _Host] -> true;
        _ -> false
    end;
is_node_name(_Node) ->
    false.

tick({Sock, Assoc}) ->
    ?to_port(Sock,Assoc,[],[force]).

getstat({Socket, _Assoc}) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
    {ok, Stat} ->
        split_stat(Stat,0,0,0);
    Error ->
        Error
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

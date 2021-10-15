-module(sqmfeedback).
-export([main/0,pingtimes_ms/3,monitor_ifaces/3,monitor_a_site/1,adjuster/1,monitor_delays/2,timer_process/3,monitor_bw/2 ]).


%% Read a line of ping output and extract the time in milliseconds

pingline_to_ms(Line) ->
    case re:replace(Line,".*time=([0-9.]+) ms","\\1") of
	[[L]] ->
	    try binary_to_float(L) 
	    catch 
		error:_Reason -> 
		    try float(binary_to_integer(L)) 
		    catch
			error:_ -> false
		    end
	    end;
	_ -> false
    end.


%% ping an Internet site I, N times, using packets sized S bytes and
%% extract the ping times into a list of floating point times in ms

pingtimes_ms(I,N,S) -> 
    Pingstr = os:cmd(io_lib:format("ping -c ~B -i 0.2 -s ~B ~s",[N,S,I])),
    [_|Lines]=string:split(Pingstr,"\n",all),
    TT = [pingline_to_ms(L) || L <- Lines],
    Times = lists:sort([T || T <-TT, is_float(T)]),
    io:format("ping ~s with results: ~w\n",[I,Times]),
    Times.


%% change the bandwidth on a given interface to NewBW by using the Cmd
%% as a format string and then running the command, also print the
%% command for debugging

new_bandwidth(Cmd,NewBW) ->
    os:cmd(io_lib:format(Cmd,[NewBW])),
    io:format(Cmd ++"\n",[NewBW]).


%% repeatedly select a random delay, and then sleep that long and try
%% pinging a given site and report to our parent PID if we detect a
%% delay. Throw away the shortest ping, keep the next 5 as our
%% baseline. Always comparing the 3rd ping to the highest ping to
%% detect delay.

monitor_a_site(Rpid,Name,N,Inc,FiveTimes) ->
    T = rand:uniform()*20+10, % sleep 10 to 30 seconds
    erlang:send_after(round(T*1000),self(),go),
    receive
	_ ->
	    try pingtimes_ms(Name,N,50) of
		[] -> 
		    monitor_a_site(Rpid,Name,N,Inc,FiveTimes);
		MS ->
		    Times = lists:sort(lists:append(FiveTimes,MS)),
		    Delay= lists:last(Times) - lists:nth(3,Times),
		    if 
			
			Delay > 20.0 -> 
			    Rpid ! {delay,Name,Delay,erlang:system_time(seconds)};
		       true -> 
			    true
		    end,
		    monitor_a_site(Rpid,Name,N,Inc,lists:sublist(Times,2,5))
						% throw out the lowest, keep the next 5;
	    catch
		error:_ERR -> monitor_a_site(Rpid,Name,N,Inc,FiveTimes);
		exit:_ -> monitor_a_site(Rpid,Name,N,Inc,FiveTimes);
		throw:_ -> monitor_a_site(Rpid,Name,N,Inc,FiveTimes)			      
				  
	    end
    end.


%% gets us started in the loop, by trying to get a minimum of 5 pings
%% as our initial baseline

monitor_a_site(Monitor) ->
    {RPid,Name} = Monitor,
    try pingtimes_ms(Name,5,50) of
	T ->
	    monitor_a_site(RPid,Name,5,20,T)
    catch
	error:_ -> monitor_a_site(Monitor)
    end.



read_bw(Iface) ->
    {ok,IoDevRx} = file:open("/sys/class/net/" ++ Iface ++ "/statistics/rx_bytes",read),
    {ok,IoDevTx} = file:open("/sys/class/net/" ++ Iface ++ "/statistics/tx_bytes",read),
    {ok,[Rxbytes]} = io:fread(IoDevRx,"","~d"),
    {ok,[Txbytes]} = io:fread(IoDevTx,"","~d"),
    {Rxbytes,Txbytes}.

monitor_bw(Iface,AdjPid,LastMSecs,Rxbytes,Txbytes) ->
    receive
	{timer,MSecs} ->
	    {NewRx,NewTx} = read_bw(Iface),
	    RxBW = 8 * (NewRx - Rxbytes) / (MSecs - LastMSecs), % kBps
	    TxBW = 8 * (NewTx - Txbytes) / (MSecs - LastMSecs),
	    AdjPid ! {bandwidths,RxBW,TxBW,MSecs},
	    %io:format("Current bandwidth: ~f, ~f kBps\n",[RxBW,TxBW]),
	    monitor_bw(Iface,AdjPid,MSecs,NewRx,NewTx);
	_ ->
	    io:format("wth?\n"),
	    monitor_bw(Iface,AdjPid,LastMSecs,Rxbytes,Txbytes)
    end.



monitor_bw(Iface,AdjPid) ->
    {Rxbytes,Txbytes} = read_bw(Iface),
    Now = erlang:system_time(millisecond),
    monitor_bw(Iface,AdjPid,Now,Rxbytes,Txbytes).


%% run this in a thread, we wait for our pingers to report delays to
%% us. We keep track of delays and a separate thread gives us timer
%% interrupts where we regularly check if a sufficient number of sites
%% experience a delay in the last 30 seconds, we adjust the bandwidth
%% down, otherwise we adjust the bandwidth up.

monitor_delays(RepPid, Sites) ->
    receive 
	{delay,Site,Delay,Time} ->
	    NewSites=[{Site,Delay,Time}|Sites],
	    monitor_delays(RepPid,NewSites);
	{timer,_Time} -> 
	    io:format("Checking up on things: ~B\n",[erlang:system_time(seconds)]),
	    Now=erlang:system_time(seconds),
	    RecentSites = [{Site, Del, T} || {Site,Del,T} <- Sites, T > Now-30],
	    io:format("Full Delayed Site List: ~w\n",[Sites]),
	    io:format("Recent Delayed Site List: ~w\n",[RecentSites]),

	    %% use random scaling factors, but make sure down followed
	    %% by up averages slightly less than 1, based on
	    %% simulations this averages around .98... this ensures
	    %% we don't grow too fast.

	    if length(RecentSites) > 3 ->
		    Factor = rand:uniform() * 0.15 + 0.85,
		    RepPid ! {factor,Factor};
	       true ->
		    Factor = rand:uniform() * 0.12 + 1.0,
		    RepPid ! {factor,Factor}
	    end,
	    monitor_delays(RepPid,RecentSites)
    end.

%% When the monitor_delays thread detects a need to change the
%% bandwidth it fires off a message to a thread running this code,
%% where it just multiplies the bandwidth by a random factor and then
%% calls the OS to change the bandwidth. Note that we keep track of
%% bandwidth in integer Kbps units so we round the number.

adjuster(Tuples) ->
    [{I1,L1,C1,H1},{I2,L2,C2,H2}] = Tuples, %% teh L1,C1 etc are upload, L2,C2 etc are download
    receive
	{bandwidths,RxBW,TxBW,MSecs} ->
	    io:format("Received bandwidth report: Rx: ~f, Tx: ~f at ~w\n",[RxBW,TxBW,MSecs]),
	    %% here is where we need to make decisions about bandwidth relevant changes
	    %% for now, if current bandwidth is lower than 1/2 the current set point, 
	    %% we're going to decay down until we either hit the low end, or we're less than 2x the current 
	    %% usage. This ensures we're never super high when a bandwidth spike occurs, causing bbloat
	    RND = rand:uniform(),
	    if
		(((C2 > L2) and (RxBW < C2/2)) or ((C1 > L1) and (TxBW < C1/2))) and (RND < 0.05) ->
		    self() ! {factor,0.99} ;
		true -> true
	    end,
	    adjuster(Tuples);

	{factor,F} ->
	    CC1 = round(min(max(L1,C1*F),H1)),
	    CC2 = round(min(max(L2,C2*F),H2)),
	    new_bandwidth(I1,CC1),
	    new_bandwidth(I2,CC2),
	    adjuster([{I1,L1,CC1,H1},{I2,L2,CC2,H2}])
    end.


%% this is a process that just tells the monitor process to check for
%% sufficient delay on a regular basis. P1 is the adjuster process, P2
%% is the bw estimator

timer_process(P1,P2,T) ->
    erlang:send_after(T*1000,P1,{timer,erlang:system_time(seconds)}),
    erlang:send_after(1000,P2,{timer,erlang:system_time(millisecond)}),
    erlang:send_after(1000,self(),go),
    receive
	_ -> timer_process(P1,P2,T)
    end.


%% a supervisory thread that fires off processes to do all the
%% pinging, and checking and timing etc, and then restarts them if
%% they die.


monitor_ifaces(Tuples,Sites,Iface) ->
    AdjPid = proc_lib:spawn_link(sqmfeedback,adjuster,[Tuples]),
    MonPid = proc_lib:spawn_link(sqmfeedback,monitor_delays,[AdjPid,[]]),
    BwMonPid = proc_lib:spawn_link(sqmfeedback,monitor_bw,[Iface,AdjPid]),
    SitePids = lists:map(fun(Tup) -> 
				 {_,Site} = Tup,
				 {proc_lib:spawn_link(sqmfeedback,monitor_a_site,[Tup]),Site} end,
			 [{MonPid,S}||S <- Sites]),
    TimerPid = proc_lib:spawn_link(sqmfeedback,timer_process,[MonPid,BwMonPid,40]),
    process_flag(trap_exit,true), %% we want to hear about exits
    monitor_ifaces(Tuples,Sites,SitePids,AdjPid,MonPid,BwMonPid,TimerPid,Iface).

monitor_ifaces(Tuples,Sites,SitePids,AdjPid,MonPid,BwMonPid,TimerPid,Iface) ->
    receive
	{'EXIT',Pid, _Reason} -> 
	    DeadSite = [ S  || {P,S} <- Sites,P == Pid],
	    if
		DeadSite /= [] ->
		    {Pid,Site} = lists:nth(1,DeadSite),
		    NewSitePid = proc_lib:spawn_link(sqmfeedback,monitor_a_site,{MonPid,Site}),
		    monitor_ifaces(Tuples,Sites,[{NewSitePid,Site}|Sites--[Pid]],
				   AdjPid,MonPid,BwMonPid,TimerPid,Iface);
		Pid == AdjPid ->
		    NewAdj = proc_lib:spawn_link(sqmfeedback,adjuster,[Tuples]),
		    monitor_ifaces(Tuples,Sites,SitePids,NewAdj,MonPid,BwMonPid,TimerPid,Iface);
		Pid == MonPid ->
		    NewMon = proc_lib:spawn_link(sqmfeedback,monitor_delays,[AdjPid,[]]),
		    monitor_ifaces(Tuples,Sites,SitePids,AdjPid,NewMon,BwMonPid,TimerPid,Iface);
		Pid == TimerPid ->
		    io:format("respawning timer:\n"),
		    NewTimer = proc_lib:spawn_link(sqmfeedback,timer_process,[MonPid,BwMonPid,40]),
		    monitor_ifaces(Tuples,Sites,SitePids,AdjPid,MonPid,BwMonPid,NewTimer,Iface);
		Pid == BwMonPid ->
		    io:format("respawning bw usage monitor;\n"),
		    NewBwMon = proc_lib:spawn_link(sqmfeedback,monitor_bw,[Iface,MonPid]),
		    monitor_ifaces(Tuples,Sites,SitePids,AdjPid,MonPid,NewBwMon,TimerPid,Iface);
		true -> true
	    end;
	_ -> monitor_ifaces(Tuples,Sites,SitePids,AdjPid,MonPid,BwMonPid,TimerPid,Iface)
    end.


%% entry point to the code, adjust the interface names and bandwidths
%% here. the 3 bandwidths are min, start, and max values for the
%% interface in *integer* Kbps units. You can also select different
%% sites to monitor with pings


main() ->
    
    monitor_ifaces([{"tc qdisc change root dev eth0.2 cake bandwidth ~BKbit diffserv4 dual-srchost overhead 34 ", 4000, 6000, 8000},
		    {"tc qdisc change root dev ifb4eth0.2 cake bandwidth ~BKbit diffserv4 dual-dsthost nat overhead 34 ingress",15000,30000,35000}],
    ["dns.google.com","one.one.one.one","quad9.net","facebook.com",
     "gstatic.com","cloudflare.com","fbcdn.com","akamai.com","amazon.com"],
		  "lan"),
    receive
	_ -> true
    end.



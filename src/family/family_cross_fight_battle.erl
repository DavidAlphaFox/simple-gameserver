-module(family_cross_fight_battle).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_carlos.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,start/1]).

-record(state,{warID=0,startTime=0}).
-record(plan,{sec=0,key=0,value=0}).

-define(city,city).
-define(site,site).
-define(player,player).
-define(car,car).
-define(family,family).
-define(endInfo,endInfo).
-define(plan,plan).
-define(mark_bc,mark_bc).
-define(bc_mark,bc_mark).
-define(bcList,bcList).
-define(type_kill, type_kill).
-define(born_city, born_city).
-define(rank_data, rank_data).
-define(DumpInterval,10000).
-define(anubis_family_player,anubis_family_player).


start_link(Args)-> gen_server:start_link(?MODULE,[Args],[]).
start_link(WarID,Team1,Team2,Team3,Team1_1,Team1_2,Team1_3)->
	gen_server:start_link(?MODULE,[WarID,Team1,Team2,Team3,Team1_1,Team1_2,Team1_3],[]).
start(Args) -> supervisor:start_child(family_cross_fight_battle_sup, Args).

init([WarID,Team1,Team2,Team3,Team1_1,Team1_2,Team1_3]) ->
	add_ets_war_info(WarID,self()),
	init_player(Team1,Team2,Team3),
	init_family([Team1_1,Team1_2,Team1_3]),
	init_map(),
	tick(),
	tick_plan(),
	bc_open(),
	set_end_info(WarID),
	check_winner_plan(),
	{ok,#state{warID=WarID,startTime=util:now()}}.

handle_call(_,_,State)-> {reply,ok,State}.
handle_cast(_,State)->{noreply,State}.
handle_info(stop,State)-> {stop,normal, State};
handle_info(hibernate,State)-> tick(), {noreply,State,hibernate};
handle_info(Info,State)->
	?CATCH(do_handle_info(Info)),
	{noreply,State}.

terminate(_Reason, _State)->
	ok.

code_change(_,State,_)->{ok,State}.

init_map()->
	[begin
		 put({?city,ID},#city{id=ID,pos=Pos,sites=Sites}),
		 [begin
			  put({?site,ID,SID},#site{cityID=ID,id=SID,pos={ID,X,Y},display=Display}),
			  ok
		  end ||{SID,{X,Y},Display}<-Sites],
		 [put({?site, ID,SSID}, #site{cityID=ID,id=SSID,pos={ID,X,Y}})
			||{_,SSID,{X,Y}}<-data_family_cross:get({born_site, ID})]
	 end ||{ID,Pos,Sites}<-data_family_cross:get(cities_list)],
	[begin 
		 put({?born_city, ID}, #p_born_pos{city=ID,x=X,y=Y,type=Type})
	 end||{ID,{X,Y},Type}<-data_family_cross:get(born_cities)],
	ok.

init_player(Team1,Team2,Team3) ->
	init_player(Team1),
	init_player(Team2),
	init_player(Team3).

init_player([]) -> ok;
init_player({Type,PlayerList, CarList,CSeasonFamilyPlayer})->
	set_anubis_family_player(Type,CSeasonFamilyPlayer),
	[begin
		 set_fly(Player#familycross_player{type=Type})
	 end||#familycross_player{roleID=RoleID}=Player<-PlayerList,RoleID /= 0],
	BornCityList = data_family_cross:get(born_cities),
	add_bcList(PlayerList),
	[begin
		 {_,CPos,_} = lists:keyfind(Type,3,BornCityList),
		 Players2 = [{RoleID,ServerID}||{RoleID,ServerID}<-Players, RoleID /= 0],
		 put({?car,Type,ID},Car#car{type=Type,startPos=CPos,speed=data_family_cross:get(car_speed),players=Players2})
	 end||#car{id=ID,players=Players}=Car<-CarList].

init_family(Families) ->
	[put({?family,Type},F)||#fc_fighter{familyID=FamilyID,type=Type}=F<-Families, FamilyID /= 0].

tick()-> erlang:send_after(?DumpInterval, self(), dump_interval).

set_end_info(WarID)->
	Interval=data_family_cross:get(interval_time),
	StartTime = util:now(),
	put(?endInfo, {WarID,StartTime+Interval,StartTime,Interval}),
	erlang:send_after(Interval*1000, self(), do_end_war),
	ok.

tick_plan()-> erlang:send_after(1000, self(), tick_plan).

bc_open()-> erlang:send(self(), {bc_info, open}).

	
do_handle_info({bc_info, Type}) ->	bc_info(Type);
do_handle_info(tick_plan)-> tick_plan(), plan();
do_handle_info(do_end_war)-> end_war();
do_handle_info({{cs_familycross_war_info},{RoleID,ServerID}})->
	Msg = get_war_base_info(),
	family_cross_fight_router:send_client(ServerID, RoleID,Msg);
do_handle_info({{cs_familycross_self_car},{RoleID,ServerID}} ) ->
	Msg = case get_fly(RoleID,ServerID) of
		#familycross_player{car=CarID,type=Type} ->
			Car = get_car(CarID,Type),
			#sc_familycross_self_car{result=1,car=[carDtl(Car)],flys=[flyDtl(get_fly(LRoleID,LServerID))||{LRoleID,LServerID}<-Car#car.players]};
		_ -> #sc_familycross_self_car{result=2,car=[],flys=[]}
	end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);	
do_handle_info({{cs_familycross_drive_car,ID,TarCity},{RoleID,ServerID}}) ->
		#familycross_player{type=Type} = get_fly(RoleID,ServerID),
	Msg =
		case check_can_drive(RoleID,ServerID,Type,ID,TarCity) of
			{true,Car,IsUpdate} ->
				NewCar = update_car_mov(Car,TarCity,IsUpdate),
				set_car(NewCar),
				add_bc_info(car, ID,Type),
				#sc_familycross_drive_car{result=1};
		{false,Reason} -> #sc_familycross_drive_car{result=Reason}
		end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);
do_handle_info({{cs_familycross_drive_car_stop,ID},{RoleID,ServerID}}) ->
		#familycross_player{type=Type} = get_fly(RoleID,ServerID),
	Msg = case check_can_drive(RoleID,ServerID,Type,ID,0) of
			  {true, Car, IsUpdate} ->
				  NewCar = stop_car(Car,IsUpdate),
				  set_car(NewCar),
				  add_bc_info(car,ID,Type),
				  #sc_familycross_drive_car_stop{result=1};
			  {false,Reason} -> #sc_familycross_drive_car_stop{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);
do_handle_info({{cs_familycross_mov,City,TarSite}, {RoleID,ServerID}})->
	Msg = case check_can_mov(RoleID,ServerID,City,TarSite) of
			  {true,Fly,IsUpdate}  ->
				  NewFly = update_fly_mov(Fly,City,TarSite, IsUpdate),
				  set_fly(NewFly),
				  add_bc_info(fly, RoleID,ServerID, City),
				  #sc_familycross_mov{result=1};
			  {false,Reason} -> #sc_familycross_mov{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);
do_handle_info({{cs_familycross_mov_stop},{RoleID,ServerID}}) ->
	Msg = case check_can_stop(RoleID,ServerID) of
			  {true,Fly, IsUpdate} ->
				  NewFly = stop_fly(Fly,IsUpdate),
				  set_fly(NewFly),
				  add_bc_info(fly,RoleID,ServerID,NewFly#familycross_player.city),
				  #sc_familycross_mov_stop{result=1};
			  {false,Reason} -> #sc_familycross_mov_stop{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID, Msg);
do_handle_info({{cs_familycross_mov_back,City,CarID}, {RoleID,ServerID}}) ->
	Msg = case check_can_back(RoleID,ServerID,CarID,City) of
			  {true,Fly,IsUpdate,Type} ->
				  NewFly = update_fly_mov_back(Fly,City,CarID,Type,IsUpdate),
				  set_fly(NewFly),
				  add_bc_info(fly, RoleID,ServerID,City),
				  #sc_familycross_mov_back{result=1};
			  {false,Reason} -> #sc_familycross_mov_back{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID, RoleID,Msg);

do_handle_info({{cs_familycross_be_driver,City,Car},{RoleID,ServerID}})->
	Msg = case can_be_driver(RoleID,ServerID,City,Car) of
			  {true,Fly,CarData} -> be_driver(Fly,CarData),#sc_familycross_be_driver{result=1};
			  {false,Reason} ->#sc_familycross_be_driver{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_be_driver2,City,Car},{RoleID,ServerID}})->
	Msg = case can_be_driver2(RoleID,ServerID,City,Car) of
			  {true,Fly,CarData} -> be_driver2(Fly,CarData),#sc_familycross_be_driver2{result=1};
			  {false,Reason} ->#sc_familycross_be_driver2{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_reborn},{RoleID,ServerID}}) ->
	Msg = case do_reborn(RoleID,ServerID) of
			  true -> #sc_familycross_reborn{result=1};
			  {false,Reason} -> #sc_familycross_reborn{result=Reason}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);
	
do_handle_info({{cs_familycross_attack,City,Site,TarRoleID,TarServerID},{RoleID,ServerID}})->
	Msg = case check_can_attack(City,Site,TarRoleID,TarServerID,RoleID,ServerID) of
			  {true,SiteData,TarRole,Self} -> do_attack(City,SiteData,TarRole,Self);
			  {false,Reason} -> #sc_familycross_attack{result=Reason,fightInfo=[]}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_self_info},{RoleID,ServerID}}) ->
	case get_fly(RoleID,ServerID) of
		#familycross_player{}=Fly ->
			Msg = #sc_familycross_self_info{self=flyDtl(Fly)};
		_ ->
			Msg = #sc_familycross_self_info{self=flyDtl(#familycross_player{})}
	end,
	
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_city_dtl,City},{RoleID,ServerID}})->
	%Msg = cityDtl(get_city(City)), -record(city,{id=0,pos={0,0},cars=[],flys=[],sites=[]}). 
	#familycross_player{city=LocalCityID} = get_fly(RoleID,ServerID),
	CityData = get_city(City),
	Msg = 
		case LocalCityID of
			City ->
				#sc_familycross_city_dtl{	   id=CityData#city.id
											   ,pos=cityPos(CityData#city.pos)
											   ,cars=warCar2(CityData#city.cars)
											   ,flys=[flyDtl(get_fly(FRoleID,FServerID))||{FRoleID,FServerID}<-CityData#city.flys]
											   ,sites=[siteDtl(get_site(City,SID))||{SID,_,_}<-CityData#city.sites]
											   ,bornPos=[#p_born_pos{city=SiteID,type=Type,x=BPX,y=BPY}||{Type,SiteID,{BPX,BPY}}<-data_family_cross:get({born_site,City})]
										};
			_ ->
				#sc_familycross_city_dtl{id=0,pos=cityPos(CityData#city.pos)
										 ,cars=[],flys=[],sites=[],bornPos=[]}
		end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_site_dtl,City,Site},{RoleID,ServerID}})->
	SiteData=get_site(City,Site),
	{A,B,C} = SiteData#site.flys,
	Msg = #sc_familycross_site_dtl{id=SiteData#site.id
								  ,pos=sitePos(SiteData#site.pos)
								  ,fly=[flyDtl(get_fly(FRoleID,FServerID))||{FRoleID,FServerID}<-A++B++C]
								  ,ownerType=SiteData#site.owner
								  ,ownSec=SiteData#site.ownSec},
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_fly_dtl,TarRoleID,TarServerID},{RoleID,ServerID}}) ->
	Fly = get_fly(TarRoleID,TarServerID),
	FlyDtl = flyDtl(Fly),
	Replay = Fly#familycross_player.replayList,
	Grade = Fly#familycross_player.grade,
	Msg = #sc_familycross_fly_dtl{fly=FlyDtl,replay=Replay,grade=Grade},
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({car_arrive,ID,Type,TarCity})->
	car_arrive(ID,Type,TarCity);

do_handle_info({fly_arrive,RoleID,ServerID,TarSite,City,CarID})->
	fly_arrive(RoleID,ServerID,TarSite,City,CarID);

do_handle_info(check_winner) ->
	Sites = lists:foldl(fun({CID,_,SiteIDs} ,Acc1)->
								lists:foldl(fun({SID,_,_},Acc2) ->
													NewSite = update_site_gas(get_site(CID,SID)),
													set_site(NewSite),
													[NewSite|Acc2]
											end , Acc1, SiteIDs)
						end,[], data_family_cross:get(cities_list)),
	case get_war_winner(Sites,normal) of
		ignore -> check_winner_plan();
		{Winner,Team1Gas,Team2Gas,Team3Gas} -> end_war(Winner, Team1Gas,Team2Gas,Team3Gas)
	end;

do_handle_info({{cs_familycross_own_site,City,Site},{RoleID,ServerID}}) ->
	Msg = case check_can_own(City,Site, RoleID,ServerID) of
		{true,OwnType,Type,SiteData} -> do_own_site(City,Site,RoleID,ServerID,OwnType,Type,SiteData);
			  {false,Reason} -> #sc_familycross_own_site{result=Reason,ownSec=0}
		  end,
	family_cross_fight_router:send_client(ServerID,RoleID,Msg);

do_handle_info({{cs_familycross_battle_get_rank}, {RoleID,ServerID}}) ->
	RankList = get_rank_data(),
	family_cross_fight_router:send_client(ServerID,RoleID, #sc_familycross_battle_get_rank{rank=RankList});

do_handle_info({site_gas, City,MineID,RoleID,ServerID}) ->
	#site{owner=Owner,lastOwner=LastOwner} = SiteData = get_site(City,MineID),
	case Owner of LastOwner -> ?ERR("something wrong, gas plan works, bug mine start get gas");
		_ -> 	SiteData2 = start_get_gas(Owner,SiteData),
				role_own_site(RoleID,ServerID),
				add_bc_info(city,City,MineID),
				set_site(SiteData2#site{lastOwner=Owner}),
				update_gas_speed(City, Owner),
				update_gas_speed(City, LastOwner),
				case LastOwner of 0 -> add_sys_msg(zhanling,RoleID,ServerID);
					_ -> add_sys_msg(tuxi, RoleID, ServerID)
				end
	end;
		
do_handle_info(Msg)->?ERR("can't handle msg:~w",[Msg]).


add_sys_msg(_,_,_) -> ok.

do_own_site(City,Site,RoleID,ServerID,OwnType,Type,SiteData) ->
	case OwnType of
		fangshou -> 
			OwnSec = util:now(),
			role_own_site(RoleID,ServerID),
			update_gas_speed(City, SiteData#site.lastOwner),
			add_sys_msg(fangshou, RoleID,ServerID);
		zhanling ->
			OwnSec = util:now()  + data_family_cross:get(own_site_interval),
			cancel_site_gas_plan(City,Site),
			add_site_gas_plan(City,Site,OwnSec,RoleID,ServerID)
	end,
	add_bc_info(city,City,Site),
	set_site(SiteData#site{owner=Type,ownSec=OwnSec}),
	update_gas_speed(City,Type),
	#sc_familycross_own_site{result=1,ownSec=OwnSec}.

role_own_site(RoleID,ServerID) ->
	#familycross_player{ownSiteTimes=OST} = Fly = get_fly(RoleID,ServerID) ,
	set_fly(Fly#familycross_player{ownSiteTimes=OST+1}).

update_gas_speed(City,Type) ->
	{_,_,Sites} = lists:keyfind(City,1,data_family_cross:get(cities_list)),
	TypeSites = lists:foldl(fun({SID,_,_},Acc)->
									#site{owner=LocalType,lastOwner=LastOwner}=Site=get_site(City,SID),
									case LocalType ==Type andalso LastOwner==Type of
										true -> [Site|Acc];
										_ -> Acc
									end
							end,[],Sites),
	case length(TypeSites) == length(Sites) of
		true -> set_gas_speed(TypeSites,Type,data_family_cross:get(high_speed));
		_ -> set_gas_speed(TypeSites,Type,data_family_cross:get(low_speed))
	end.

start_get_gas(1,#site{team1Gas=Team1Gas}=SiteData) ->SiteData#site{team1Gas=Team1Gas#teamGas{startGetSec=util:now()}};
start_get_gas(2,#site{team2Gas=Team2Gas}=SiteData) ->SiteData#site{team2Gas=Team2Gas#teamGas{startGetSec=util:now()}};
start_get_gas(3,#site{team3Gas=Team3Gas}=SiteData) -> SiteData#site{team3Gas=Team3Gas#teamGas{startGetSec=util:now()}};
start_get_gas(_,Site) -> Site.


set_gas_speed(Sites,1,Speed)->
	[begin 
		 Site2 = update_site_gas(Site),
		 set_site(Site2#site{team1Gas=Team1Gas#teamGas{getspeed=Speed}})
	 end||#site{team1Gas=Team1Gas}=Site<-Sites];
set_gas_speed(Sites,2,Speed) ->
	[begin
		 Site2 = update_site_gas(Site),
		 set_site(Site2#site{team2Gas=Team2Gas#teamGas{getspeed=Speed}})
	 end||#site{team2Gas=Team2Gas}=Site<-Sites];
set_gas_speed(Sites,3,Speed) ->
	[begin
		 Site2 = update_site_gas(Site), 
		 set_site(Site2#site{team3Gas=Team3Gas#teamGas{getspeed=Speed}})
	 end||#site{team3Gas=Team3Gas}=Site<-Sites];
set_gas_speed(Sites,_,_) -> Sites.

check_can_own(City,Site,RoleID,ServerID) ->
	#familycross_player{type=Type}=get_fly(RoleID,ServerID),
	Now = util:now(),
	#site{owner=Owner,lastOwner=LastOwner,ownSec=OwnSec,flys=Flys}=SiteData=get_site(City,Site),
	case lists:member({RoleID,ServerID}, get_type_flys(Type,Flys)) of
		false ->{false,2};%% ���ڸ�site
		_ ->
			case alive_other_flys(Type, Flys) of
				false -> {false,5};%%�����������
				_ ->
					case Owner of
						Type ->
							if LastOwner == Type ->
									{false,3};
							   true ->
									if OwnSec > Now -> {false,6};%% ��������ʱ��
									   true -> {false,4} %% �Ѿ�������ռ��
									end
							end;
						_ ->
							case LastOwner of
							    Type ->
									{true,fangshou,Type,SiteData};
								_ ->
									{true,zhanling,Type,SiteData}
							end
					end
			end
	end.
		

update_site_gas(#site{owner=0}=Site) -> Site;
update_site_gas(#site{owner=1,team1Gas=Team1Gas}=Site) -> 
	#teamGas{startGetSec=SGS,gas=NGAS,getspeed=Speed}=Team1Gas,
	if SGS == 0 -> Site;
	   true ->Now=util:now(),
			  Site#site{team1Gas=Team1Gas#teamGas{startGetSec=Now,gas=NGAS+Speed*(Now-SGS)}}
	end;
update_site_gas(#site{owner=2,team2Gas=Team2Gas}=Site) -> 
	#teamGas{startGetSec=SGS,gas=NGAS,getspeed=Speed}=Team2Gas,
	if SGS == 0 -> Site;
	   true -> 	Now=util:now(),
				Site#site{team2Gas=Team2Gas#teamGas{startGetSec=Now,gas=NGAS+Speed*(Now-SGS)}}
	end;
update_site_gas(#site{owner=3,team3Gas=Team3Gas}=Site) -> 
	#teamGas{startGetSec=SGS,gas=NGAS,getspeed=Speed}=Team3Gas,
	if SGS == 0 -> Site;
	   true -> 	Now=util:now(),
				Site#site{team3Gas=Team3Gas#teamGas{startGetSec=Now,gas=NGAS+Speed*(Now-SGS)}}
	end.

get_war_winner(Sites,State) ->
	Now = util:now(),
	{Gas1,Gas2,Gas3} = 
		lists:foldl(fun(#site{owner=1,ownSec=OwnSec}=Site,{{1,Count1,Gas1},Gas2,Gas3}) when OwnSec =< Now andalso OwnSec /= 0->
							calc_gas_add(Site, {{1,Count1+1,Gas1},Gas2,Gas3});
					   (#site{owner=2,ownSec=OwnSec}=Site,{Gas1,{2,Count2,Gas2},Gas3}) when OwnSec =< Now andalso OwnSec /= 0->
							calc_gas_add(Site, {Gas1,{2,Count2+1,Gas2},Gas3});
					   (#site{owner=3,ownSec=OwnSec}=Site,{Gas1,Gas2,{3,Count3,Gas3}}) when OwnSec =< Now andalso OwnSec /= 0->
							calc_gas_add(Site,{Gas1,Gas2,{3,Count3+1,Gas3}});
					   (#site{}=Site, Acc) -> calc_gas_add(Site,Acc);
					   (_,Acc) -> Acc
					end,{{1,0,0},{2,0,0},{3,0,0}}, Sites),
	WinNeed = data_family_cross:get(win_need_gas),
	Data = update_des_info([Gas1,Gas2,Gas3]),
	case Data of [] -> ignore;
		_ ->  do_bc(#sc_familycross_des_update{des=Data},familycross_bc_info)
	end,
	[{TypeF,_,FirstGas},{TypeS,_,SecondGas},{TypeT,_,ThirdGas}] = lists:reverse(lists:keysort(3, [Gas1,Gas2,Gas3])),
	if FirstGas < WinNeed andalso State == normal ->
		   ignore;
	   true ->
		   if FirstGas == SecondGas -> 
				  if FirstGas == ThirdGas -> {equal, {TypeF,FirstGas,first_base},{TypeS,SecondGas,first_base},{TypeT,ThirdGas,first_base}}; %%123一样多
					 true -> {win1, {TypeF,FirstGas,first_base}, {TypeS,SecondGas,first_base}, {TypeT,ThirdGas,second_base}}%%12一样多,3少
				  end;
			  true -> 
				  if SecondGas == ThirdGas -> {win2, {TypeF,FirstGas,first_base}, {TypeS,SecondGas,second_base},{TypeT,ThirdGas,second_base}};%%1多,23一样少
					 true -> {win3, {TypeF,FirstGas,first_base},{TypeS,SecondGas,second_base},{TypeT,ThirdGas,third_base}}%% 1多,其次2,最少3
				  end
		   end
	end.

calc_gas_add(#site{team1Gas=#teamGas{gas=Gas1},team2Gas=#teamGas{gas=Gas2},team3Gas=#teamGas{gas=Gas3}}, {{1,C1,G1},{2,C2,G2},{3,C3,G3}}) ->
	{{1,C1,Gas1+G1},{2,C2,Gas2+G2},{3,C3,Gas3+G3}}.

update_des_info(GasList) ->
	[begin
		 #fc_fighter{familyID=FamilyID,serverID=ServerID}=erlang:get({?family, Type}),
		 #p_familycross_head_des{familyID=FamilyID,serverID=ServerID, count=Count,total=Total}
	 end||{Type,Count,Total}<-GasList, not(Count == 0 andalso Total == 0)].

car_arrive(ID,Type,TarCity) ->
	#city{cars=CityCars,flys=CityFlys,pos=Pos,typeNum=TypeNum}=City=get_city(TarCity),
	#car{players=CarFlys,id=CarID}=Car = get_car(ID,Type),
	case isMaxTypeNum(Type,TypeNum) of
		true -> 
			Car2 = Car#car{tarCity=0, startPos=Pos, startTime=0},
			set_car(Car2);
		false ->
			add_bc_info(car,ID,Type),
			update_cars_city(CarFlys, TarCity,TarCity),
			CityFlys2 = add_city_obj(CarFlys,CityFlys),
			CityCars2 = add_city_obj([{CarID,Type}], CityCars),
			City2 = City#city{cars=CityCars2,flys=CityFlys2,typeNum=inc(Type,TypeNum)},
			Car2 = arrive_car(Car,TarCity,Pos),
			set_city(City2),
			set_car(Car2)
	end.

isMaxTypeNum(Type, TypeNum) -> erlang:element(Type, TypeNum) >= data_family_cross:get(max_cars_one_city).
inc(Type, TypeNum) -> erlang:setelement(Type, TypeNum,erlang:min(erlang:element(Type, TypeNum)+1
																,data_family_cross:get(max_cars_one_city))).
dec(Type,TypeNum) -> erlang:setelement(Type,TypeNum, erlang:max(erlang:element(Type,TypeNum)-1,0)).

update_cars_city(CarFlys, TarCity,LastCity) ->
	[begin
		 Fly = get_fly(RoleID,ServerID),
		 add_bc_info(fly,RoleID,ServerID,LastCity),
		 set_fly(Fly#familycross_player{city=TarCity})
	 end||{RoleID,ServerID} <- CarFlys].

arrive_car(Car,TarCity,Pos)-> Car#car{tarCity=TarCity,startPos=Pos,startTime=0}.

fly_arrive(RoleID,ServerID,TarSite,City,CarID) ->
	#familycross_player{type=Type}=Fly=get_fly(RoleID,ServerID),
	BornSites = data_family_cross:get({born_site, City}),
	%add_bc_info(fly,RoleID,ServerID,City),
	case lists:keyfind(TarSite, 2, BornSites) of
		{_,_,{PX,PY}} ->
			add_bc_info(site,City,TarSite,RoleID,ServerID),
			case join_car(City,CarID,Fly) of
				false -> 
					%#site{pos=Pos} = get_site(City,TarSite),
					set_fly(Fly#familycross_player{startPos={City,PX,PY}, tarSite=0,startTime=0});
				Fly2 ->
					add_bc_info(car,CarID,Type),
					add_bc_info(fly, RoleID,ServerID,City),
					set_fly(Fly2#familycross_player{car=CarID})
			end;
		_ ->
			#site{pos=Pos,flys=Flys} = Site = get_site(City,TarSite),
			TypeFly = get_type_flys(Type,Flys),
			case length(TypeFly) >= data_family_cross:get(site_max_fly) of
				false ->
					%% site还有位置
					TypeFly2 = case lists:member({RoleID,ServerID}, TypeFly) of
								   true -> TypeFly;
								   _ -> [{RoleID,ServerID}|TypeFly]
							   end,
					SiteFlys2 = set_type_flys(Type,TypeFly2,Flys),
					Site2 = Site#site{flys=SiteFlys2},
					Fly2 = arrive_fly(Fly,TarSite,Pos),
					add_bc_info(site,City,TarSite,RoleID,ServerID),
					set_site(Site2),
					set_fly(Fly2);
				true ->
					%% site 满了，飞不进去了
					Fly2 = Fly#familycross_player{startPos=Pos,tarSite=0,startTime=0},
					add_bc_info(fly,RoleID,ServerID,Fly2#familycross_player.city),
					set_fly(Fly2)
			end
	end.
	
arrive_fly(Fly,TarSite,Pos)-> Fly#familycross_player{startPos=Pos,tarSite=TarSite,startTime=0}.
	
add_city_obj(CarFlys,CityFly) ->
	lists:foldl(fun(K,Acc)-> case lists:member(K,Acc) of true ->Acc; _ -> [K|Acc] end end,CityFly,CarFlys).

delete_city_obj(CarFlys,CityFlys)->
	lists:foldl(fun(K,Acc)-> lists:delete(K,Acc) end,CityFlys,CarFlys).

do_attack(CityID,#site{id=Site}=SiteData,TarRole,Self) ->
	#familycross_player{roleID=TarRoleID,serverID=TarServerID,fighters=TarFighters,type=TarType
						,addBuff=TarAddBuff,talent=TarTalent,itemList=TarItemList,replayList=TarReplay
						,roleName=TarName,trSpecial=TarSpecial,skin_info=TarSkin}=TarRole,
	#familycross_player{roleID=RoleID,serverID=ServerID,fighters=Fighters,type=SelfType
						,addBuff=AddBuff,talent=Talent,itemList=ItemList,replayList=Replay
						,roleName=RoleName,trSpecial=Special,skin_info=Skin}=Self,
	GerEquipList1 = role_item:assort_ger_equiplist(ItemList),
    LegendAddList1 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList1],
    GerEquipList2 = role_item:assort_ger_equiplist(TarItemList),
    LegendAddList2 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList2],
	case ?CATCH(role_fight:new(filter_out_zero_hp(Fighters),filter_out_zero_hp(TarFighters)
							   ,AddBuff,TarAddBuff,Talent,TarTalent,Special,TarSpecial,Skin,TarSkin,LegendAddList1,LegendAddList2)) of
		{IsWin,FightRecord,{_,_,NewFightersSelf0,NewFightersTar0}} ->
			FighterList2 = role_data:get_FighterList_with_effect(ItemList,TarItemList,FightRecord#sc_fight_request.fighterList),
			FightRecord1 = FightRecord#sc_fight_request{fighterList=FighterList2},
			ReplayUID = tk_id:gen_replayUID(),
			family_cross_replay_server:record_replay(ReplayUID,FightRecord1),
			ReplayDtl = #p_familycross_replay_dtl{replayUID=ReplayUID,isRole1Win =bool2int(IsWin),role1Name=RoleName,role2Name=TarName},
			NewRL1 = add_replay(Replay,ReplayDtl),
			NewRL2 = add_replay(TarReplay,ReplayDtl),
			NewFightersSelf = recalc_fighter_list(Fighters, NewFightersSelf0),
			NewFightersTar = recalc_fighter_list(TarFighters,NewFightersTar0),
			Blood1 = calc_hp_percent(NewFightersSelf),
			Blood2 = calc_hp_percent(NewFightersTar),
			if Blood1 == 0 andalso Blood2 == 0 ->
					NewSelf = update_player_fighters(Self,Blood1,NewRL1,NewFightersSelf),
					NewTar = update_player_fighters(TarRole,Blood2,NewRL2,NewFightersTar),
					NewSelf2 = update_player_killNum(NewSelf),
					NewTar2 = update_player_killNum(NewTar),
					NewSelf3 = reborn_player(NewSelf2),
					NewTar3 = reborn_player(NewTar2),
					set_fly(NewSelf3),
					set_fly(NewTar3),
					SiteData2 = delete_site_player(SelfType,{RoleID,ServerID},SiteData),
					SiteData3 = delete_site_player(TarType,{TarRoleID,TarServerID},SiteData2),
					
					set_site(SiteData3),
					sync_role([{RoleID,ServerID},{TarRoleID,TarServerID}],CityID),
					add_bc_info(site,CityID,Site,TarRoleID,TarServerID),
					add_bc_info(site,CityID,Site,RoleID,ServerID),
					#sc_familycross_attack{result=1,fightInfo=[FightRecord1]};
			   true ->
					if IsWin ->
							NewSelf = update_player_fighters(Self,Blood1,NewRL1,NewFightersSelf),
							NewTar = update_player_fighters(TarRole,Blood2,NewRL2,NewFightersTar),
							NewSelf2 = update_player_killNum(NewSelf),
							NewTar2 = reborn_player(NewTar),
							set_fly(NewSelf2),
							set_fly(NewTar2),
							SiteData2 = delete_site_player(TarType,{TarRoleID,TarServerID},SiteData),
							set_site(SiteData2),
							sync_role([{RoleID,ServerID},{TarRoleID,TarServerID}],CityID),
							add_bc_info(site,CityID,Site,TarRoleID,TarServerID),
							#sc_familycross_attack{result=1,fightInfo=[FightRecord1]};
					   true ->
							NewSelf = update_player_fighters(Self,Blood1,NewRL1,NewFightersSelf),
							NewTar = update_player_fighters(TarRole,Blood2,NewRL2,NewFightersTar),
							NewSelf2 = reborn_player(NewSelf),
							NewTar2 = update_player_killNum(NewTar),
							set_fly(NewSelf2),
							set_fly(NewTar2),
							SiteData2 = delete_site_player(SelfType,{RoleID,ServerID},SiteData),
							set_site(SiteData2),
							sync_role([{RoleID,ServerID},{TarRoleID,TarServerID}],CityID),
							add_bc_info(site,CityID,Site,RoleID,ServerID),
							#sc_familycross_attack{result=1,fightInfo=[FightRecord1]}
					end
			end;
		_ -> #sc_familycross_attack{result=5,fightInfo=[]}
	end.
				
			   

check_can_attack(City,Site,TarRoleID,TarServerID,RoleID,ServerID)->
	SiteData = get_site(City,Site),
	#familycross_player{type=TarType}=TarRole = get_fly(TarRoleID,TarServerID),
	#familycross_player{type=SelfType}=Self = get_fly(RoleID,ServerID),
	case check_same_site(SiteData,{RoleID,ServerID},SelfType,{TarRoleID,TarServerID},TarType) of
		true ->
			case check_same_type(SelfType,TarType) of
				true -> {false,5};
				false -> {true, SiteData,TarRole,Self}
			end;
		false -> {false,2}
	end.

check_same_site(#site{flys=Flys}, A,TypeA,B,TypeB) ->
	FlysA = get_type_flys(TypeA,Flys),
	FlysB = get_type_flys(TypeB,Flys),
	lists:member(A,FlysA) andalso lists:member(B,FlysB);
check_same_site(_, _,_,_,_) -> false.

delete_site_player(Type,P,#site{flys=Flys}=SiteData) ->
	Fly2 = lists:delete(P,get_type_flys(Type,Flys)),
	SiteData#site{flys=set_type_flys(Type,Fly2,Flys)}.
	

check_same_type(A,B)->	A == B.

be_driver(#familycross_player{roleID=RoleID},#car{id=ID,type=Type}=Car) ->
	NewCar=Car#car{driver=RoleID},
	set_car(NewCar),
	add_bc_info(car,ID,Type).	

can_be_driver(RoleID,ServerID,City,CarID) ->
	#familycross_player{type=Type} =Fly= get_fly(RoleID,ServerID),
	case get_car(CarID,Type) of
		#car{players=Flys,driver=Driver,driver2=Driver2,tarCity=CarCity}=Car ->
			if Driver == 0 ->
				   if Driver2 /= RoleID ->
						  case lists:member({RoleID,ServerID}, Flys) of
							  true ->
								  if CarCity == City-> {true,Fly,Car};
									 true -> {false,5}
								  end;
							  false -> {false,4}
						  end;
					  true -> {false,6}
				   end;
			   true -> {false,2}
			end;
		_-> {false,3}
	end.

be_driver2(#familycross_player{roleID=RoleID},#car{id=ID,type=Type}=Car) ->
	NewCar=Car#car{driver2=RoleID},
	set_car(NewCar),
	add_bc_info(car,ID,Type).	

can_be_driver2(RoleID,ServerID,City,CarID) ->
	#familycross_player{type=Type} =Fly= get_fly(RoleID,ServerID),
	case get_car(CarID,Type) of
		#car{players=Flys,driver2=Driver2,driver=Driver,tarCity=CarCity}=Car ->
			if Driver2 == 0 ->
				   if Driver /= RoleID ->
						  case lists:member({RoleID,ServerID}, Flys) of
							  true ->
								  if CarCity == City-> {true,Fly,Car};
									 true -> {false,5}
								  end;
							  false -> {false,4}
						  end;
					  true -> {false,5}
				   end;
			   true -> {false,2}
			end;
		_-> {false,3}
	end.

do_reborn(RoleID,ServerID) ->
	#familycross_player{city=City,rebornSec=RebornSec}=Fly=get_fly(RoleID,ServerID),
	Now = util:now(),
	if RebornSec > Now -> 
			{_,_BornSite,{BX,BY}} = get_born_site(Fly),
			NewFly = Fly#familycross_player{startPos={City,BX,BY},rebornSec=0},
			set_fly(NewFly),
			add_bc_info(fly,RoleID,ServerID,City),
			send_msg:direct(ServerID,family_cross_fight_server, {cs_familycross_reborn, RoleID}),
			true;
	   true -> {false,2}
	end.

reborn_player(#familycross_player{baseFighters=BF,city=City}=Fly)->
	{_,_BornSite,{BX,BY}} = get_born_site(Fly),
	RebornSec = util:now() + data_family_cross:get(reborn_interval),
	Fly#familycross_player{startPos={City,BX,BY},tarSite=0,fighters=BF,rebornSec=RebornSec}.

update_player_fighters(Player,Blood,NewRL,NewFighters) ->
	Player#familycross_player{blood=Blood,fighters=NewFighters,replayList=NewRL}.

check_can_back(RoleID,ServerID,CarID,City)->
	case get_fly(RoleID,ServerID) of
		#familycross_player{city=NowCity,car=NowCar,type=Type,tarSite=NowTarSite,rebornSec=RSec}=Fly ->
			case util:now() < RSec of
				true -> {false,3};
				_ -> if NowCity == City ->
							 if CarID == NowCar -> {false,4};
								true -> if NowCity == City ->
												if NowTarSite == 0 -> {true,Fly,noupdate,Type};
												   true -> {true,Fly,update,Type}
												end;
										   true -> {false,2}
										end
							 end
					 end
			end;
		_ -> {false,5}
	end.

update_fly_mov_back(Fly,City,CarID,_Type,IsUpdate) ->
	#familycross_player{car=NowCar,tarSite=LastTarSite}=Fly,
	{_,BornSite,_BornPos} = get_born_site(Fly),
	NewFly = case NowCar of
				 0 ->
					 Fly2 = case IsUpdate of update->update_fly(Fly);_-> Fly end,
					 case LastTarSite of 0 -> ignore; _ -> leave_site(Fly2, LastTarSite, City) end,
					 Fly3 = Fly2#familycross_player{tarSite=BornSite,startTime=util:now()},
					 add_fly_mov_plan(Fly3,CarID),
					 Fly3;
				 _ ->
					 Fly2 = leave_car(City,NowCar,Fly),
					 Fly3 = join_car(City,CarID,Fly2),
					 Fly3#familycross_player{car=CarID}
			 end,
	NewFly.
								

check_can_stop(RoleID,ServerID) ->
	case get_fly(RoleID,ServerID) of
		#familycross_player{startTime=ST,car=NowCar,tarSite=_NowTarSite,rebornSec=RSec}=Fly ->
			case util:now()< RSec of
				true -> {false,3};
				_ -> case NowCar of
						0 ->
							 case ST of
								 0 -> {true,Fly,noupdate};
								 _ -> {true,Fly,update}
							 end;
						_ -> {false,2}
					 end
			end;
		_ -> {false,5}
	end.
																											 

check_can_mov(RoleID,ServerID,City,TarSite) ->
	case get_fly(RoleID,ServerID) of
		#familycross_player{city=NowCity,car=NowCar,rebornSec=RSec,type=Type,startTime=ST}=Fly->
			case util:now() < RSec of
				true -> {false,3};
				_ ->
					if NowCity == City ->
							case check_in_site(RoleID,ServerID,City,TarSite,Type) of
								true -> {false,4};
								_ ->
									case NowCar of
										 0 ->
											 case ST of
												 0 -> {true,Fly,noupdate};
												 _->  {true,Fly,update}
											 end;
										 _ ->
											 Fly2 = leave_car(NowCity,NowCar,Fly),
											 {true,Fly2,noupdate}
									 end
							end;
					   true -> {false,2}
					end
			end;
		_ -> {false,5}
	end.

check_in_site(RoleID,ServerID,City,Site,Type) ->
	#site{flys=Flys} = get_site(City, Site),
	lists:member({RoleID,ServerID}, get_type_flys(Type,Flys)).


update_fly_mov(#familycross_player{tarSite=LastTarSite}=Fly, City,TarSite,IsUpdate) ->
	Fly2 = case Fly#familycross_player.car of 0 -> Fly; CarID -> leave_car(City,CarID,Fly) end,
	Fly3 = case IsUpdate of
			   update -> update_fly(Fly2);
			   _ ->
				   case LastTarSite of 0 -> ignore; _ -> leave_site(Fly,LastTarSite,City) end,Fly2
		   end,
	NewFly = Fly3#familycross_player{tarSite=TarSite, startTime=util:now()},
	add_fly_mov_plan(NewFly),
	NewFly.

stop_fly(Fly,IsUpdate) ->
	case IsUpdate of update -> update_fly(Fly);_ -> Fly end.

leave_site(Fly,LastTarSite,City)->
	#familycross_player{type=Type,roleID=RoleID,serverID=ServerID} = Fly,
	add_bc_info(site,City,LastTarSite,RoleID,ServerID),
	#site{flys=Flys} = Site = get_site(City,LastTarSite),
	Flys2 = set_type_flys(Type,lists:delete({RoleID,ServerID},get_type_flys(Type,Flys)),Flys),
	Site2 = Site#site{flys=Flys2},
	set_site(Site2).

update_fly(#familycross_player{startPos=StartPos,tarSite=TarSite,startTime=StartTime,speed=Speed,type=_Type,roleID=RoleID,serverID=ServerID,city=City}=Fly)->
	#site{pos=SitePos} = get_site(City,TarSite),
	Len=(util:now() - StartTime) * Speed,
	StopPos = calc_new_pos(StartPos,SitePos,Len),
	NewFly = Fly#familycross_player{startPos=StopPos,tarSite=0,startTime=0},
	cancel_fly_mov_plan(RoleID,ServerID),
	NewFly.
						

check_can_drive(RoleID,_ServerID,Type,ID,TarCity) ->
	case get_car(ID,Type) of
		#car{driver=Driver,driver2=Driver2,tarCity=_LastCity,startTime=ST}=Car->
		    if Driver == RoleID orelse Driver2==RoleID->
					case check_in_city(Type,ID,TarCity) of
						true -> {false,4};
						false ->
							case ST of 
								0 -> {true,Car,noupdate};
								_ -> {true, Car,update} 
							end;
						_ -> {false,5}
					end;
			   true -> {false,2}
			end;
		_ -> {false,3}
	end.

update_car_mov(#car{tarCity=LastCity,id=ID,type=Type} = Car,TarCity,IsUpdate) ->
	StartTime= util:now(),
	Car2 = case IsUpdate of
			   update -> update_car(Car,StartTime);
			   _ ->
				   case LastCity of 0 ->ignore;_->leave_city(Car,LastCity) end,Car
		   end,
	NewCar = Car2#car{tarCity=TarCity, startTime=StartTime},
	add_car_mov_plan(NewCar),
	add_bc_info(car,ID,Type),
	NewCar.

leave_city(#car{id=ID,players=Flys,type=Type},LastCity)->
	#city{flys=CityFlys,cars=CityCars,typeNum=TypeNum}=CityData = get_city(LastCity),
	update_cars_city(Flys, 0,LastCity),
	CityFlys2 = delete_city_obj(Flys, CityFlys),
	CityCars2 = delete_city_obj([{ID,Type}],CityCars),
	City2 = CityData#city{flys=CityFlys2,cars=CityCars2,typeNum=dec(Type,TypeNum)},
	set_city(City2).
	

stop_car(Car,IsUpdate) ->
	case IsUpdate of update -> update_car(Car,util:now()); _ -> Car end.

update_car(#car{startPos=StartPos,tarCity=TarCity, startTime=StartTime,speed=Speed,type=Type,id=ID}=Car,Now)->
	#city{pos=CityPos} = get_city( TarCity),
	Len = (Now - StartTime )* Speed,
	StopPos = calc_new_pos(StartPos, CityPos, Len),
	NewCar = Car#car{startPos=StopPos,tarCity=0,startTime=0},
	cancel_car_mov_plan(ID,Type),
	NewCar.

add_car_mov_plan(#car{id=ID,startPos=StartPos,type=Type,startTime=StartTime,speed=Speed,tarCity=TarCity})->
	#city{pos=CityPos} = get_city(TarCity),
	TimeDiff = calc_time(StartPos,CityPos,Speed),
	F = fun()-> erlang:send(self(), {car_arrive, ID,Type,TarCity}) end,
	add_plan(#plan{sec=StartTime+TimeDiff, key={ID,Type},value=F}).
cancel_car_mov_plan(ID,Type)-> delete_plan({ID,Type}).

add_fly_mov_plan(Player) -> add_fly_mov_plan(Player,0).
add_fly_mov_plan(#familycross_player{startPos=StartPos,roleID=RoleID,serverID=ServerID,tarSite=TarSite,startTime=StartTime,speed=Speed,city=City},CarID)->
	#site{pos=SitePos} = get_site(City,TarSite),
	TimeDiff = calc_time(StartPos,SitePos,Speed),
	F = fun()-> erlang:send(self(), {fly_arrive, RoleID,ServerID,TarSite,City,CarID}) end,
	add_plan(#plan{sec=StartTime+TimeDiff, key={RoleID,ServerID}, value=F}).
cancel_fly_mov_plan(RoleID,ServerID) -> delete_plan({RoleID,ServerID}).

add_site_gas_plan(City, SiteID, Sec,RoleID,ServerID) ->
	F = fun()-> erlang:send(self(), {site_gas, City,SiteID,RoleID,ServerID}) end,
	add_plan(#plan{sec=Sec,key={City,SiteID},value=F}).
cancel_site_gas_plan(City,SiteID) -> delete_plan({City,SiteID}).


calc_new_pos(A,A,_) ->A;
calc_new_pos({A,B},{C,D},Dis) ->
	Dx = A - C,
	Dy = B - D,
	DL = math:sqrt(Dx*Dx+Dy*Dy),
	Tx = trunc((Dx * Dis) / DL) -1,
	Ty = trunc((Dy * Dis) / DL) -1,
	{A-Tx,B-Ty};
calc_new_pos({ID,A,B},{_,C,D},Dis)->
	Dx = A - C,
	Dy = B - D,
	DL = math:sqrt(Dx*Dx + Dy*Dy),
	Tx = trunc((Dx*Dis)/ DL)- 1,
	Ty = trunc((Dy*Dis)/ DL) -1,
	{ID,A-Tx,B-Ty}.
	
calc_time({X1,Y1},{X2,Y2},Speed) ->
	Dx = X1 - X2,
	Dy = Y1 - Y2,
	Len = math:sqrt(Dx * Dx + Dy * Dy),
	calc_time(Len,Speed);
calc_time({_,X1,Y1},{_,X2,Y2}, Speed) ->
	Dx = X1 - X2,
	Dy = Y1 - Y2,
	Len = math:sqrt(Dx*Dx + Dy*Dy),
	calc_time(Len, Speed).
calc_time(Len,0)-> Len;
calc_time(Len,Speed) -> trunc(Len/Speed).

get_city(TarCity) -> get({?city, TarCity}).
set_city(#city{id=ID}=City) -> put({?city, ID}, City).

get_site(City,TarSite) -> get({?site,City,TarSite}).
set_site(#site{cityID=City,id=ID}=Site) -> put({?site, City,ID},Site).

get_car(ID,Type) -> get({?car, Type,ID}).
set_car(#car{id=ID,type=Type}=Car) -> put({?car, Type,ID}, Car).

get_fly(RoleID,ServerID) -> get({?player, RoleID,ServerID}).
set_fly(#familycross_player{roleID=RoleID,serverID=ServerID}=Fly) ->
	update_player_rank_data(Fly),
	put({?player, RoleID,ServerID},Fly).

get_type_kill(Type)-> case get({?type_kill, Type}) of X when is_number(X) -> X; _ -> 0 end.
set_type_kill(Type,Num) -> put({?type_kill, Type}, Num).

get_type_flys(1,{A,_,_}) -> A;
get_type_flys(2,{_,A,_}) -> A;
get_type_flys(_,{_,_,A}) -> A.

alive_other_flys(1, {_,B,C}) -> B == [] andalso C == [];
alive_other_flys(2, {A,_,C}) -> A == [] andalso C == [];
alive_other_flys(_, {A,B,_}) -> A == [] andalso B == [].

set_type_flys(1,F,{_,B,C})->{F,B,C};
set_type_flys(2,F,{A,_,C}) ->{A,F,C};
set_type_flys(_,F,{A,B,_}) ->{A,B,F}.

get_rank_data() ->
	case get(?rank_data) of
		?undefined ->
			[];
		X ->
			X
	end.
set_rank_data(Data) ->
	put(?rank_data, Data).
update_player_rank_data(#familycross_player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_familycross_battle_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_familycross_rank_dtl(Player)|Data2],
	set_rank_data(Data3).

player2p_familycross_rank_dtl(Player)->
	#p_familycross_battle_rank_dtl{roleID=Player#familycross_player.roleID
					  ,serverID=Player#familycross_player.serverID
					  ,level=Player#familycross_player.level
					  ,get=Player#familycross_player.ownSiteTimes
					  ,score=Player#familycross_player.score
					  ,kill=Player#familycross_player.killNum
					  ,name=Player#familycross_player.roleName
								  ,roleType=Player#familycross_player.type
					  }.

check_winner_plan()->
	F = fun() -> erlang:send(self(), check_winner) end,
	add_plan(#plan{sec=util:now()+data_family_cross:get(check_interval)
				   ,key=check_winner_plan, value=F}).

add_plan(#plan{key=K}=Plan) -> set_plan([Plan|lists:keydelete(K, #plan.key, get_plan())]).
delete_plan(Key) -> set_plan(lists:keydelete(Key,#plan.key, get_plan())).
get_plan()-> case get(?plan) of ?undefined -> []; X -> X end.
set_plan(Plan)-> put(?plan, Plan).

plan()->
	Plans = get_plan(),
	Now = util:now(),
	Plans2 = lists:foldl(fun(Plan=#plan{sec=Sec,value=Value},Acc) ->
								 if Sec =< Now -> plan(Value) ,Acc;
									true -> [Plan|Acc]
								 end end ,[],Plans),
	set_plan(Plans2).

plan(Value) -> ?LOOSE_CATCH(Value()).
									 


check_in_city(_,_,0)-> false;
check_in_city(Type,ID,TarCity) ->
	case get_city(TarCity) of
		#city{cars=Cars} ->
			lists:member({ID,Type}, Cars);
		_ -> no_city
	end.

get_war_base_info()->
	{_,_,StartTime,Interval} = get(?endInfo),
	{Familys,Cities,Cars,CityDes,BornPos} =
		lists:foldl(fun({{?family,_},F},{FAcc,CAcc,RAcc,SAcc,BAcc}) -> {[familyDtl(F)|FAcc],CAcc,RAcc,SAcc,BAcc};
					   ({{?city,_},C},{FAcc,CAcc,RAcc,SAcc,BAcc})-> {FAcc,[cityDtl(C)|CAcc],RAcc,SAcc,BAcc};
					   ({{?car,_,_},C},{FAcc,CAcc,RAcc,SAcc,BAcc})-> {FAcc,CAcc,[carDtl(C)|RAcc],SAcc,BAcc};
					   ({{?site,_,_},S},{FAcc,CAcc,RAcc,SAcc,BAcc}) -> {FAcc,CAcc,RAcc,cityDes(S,SAcc),BAcc};
					   ({{?born_city,_},B},{FAcc,CAcc,RAcc,SAcc,BAcc}) -> {FAcc,CAcc,RAcc,SAcc,[B|BAcc]};
					   (_,D) -> D
					end,{[],[],[],[],[]},get()),
	{CityDes2,MapDes2} = cityDes2(CityDes),
	#sc_familycross_war_info{startTime=StartTime,interval=Interval,familys=Familys,city=Cities
							,cars=Cars,needGas=data_family_cross:get(win_need_gas)
							,des=MapDes2,cityDes=CityDes2,bornPos=BornPos}.

% [{CityID, [{Type, FamilyID,ServerID,Count,Total}]}]
%% cityDes(#site{owner=Type,cityID=City}=Site, DesList) when Type > 0->
%% 	case lists:keytake(City, 1, DesList) of
%% 		false ->
%% 			#fc_fighter{familyID=FamilyID,serverID=ServerID}=get({?family, Type}),
%% 			[{Type,FamilyID,ServerID,1,get_type_gas(Type,Site)}|DesList];
%% 		{value,{Type,FamilyID,ServerID,Count,Total}, Other} ->
%% 			[{Type,FamilyID,ServerID,Count+1,Total+get_type_gas(Type,Site)}|Other]
%% 	end;
cityDes(#site{owner=OwnerType, cityID=City, id=SID}=Site, DesList) when OwnerType > 0 andalso SID < 10 ->
	case lists:keytake({City, OwnerType}, 1,DesList) of
		false -> [{{City, OwnerType}, 1, get_type_gas(OwnerType,Site)}|DesList];
		{value,{{City,Type}, Count, Total} ,Other} ->
			case Type of 
				OwnerType  ->  [{{City, Type}, Count+1, Total+get_type_gas(Type, Site)}|Other];
				_ -> [{{City, Type}, Count, Total+get_type_gas(Type, Site)}|Other]
			end
	end;				
cityDes(_,DesList) -> DesList.

cityDes2(DesList) ->
	CityDes = [begin
				    #fc_fighter{familyID=FamilyID,serverID=ServerID}=get({?family, Type}),
				   #p_familycross_city_des{cityID=CityID,familyID=FamilyID,serverID=ServerID, count=Count}
			   end||{{CityID,Type},Count,_}<-DesList],
	DesList2 = 
		lists:foldl(fun({{_,Type}, Count,Total},Acc)->
							#fc_fighter{familyID=FamilyID,serverID=ServerID}=get({?family, Type}),
							case lists:keytake({FamilyID,ServerID}, 1, Acc) of
								false->
									[{{FamilyID,ServerID}, Count,Total}|Acc];
								{value,{_,CountAcc,TotalAcc},Other} ->
									[{{FamilyID,ServerID},Count+CountAcc,Total+TotalAcc}|Other]
						end
				end,generate_base_desList(),DesList),
	MapDes = [#p_familycross_head_des{familyID=FamilyID,serverID=ServerID,count=Count,total=Total}
			 ||{{FamilyID,ServerID},Count,Total}<-DesList2],
	{CityDes,MapDes}.
	
generate_base_desList() ->
	lists:foldl(fun(Type,Acc) ->
						case get({?family, Type}) of
							#fc_fighter{familyID=FamilyID,serverID=ServerID} ->
								[{{FamilyID,ServerID}, 0,0}|Acc];
							_ -> Acc
						end end, [], [1,2,3]).


get_type_gas(1,#site{team1Gas=#teamGas{startGetSec=SGS,gas=GAS}}) when SGS > 0 -> GAS;
get_type_gas(2,#site{team2Gas=#teamGas{startGetSec=SGS,gas=GAS}}) when SGS > 0 -> GAS;
get_type_gas(3,#site{team3Gas=#teamGas{startGetSec=SGS,gas=GAS}}) when SGS > 0 -> GAS;
get_type_gas(_,_) -> 0.

cityPos({X,Y}) -> #p_city_pos{x=X,y=Y}.
sitePos({ID,X,Y}) -> #p_site_pos{city=ID,x=X,y=Y}.
familyDtl(#fc_fighter{familyID=FamilyID,type=Type,familyName=FamilyName})->
	#p_familycross_war_family_dtl{familyID=FamilyID,type=Type,familyName=FamilyName}.
flyDtl(Player)->
	#p_familycross_war_fly{
	   % id=Player#familycross_player.car
	   city=Player#familycross_player.city
	   ,type=Player#familycross_player.type
	   ,flyType=Player#familycross_player.flyType
	   ,startPos=sitePos(Player#familycross_player.startPos)
	   ,tarSite=Player#familycross_player.tarSite
	   ,rebornTime=Player#familycross_player.rebornSec
	   ,roleID=Player#familycross_player.roleID
	   ,serverID=Player#familycross_player.serverID
	   ,fighters=fightersDtl(Player#familycross_player.fighters)
	   ,roleName=Player#familycross_player.roleName
	   ,startTime=Player#familycross_player.startTime
	   ,car=Player#familycross_player.car
       ,level=Player#familycross_player.level
        ,speed=Player#familycross_player.speed
	  }.
fightersDtl(Fighters) -> [fighterDtl(Fighter)||Fighter<-Fighters].
fighterDtl(#ger{gerBase=GerBase,gerAttr=GerAttr,gerHp=Hp}) ->
	#gerBase{gerTypeID=GerTypeID,gerPos=Pos}=GerBase,
	#gerAttr{gerHpMax=GerHpMax} = GerAttr,
	#p_familycross_fighter{typeID=GerTypeID,pos=Pos,blood=GerHpMax,nowBlood=Hp}.
cityDtl(City)->
	#p_familycross_war_city{
	   id=City#city.id
	   ,pos=cityPos(City#city.pos)
	%   ,cars=warCar2(City#city.cars)
						   ,siteInfo=sitesInfo(City#city.id,City#city.sites)
	  }.
siteDtl(Site)->
	#p_familycross_war_site{
	   id=Site#site.id
	   ,city=Site#site.cityID
	   ,pos=sitePos(Site#site.pos)
	   ,fly1=warFly2(element(1,Site#site.flys))
	   ,fly2=warFly2(element(2,Site#site.flys))
	   ,fly3=warFly2(element(3,Site#site.flys))
						   ,ownType=Site#site.owner
						   ,onwSec=Site#site.ownSec
	  }.
sitesInfo(City,SiteIDS) ->
	Now = util:now(),
	lists:foldl(fun({SiteID,_,_},Acc) ->
						#site{owner=Owner,ownSec=OwnSec} = get_site(City,SiteID),
						if OwnSec =< Now -> [#p_familycross_war_site3{city=City,site=SiteID,owner=Owner}|Acc];
						   true -> Acc
						end end, [], SiteIDS).
sitesInfo(Sites) ->
		Now = util:now(),
	lists:foldl(fun({City,SiteID},Acc) ->
						#site{owner=Owner,ownSec=OwnSec} = get_site(City,SiteID),
						if OwnSec =< Now -> [#p_familycross_war_site3{city=City,site=SiteID,owner=Owner}|Acc];
						   true -> Acc
						end end, [], Sites).
	
carDtl(Car) ->
	#p_familycross_war_car{
	   id=Car#car.id
	   ,type=Car#car.type
	   ,startPos=cityPos(Car#car.startPos)
	   ,tarCity=Car#car.tarCity
						  ,startTime=Car#car.startTime
						  ,driver=Car#car.driver
						  ,driver2=Car#car.driver2
						  ,all=[RoleID||{RoleID,_}<-Car#car.players]
	  }.
warCar2(Cars) ->
	[#p_familycross_war_car2{id=CarID,type=Type}||{CarID,Type}<-Cars].
warFly2(Flys)->
	[#p_familycross_war_fly2{roleID=RoleID,serverID=ServerID}||{RoleID,ServerID}<-Flys].

bool2int(true ) -> 1;
bool2int(_ ) -> 0.

get_born_site(#familycross_player{city=City,type=Type})->
	BornSiteList = data_family_cross:get({born_site,City}),
	lists:keyfind(Type,1,BornSiteList).

add_replay(RL,RUID)-> lists:sublist([RUID|RL],data_family_cross:get(max_replay_num)).

update_player_killNum(#familycross_player{type=Type,killNum=K,score=S}=Player)->
	set_type_kill(Type,get_type_kill(Type)+1),
	Player#familycross_player{killNum=K+1,score=erlang:min(S+data_family_cross:get(kill_honor), data_family_cross:get(max_honor))}.

add_ets_war_info(WarID,Pid)->
	ets:insert(?ETS_FAMILY_CROSS_INFO,#ets_family_cross_info{warID=WarID,pid=Pid}).

filter_out_zero_hp(List)->
	lists:filter(fun(#ger{gerHp=GerHp}) -> GerHp > 0 end,List).

recalc_fighter_list(FighterList, FighterList2) ->
    lists:foldr(fun(#ger{gerID=GerID}=Ger, Acc) ->
                        case lists:keyfind(GerID, #ger.gerID, FighterList2) of
                            false ->
                                [Ger|Acc];
                            #ger{gerHp=GerHp,gerProHp=GerProHp} ->
                                NewHp = min(GerHp,Ger#ger.gerAttr#gerAttr.gerHpMax),
                                [Ger#ger{gerHp=NewHp,gerProHp=GerProHp}|Acc]
                        end
                end, [], FighterList).

calc_hp_percent(FighterList) ->
	{SumHP, SumHPMax} = lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
											if HP < 0->
												   {AccHP,AccHPMax+HPMax};
											   true ->
												   
												   {AccHP + HP, AccHPMax + HPMax}
											end
									end, {0, 0}, FighterList),
	B = erlang:trunc(SumHP / SumHPMax * 100),
	if SumHP =< 0 ->
		   0;
	   true ->
		   if SumHP == SumHPMax->
				  100;
			  true ->
				  B
		   end
	end.
calc_hp_percent(SumHP,SumHPMax) ->
    erlang:trunc(SumHP / SumHPMax * 100).

leave_car(CityID,CarID,Fly=#familycross_player{roleID=RoleID,serverID=ServerID,type=Type})->
	#car{players=Flys,driver2=Driver2,driver=Driver}=Car = get_car(CarID,Type),
	Driver_2 = case Driver of RoleID -> 0;_ -> Driver end,
	Driver2_2 = case Driver2 of RoleID -> 0; _ -> Driver2 end,
	Car2 = Car#car{players=lists:delete({RoleID,ServerID},Flys),driver=Driver_2,driver2=Driver2_2},
	set_car(Car2),
	{_,_BornSite,{BX,BY}} = get_born_site(Fly),
	Fly2 = Fly#familycross_player{car=0,startPos={CityID, BX,BY}},
	add_bc_info(car,CarID,Type),
	set_fly(Fly2),
	Fly2.
join_car(City,CarID,Fly=#familycross_player{roleID=RoleID,serverID=ServerID,type=Type}) ->
	#car{players=Flys,driver=Driver,tarCity=CarCity,driver2=Driver2}=Car = get_car(CarID,Type),
	case length(Flys) >= data_family_cross:get(car_sits) of
		true -> false;
		_ ->
			case City of
				CarCity -> 

					Car2 = 
						case Driver of 0 -> Car#car{players=[{RoleID,ServerID}|Flys], driver=RoleID};
							_ -> case Driver2 of 0 -> Car#car{players=[{RoleID,ServerID}|Flys], driver2=RoleID};
									 _ -> Car#car{players=[{RoleID,ServerID}|Flys]}
								 end
						end,
					set_car(Car2),
					Fly2 = Fly#familycross_player{car=CarID,startPos={City,0,0},tarSite=0,startTime=0},
					add_bc_info(car,CarID,Type),
					set_fly(Fly2),
					Fly2;
				_ -> false
			end
	end.


do_bc(Info,Type)->
	[send_msg:direct(ServerID,family_cross_fight_server, {Type,RoleID,Info})||{RoleID,ServerID}<-get_bcList()].

bc_info(open)->
	do_bc(get_war_base_info(),familycross_bc_info);
bc_info(mark) ->
	put(?bc_mark,0),
	{A,B,C,D} = get_mark_bc(),
	bc_all(get_car_msg(A),get_city_msg(C)),
	
	bc_city(get_fly_msg(B), get_site_msg(D)),
	set_mark_bc({[],[],[],[]}),
%% 	CarMsg = get_car_msg(A),
%% 	FlyMsgList = get_fly_msg(B),
%% 	SetMsgList = get_fly_msg(D),
	ok;
bc_info(_Msg) -> ok.

bc_all([],[]) -> ignore;
bc_all(Msg,Msg2) -> 
	do_bc(#sc_familycross_map_update{car=Msg,site=Msg2}, familycross_bc_info).
bc_city(FlyMsg,SiteMsg) ->
	lists:foreach(fun({CityID, _,_}) -> 
						  F = case lists:keyfind(CityID, 1,FlyMsg) of false -> []; {_,M} -> M end,
						  S = case lists:keyfind(CityID, 1,SiteMsg) of false -> [];{_,N} -> N end,
						  #city{flys=BCList} = get_city(CityID),
						  if F == []  andalso S == [] -> ignore;
							 true ->
								 [send_msg:direct(ServerID, family_cross_fight_server,{familycross_bc_info, RoleID, 
																					   #sc_familycross_city_update{fly=F,site=S}})
									||{RoleID,ServerID}<-BCList]
						  end
				  end, data_family_cross:get(cities_list)).
	
get_car_msg(CarInfoList) -> 
	[carDtl(get_car(ID,Type))||{Type,ID}<-CarInfoList].
get_city_msg(CityInfoList) ->
	sitesInfo(CityInfoList).
get_fly_msg(FlyInfoList) -> 
	lists:foldl(fun({RoleID,ServerID,City},Acc)->
						case lists:keytake(City, 1, Acc) of
							false ->
								[{City, [flyDtl(get_fly(RoleID,ServerID))]}|Acc];
							{value,{City,NL},Other} ->
								[{City,[flyDtl(get_fly(RoleID,ServerID))|NL]}|Other]
						end end, [], FlyInfoList).
get_site_msg(SiteInfoList) ->
	lists:foldl(fun({City,SiteID},Acc) ->
						case lists:keytake(City, 1, Acc) of
							false ->
								[{City, [siteDtl(get_site(City,SiteID))]}|Acc];
							{value,{City, NL}, Other} ->
								[{City, [siteDtl(get_site(City,SiteID))|NL]}|Other]
						end end, [], SiteInfoList).

% {car,fly,city,site}
get_mark_bc() ->	case get(?mark_bc) of ?undefined -> {[],[],[],[]}; X -> X  end.
add_bc_list([],List) -> List;
add_bc_list(Value,List) -> case lists:member(Value,List) of true-> List; _ -> [Value|List] end.
set_mark_bc(Info) -> put(?mark_bc,Info).

add_bc_info(car,ID,Type) ->
	add_mark(),
	{A,B,C,D}=get_mark_bc(),
	set_mark_bc({add_bc_list({Type,ID},A),B,C,D});
add_bc_info(city, City, Site) ->
	add_mark(),
	{A,B,C,D} = get_mark_bc(),
	set_mark_bc({A,B,add_bc_list({City,Site},C),D}).
add_bc_info(fly,RoleID,ServerID,City) ->
	add_mark(),
	{A,B,C,D}=get_mark_bc(),
	set_mark_bc({A,add_bc_list({RoleID,ServerID,City},B),C,D}).
add_bc_info(site,City,Site,RoleID,ServerID) ->
	if Site > 10 -> ok;
	   true ->
	add_mark(),
	{A,B,C,D}=get_mark_bc(),
	set_mark_bc({A,add_bc_list({RoleID,ServerID,City},B),C,add_bc_list({City,Site},D)})
	end.

add_mark()->
	case get(?bc_mark) of
		1 -> ignore;
		_ -> put(?bc_mark,1),
			 erlang:send_after(1000,self(), {bc_info,mark})
	end.

add_bcList(PlayerList) ->
	RL=[{RoleID,ServerID}||#familycross_player{roleID=RoleID,serverID=ServerID}<-PlayerList],
	put(?bcList, RL++get_bcList()).
get_bcList()-> case get(?bcList) of ?undefined -> []; X -> X end.
get_bcList(Type) -> case get({?bcList,Type}) of ?undefined -> []; X -> X end.

sync_role([{RoleID,ServerID},{TarRoleID,TarServerID}],_CityID) ->
	P1 = flyDtl(get_fly(RoleID,ServerID)),
	P2 = flyDtl(get_fly(TarRoleID,TarServerID)),
	Msg = #sc_familycross_attack_update{flys=[P1,P2]},
	family_cross_fight_router:send_client(ServerID,RoleID,Msg),
	family_cross_fight_router:send_client(TarServerID,TarRoleID,Msg).

stop() ->
	{WarID,_} = get(?endInfo),
	family_cross_fight_manager:war_stopped(WarID),
	erlang:send(self(), stop).

push_end_rank(TypeGasList)->
	RankList2 = [begin
				 {_,GasScore} = lists:keyfind(RoleType, 1, TypeGasList),
				 RD#p_familycross_battle_rank_dtl{score=Score+GasScore}
				 end||#p_familycross_battle_rank_dtl{roleType=RoleType,score=Score}=RD<-get_rank_data()],
	do_bc(#sc_familycross_battle_end{rank=RankList2},familycross_bc_info).

end_war()-> 
	Sites = lists:foldl(fun({CID,_,SiteIDs} ,Acc1)->
								lists:foldl(fun({SID,_,_},Acc2) ->
													NewSite = update_site_gas(get_site(CID,SID)),
													set_site(NewSite),
													[NewSite|Acc2]
											end , Acc1, SiteIDs)
						end,[], data_family_cross:get(cities_list)),
	{Winner,Team1Gas,Team2Gas,Team3Gas} = get_war_winner(Sites,end_war),
	end_war(Winner, Team1Gas,Team2Gas,Team3Gas).
end_war(_WinnerType, {Type1,Gas1,KeyWord1},{Type2,Gas2,KeyWord2},{Type3,Gas3,KeyWord3}) ->
	set_plan([]),
	Ratio = data_family_cross:get(gas_ratio),
	push_end_rank([{Type1,Gas1 div Ratio},{Type2,Gas2 div Ratio},{Type3, Gas3 div Ratio}]),
	do_bc(clean_battle_info, familycross_bc_cmd),
	erlang:send(self(), stop),
%	{Honor1,Unicoin1,Jifen1} = calc_type_reward(Type1,Gas1,KeyWord1),
%	{Honor2,Unicoin2,Jifen2} = calc_type_reward(Type2,Gas2,KeyWord2),
%	{Honor3,Unicoin3,Jifen3} = calc_type_reward(Type3,Gas3,KeyWord3).
	MemberFightResultList = generate_member_fight_result(),
	lists:foreach(fun({Type,Gas,KeyWord})->
		case lists:keyfind(Type,1,MemberFightResultList) of
			false->
				?ERR("not find type:~w member_fight_result in :~w ~n",[Type,MemberFightResultList]);
			{_Type,MemberResult}->
				KillNum = get_type_kill(Type),
				case KeyWord of
					first_base->
						FightResult = #anubis_fight_result{fightresult=win,killnum=KillNum,resource=Gas,memberresult=MemberResult};
					second_base->
						FightResult = #anubis_fight_result{fightresult=equal,killnum=KillNum,resource=Gas,memberresult=MemberResult};
					third_base->
						FightResult = #anubis_fight_result{fightresult=lose,killnum=KillNum,resource=Gas,memberresult=MemberResult}
				end,
				{FamilyPlayer,CSeasonID}= get_anubis_family_player(Type),
				%%调用anubis_rank_server接口更新对应公会排行数据
				anubis_rank_server:up_anubis_rank(FamilyPlayer,FightResult,CSeasonID)
		end
	end,[{Type1,Gas1,KeyWord1},{Type2,Gas2,KeyWord2},{Type3,Gas3,KeyWord3}]).

%% calc_type_reward(FamilyType, Gas,KeyWord) ->
%% 	Base = data_family_cross:get(KeyWord),
%% 	Kill = get_type_kill(FamilyType),	
%% 	JIFEN = Base + Kill,
%% 	UNICOIN = JIFEN * data_family_cross:get(unicoin_ratio),
%% 	Resource = Gas * data_family_cross:get(gas_ratio),
%% 	Honor = Resource + JIFEN,
%% 	{erlang:min(Honor,data_family_cross:get(max_honor)),UNICOIN,erlang:min(JIFEN, data_family_cross:get(max_jifen))}.

set_anubis_family_player(Type,FamilyPlayer) ->
	put({?anubis_family_player,Type},FamilyPlayer).

get_anubis_family_player(Type)->
	case get({?anubis_family_player,Type}) of
		?undefined->
			?ERR("not find AnubisFamilyPlayer Type:~w ~n",[Type]),
			?undefined;
		X ->
			X
	end.

generate_member_fight_result()->
    %{_Dictionary,DicList} = erlang:process_info(self(),dictionary),
    MemberPlayerList = [MemberPlayer||{{?player,_RoleID,_ServerID},MemberPlayer}<-get()],
    lists:foldl(fun(#familycross_player{roleID=RoleID,killNum=KillNum,ownSiteTimes=OwnSiteTimes,type=Type},Acc)->
    		case lists:keytake(Type,1,Acc) of
    			false->
    				[{Type,[#anubis_family_member_result{roleid=RoleID,killnum=KillNum,resourcepoint=OwnSiteTimes}]}|Acc];
    			{_Vaule,{Type,FindAcc},Other}->
    				[{Type,[#anubis_family_member_result{roleid=RoleID,killnum=KillNum,resourcepoint=OwnSiteTimes}|FindAcc]}|Other]
    		end
	end,[],MemberPlayerList).

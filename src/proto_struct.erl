-module(proto_struct).
-compile(export_all).

string(Str) ->
	Str2 = iolist_to_binary(Str),
	[<<(byte_size(Str2)):16>>, Str2].

-define(int8(V), <<V:8>>).
-define(int16(V), <<V:16>>).
-define(int32(V), <<V:32>>).
-define(int64(V), <<V:64>>).
-define(bool(V), (case V of true -> <<1:8>>; false -> <<0:8>> end)).
-define(string(V), (string(V))).
-define(tuple(V), (encode_def(element(1,V),V))).
-define(any(V), (if is_integer(V) ->
						if abs(V) < 128 ->
							   [<<241:8>>,?int8(V)];
						   abs(V) < 32768 ->
							   [<<242:8>>,?int16(V)];
						   abs(V) < 2147483648 ->
							   [<<243:8>>,?int32(V)];
						   true ->
							   [<<244:8>>,?int64(V)]
						end;
					is_boolean(V) ->
						[<<245:8>>,?bool(V)];
					is_list(V) orelse is_binary(V) ->
						[<<246:8>>,?string(V)];
					true ->
						[<<(get_id(element(1,V))):16>>,?tuple(V)]
				 end)).

-define(list_int8(List),	[<<(length(List)):16>>, [ ?int8(E) || E<-List ] ]).
-define(list_int16(List),	[<<(length(List)):16>>, [ ?int16(E) || E<-List ]]).
-define(list_int32(List), 	[<<(length(List)):16>>, [ ?int32(E) || E<-List ]]).
-define(list_int64(List), 	[<<(length(List)):16>>, [ ?int64(E) || E<-List ]]).
-define(list_bool(List), 	[<<(length(List)):16>>, [ ?bool(E) || E <- List ]]).
-define(list_string(List),	[<<(length(List)):16>>, [ ?string(E) || E<-List ]]).
-define(list_tuple(List),	[<<(length(List)):16>>, [ ?tuple(E) || E<-List ]]).
-define(list_any(List),		[<<(length(List)):16>>, [ ?any(E) || E<-List ]]).

encode_def(cs_trainingRoom_info, R)->
	{_}=R,
	[];
encode_def(sc_trainingRoom_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int64(V3)];
encode_def(cs_trainingRoom_start_training, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_ger_view, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int16(V4)];
encode_def(p_item_view, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int16(V5)];
encode_def(p_reward_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?list_tuple(V7),?list_tuple(V8)];
encode_def(p_trainingRoom_reward, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?int32(V4),?int32(V5),?list_tuple(V6),?list_tuple(V7),?int16(V8)];
encode_def(sc_trainingRoom_start_training, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_diamond, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_diamond_combine, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(sc_diamond_combine, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_diamond_equip, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int16(V3),?int8(V4)];
encode_def(sc_diamond_equip, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int16(V5),?int8(V6)];
encode_def(cs_diamond_demount, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int16(V3),?int8(V4)];
encode_def(sc_diamond_demount, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int16(V5),?int8(V6)];
encode_def(cs_diamond_demount_multi, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_diamond_location, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?int8(V4)];
encode_def(sc_diamond_demount_multi, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_diamond_holygrail_info, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_diamond_holygrail_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?bool(V4)];
encode_def(cs_diamond_holygrail_sacrifice, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_diamond_holygrail_sacrifice, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_diamond_holygrail_uplevel, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_diamond_holygrail_uplevel, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(cs_conquerisland_sign, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_sign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_conquerisland_buy, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(cs_conquerisland_info, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5),?list_int32(V6),?int32(V7),?int8(V8),?int32(V9)];
encode_def(cs_conquerisland_unrequest, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_unrequest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_conquerisland_times_update, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_conquerisland_war_base_info, R)->
	{_}=R,
	[];
encode_def(p_pos, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(p_centre, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int8(V6),?tuple(V7),?int32(V8)];
encode_def(p_player, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17}=R,
	[?int32(V2),?int32(V3),?string(V4),?int16(V5),?int8(V6),?int64(V7),?int8(V8),?int8(V9),?int32(V10),?tuple(V11),?tuple(V12),?int8(V13),?int32(V14),?int16(V15),?int64(V16),?int16(V17)];
encode_def(p_conquerisland_boss, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?tuple(V3),?int16(V4),?int16(V5),?int16(V6),?int16(V7),?int64(V8),?int64(V9),?int16(V10),?int8(V11)];
encode_def(sc_conquerisland_war_base_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int32(V3),?tuple(V4),?tuple(V5),?list_tuple(V6),?list_tuple(V7),?list_tuple(V8)];
encode_def(cs_conquerisland_role_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_carlos_replay_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?string(V4),?string(V5)];
encode_def(p_conquerisland_ger, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int16(V3),?int16(V4),?int64(V5),?int64(V6)];
encode_def(sc_conquerisland_role_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?int16(V5)];
encode_def(cs_conquerisland_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_conquerisland_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_conquerisland_stop, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_conquerisland_centre_dtl, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_conquerisland_centre_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_conquerisland_attack, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int8(V4)];
encode_def(p_action, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?list_int8(V4),?int8(V5),?int64(V6),?int64(V7),?int16(V8)];
encode_def(p_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15}=R,
	[?int64(V2),?int16(V3),?int8(V4),?int64(V5),?int64(V6),?int64(V7),?int64(V8),?int16(V9),?int16(V10),?int8(V11),?int8(V12),?int8(V13),?int8(V14),?int64(V15)];
encode_def(sc_fight_request, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?bool(V4)];
encode_def(sc_conquerisland_attack, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_conquerisland_centre_occupy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_conquerisland_centre_occupy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_conquerisland_reborn, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_conquerisland_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_conquerisland_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_conquerisland_update_war, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_conquerisland_self, R)->
	{_}=R,
	[];
encode_def(sc_conquerisland_self, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_conquerisland_rank, R)->
	{_}=R,
	[];
encode_def(p_fight_rank, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?string(V4),?int16(V5),?int32(V6),?int32(V7),?int64(V8),?int32(V9)];
encode_def(sc_conquerisland_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_conquerisland_talk, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_conquerisland_get_talk, R)->
	{_}=R,
	[];
encode_def(p_carlos_talk, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?string(V3),?string(V4),?int8(V5)];
encode_def(sc_conquerisland_get_talk, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_carlos_rank_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int16(V5),?int16(V6),?int16(V7),?string(V8),?int8(V9)];
encode_def(sc_conquerisland_end_war, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_conquerisland_talk, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_recycle_restore, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int64(V4),?int8(V5)];
encode_def(sc_recycle_restore, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int64(V4),?int64(V5),?list_tuple(V6)];
encode_def(cs_crystal_uplevel, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(p_crystalinfo, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int64(V5),?int64(V6)];
encode_def(sc_crystal_uplevel, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int64(V4),?int8(V5),?tuple(V6)];
encode_def(cs_crystal_uprank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(sc_crystal_uprank, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int64(V4),?int8(V5),?tuple(V6)];
encode_def(cs_crystal_info, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_crystal_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?list_tuple(V5)];
encode_def(p_crystalinfo_brief, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(ger_crystalinfo_brief, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_doublematch_lineup, R)->
	{_}=R,
	[];
encode_def(p_doublematch_team_trainer_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?string(V3),?bool(V4),?int8(V5),?int32(V6),?int32(V7),?int16(V8),?int32(V9)];
encode_def(p_equip, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int8(V5),?int64(V6),?int8(V7),?int32(V8),?int16(V9),?int8(V10),?int8(V11),?int8(V12)];
encode_def(p_demage_rate_unit, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int32(V4)];
encode_def(p_awake_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int8(V4),?list_int32(V5)];
encode_def(p_ger, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int16(V5),?int32(V6),?int64(V7),?int64(V8),?int64(V9),?int64(V10),?list_tuple(V11),?list_tuple(V12),?int8(V13),?int16(V14),?int8(V15)];
encode_def(p_doublematch_team_ger_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?tuple(V4),?list_tuple(V5)];
encode_def(p_doublematch_team_member, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?list_tuple(V3)];
encode_def(sc_doublematch_lineup, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_ger_pos_unit, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int32(V3)];
encode_def(cs_doublematch_update_lineup, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_doublematch_update_lineup, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_doublematch_sign, R)->
	{_}=R,
	[];
encode_def(sc_doublematch_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_doublematch_sign_cancel, R)->
	{_}=R,
	[];
encode_def(sc_doublematch_sign_cancel, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_doublematch_times_buy, R)->
	{_}=R,
	[];
encode_def(sc_doublematch_times_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int32(V4)];
encode_def(cs_doublematch_roleinfo, R)->
	{_}=R,
	[];
encode_def(sc_doublematch_roleinfo, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12),?int8(V13),?int16(V14),?int32(V15)];
encode_def(cs_doublematch_rankinfo, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int32(V4)];
encode_def(p_doublematch_rank_unit, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?string(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?int32(V8),?int8(V9),?int16(V10),?int32(V11),?int32(V12),?int32(V13),?int8(V14)];
encode_def(sc_doublematch_rankinfo, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int32(V4),?list_tuple(V5),?int32(V6)];
encode_def(sc_doublematch_quit, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_doublematch_record_list, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int32(V4)];
encode_def(p_record_unit, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?list_tuple(V3),?int32(V4)];
encode_def(sc_doublematch_record_list, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int8(V5),?int32(V6)];
encode_def(cs_doublematch_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_fight_double_request, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?bool(V4)];
encode_def(sc_doublematch_replay, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?int8(V4),?int8(V5)];
encode_def(sc_doublematch_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?list_tuple(V2),?int8(V3),?int8(V4),?tuple(V5),?int32(V6)];
encode_def(cs_goldenegg_use_item, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_goldenegg_use_item, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_int32(V4)];
encode_def(cs_goldenegg_smash, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_int32(V3)];
encode_def(p_reward_view, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_goldenegg_smash, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_goldenegg_roleinfo, R)->
	{_}=R,
	[];
encode_def(sc_goldenegg_roleinfo, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?list_int32(V4)];
encode_def(cs_goldenegg_shop, R)->
	{_}=R,
	[];
encode_def(p_goods_unit, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?tuple(V3),?int32(V4),?int8(V5),?int32(V6),?int32(V7),?int32(V8)];
encode_def(sc_goldenegg_shop, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_goldenegg_open, R)->
	{_}=R,
	[];
encode_def(sc_goldenegg_open, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_goldenegg_update_score, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_member_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int8(V4),?int8(V5),?bool(V6),?int8(V7),?int32(V8)];
encode_def(sc_matchRoom_init, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?list_tuple(V3),?int32(V4)];
encode_def(sc_matchRoom_ready, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_matchRoom_cancel, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_matchRoom_kick, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_matchRoom_exit, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_int32(V3)];
encode_def(sc_matchRoom_new, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_matchRoom_ready, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_matchRoom_cancel, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_matchRoom_exit, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_matchRoom_close, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(cs_skin_compose, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_skin_compose, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_skin_activate, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_skin_activate, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_skin_equip, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_skin_equip, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_skin_demount, R)->
	{_}=R,
	[];
encode_def(sc_skin_demount, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_skin, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(cs_skin_info, R)->
	{_}=R,
	[];
encode_def(p_skin_buff, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5)];
encode_def(sc_skin_info, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?int32(V3),?tuple(V4)];
encode_def(p_skin_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_home_info, R)->
	{_}=R,
	[];
encode_def(c_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_home_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_home_build, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_home_build, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_home_up_stage, R)->
	{_}=R,
	[];
encode_def(sc_home_up_stage, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(c_need, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(cs_home_get_friend, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_home_get_friend, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_home_task_info, R)->
	{_}=R,
	[];
encode_def(home_task, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20}=R,
	[?int32(V2),?int8(V3),?int32(V4),?list_int32(V5),?int32(V6),?int32(V7),?int32(V8),?tuple(V9),?int8(V10),?int32(V11),?list_int64(V12),?list_int32(V13),?list_int8(V14),?list_int32(V15),?int32(V16),?int32(V17),?int32(V18),?string(V19),?string(V20)];
encode_def(sc_home_task_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_home_task_operate, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?list_int64(V4),?int32(V5)];
encode_def(sc_home_task_operate, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_home_bounty_task_info, R)->
	{_}=R,
	[];
encode_def(sc_home_bounty_task_info, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?int32(V3),?int32(V4)];
encode_def(cs_home_cirrus_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cirrus_node, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5),?int32(V6),?int64(V7),?int8(V8),?tuple(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13)];
encode_def(sc_home_cirrus_info, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_home_cirrus_fight, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_home_cirrus_fight, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_home_cirrus_operate, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int64(V4)];
encode_def(sc_home_cirrus_operate, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_home_exchange, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_home_exchange, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_sign_reward_info, R)->
	{_}=R,
	[];
encode_def(p_sign_reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int8(V4)];
encode_def(sc_sign_reward_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?int8(V5),?int8(V6)];
encode_def(cs_sign_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_sign_reward, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_awake_ger, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_awake_ger, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int64(V4),?int8(V5)];
encode_def(cs_awake_recast_ger, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?int8(V4)];
encode_def(sc_awake_recast_ger, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_int32(V3),?int64(V4),?int8(V5)];
encode_def(cs_awake_exchange_skill, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?int32(V4)];
encode_def(sc_awake_exchange_skill, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int32(V5)];
encode_def(cs_awake_cancel_skill, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_awake_cancel_skill, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(cs_generalteam_create, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_generalteam_create, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_generalteam_invite, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_generalteam_invite, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_team_member_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?string(V3),?bool(V4),?int32(V5),?int8(V6),?int16(V7),?int64(V8),?int32(V9)];
encode_def(sc_generalteam_invite_request, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?tuple(V2),?int8(V3),?int8(V4),?int64(V5),?int64(V6)];
encode_def(cs_generalteam_invite_response, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int32(V4),?int8(V5),?int8(V6)];
encode_def(sc_generalteam_invite_response, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int32(V4),?tuple(V5)];
encode_def(p_team_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int32(V4),?list_int32(V5),?list_tuple(V6)];
encode_def(update_generalteam_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_generalteam_leaveteam, R)->
	{_}=R,
	[];
encode_def(sc_generalteam_leaveteam, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_generalteam_disbandteam, R)->
	{_}=R,
	[];
encode_def(sc_generalteam_disbandteam, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_generalteam_kick, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_generalteam_kick, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int32(V4)];
encode_def(cs_generalteam_change_authority, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_generalteam_change_authority, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5)];
encode_def(cs_generalteam_talk, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_generalteam_talk, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(update_generalteam_talk, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?string(V3)];
encode_def(cs_generalteam_info, R)->
	{_}=R,
	[];
encode_def(sc_generalteam_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_generalteam_change_teamtype, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_generalteam_change_teamtype, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_panicbuy_info, R)->
	{_}=R,
	[];
encode_def(p_ger_unit, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(p_item_unit, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(p_sell_reward_unit, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int64(V2),?int64(V3),?int64(V4),?list_tuple(V5),?list_tuple(V6)];
encode_def(p_panic_buy_config, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?tuple(V3),?tuple(V4),?int32(V5),?int32(V6),?int32(V7),?int64(V8),?int64(V9)];
encode_def(sc_panicbuy_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?string(V4)];
encode_def(cs_panicbuy_once, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_panicbuy_once, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_discount_activity_list, R)->
	{_}=R,
	[];
encode_def(p_activity_unit, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?int64(V4),?int64(V5),?int8(V6),?int32(V7)];
encode_def(sc_discount_activity_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_discount_exchange_info, R)->
	{_}=R,
	[];
encode_def(p_exchange_config_unit, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?tuple(V3),?tuple(V4),?int32(V5),?int32(V6)];
encode_def(sc_discount_exchange_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?list_tuple(V4),?int32(V5)];
encode_def(cs_discount_exchange, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_discount_exchange, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int32(V4),?list_tuple(V5)];
encode_def(cs_discount_pay_activity_info, R)->
	{_}=R,
	[];
encode_def(p_pay_config_unit, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?tuple(V4),?int32(V5),?int32(V6)];
encode_def(sc_discount_pay_activity_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?list_tuple(V4),?int32(V5)];
encode_def(cs_discount_pay_activity, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_discount_pay_activity, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int32(V4),?list_tuple(V5)];
encode_def(p_homestead_log, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?int8(V3),?int8(V4),?string(V5),?int8(V6),?int32(V7)];
encode_def(p_homestead_machine, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int16(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int8(V8),?int32(V9)];
encode_def(p_homestead, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?string(V2),?int8(V3),?int8(V4),?int32(V5),?int8(V6),?int16(V7),?int64(V8),?int16(V9),?int16(V10),?int32(V11)];
encode_def(sc_homestead_error, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_homestead_get_info, R)->
	{_}=R,
	[];
encode_def(sc_homestead_get_info, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?list_tuple(V3)];
encode_def(cs_homestead_get_friend_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homestead_get_friend_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?tuple(V3),?list_tuple(V4)];
encode_def(cs_homestead_unlock_machine, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_homestead_unlock_machine, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_homestead_uproot_seed, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_homestead_uproot_seed, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_homestead_harvest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_homestead_harvest, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?tuple(V3)];
encode_def(cs_homestead_seeding, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_homestead_seeding, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_homestead_update_machine, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_homestead_change_ger, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_homestead_change_ger, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(cs_homestead_mating, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homestead_mating, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int8(V6)];
encode_def(sc_homestead_mating_to_friend, R)->
	{_,V2,V3,V4}=R,
	[?tuple(V2),?int32(V3),?int8(V4)];
encode_def(cs_homestead_addenergy, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_homestead_addenergy, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int8(V3),?int8(V4),?int32(V5),?int32(V6),?list_tuple(V7)];
encode_def(sc_homestead_addenergy_to_friend, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?int32(V4),?int32(V5)];
encode_def(cs_homestead_get_log, R)->
	{_}=R,
	[];
encode_def(sc_homestead_get_log, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_homestead_get_friend_log, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homestead_get_friend_log, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(sc_homestead_sync_mating_cool_second, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_homestead_sync_ger, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int16(V4)];
encode_def(sc_homestead_sync_add_enagy, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_homestead_compose, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(sc_homestead_compose, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_homestead_compose_list, R)->
	{_}=R,
	[];
encode_def(p_compose_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5)];
encode_def(sc_homestead_compose_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_task, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int32(V4)];
encode_def(cs_task_get_info, R)->
	{_}=R,
	[];
encode_def(sc_task_get_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?list_tuple(V2),?list_tuple(V3),?list_tuple(V4),?list_tuple(V5),?list_tuple(V6),?list_tuple(V7)];
encode_def(cs_task_operate, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_task_operate, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int8(V4),?list_tuple(V5)];
encode_def(sc_task_error, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_task_notify_change, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_int32(V4)];
encode_def(cs_fight_request, R)->
	{_}=R,
	[];
encode_def(cs_consumerank_info, R)->
	{_}=R,
	[];
encode_def(sc_consumerank_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_consumerank_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_consumerank_unit, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?bool(V3),?int16(V4),?int8(V5),?string(V6),?int64(V7),?int16(V8),?int32(V9),?int8(V10),?int32(V11)];
encode_def(sc_consumerank_list, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?tuple(V4)];
encode_def(cs_buddy_partner_insert, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int64(V3),?int8(V4)];
encode_def(sc_buddy_partner_insert, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int64(V4),?int64(V5)];
encode_def(cs_buddy_partner_insert_batch, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int64(V3)];
encode_def(sc_buddy_partner_insert_batch, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_int64(V4)];
encode_def(cs_buddy_partner_remove, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_buddy_partner_remove, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_dSign_unit, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int8(V5)];
encode_def(cs_dSign_info, R)->
	{_}=R,
	[];
encode_def(sc_dSign_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int32(V4),?list_tuple(V5),?int8(V6),?int32(V7),?list_tuple(V8)];
encode_def(cs_dSign_sign, R)->
	{_}=R,
	[];
encode_def(sc_dSign_sign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_dSign_reSign, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_dSign_reSign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_dSign_get_sevenReward, R)->
	{_}=R,
	[];
encode_def(sc_dSign_get_sevenReward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_dSign_get_monReward, R)->
	{_}=R,
	[];
encode_def(sc_dSign_get_monReward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_dSign_mark_mon, R)->
	{_}=R,
	[];
encode_def(sc_dSign_mark_mon, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_trainerRear_brief, R)->
	{_}=R,
	[];
encode_def(sc_trainerRear_brief, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_trainerRear_info, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_object_unit, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(p_rear_machine, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4),?int32(V5),?int32(V6)];
encode_def(sc_trainerRear_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_trainerRear_insert, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_int64(V4)];
encode_def(sc_trainerRear_insert, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_trainerRear_mature, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_trainerRear_mature, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_trainerRear_accelerate, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(sc_trainerRear_accelerate, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int32(V5)];
encode_def(cs_dojangrank_info, R)->
	{_}=R,
	[];
encode_def(sc_dojangrank_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(cs_dojangrank_rank, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_dojangrank_rank, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int8(V9),?int32(V10),?int32(V11)];
encode_def(sc_dojangrank_rank, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?tuple(V3),?list_tuple(V4)];
encode_def(cs_dojangrank_buy, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_dojangrank_buy, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(cs_dojangrank_select_ger_type, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_dojangrank_select_ger_type, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(cs_dojangrank_fight, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_dojangrank_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4),?list_tuple(V5),?tuple(V6)];
encode_def(cs_dojangrank_replay_list, R)->
	{_}=R,
	[];
encode_def(p_dojang_replay_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?string(V2),?string(V3),?int16(V4),?int16(V5),?int64(V6),?int32(V7),?int32(V8),?int32(V9)];
encode_def(sc_dojangrank_replay_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_dojangrank_replay_detail, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_dojangrank_replay_detail, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_dojangrank_ger_view_other, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_dojangrank_ger_view_other, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6)];
encode_def(cs_dojangrank_self_rank, R)->
	{_}=R,
	[];
encode_def(sc_dojangrank_self_rank, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(cs_dojangrank_world_info, R)->
	{_}=R,
	[];
encode_def(sc_dojangrank_world_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?list_int32(V2),?int32(V3),?list_int32(V4),?list_int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9)];
encode_def(cs_dojangrank_world_rank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(dr_ger_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(p_dr_world_rank, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int8(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?list_tuple(V14),?int32(V15),?int32(V16),?int32(V17),?int32(V18)];
encode_def(sc_dojangrank_world_rank, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?tuple(V3),?list_tuple(V4)];
encode_def(cs_dojangrank_world_buy, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_dojangrank_world_buy, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_dojangrank_world_refresh_enemy, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_dojangrank_world_refresh_enemy, R)->
	{_,V2,V3,V4,V5}=R,
	[?list_tuple(V2),?int32(V3),?tuple(V4),?int32(V5)];
encode_def(cs_dojangrank_world_select_ger_type, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_dojangrank_world_select_ger_type, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(cs_dojangrank_world_fight, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_dojangrank_world_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4),?list_tuple(V5),?tuple(V6)];
encode_def(cs_dojangrank_world_replay_list, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_dr_dojang_replay_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?tuple(V2),?tuple(V3),?int64(V4),?int32(V5),?int32(V6),?int32(V7)];
encode_def(sc_dojangrank_world_replay_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_dojangrank_world_replay_detail, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int32(V3)];
encode_def(sc_dojangrank_world_replay_detail, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_dojangrank_world_ger_view_other, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_dojangrank_world_ger_view_other, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?list_tuple(V3)];
encode_def(cs_dojangrank_world_top_replay_list, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_dojangrank_world_top_replay_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_trainerProf_info, R)->
	{_}=R,
	[];
encode_def(p_trainerProf_unit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_trainerProf_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_trainerProf_uplevel, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_trainerProf_uplevel, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(cs_trainerProf_battle_info, R)->
	{_}=R,
	[];
encode_def(p_trainerProf_battle_unit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_trainerProf_battle_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_trainerProf_battle_uplevel, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_trainerProf_battle_uplevel, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_trainerProf_battle_unclock, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_trainerProf_battle_unclock, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_treasurebowl_info, R)->
	{_}=R,
	[];
encode_def(p_treasurebowl_draw, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int8(V3),?int8(V4),?tuple(V5)];
encode_def(p_treasurebowl_activity, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int8(V3),?list_tuple(V4),?int8(V5),?int32(V6)];
encode_def(sc_treasurebowl_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?list_tuple(V3),?int8(V4),?list_int32(V5),?int32(V6),?int32(V7),?string(V8)];
encode_def(sc_treasurebowl_update, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_treasurebowl_exchange, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_treasurebowl_exchange, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_treasurebowl_draw, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_treasurebowl_draw, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_treasurebowl_open, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_maintask_info, R)->
	{_}=R,
	[];
encode_def(p_task_unit, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int64(V3),?int8(V4)];
encode_def(sc_maintask_info, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_maintask_draw, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_maintask_draw, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_xbattle_info, R)->
	{_}=R,
	[];
encode_def(sc_xbattle_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int16(V2),?int32(V3),?int32(V4),?int8(V5),?int32(V6),?int32(V7),?int16(V8)];
encode_def(cs_xbattle_challenge, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_xbattle_challenge, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4),?list_tuple(V5)];
encode_def(cs_xbattle_raid, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(p_xbattle_raid, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4)];
encode_def(sc_xbattle_raid, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_xbattle_offline_reward, R)->
	{_}=R,
	[];
encode_def(sc_xbattle_offline_reward, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?list_tuple(V3),?int16(V4)];
encode_def(sc_xbattle_tri, R)->
	{_,V2,V3}=R,
	[?int16(V2),?list_tuple(V3)];
encode_def(cs_xbattle_use_elixir, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_xbattle_use_elixir, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?list_tuple(V4)];
encode_def(cs_xbattle_buy_quick, R)->
	{_}=R,
	[];
encode_def(sc_xbattle_buy_quick, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int32(V4),?list_tuple(V5)];
encode_def(cs_xbattle_get_pass_reward, R)->
	{_}=R,
	[];
encode_def(sc_xbattle_get_pass_reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_xbattle_chapter_info, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_xbattle_dungeon, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_xbattle_chapter_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5)];
encode_def(cs_xbattle_set_reward_chapter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_xbattle_set_reward_chapter, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_xbattle_start_reward, R)->
	{_}=R,
	[];
encode_def(sc_xbattle_start_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_xbattle_challenge_boss, R)->
	{_}=R,
	[];
encode_def(sc_xbattle_challenge_boss, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_xbattle_package_full, R)->
	{_}=R,
	[];
encode_def(cs_xbattle_package_ok, R)->
	{_}=R,
	[];
encode_def(cs_exBoss_get_info, R)->
	{_}=R,
	[];
encode_def(p_exBoss_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int64(V5),?int64(V6),?tuple(V7)];
encode_def(p_exBoss_times_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int32(V4),?int8(V5)];
encode_def(p_exBoss_hit_list, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int64(V3)];
encode_def(sc_exBoss_get_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?tuple(V2),?list_tuple(V3),?tuple(V4),?int16(V5),?int16(V6),?int64(V7)];
encode_def(cs_exBoss_hit, R)->
	{_}=R,
	[];
encode_def(sc_exBoss_hit, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_exBoss_buy_times, R)->
	{_}=R,
	[];
encode_def(sc_exBoss_buy_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_exBoss_get_reward, R)->
	{_}=R,
	[];
encode_def(sc_exBoss_get_reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(sc_exBoss_update_times, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_exBoss_refresh, R)->
	{_}=R,
	[];
encode_def(sc_exBoss_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_exBoss_buy_cost, R)->
	{_}=R,
	[];
encode_def(sc_exBoss_buy_cost, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5)];
encode_def(cs_exBoss_oneKey, R)->
	{_}=R,
	[];
encode_def(sc_exBoss_oneKey, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_tasklink_get_info, R)->
	{_}=R,
	[];
encode_def(p_level_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4)];
encode_def(sc_tasklink_get_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?list_tuple(V6),?int32(V7),?int32(V8),?int32(V9)];
encode_def(cs_tasklink_get_progress, R)->
	{_}=R,
	[];
encode_def(p_point_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?list_int32(V5)];
encode_def(sc_tasklink_get_progress, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?list_tuple(V6),?int32(V7),?list_tuple(V8),?int32(V9)];
encode_def(cs_tasklink_get_reward_log, R)->
	{_}=R,
	[];
encode_def(p_reward_log, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?list_string(V3),?int32(V4),?list_tuple(V5)];
encode_def(sc_tasklink_get_reward_log, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_tasklink_sign, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_tasklink_sign, R)->
	{_,V2,V3}=R,
	[?int32(V2),?string(V3)];
encode_def(cs_tasklink_buy_time, R)->
	{_}=R,
	[];
encode_def(sc_tasklink_buy_time, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(cs_tasklink_ready_notice, R)->
	{_}=R,
	[];
encode_def(sc_tasklink_ready_notice, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?int32(V3),?int32(V4)];
encode_def(cs_tasklink_ready_opt, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_tasklink_ready_opt, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_tasklink_get_reward, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_tasklink_get_reward, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_activityFestival_info, R)->
	{_}=R,
	[];
encode_def(p_activityFestival_data, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?int16(V4)];
encode_def(p_activityFestival_box, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?tuple(V4)];
encode_def(sc_activityFestival_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int16(V5),?int8(V6),?list_tuple(V7),?list_tuple(V8)];
encode_def(p_activityFestival_self, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_activityFestival_box_get, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_activityFestival_self, R)->
	{_}=R,
	[];
encode_def(sc_activityFestival_self, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_activityFestival_sign, R)->
	{_}=R,
	[];
encode_def(sc_activityFestival_sign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_activityFestival_box_get, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_activityFestival_box_get, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_activityFestival_sign2, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_activityFestival_sign2, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familycross_info, R)->
	{_}=R,
	[];
encode_def(p_familycross_enermy, R)->
	{_,V2,V3}=R,
	[?int16(V2),?string(V3)];
encode_def(p_familycross_info_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?list_tuple(V9)];
encode_def(sc_familycross_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familycross_sign, R)->
	{_}=R,
	[];
encode_def(sc_familycross_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_familycross_car, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int8(V6),?int8(V7)];
encode_def(cs_familycross_displayer, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_familycross_displayer, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_displayer_info, R)->
	{_}=R,
	[];
encode_def(sc_familycross_displayer_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_familycross_player_fly, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(cs_familycross_player_fly, R)->
	{_}=R,
	[];
encode_def(sc_familycross_player_fly, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_familycross_enermy_info, R)->
	{_}=R,
	[];
encode_def(sc_familycross_enermy_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?string(V3),?int16(V4),?int16(V5)];
encode_def(p_familycross_war_site3, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(p_city_pos, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_site_pos, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int32(V4)];
encode_def(p_born_pos, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5)];
encode_def(p_familycross_war_car, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?tuple(V4),?int8(V5),?int32(V6),?int32(V7),?list_int32(V8),?int32(V9)];
encode_def(p_familycross_fighter, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int8(V3),?int64(V4),?int64(V5)];
encode_def(p_familycross_war_fly, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?tuple(V6),?int8(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?list_tuple(V12),?string(V13),?int32(V14),?int16(V15)];
encode_def(p_familycross_war_fly2, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_familycross_war_site, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?tuple(V4),?list_tuple(V5),?list_tuple(V6),?list_tuple(V7),?int8(V8),?int32(V9)];
encode_def(p_familycross_war_city, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4)];
encode_def(p_familycross_war_car2, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_familycross_war_family_dtl, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?string(V4)];
encode_def(cs_familycross_war_info, R)->
	{_}=R,
	[];
encode_def(p_familycross_city_des, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int16(V5)];
encode_def(p_familycross_head_des, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int16(V3),?int16(V4),?int32(V5)];
encode_def(sc_familycross_war_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int32(V3),?int32(V4),?list_tuple(V5),?list_tuple(V6),?list_tuple(V7),?list_tuple(V8),?list_tuple(V9),?list_tuple(V10)];
encode_def(cs_familycross_self_info, R)->
	{_}=R,
	[];
encode_def(sc_familycross_self_info, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_familycross_drive_car, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_drive_car, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_mov, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_attack, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5)];
encode_def(sc_familycross_attack, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familycross_city_dtl, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familycross_city_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4),?list_tuple(V5),?list_tuple(V6),?list_tuple(V7)];
encode_def(cs_familycross_site_dtl, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_site_dtl, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4),?int8(V5),?int32(V6)];
encode_def(cs_familycross_fly_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_familycross_replay_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?string(V4),?string(V5)];
encode_def(sc_familycross_fly_dtl, R)->
	{_,V2,V3,V4}=R,
	[?tuple(V2),?list_tuple(V3),?int16(V4)];
encode_def(sc_familycross_map_update, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(sc_familycross_city_update, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(sc_familycross_site_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_familycross_fly_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_familycross_be_driver, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_be_driver, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_drive_car_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familycross_drive_car_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_mov_stop, R)->
	{_}=R,
	[];
encode_def(sc_familycross_mov_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_mov_back, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_mov_back, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_reborn, R)->
	{_}=R,
	[];
encode_def(sc_familycross_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_familycross_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_familycross_attack_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_familycross_own_site, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_own_site, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_familycross_season_rank, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(p_anubis_family, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?string(V3),?int16(V4),?string(V5),?int32(V6),?int32(V7)];
encode_def(sc_familycross_season_rank, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int32(V3),?int16(V4),?int8(V5),?list_tuple(V6),?list_tuple(V7)];
encode_def(cs_familycross_seasoninfo, R)->
	{_}=R,
	[];
encode_def(sc_familycross_seasoninfo, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_int32(V4)];
encode_def(cs_familycross_family_rank, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_anubis_family_member, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int32(V4),?int32(V5),?int16(V6)];
encode_def(sc_familycross_family_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_familycross_des_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_familycross_battle_rank_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int16(V5),?int16(V6),?int16(V7),?string(V8),?int8(V9)];
encode_def(cs_familycross_battle_get_rank, R)->
	{_}=R,
	[];
encode_def(sc_familycross_battle_get_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_familycross_battle_end, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_familycross_be_driver2, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familycross_be_driver2, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familycross_self_car, R)->
	{_}=R,
	[];
encode_def(sc_familycross_self_car, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_payGuide_info, R)->
	{_}=R,
	[];
encode_def(p_payGuide_unit, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int8(V5),?int8(V6),?int16(V7),?list_tuple(V8),?list_tuple(V9)];
encode_def(sc_payGuide_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?tuple(V3)];
encode_def(cs_payGuide_get_reward, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_payGuide_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_payGuide_seven_info, R)->
	{_}=R,
	[];
encode_def(period_state, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_payGuide_seven_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int32(V3),?int32(V4),?int32(V5),?int8(V6),?list_tuple(V7),?int8(V8)];
encode_def(cs_payGuide_seven_period_info, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_payGuide_seven_period_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_payGuide_seven_draw, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_payGuide_seven_draw, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_payGuide_seven_task_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_payGuide_seven_period_summary, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_homeBoss_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_homeBoss, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5),?int8(V6),?tuple(V7),?int8(V8),?int8(V9),?int64(V10),?int64(V11),?string(V12)];
encode_def(sc_homeBoss_info, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?tuple(V3)];
encode_def(p_boss, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(cs_homeBoss_attack, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_homeBoss_attack, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_homeBoss_self, R)->
	{_}=R,
	[];
encode_def(sc_homeBoss_self, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_int16(V4)];
encode_def(cs_homeBoss_buy_times, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_homeBoss_buy_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_homeBoss_get_reward, R)->
	{_}=R,
	[];
encode_def(sc_homeBoss_get_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_homeBoss_read, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homeBoss_read, R)->
	{_}=R,
	[];
encode_def(p_lvlSgAttr_attr, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V24,V25,V26,V27,V28,V29}=R,
	[?int32(V2),?int64(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?int32(V14),?int32(V15),?int32(V16),?int32(V17),?int16(V18),?int16(V19),?int32(V20),?int32(V21),?int32(V22),?int32(V23),?int32(V24),?int32(V25),?int32(V26),?int32(V27),?int32(V28),?int32(V29)];
encode_def(sc_lvlSgAttr_inc, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?tuple(V4)];
encode_def(cs_galactica_info, R)->
	{_}=R,
	[];
encode_def(sc_galactica_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5),?list_int32(V6),?int32(V7),?int8(V8),?int32(V9)];
encode_def(cs_galactica_sign, R)->
	{_}=R,
	[];
encode_def(sc_galactica_sign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_galactica_buy, R)->
	{_}=R,
	[];
encode_def(sc_galactica_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_galactica_times_update, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_galactica_p_s, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(p_galactica_pos, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(p_galactica_mine, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int8(V3),?int8(V4),?tuple(V5),?list_tuple(V6),?list_tuple(V7),?int32(V8),?int32(V9),?list_tuple(V10),?list_tuple(V11)];
encode_def(p_galactica_mine_s, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?tuple(V4),?list_tuple(V5),?list_tuple(V6),?int32(V7),?list_tuple(V8),?list_tuple(V9)];
encode_def(p_galactica_player, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?int16(V3),?int8(V4),?int8(V5),?int8(V6),?int8(V7),?int32(V8),?string(V9),?int64(V10),?int16(V11),?tuple(V12),?int32(V13),?int16(V14)];
encode_def(p_galactica_player_s, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int16(V3),?int8(V4),?int8(V5),?tuple(V6),?int32(V7),?int32(V8)];
encode_def(p_galactica_fairy, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5)];
encode_def(p_galactica_home, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?list_int8(V4),?tuple(V5)];
encode_def(sc_galactica_gas, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(cs_galactica_war_base_info, R)->
	{_}=R,
	[];
encode_def(sc_galactica_war_base_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int8(V2),?int32(V3),?tuple(V4),?tuple(V5),?list_tuple(V6),?list_tuple(V7),?list_tuple(V8),?int32(V9),?int32(V10)];
encode_def(cs_galactica_self, R)->
	{_}=R,
	[];
encode_def(sc_galactica_self, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_galactica_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_galactica_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_galactica_attack, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int8(V4)];
encode_def(sc_galactica_attack, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_galactica_home_s, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int8(V3)];
encode_def(sc_galactica_home_s, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_galactica_update, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(cs_galactica_role_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_galactica_replay_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?string(V4),?string(V5)];
encode_def(sc_galactica_role_dtl, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_galactica_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_galactica_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_galactica_mov_stop, R)->
	{_}=R,
	[];
encode_def(sc_galactica_mov_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_galactica_rank_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int16(V5),?int16(V6),?string(V7),?int8(V8)];
encode_def(p_galactica_talk, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?string(V3),?string(V4),?int8(V5)];
encode_def(cs_galactica_talk, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_galactica_get_talk, R)->
	{_}=R,
	[];
encode_def(sc_galactica_get_talk, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_galactica_get_rank, R)->
	{_}=R,
	[];
encode_def(sc_galactica_get_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_galactica_talk, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_galactica_end_war, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_galactica_reborn, R)->
	{_}=R,
	[];
encode_def(sc_galactica_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_galactica_unrequest, R)->
	{_}=R,
	[];
encode_def(sc_galactica_unrequest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_galactica_mov_in, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_galactica_mov_in, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_twins_info, R)->
	{_}=R,
	[];
encode_def(p_twins_rank_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?int16(V4),?string(V5),?int64(V6),?int8(V7)];
encode_def(p_twins_box_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(sc_twins_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5),?list_int32(V6),?int8(V7),?int8(V8),?list_tuple(V9),?list_tuple(V10),?int8(V11),?int32(V12)];
encode_def(cs_twins_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_twins_sign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_twins_buy, R)->
	{_}=R,
	[];
encode_def(sc_twins_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_twins_times_update, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_twins_p_s, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(p_twins_pos, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(p_twins_mine, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int16(V6),?int16(V7),?int32(V8),?int64(V9),?int64(V10),?tuple(V11),?list_tuple(V12),?int8(V13)];
encode_def(p_twins_mine_s, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int32(V6),?int64(V7),?tuple(V8),?list_tuple(V9)];
encode_def(p_twins_player, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?int16(V3),?int8(V4),?int8(V5),?int8(V6),?int8(V7),?int32(V8),?int64(V9),?int64(V10),?tuple(V11),?string(V12),?int32(V13),?int16(V14)];
encode_def(p_twins_player_s, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int16(V3),?int8(V4),?int8(V5),?tuple(V6),?int32(V7),?int64(V8)];
encode_def(p_twins_fairy, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5)];
encode_def(p_twins_home, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_twins_unrequest, R)->
	{_}=R,
	[];
encode_def(sc_twins_unrequest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_twins_war_base_info, R)->
	{_}=R,
	[];
encode_def(sc_twins_war_base_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int8(V3),?int32(V4),?tuple(V5),?list_tuple(V6),?list_tuple(V7),?list_tuple(V8)];
encode_def(cs_twins_self, R)->
	{_}=R,
	[];
encode_def(sc_twins_self, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?int16(V3)];
encode_def(cs_twins_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_twins_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_twins_attack, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_twins_attack, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(sc_twins_update, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(cs_twins_role_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_twins_role_dtl, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int16(V3)];
encode_def(cs_twins_mov_stop, R)->
	{_}=R,
	[];
encode_def(sc_twins_mov_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_twins_talk, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?string(V4)];
encode_def(cs_twins_talk, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_twins_get_talk, R)->
	{_}=R,
	[];
encode_def(sc_twins_get_talk, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_twins_get_rank, R)->
	{_}=R,
	[];
encode_def(sc_twins_get_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_twins_talk, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_twins_end_war, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_twins_open_reward_box, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_twins_open_reward_box, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_bounty_self_info, R)->
	{_}=R,
	[];
encode_def(p_bounty_unit, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int8(V3),?int8(V4),?int8(V5)];
encode_def(p_bounty_data, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(p_bounty_chapter_blood, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int64(V3),?int64(V4)];
encode_def(sc_bounty_self_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?list_tuple(V2),?list_int16(V3),?int32(V4),?list_tuple(V5)];
encode_def(cs_bounty_challenge, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_bounty_challenge, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?tuple(V4)];
encode_def(cs_bounty_buy_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_bounty_buy_times, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5)];
encode_def(cs_bounty_get_reward, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_bounty_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_trSpecial_info, R)->
	{_}=R,
	[];
encode_def(sc_trSpecial_info, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int32(V3),?tuple(V4)];
encode_def(cs_trSpecial_select, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_trSpecial_select, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_trSpecial_clear, R)->
	{_}=R,
	[];
encode_def(sc_trSpecial_clear, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_trSpecial_fightPower, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(cs_tvcard_info, R)->
	{_}=R,
	[];
encode_def(p_tvcard_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_tvcard_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_tvcard_select, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_tvcard_select, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?tuple(V4)];
encode_def(cs_tvcard_rand, R)->
	{_}=R,
	[];
encode_def(sc_tvcard_rand, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_magicBook_swallow_ger, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int64(V4),?int8(V5)];
encode_def(sc_magicBook_swallow_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_magicBook_summary, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int8(V3),?int8(V4)];
encode_def(cs_magicBook_summary, R)->
	{_}=R,
	[];
encode_def(sc_magicBook_summary, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_magicBook_picture_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_magicBook_picture_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_magicBook_book_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_magicBook_book_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_magicBook_attr, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23}=R,
	[?int32(V2),?int64(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?int32(V14),?int32(V15),?int32(V16),?int32(V17),?int16(V18),?int16(V19),?int32(V20),?int32(V21),?int32(V22),?int32(V23)];
encode_def(cs_magicBook_book_info, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_magicBook_book_info, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?string(V3)];
encode_def(sc_magicBook_book_detial, R)->
	{_,V2,V3,V4,V5}=R,
	[?tuple(V2),?string(V3),?int8(V4),?int8(V5)];
encode_def(cs_luckyRoll_get_list, R)->
	{_}=R,
	[];
encode_def(p_luckyRoll_card_inner, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_baseBoxInfo, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int32(V4),?tuple(V5)];
encode_def(p_lucky_role_card_p, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_luckyRoll_card_outer, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5)];
encode_def(sc_luckyRoll_get_list, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?tuple(V5),?int8(V6),?list_tuple(V7),?int32(V8),?int32(V9),?int16(V10),?int16(V11),?int32(V12),?int32(V13)];
encode_def(cs_luckyRoll_explore_one, R)->
	{_}=R,
	[];
encode_def(p_luckyRoll_card_oneTime, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?int8(V4)];
encode_def(sc_luckyRoll_explore_one, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_luckyRoll_refresh, R)->
	{_}=R,
	[];
encode_def(sc_luckyRoll_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_luckyRoll_open_base_box, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_luckyRoll_open_base_box, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_luckyRoll_ranker, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?int32(V4),?string(V5),?tuple(V6)];
encode_def(cs_luckyRoll_get_rankInfo, R)->
	{_}=R,
	[];
encode_def(sc_luckyRoll_get_rankInfo, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?tuple(V4),?list_tuple(V5)];
encode_def(cs_luckyRoll_get_rank_Reward, R)->
	{_}=R,
	[];
encode_def(sc_luckyRoll_get_rank_Reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?tuple(V4)];
encode_def(sc_luckyRoll_change_state, R)->
	{_}=R,
	[];
encode_def(cs_luckyRoll_explore_ten, R)->
	{_}=R,
	[];
encode_def(sc_luckyRoll_explore_ten, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_carlos_sign, R)->
	{_}=R,
	[];
encode_def(sc_carlos_sign, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_carlos_plane_uplevel, R)->
	{_}=R,
	[];
encode_def(sc_carlos_plane_uplevel, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_carlos_buy, R)->
	{_}=R,
	[];
encode_def(sc_carlos_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(cs_carlos_info, R)->
	{_}=R,
	[];
encode_def(sc_carlos_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5),?list_int32(V6),?int32(V7),?int8(V8),?int32(V9)];
encode_def(sc_carlos_times_update, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_carlos_pos, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(p_carlos_mine, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?tuple(V11)];
encode_def(p_carlos_player, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15}=R,
	[?int32(V2),?int32(V3),?tuple(V4),?tuple(V5),?int8(V6),?int8(V7),?int8(V8),?int8(V9),?int32(V10),?string(V11),?int32(V12),?int64(V13),?int16(V14),?int16(V15)];
encode_def(p_carlos_fairy, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5)];
encode_def(cs_carlos_war_base_info, R)->
	{_}=R,
	[];
encode_def(sc_carlos_war_base_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int32(V3),?tuple(V4),?tuple(V5),?list_tuple(V6),?list_tuple(V7)];
encode_def(sc_carlos_war_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_carlos_mine_detail, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_carlos_mine_detail, R)->
	{_,V2,V3,V4,V5}=R,
	[?tuple(V2),?int8(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_carlos_self, R)->
	{_}=R,
	[];
encode_def(sc_carlos_self, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_carlos_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_carlos_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_attack, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int8(V4)];
encode_def(sc_carlos_attack, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_carlos_ownMine, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_carlos_ownMine, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_carlos_update, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(cs_carlos_role_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_carlos_role_dtl, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_carlos_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_carlos_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_carlos_mov_stop, R)->
	{_}=R,
	[];
encode_def(sc_carlos_mov_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_talk, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_carlos_get_talk, R)->
	{_}=R,
	[];
encode_def(sc_carlos_get_talk, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_carlos_get_rank, R)->
	{_}=R,
	[];
encode_def(sc_carlos_get_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_carlos_talk, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_carlos_end_war, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_carlos_reborn, R)->
	{_}=R,
	[];
encode_def(sc_carlos_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_unrequest, R)->
	{_}=R,
	[];
encode_def(sc_carlos_unrequest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_rank_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(player_rank_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15}=R,
	[?int32(V2),?string(V3),?bool(V4),?int32(V5),?int8(V6),?int16(V7),?int64(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?int8(V14),?int8(V15)];
encode_def(sc_carlos_rank_list, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5),?list_tuple(V6),?tuple(V7)];
encode_def(cs_carlos_season_info, R)->
	{_}=R,
	[];
encode_def(sc_carlos_season_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_carlos_plane_select, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_carlos_plane_select, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_relic_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_carlos_relic_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_relic_info, R)->
	{_}=R,
	[];
encode_def(sc_carlos_relic_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int16(V2),?int32(V3),?int32(V4),?int8(V5),?int16(V6),?int8(V7),?list_string(V8),?list_int32(V9),?list_int8(V10),?int8(V11),?int32(V12)];
encode_def(cs_carlos_relic_war_base_info, R)->
	{_}=R,
	[];
encode_def(relic_role_other, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?int64(V4),?int16(V5),?bool(V6),?int8(V7),?int32(V8),?int64(V9)];
encode_def(relic_island, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?int8(V4),?int64(V5),?int64(V6),?tuple(V7)];
encode_def(sc_carlos_relic_war_base_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4),?list_tuple(V5),?list_tuple(V6),?int32(V7),?int16(V8),?int16(V9)];
encode_def(sc_carlos_relic_war_update, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?int16(V3),?int16(V4)];
encode_def(cs_carlos_relic_mov, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_carlos_relic_mov, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_relic_mov_stop, R)->
	{_}=R,
	[];
encode_def(sc_carlos_relic_mov_stop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_relic_attack, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_carlos_relic_attack, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?tuple(V4),?tuple(V5),?int8(V6)];
encode_def(cs_carlos_relic_active, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_carlos_relic_active, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_carlos_relic_open_reward_box, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_carlos_relic_open_reward_box, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_carlos_relic_buy, R)->
	{_}=R,
	[];
encode_def(sc_carlos_relic_buy, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int32(V5)];
encode_def(cs_carlos_relic_sign_cancel, R)->
	{_}=R,
	[];
encode_def(sc_carlos_relic_sign_cancel, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_carlos_relic_self, R)->
	{_}=R,
	[];
encode_def(sc_carlos_relic_self, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_carlos_relic_role_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_carlos_relic_role_dtl, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?int8(V3),?int16(V4)];
encode_def(sc_carlos_relic_times_update, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_carlos_relic_end_war, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?list_string(V4),?list_int32(V5),?list_tuple(V6)];
encode_def(cs_carlos_relic_island_detail, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_carlos_relic_island_detail, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?list_tuple(V3)];
encode_def(cs_carlos_relic_talk, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_carlos_relic_get_talk, R)->
	{_}=R,
	[];
encode_def(sc_carlos_relic_get_talk, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_carlos_relic_talk, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_carlos_relic_update, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?list_tuple(V2),?list_tuple(V3),?int32(V4),?int16(V5),?int16(V6)];
encode_def(cs_carlos_change_plane, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_carlos_change_plane, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_carlos_plane_dtl, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_carlos_plane_use_info, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int8(V3)];
encode_def(cs_monthVIP_info, R)->
	{_}=R,
	[];
encode_def(sc_monthVIP_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20}=R,
	[?int16(V2),?int16(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int8(V12),?int8(V13),?int16(V14),?int16(V15),?int16(V16),?tuple(V17),?tuple(V18),?int8(V19),?int8(V20)];
encode_def(sc_monthVip_success, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?tuple(V4)];
encode_def(cs_monthVIP_get_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_monthVIP_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_monthVIP_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_monthVIP_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?list_tuple(V4)];
encode_def(cs_monthVIP_get_growth_fund_info, R)->
	{_}=R,
	[];
encode_def(p_growth_fund_state, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?tuple(V4)];
encode_def(sc_monthVIP_get_growth_fund_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_monthVIP_buy_growth_fund, R)->
	{_}=R,
	[];
encode_def(sc_monthVIP_buy_growth_fund, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_monthVIP_get_growth_reward, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_monthVIP_get_growth_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_talent_get_info, R)->
	{_}=R,
	[];
encode_def(p_talent, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_talent_get_info, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?int16(V3),?int32(V4)];
encode_def(cs_talent_study, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_talent_study, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?int32(V4),?int16(V5)];
encode_def(cs_talent_undo, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_talent_undo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_talent_cooldown, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_talent_cooldown, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_trumpet_message, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?string(V4),?bool(V5)];
encode_def(sc_trumpet_message, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_trumpet_message_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int8(V2),?int16(V3),?string(V4),?int16(V5),?string(V6),?int8(V7),?int64(V8),?int32(V9),?int32(V10),?bool(V11),?int32(V12)];
encode_def(cs_trumpet_recent_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_trumpet_recent_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_trumpet_get_bonus, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(p_get_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int32(V3),?string(V4),?int32(V5)];
encode_def(p_bonus_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int16(V5),?int32(V6),?list_tuple(V7)];
encode_def(sc_trumpet_get_bonus_failed, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int8(V4),?tuple(V5)];
encode_def(sc_trumpet_get_bonus, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int32(V4),?tuple(V5)];
encode_def(cs_trumpet_redpacket_status, R)->
	{_}=R,
	[];
encode_def(sc_trumpet_redpacket_status, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5),?int32(V6)];
encode_def(sc_trumpet_new_redpacket_status, R)->
	{_,V2,V3}=R,
	[?string(V2),?int32(V3)];
encode_def(cs_trumpet_get_all_publishers, R)->
	{_}=R,
	[];
encode_def(redpacket_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int32(V4)];
encode_def(p_publisher_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?string(V3),?int32(V4),?int8(V5),?bool(V6),?int32(V7),?tuple(V8),?int32(V9),?list_tuple(V10),?int32(V11),?int32(V12)];
encode_def(sc_trumpet_get_all_publishers, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_trumpet_notice_publisher, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_trumpet_redpacket_openclose, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_trumpet_redpacket_openclose, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_trumpet_redpacket_get_reward, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_trumpet_redpacket_get_reward, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(cs_trumpet_get_world_publishers, R)->
	{_}=R,
	[];
encode_def(sc_trumpet_get_world_publishers, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_changename, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(sc_changename, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_changename_freetimes, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_changename_freetimes, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_int16(V4)];
encode_def(cs_familyBoss_base_info, R)->
	{_}=R,
	[];
encode_def(p_family_boss_base_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int16(V6),?int16(V7),?int32(V8),?int64(V9),?int64(V10),?int8(V11),?int8(V12),?int32(V13),?int32(V14)];
encode_def(sc_familyBoss_base_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familyBoss_attack, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familyBoss_attack, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familyBoss_hatch_egg, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familyBoss_hatch_egg, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familyBoss_feed_boss, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familyBoss_feed_boss, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familyBoss_set_boss_time, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_familyBoss_set_boss_time, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familyBoss_boss_be_boss, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familyBoss_boss_born, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(cs_familyBoss_get_rank, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_family_boss_ranker, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int8(V3),?int64(V4),?string(V5)];
encode_def(sc_familyBoss_get_rank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(sc_familyBoss_bc_attack, R)->
	{_,V2,V3,V4}=R,
	[?string(V2),?int64(V3),?int8(V4)];
encode_def(sc_familyBoss_boss_dead, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_familyBoss_bc_set_boss_time, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_familyBoss_boss_unlock, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familyTek_info, R)->
	{_}=R,
	[];
encode_def(p_ger_view2, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int16(V3),?int16(V4),?int16(V5)];
encode_def(p_reward_info2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?list_tuple(V7),?list_tuple(V8)];
encode_def(p_familyTekDtl, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int16(V3),?int8(V4),?tuple(V5),?int8(V6)];
encode_def(sc_familyTek_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_familyTek_upLevel, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_familyTek_upLevel, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int16(V4)];
encode_def(cs_familyTek_cost, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_familyTek_cost, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int16(V3),?tuple(V4)];
encode_def(cs_familyTek_wallet, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_familyTek_wallet, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(p_familyTek_Ger_attr, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19}=R,
	[?int32(V2),?int64(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?int32(V14),?int32(V15),?int32(V16),?int32(V17),?int32(V18),?int16(V19)];
encode_def(p_familyTek_Generate_buff, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12)];
encode_def(cs_familyTek_donate, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int32(V3),?int32(V4),?int32(V5),?int16(V6)];
encode_def(sc_familyTek_donate, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int32(V3),?int32(V4),?int32(V5),?int16(V6),?tuple(V7)];
encode_def(sc_family_levelup, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_familyfight_info, R)->
	{_}=R,
	[];
encode_def(p_familyfight_info_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int16(V12),?int16(V13),?int16(V14),?int16(V15),?int16(V16),?int32(V17),?int16(V18),?int16(V19),?int16(V20)];
encode_def(sc_familyfight_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familyfight_sign, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_familyfight_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_familyfighter_member_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int32(V2),?string(V3),?int8(V4),?int8(V5),?int8(V6),?int8(V7),?int8(V8),?int32(V9),?int8(V10),?bool(V11),?int16(V12),?int64(V13)];
encode_def(p_familyfighter_info_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?string(V4),?int32(V5),?int32(V6),?list_tuple(V7)];
encode_def(cs_familyfight_fighter_info, R)->
	{_}=R,
	[];
encode_def(sc_familyfight_fighter_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_familyfight_attack, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(p_familyfight_record_dtl, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int64(V2),?int8(V3),?tuple(V4),?string(V5),?string(V6)];
encode_def(sc_familyfight_attack, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_familyfight_result, R)->
	{_}=R,
	[];
encode_def(p_familyfight_result_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int32(V5),?int32(V6),?int32(V7),?string(V8)];
encode_def(sc_familyfight_result, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familyfight_get_fight_record_list, R)->
	{_}=R,
	[];
encode_def(p_familyfight_record_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?string(V4),?string(V5),?string(V6),?int64(V7),?int32(V8),?int32(V9)];
encode_def(sc_familyfight_get_fight_record_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_familyfight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_familyfight_replay, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_familyfight_get_fighter_history, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_familyfight_get_fighter_history, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_familyfight_update_star_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int8(V4),?int16(V5),?tuple(V6)];
encode_def(sc_familyfight_update_state_info, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familyfight_rankerList, R)->
	{_}=R,
	[];
encode_def(p_familyfight_ranker, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int16(V3),?int16(V4),?int32(V5),?string(V6),?string(V7),?int64(V8),?int16(V9)];
encode_def(sc_familyfight_rankerList, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_familyfight_instance_open_state, R)->
	{_}=R,
	[];
encode_def(family_instance_open_state, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_familyfight_instance_open_state, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?list_tuple(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int8(V8)];
encode_def(cs_familyfight_instance_boss_info, R)->
	{_}=R,
	[];
encode_def(instance_boss_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int64(V3),?int64(V4)];
encode_def(sc_familyfight_instance_boss_info, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_familyfight_attack_boss, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_familyfight_attack_boss, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int8(V4),?tuple(V5)];
encode_def(cs_familyfight_select_instance, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_familyfight_select_instance, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_familyfight_instance_reward_info, R)->
	{_}=R,
	[];
encode_def(instance_damage_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?string(V3),?int32(V4),?int64(V5)];
encode_def(instance_reward, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?int32(V3),?tuple(V4),?int8(V5)];
encode_def(sc_familyfight_instance_reward_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?tuple(V4),?list_tuple(V5)];
encode_def(cs_familyfight_instance_get_reward, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_familyfight_instance_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_familyfight_bug_attack_time, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_familyfight_bug_attack_time, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_familyfight_get_fighter, R)->
	{_}=R,
	[];
encode_def(sc_familyfight_get_fighter, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(cs_familyfight_select_fighter, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_familyfight_select_fighter, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_alien_info, R)->
	{_}=R,
	[];
encode_def(p_alien_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int16(V10),?int8(V11),?int32(V12),?int16(V13),?int8(V14)];
encode_def(sc_alien_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int8(V2),?int32(V3),?int8(V4),?bool(V5),?int8(V6),?list_tuple(V7),?int32(V8),?int16(V9),?int8(V10),?int8(V11),?int16(V12)];
encode_def(sc_alien_sign_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?bool(V4),?int32(V5)];
encode_def(cs_alien_first_five, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_alien_first_five, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_kill_num_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_fighter2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int16(V10),?int16(V11),?int8(V12)];
encode_def(sc_alien_kill_num_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_kill_continuous_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_fighter3, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int16(V10),?int16(V11),?bool(V12),?int8(V13)];
encode_def(sc_alien_kill_continuous_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_guess_info, R)->
	{_}=R,
	[];
encode_def(sc_alien_guess_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?bool(V3),?int32(V4),?int32(V5),?list_int32(V6)];
encode_def(cs_alien_guess, R)->
	{_,V2,V3}=R,
	[?int32(V2),?bool(V3)];
encode_def(sc_alien_guess, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_alien_reset, R)->
	{_}=R,
	[];
encode_def(sc_alien_reset, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_alien_fight, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_alien_fight, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4),?int32(V5),?list_tuple(V6),?int16(V7),?int16(V8)];
encode_def(cs_alien_sign, R)->
	{_}=R,
	[];
encode_def(sc_alien_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_alien_self_record, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_self_record, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?bool(V2),?bool(V3),?string(V4),?int16(V5),?int64(V6),?int32(V7)];
encode_def(sc_alien_self_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_record, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_id_num, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(p_mail_reward, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(p_alien_record3, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int8(V2),?bool(V3),?string(V4),?string(V5),?int16(V6),?int16(V7),?int64(V8),?int32(V9),?tuple(V10)];
encode_def(sc_alien_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_alien_record, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?string(V3),?string(V4),?int16(V5),?int16(V6),?int64(V7),?int32(V8)];
encode_def(sc_alien_update_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_alien_self_fight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_alien_self_fight_replay, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_alien_fight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_alien_fight_repaly, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_alien_new_fighter_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_leave, R)->
	{_}=R,
	[];
encode_def(sc_alien_new_self_record, R)->
	{_}=R,
	[];
encode_def(cs_alien_view_other, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_alien_view_other, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6),?int32(V7),?string(V8),?int16(V9),?int64(V10),?list_tuple(V11)];
encode_def(cs_alien_view_other_dtl, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_alien_buy_times, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_alien_buy_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_alien_active, R)->
	{_}=R,
	[];
encode_def(sc_alien_active, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_alien_record2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?string(V3),?string(V4),?int16(V5),?int16(V6),?int64(V7),?int32(V8),?tuple(V9)];
encode_def(p_alien_finals_record, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?int32(V3),?string(V4),?int32(V5),?bool(V6),?int64(V7)];
encode_def(p_alien_finals_round_record, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_alien_finals_records, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_alien_finals_records, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(p_alien_finals_role_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?string(V3),?int64(V4),?int16(V5),?bool(V6),?int8(V7),?int32(V8),?int8(V9)];
encode_def(cs_alien_finals_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_alien_finals_list, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_alien_finals_guess, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_alien_finals_guess, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_alien_finals_fight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_alien_finals_fight_replay, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_alien_finals_info, R)->
	{_}=R,
	[];
encode_def(sc_alien_finals_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(cs_alien_finals_stake_list, R)->
	{_}=R,
	[];
encode_def(sc_alien_finals_stake_list, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(cs_alien_finals_self_info, R)->
	{_}=R,
	[];
encode_def(sc_alien_finals_self_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int8(V4)];
encode_def(cs_alien_self_rank, R)->
	{_}=R,
	[];
encode_def(sc_alien_self_rank, R)->
	{_,V2,V3,V4}=R,
	[?bool(V2),?int32(V3),?int32(V4)];
encode_def(cs_team_pk_info, R)->
	{_}=R,
	[];
encode_def(p_team_member, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int8(V9)];
encode_def(sc_team_pk_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int64(V2),?int32(V3),?int32(V4),?int8(V5),?int8(V6),?list_tuple(V7),?list_tuple(V8),?int32(V9)];
encode_def(sc_team_pk_close, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int64(V2),?int32(V3),?int32(V4),?int32(V5),?list_tuple(V6)];
encode_def(cs_team_refresh, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_team_refresh, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_team_fight, R)->
	{_}=R,
	[];
encode_def(p_team_member2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int8(V9),?bool(V10)];
encode_def(sc_team_fight_result, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?bool(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int8(V8),?int8(V9),?list_tuple(V10),?list_tuple(V11),?list_tuple(V12),?list_tuple(V13),?list_tuple(V14)];
encode_def(cs_team_rank, R)->
	{_}=R,
	[];
encode_def(p_team_member3, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int32(V10),?int8(V11)];
encode_def(sc_team_rank, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_team_record, R)->
	{_}=R,
	[];
encode_def(p_team_record, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?bool(V2),?int32(V3),?string(V4),?string(V5),?list_int64(V6)];
encode_def(sc_team_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_team_self_record, R)->
	{_}=R,
	[];
encode_def(p_team_self_record, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?bool(V3),?int32(V4),?int32(V5),?int32(V6),?list_string(V7),?list_string(V8),?list_int64(V9)];
encode_def(sc_team_self_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_team_move, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_team_move, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_team_pk_not_open, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_team_fight_error, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_team_fight_replay, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_team_fight_replay, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_team_self_fight_replay, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_team_self_fight_replay, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_team_view_other, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_team_view_other, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6)];
encode_def(cs_team_view_other_dtl, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_lieu_view, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int16(V5)];
encode_def(p_ger_pos, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_team_view_other_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?string(V3),?bool(V4),?int16(V5),?int64(V6),?list_tuple(V7),?list_tuple(V8),?list_tuple(V9),?int16(V10),?int16(V11),?list_tuple(V12),?int32(V13),?int8(V14)];
encode_def(cs_team_new_status, R)->
	{_}=R,
	[];
encode_def(sc_team_new_status, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(p_race_rec, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17}=R,
	[?string(V2),?string(V3),?list_int64(V4),?int32(V5),?int32(V6),?int64(V7),?int64(V8),?int8(V9),?int8(V10),?bool(V11),?bool(V12),?int8(V13),?int8(V14),?int32(V15),?int32(V16),?list_bool(V17)];
encode_def(sc_race_new_fight, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(p_race_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?string(V3),?int64(V4),?int16(V5),?bool(V6),?int8(V7),?int32(V8)];
encode_def(cs_race_history, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5)];
encode_def(sc_race_history, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_race_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_race_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_race_fight_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_race_fight_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_race_sign, R)->
	{_}=R,
	[];
encode_def(sc_race_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race_info, R)->
	{_}=R,
	[];
encode_def(p_race_pos, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?string(V3),?bool(V4),?int8(V5),?int32(V6),?int8(V7)];
encode_def(sc_race_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int8(V2),?int32(V3),?int16(V4),?bool(V5),?list_tuple(V6),?string(V7),?int32(V8),?int8(V9),?bool(V10)];
encode_def(cs_race_enter, R)->
	{_}=R,
	[];
encode_def(cs_race_leave, R)->
	{_}=R,
	[];
encode_def(cs_race_pos_history, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_race_pos_history, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_race_new_first, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_race_new_status, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_race_is_open, R)->
	{_}=R,
	[];
encode_def(sc_race_is_open, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(cs_race_auto_sign, R)->
	{_}=R,
	[];
encode_def(sc_race_auto_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race_auto_unsign, R)->
	{_}=R,
	[];
encode_def(sc_race_auto_unsign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race_self_history, R)->
	{_}=R,
	[];
encode_def(sc_race_self_history, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_race_guess_info, R)->
	{_}=R,
	[];
encode_def(sc_race_guess_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?list_int32(V4)];
encode_def(cs_race_guess, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_race_guess, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race2_info, R)->
	{_}=R,
	[];
encode_def(arena_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?string(V3),?bool(V4),?int8(V5),?int32(V6),?int64(V7),?int8(V8),?int32(V9),?int32(V10),?int16(V11),?int32(V12)];
encode_def(sc_race2_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?list_int32(V3),?int8(V4),?int8(V5),?list_tuple(V6),?int32(V7)];
encode_def(cs_race2_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_race2_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race2_openclose, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_race2_openclose, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race2_arena_fighter, R)->
	{_}=R,
	[];
encode_def(sc_race2_arena_fighter, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_race2_fight, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_race2_fight, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_race2_knockout_fighter, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(race2_fight_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?list_tuple(V2),?int32(V3),?int8(V4),?int8(V5),?int32(V6),?int32(V7),?int32(V8),?int8(V9),?int8(V10)];
encode_def(sc_race2_knockout_fighter, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_race2_get_gamble, R)->
	{_}=R,
	[];
encode_def(gamble_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int8(V6)];
encode_def(sc_race2_get_gamble, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_int32(V3)];
encode_def(cs_race2_do_gamble, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_race2_do_gamble, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race2_final_info, R)->
	{_}=R,
	[];
encode_def(sc_race2_final_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_race2_get_fight_rec, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_race2_get_fight_rec, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_race2_cancel_sign, R)->
	{_}=R,
	[];
encode_def(sc_race2_cancel_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_family_member_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17}=R,
	[?int32(V2),?string(V3),?int32(V4),?int32(V5),?int32(V6),?int8(V7),?bool(V8),?bool(V9),?int16(V10),?int64(V11),?int8(V12),?int32(V13),?int32(V14),?int8(V15),?int32(V16),?int32(V17)];
encode_def(p_family_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20}=R,
	[?int32(V2),?string(V3),?int16(V4),?int32(V5),?string(V6),?int32(V7),?string(V8),?int16(V9),?int32(V10),?string(V11),?list_tuple(V12),?int32(V13),?int32(V14),?int32(V15),?int32(V16),?string(V17),?int64(V18),?int32(V19),?string(V20)];
encode_def(p_family_request, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?int32(V6),?int32(V7),?bool(V8),?int32(V9),?int32(V10),?int8(V11),?bool(V12)];
encode_def(p_family_summary, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16}=R,
	[?int32(V2),?string(V3),?string(V4),?int16(V5),?int16(V6),?int32(V7),?string(V8),?bool(V9),?int32(V10),?int32(V11),?string(V12),?int64(V13),?int32(V14),?int32(V15),?int16(V16)];
encode_def(cs_family_get_list, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_family_get_list, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_family_create, R)->
	{_,V2,V3}=R,
	[?string(V2),?bool(V3)];
encode_def(sc_family_create, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?int32(V4)];
encode_def(cs_family_request_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_request_join, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?bool(V3),?int32(V4),?int32(V5)];
encode_def(cs_family_cancel_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_cancel_join, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?int32(V4)];
encode_def(cs_family_agree_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_agree_join, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?tuple(V4)];
encode_def(cs_family_refuse_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_refuse_join, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_family_get_info, R)->
	{_}=R,
	[];
encode_def(sc_family_get_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?int32(V4)];
encode_def(cs_family_kick, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_kick, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?tuple(V4)];
encode_def(cs_family_create_consume, R)->
	{_}=R,
	[];
encode_def(sc_family_create_consume, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(cs_family_leave, R)->
	{_}=R,
	[];
encode_def(sc_family_leave, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?tuple(V4)];
encode_def(cs_family_change_notice, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_family_change_notice, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?string(V4)];
encode_def(cs_family_request_list, R)->
	{_}=R,
	[];
encode_def(sc_family_request_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_family_del_request, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_family_get_log_list, R)->
	{_}=R,
	[];
encode_def(p_family_log_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?string(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?string(V8),?int8(V9),?bool(V10)];
encode_def(sc_family_get_log_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_family_get_contribute_info, R)->
	{_}=R,
	[];
encode_def(p_familyContributeLog, R)->
	{_,V2,V3}=R,
	[?string(V2),?int16(V3)];
encode_def(p_familyContributeType, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int16(V2),?int8(V3),?int32(V4),?int32(V5),?int32(V6),?tuple(V7)];
encode_def(sc_family_get_contribute_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_family_do_contribute, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_family_do_contribute, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_family_update_exp, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(cs_family_search_by_family_name, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_family_search_by_family_name, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_family_change_member_power, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_family_change_member_power, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_family_update_family_info, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_family_send_role_energy, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_send_role_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_family_get_role_energy, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_family_get_role_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_family_get_role_send_energy_list, R)->
	{_}=R,
	[];
encode_def(sc_family_get_role_send_energy_list, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(cs_family_get_member_power, R)->
	{_}=R,
	[];
encode_def(sc_family_get_member_power, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_family_get_send_role_energy_list, R)->
	{_}=R,
	[];
encode_def(sc_family_get_send_role_energy_list, R)->
	{_,V2}=R,
	[?list_string(V2)];
encode_def(sc_family_update_contribute_log, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_family_storage, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int16(V3),?int8(V4),?list_int32(V5)];
encode_def(cs_family_storage_info, R)->
	{_}=R,
	[];
encode_def(sc_family_storage_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int32(V5),?list_tuple(V6)];
encode_def(cs_family_storage_req, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_family_storage_req, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_family_storage_assign, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int32(V3)];
encode_def(sc_family_storage_assign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_family_storage_update, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?list_int32(V4)];
encode_def(cs_family_owner_impeach, R)->
	{_}=R,
	[];
encode_def(sc_family_owner_impeach, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_family_wallet, R)->
	{_}=R,
	[];
encode_def(sc_family_wallet, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(cs_family_change_slogan, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_family_change_slogan, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?string(V4)];
encode_def(cs_family_worship, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_family_worship, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(cs_family_worship_fight, R)->
	{_}=R,
	[];
encode_def(sc_family_worship_fight, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_family_worship_info, R)->
	{_}=R,
	[];
encode_def(sc_family_worship_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_family_worship_self_push, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_family_worship_refresh_fight_reward, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_worship_family_push, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_family_worship_limit, R)->
	{_}=R,
	[];
encode_def(sc_family_worship_limit, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(cs_family_get_member_list, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_get_member_list, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_family_get_donate_contribute_list, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_ger_donate_record_unit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_item_donate_record_unit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_donate_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int64(V2),?int64(V3),?int64(V4),?list_tuple(V5),?list_tuple(V6)];
encode_def(sc_family_get_donate_contribute_list, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?tuple(V4)];
encode_def(cs_family_impeach_list, R)->
	{_}=R,
	[];
encode_def(sc_family_impeach_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int32(V3)];
encode_def(cs_family_donate_contribution_summary, R)->
	{_}=R,
	[];
encode_def(p_family_memeber_donate_info_summary, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int8(V3),?string(V4),?bool(V5),?int16(V6),?int64(V7),?int32(V8),?int8(V9),?int8(V10)];
encode_def(sc_family_donate_contribution_summary, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_family_invite_request, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_invite_request, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_combine_do, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int8(V4),?list_int64(V5)];
encode_def(sc_combine_fail, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_newGer, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int8(V4)];
encode_def(sc_combine_ger, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_int32(V3)];
encode_def(p_newEquip, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int8(V5)];
encode_def(sc_combine_equip, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_int32(V3)];
encode_def(cs_combine_info, R)->
	{_}=R,
	[];
encode_def(sc_combine_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?string(V3),?list_int8(V4),?list_int8(V5)];
encode_def(cs_combine_mage_info, R)->
	{_}=R,
	[];
encode_def(p_mage_config, R)->
	{_,V2,V3}=R,
	[?int16(V2),?tuple(V3)];
encode_def(sc_combine_mage_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_combine_do_mage, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int64(V3)];
encode_def(sc_combine_do_mage, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int64(V4),?tuple(V5)];
encode_def(cs_firecracker_open, R)->
	{_}=R,
	[];
encode_def(p_discount, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_firecracker_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18}=R,
	[?int8(V2),?string(V3),?string(V4),?string(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int8(V10),?int8(V11),?int32(V12),?int8(V13),?int8(V14),?int32(V15),?list_tuple(V16),?int16(V17),?int16(V18)];
encode_def(sc_firecracker_info_sync, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int32(V4)];
encode_def(cs_firecracker_close, R)->
	{_}=R,
	[];
encode_def(cs_firecracker_setoff, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_firecracker_setoff, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int32(V3),?int32(V4),?int8(V5),?list_tuple(V6)];
encode_def(cs_firecracker_rank, R)->
	{_}=R,
	[];
encode_def(p_firecracker_rank, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?string(V3),?int32(V4),?list_tuple(V5),?int16(V6)];
encode_def(sc_firecracker_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_firecracker_get_reward, R)->
	{_}=R,
	[];
encode_def(sc_firecracker_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_firecracker_rewardList, R)->
	{_}=R,
	[];
encode_def(sc_firecracker_rewardList, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_treaHouse_get_list, R)->
	{_}=R,
	[];
encode_def(p_treaHouse_card, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int32(V6)];
encode_def(p_baseBoxOpenInfo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_treaHouse_get_list, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5),?int32(V6),?list_tuple(V7),?int32(V8),?int32(V9),?int32(V10),?int8(V11)];
encode_def(cs_treaHouse_is_open, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_is_open, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(cs_treaHouse_explore_one, R)->
	{_}=R,
	[];
encode_def(p_treaHouse_card_oneTime, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(sc_treaHouse_explore_one, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_treaHouse_explore_ten, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_explore_ten, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int32(V3),?int8(V4),?int8(V5),?list_tuple(V6)];
encode_def(cs_treaHouse_refresh, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_treaHouse_open_base_box, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_treaHouse_open_base_box, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_treaHouse_ranker, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?int32(V4),?string(V5),?tuple(V6)];
encode_def(cs_treaHouse_get_rankInfo, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_get_rankInfo, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?tuple(V4),?list_tuple(V5)];
encode_def(cs_treaHouse_get_rank_Reward, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_get_rank_Reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?tuple(V4)];
encode_def(cs_treaHouse_get_baseBoxRewardInfo, R)->
	{_}=R,
	[];
encode_def(p_treaHouse_BaseReward_Info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?tuple(V4)];
encode_def(sc_treaHouse_get_baseBoxRewardInfo, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_treaHouse_change_state, R)->
	{_}=R,
	[];
encode_def(p_cross_rec, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16}=R,
	[?string(V2),?string(V3),?list_int64(V4),?int32(V5),?int32(V6),?int64(V7),?int64(V8),?int8(V9),?bool(V10),?bool(V11),?int8(V12),?int8(V13),?int16(V14),?int16(V15),?list_bool(V16)];
encode_def(sc_cross_new_fight, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_cross_history, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_cross_history, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_cross_replay, R)->
	{_,V2,V3}=R,
	[?int64(V2),?bool(V3)];
encode_def(sc_cross_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_cross_fight_list, R)->
	{_,V2,V3}=R,
	[?list_string(V2),?list_string(V3)];
encode_def(cs_cross_sign, R)->
	{_}=R,
	[];
encode_def(sc_cross_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_cross_info, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_cross_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int8(V2),?int32(V3),?int16(V4),?bool(V5),?list_tuple(V6),?string(V7),?int32(V8),?int32(V9),?int8(V10),?bool(V11),?bool(V12),?list_string(V13),?list_string(V14)];
encode_def(cs_cross_enter, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_cross_leave, R)->
	{_}=R,
	[];
encode_def(cs_cross_view_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_view_data, R)->
	{_,V2,V3}=R,
	[?string(V2),?int16(V3)];
encode_def(sc_cross_view_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_cross_support, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_cross_support, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_cross_price_list, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int8(V4)];
encode_def(p_price_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?string(V3),?bool(V4),?int16(V5),?int64(V6),?int64(V7),?int8(V8),?int16(V9)];
encode_def(sc_cross_price_list, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int32(V3)];
encode_def(cs_emperor_get_open_time, R)->
	{_}=R,
	[];
encode_def(sc_emperor_get_open_time, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(cs_emperor_enter, R)->
	{_}=R,
	[];
encode_def(p_emp_fighter, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int8(V4),?int8(V5),?int8(V6)];
encode_def(sc_emperor_enter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int16(V6),?int32(V7),?int32(V8),?int32(V9),?list_tuple(V10),?int64(V11)];
encode_def(sc_emperor_broadcast_fightInfo, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?string(V3),?int8(V4),?int8(V5),?int8(V6),?int64(V7)];
encode_def(cs_emperor_replay, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_emperor_replayInfo, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?string(V3),?int8(V4),?int64(V5)];
encode_def(sc_emperor_replay, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_emperor_quit, R)->
	{_}=R,
	[];
encode_def(cs_emperor_get_bet_info, R)->
	{_}=R,
	[];
encode_def(p_bet, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?string(V2),?string(V3),?string(V4),?int32(V5),?int8(V6)];
encode_def(sc_emperor_get_bet_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_emperor_role_bet, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_emperor_role_bet, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_emperor_bet_info, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_emperor_bet_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int8(V5),?int32(V6),?int8(V7),?int32(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12)];
encode_def(cs_emperor_get_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_emperor_get_replay, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_emperor_bc_fight_end, R)->
	{_}=R,
	[];
encode_def(cs_challengeGod_info, R)->
	{_}=R,
	[];
encode_def(sc_challengeGod_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int8(V5)];
encode_def(cs_challengeGod_select_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_challengeGod_select_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_challengeGod_challenge_dungeon_one, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_ger_add_exp, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(p_reward, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4),?int32(V5),?list_tuple(V6),?list_tuple(V7),?int32(V8)];
encode_def(sc_challengeGod_challenge_dungeon_one, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_challengeGod_challenge_dungeon_ten, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_challengeGod_result, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_challengeGod_challenge_ten, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_talk_world, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(sc_talk_world, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_talk_world_message, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int8(V2),?string(V3),?string(V4),?int8(V5),?int64(V6),?int32(V7),?int8(V8),?string(V9),?int32(V10),?bool(V11),?int8(V12),?int8(V13),?int32(V14)];
encode_def(cs_talk_gag_one, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_talk_ungag_one, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_talk_get_gag_list, R)->
	{_}=R,
	[];
encode_def(gag_info, R)->
	{_,V2,V3}=R,
	[?string(V2),?list_int32(V3)];
encode_def(sc_talk_get_gag_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_talk_recent_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_talk_recent_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_talk_send_whisper, R)->
	{_,V2,V3}=R,
	[?int32(V2),?string(V3)];
encode_def(sc_talk_send_whisper, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_talk_get_whisper, R)->
	{_}=R,
	[];
encode_def(p_whisper_record, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?int64(V4)];
encode_def(sc_talk_get_whisper, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_talk_whisper_notice, R)->
	{_}=R,
	[];
encode_def(sc_push_highlight_Info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_nanm_open, R)->
	{_}=R,
	[];
encode_def(sc_nanm_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int8(V2),?int32(V3),?int64(V4),?int16(V5),?bool(V6),?int16(V7),?bool(V8),?int32(V9),?int8(V10)];
encode_def(sc_nanm_init_state, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int64(V3),?int32(V4),?int32(V5)];
encode_def(cs_nanm_close, R)->
	{_}=R,
	[];
encode_def(cs_nanm_buff, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_nanm_buff, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_nanm_last_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_nanm_last_info_ignore, R)->
	{_}=R,
	[];
encode_def(p_nanm_info, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_nanm_last_info_win, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int16(V2),?int32(V3),?int64(V4),?list_tuple(V5),?list_string(V6),?int32(V7)];
encode_def(sc_nanm_last_info_fail, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int32(V3),?int32(V4)];
encode_def(cs_nanm_cur_info, R)->
	{_}=R,
	[];
encode_def(sc_nanm_cur_info_ignore, R)->
	{_}=R,
	[];
encode_def(sc_nanm_cur_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_nanm_hp_sync, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(p_nanm_harm, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_nanm_harm_broadcast, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_nanm_buff_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_role_stastic, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int32(V3),?int32(V4)];
encode_def(sc_nanm_stop, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_nanm_rank_sync, R)->
	{_}=R,
	[];
encode_def(sc_nanm_rank_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_nanm_fight, R)->
	{_}=R,
	[];
encode_def(sc_nanm_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_nanm_reborn, R)->
	{_}=R,
	[];
encode_def(sc_nanm_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_nanm_offline_play, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(sc_nanm_offline_play, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_nanm_open_time, R)->
	{_}=R,
	[];
encode_def(sc_nanm_open_time, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_nanm_reward, R)->
	{_}=R,
	[];
encode_def(sc_nanm_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_version, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_version, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_gift_request, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_gift_request, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_king_enter, R)->
	{_}=R,
	[];
encode_def(sc_king_enter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?int8(V3),?int8(V4),?int16(V5),?int32(V6),?string(V7),?int16(V8),?int8(V9),?bool(V10),?int16(V11)];
encode_def(sc_king_enter_wait_first_session, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_king_quit, R)->
	{_}=R,
	[];
encode_def(cs_king_sign, R)->
	{_}=R,
	[];
encode_def(sc_king_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_king_buff, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_king_buff, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(p_rec, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?string(V3),?int8(V4),?int64(V5)];
encode_def(sc_king_new_fight, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_king_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_king_replay, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_king_history, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int8(V5)];
encode_def(sc_king_history, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_king_rank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int8(V4)];
encode_def(p_king_rank, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?list_int16(V3),?string(V4)];
encode_def(sc_king_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_box_item, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_box_item, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_box_shop, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_box_shop, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_reward_view2, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(cs_box_shop_info, R)->
	{_}=R,
	[];
encode_def(p_shop_box_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int16(V2),?int32(V3),?int32(V4),?int8(V5),?int32(V6),?int32(V7)];
encode_def(sc_box_shop_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_reward_view3, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int32(V4)];
encode_def(cs_box_get_spirit_equip_count, R)->
	{_}=R,
	[];
encode_def(sc_box_get_spirit_equip_count, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10)];
encode_def(cs_box_item_multi, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_box_item_multi, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_box_free_info, R)->
	{_}=R,
	[];
encode_def(sc_box_free_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int8(V6),?int8(V7),?int8(V8),?int8(V9)];
encode_def(cs_box_free_open, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_box_free_open, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_box_mystery_notice, R)->
	{_,V2,V3,V4}=R,
	[?string(V2),?string(V3),?list_tuple(V4)];
encode_def(cs_box_shopBoxDtl, R)->
	{_}=R,
	[];
encode_def(sc_box_shopBoxDtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int32(V10)];
encode_def(cs_box_shop_view, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_box_shop_view, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_box_shop_refresh, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_box_shop_refresh, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_box_shop_get, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_box_shop_get, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_box_free_get, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_box_free_get, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_box_ticket_shop, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_box_ticket_shop, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_box_ticket_info, R)->
	{_}=R,
	[];
encode_def(sc_box_ticket_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(cs_activity_get_list, R)->
	{_}=R,
	[];
encode_def(p_activity_icon, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?string(V3),?string(V4),?int16(V5)];
encode_def(sc_activity_get_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_activity_info, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_activity_draw, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int16(V2),?string(V3),?int16(V4),?int16(V5),?int16(V6),?tuple(V7),?list_tuple(V8),?int8(V9),?int8(V10)];
encode_def(sc_activity_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int16(V2),?int8(V3),?string(V4),?list_tuple(V5),?int32(V6),?int32(V7),?list_int32(V8),?int8(V9),?int8(V10)];
encode_def(cs_activity_draw, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?tuple(V4)];
encode_def(sc_activity_draw, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int16(V5),?int16(V6)];
encode_def(sc_activity_update, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int16(V4)];
encode_def(sc_activity_record_update, R)->
	{_,V2,V3}=R,
	[?int16(V2),?list_int32(V3)];
encode_def(p_energy_activity, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int8(V4),?int8(V5)];
encode_def(cs_activity_energy, R)->
	{_}=R,
	[];
encode_def(sc_activity_energy, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_activity_sign_emperor_info, R)->
	{_}=R,
	[];
encode_def(sc_activity_sign_emperor_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?string(V6)];
encode_def(cs_activity_sign_get_reward, R)->
	{_}=R,
	[];
encode_def(sc_activity_sign_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_activity_sign_up, R)->
	{_}=R,
	[];
encode_def(sc_activity_sign_up, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_activity_rebate_info, R)->
	{_}=R,
	[];
encode_def(p_rebate_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(p_rebate_list, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?string(V3),?int8(V4),?int32(V5),?int32(V6),?int32(V7),?list_tuple(V8)];
encode_def(sc_rebate_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?string(V3),?string(V4),?string(V5),?int32(V6),?int32(V7),?list_tuple(V8)];
encode_def(cs_activity_rebate_get_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_rebate_reward, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_rebate_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_rebate_update, R)->
	{_}=R,
	[];
encode_def(cs_activity_levelRank_open, R)->
	{_}=R,
	[];
encode_def(levelRank_rankerInfo, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int64(V2),?int16(V3),?int8(V4),?int32(V5),?string(V6),?tuple(V7),?bool(V8),?int8(V9),?int32(V10)];
encode_def(sc_activity_levelRank_open, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4),?int32(V5),?int32(V6)];
encode_def(cs_activity_levelRank_refresh, R)->
	{_}=R,
	[];
encode_def(sc_activity_levelRank_refresh, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_activity_get_payExt_info, R)->
	{_}=R,
	[];
encode_def(cs_activity_vip_shop, R)->
	{_}=R,
	[];
encode_def(p_activity_vip_shop, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8),?tuple(V9),?int16(V10),?string(V11)];
encode_def(sc_activity_vip_shop, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_activity_vip_shop_buy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_activity_vip_shop_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_activity_pay_reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?string(V4)];
encode_def(cs_activity_first_pay, R)->
	{_}=R,
	[];
encode_def(sc_activity_first_pay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_activity_firstPay_update, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_activity_consume_reback, R)->
	{_}=R,
	[];
encode_def(p_consume_unit, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int64(V3),?int64(V4),?int32(V5),?string(V6)];
encode_def(sc_activity_consume_reback, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4),?int64(V5),?int64(V6),?int8(V7),?int8(V8),?int32(V9),?int32(V10),?int8(V11)];
encode_def(cs_activity_consume_state, R)->
	{_}=R,
	[];
encode_def(sc_activity_consume_state, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_activity_energy_pac_info, R)->
	{_}=R,
	[];
encode_def(sc_activity_energy_pac_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(cs_activity_energy_pac_use, R)->
	{_}=R,
	[];
encode_def(sc_activity_energy_pac_use, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_invite_info, R)->
	{_}=R,
	[];
encode_def(sc_invite_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?bool(V2),?bool(V3),?int16(V4),?string(V5),?int16(V6)];
encode_def(cs_invite_bind_weibo, R)->
	{_}=R,
	[];
encode_def(sc_invite_bind_weibo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_invite_weibo_share_levelup, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_invite_weibo_share_levelup, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_invite_input_invite_code, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_invite_input_invite_code, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(cs_invite_list, R)->
	{_}=R,
	[];
encode_def(p_invite, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?bool(V3),?int16(V4),?int8(V5),?string(V6),?bool(V7)];
encode_def(sc_invite_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_get_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_friend, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21}=R,
	[?int32(V2),?bool(V3),?int16(V4),?int8(V5),?string(V6),?int64(V7),?int32(V8),?string(V9),?int32(V10),?int32(V11),?int32(V12),?int16(V13),?int8(V14),?int8(V15),?int32(V16),?int32(V17),?int32(V18),?int32(V19),?int32(V20),?int16(V21)];
encode_def(sc_friend_get_list, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int8(V4),?int8(V5)];
encode_def(cs_friend_more, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_more, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_get_add_list, R)->
	{_}=R,
	[];
encode_def(p_stranger, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?bool(V3),?int16(V4),?int8(V5),?string(V6),?int64(V7),?int32(V8),?string(V9),?int32(V10),?int8(V11)];
encode_def(sc_friend_get_add_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_add, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_friend_add, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_friend_explore, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_friend_explore, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_delete, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_friend_delete, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_friend_notify_delete, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_friend_new, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_friend_send_enargy, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_friend_send_enargy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_friend_send_enargy_me, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_friend_give_enargy, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_give_enargy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_int32(V3),?int8(V4)];
encode_def(sc_frend_give_enargy_me, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(cs_friend_give_all_enargy, R)->
	{_}=R,
	[];
encode_def(sc_friend_remove_request, R)->
	{_}=R,
	[];
encode_def(cs_gather_get_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_gather_get_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int32(V3)];
encode_def(sc_gather_new, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int32(V3)];
encode_def(cs_gather_manual_info_for_tag, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_ger_manual_unit, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int8(V4)];
encode_def(p_collect_unit, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int8(V4),?int8(V5),?int8(V6)];
encode_def(sc_gather_manual_info_for_tag, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(sc_gather_manual_update, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(cs_gather_manual_info, R)->
	{_}=R,
	[];
encode_def(sc_gather_manual_info, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_int8(V3)];
encode_def(cs_gather_manual_collect_draw, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_gather_manual_collect_draw, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_hist_get_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_hist, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int64(V2),?int8(V3),?string(V4),?int32(V5),?int32(V6),?int16(V7)];
encode_def(sc_hist_get_list, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?bool(V3),?list_tuple(V4),?int16(V5)];
encode_def(cs_hist_more, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_hist_more, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_hist_replay, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_hist_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_hist_unreadNum, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_mail_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_mail, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int64(V2),?int8(V3),?int32(V4),?string(V5),?string(V6),?int32(V7),?int16(V8),?list_any(V9),?list_tuple(V10),?int32(V11),?bool(V12)];
encode_def(sc_mail_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?bool(V4),?list_tuple(V5),?list_int8(V6)];
encode_def(cs_mail_draw_reward, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_mail_draw_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_delete, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_mail_delete, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_new, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?string(V4)];
encode_def(sc_mail_new, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_unread_num, R)->
	{_}=R,
	[];
encode_def(sc_mail_unread_num, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_mail_more, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_mail_more, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4),?list_int8(V5)];
encode_def(cs_mail_agree_friend, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_mail_agree_friend, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_mail_del_spec_mail, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_mail_del_spec_mail, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(cs_mail_invite_operate, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_mail_invite_operate, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_draw_reward_all, R)->
	{_}=R,
	[];
encode_def(sc_mail_draw_reward_all, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int8(V3)];
encode_def(cs_mail_canvass_info, R)->
	{_}=R,
	[];
encode_def(sc_mail_canvass_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_mail_get_question, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_mail_get_question, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?list_string(V4)];
encode_def(cs_mail_do_select, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_mail_do_select, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_hron_info, R)->
	{_}=R,
	[];
encode_def(sc_hron_info_wait, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?int8(V4),?int8(V5),?int32(V6)];
encode_def(sc_hron_info_stop, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_hron_info_on, R)->
	{_}=R,
	[];
encode_def(sc_hron_info_on, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int32(V5),?int16(V6),?int16(V7),?int16(V8),?int16(V9),?int8(V10),?int8(V11),?int16(V12),?int32(V13)];
encode_def(sc_hron_info_on_fail, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_hron_last_rank_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_hron_role, R)->
	{_,V2,V3,V4}=R,
	[?string(V2),?int16(V3),?int32(V4)];
encode_def(sc_hron_last_rank_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_hron_cur_rank_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_hron_cur_rank_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_hron_buy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_hron_buy, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int16(V5),?int16(V6)];
encode_def(cs_hron_fight, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_hron_fight, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(sc_hron_stop, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(cs_hron_rank, R)->
	{_}=R,
	[];
encode_def(sc_hron_rank, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(cs_hron_open_time, R)->
	{_}=R,
	[];
encode_def(sc_hron_open_time, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int8(V4)];
encode_def(cs_hron_succ_reward, R)->
	{_}=R,
	[];
encode_def(sc_hron_succ_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(p_hron_succ_reward, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int32(V3),?int16(V4),?bool(V5)];
encode_def(cs_hron_select, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_hron_select, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_hron_pass, R)->
	{_}=R,
	[];
encode_def(sc_hron_pass, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_hron_reward_view, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_hron_reward_view, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_hron_raids, R)->
	{_}=R,
	[];
encode_def(sc_hron_raids, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?list_tuple(V5)];
encode_def(sc_hron_update_history, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_hula_open, R)->
	{_}=R,
	[];
encode_def(sc_hula_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?bool(V2),?int64(V3),?int16(V4),?bool(V5),?int16(V6),?bool(V7),?int32(V8)];
encode_def(sc_hula_init_state, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int64(V3),?int32(V4),?int32(V5)];
encode_def(cs_hula_close, R)->
	{_}=R,
	[];
encode_def(cs_hula_buff, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_hula_buff, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_hula_last_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_hula_last_info_ignore, R)->
	{_}=R,
	[];
encode_def(p_hula_info, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_hula_last_info_win, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int16(V2),?int32(V3),?int64(V4),?list_tuple(V5),?list_string(V6)];
encode_def(sc_hula_last_info_fail, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(cs_hula_cur_info, R)->
	{_}=R,
	[];
encode_def(sc_hula_cur_info_ignore, R)->
	{_}=R,
	[];
encode_def(sc_hula_cur_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_hula_hp_sync, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(p_hula_harm, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_hula_harm_broadcast, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_hula_buff_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_hula_stop, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_hula_rank_sync, R)->
	{_}=R,
	[];
encode_def(sc_hula_rank_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_hula_fight, R)->
	{_}=R,
	[];
encode_def(sc_hula_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_hula_reborn, R)->
	{_}=R,
	[];
encode_def(sc_hula_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_hula_offline_play, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(sc_hula_offline_play, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_hula_open_time, R)->
	{_}=R,
	[];
encode_def(sc_hula_open_time, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_card_get_list, R)->
	{_}=R,
	[];
encode_def(p_card, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(p_opened_card, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_card_get_list, R)->
	{_,V2,V3,V4,V5}=R,
	[?list_tuple(V2),?list_tuple(V3),?int32(V4),?int32(V5)];
encode_def(cs_card_draw, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_card_draw, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4),?int32(V5),?int32(V6)];
encode_def(cs_card_refresh, R)->
	{_}=R,
	[];
encode_def(sc_card_refresh, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5)];
encode_def(cs_card_onekey, R)->
	{_}=R,
	[];
encode_def(sc_card_onekey, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_card_activity_info, R)->
	{_}=R,
	[];
encode_def(sc_card_activity_card, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int32(V5),?int32(V6)];
encode_def(cs_daily_get_list, R)->
	{_}=R,
	[];
encode_def(p_daily, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?bool(V4)];
encode_def(sc_daily_get_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_daily_draw, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_daily_draw, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_daily_family_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_plunder_info, R)->
	{_}=R,
	[];
encode_def(p_stonechip, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int16(V3),?int16(V4),?int16(V5),?int16(V6)];
encode_def(sc_plunder_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?bool(V2),?int32(V3),?int16(V4),?int16(V5),?int32(V6),?int16(V7),?list_tuple(V8),?int16(V9)];
encode_def(cs_plunder_compose, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_plunder_compose, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_plunder_get_target, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(p_plunder_tar, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?bool(V3),?int16(V4),?string(V5),?int8(V6),?int32(V7),?int64(V8),?int16(V9),?bool(V10)];
encode_def(sc_plunder_get_target, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_plunder_fight, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int16(V3),?int8(V4)];
encode_def(sc_plunder_fight, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int8(V3),?list_tuple(V4),?int16(V5),?int16(V6),?int16(V7),?int16(V8),?int8(V9)];
encode_def(cs_plunder_use_protect, R)->
	{_}=R,
	[];
encode_def(sc_plunder_use_protect, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_plunder_buy_attacktime, R)->
	{_}=R,
	[];
encode_def(sc_plunder_buy_attacktime, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_plunder_notice_attacktime, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(cs_plunder_fight_ten, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int16(V3),?int8(V4)];
encode_def(p_plunder_fight_mystery, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_plunder_fight_ten, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int8(V3),?list_bool(V4),?int16(V5),?int16(V6),?int16(V7),?int16(V8),?int8(V9),?list_tuple(V10)];
encode_def(cs_plunder_protect_time, R)->
	{_}=R,
	[];
encode_def(sc_plunder_protect_time, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_plunder_multi_compose, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_plunder_multi_compose, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_pvp_get_list, R)->
	{_}=R,
	[];
encode_def(p_pvp, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?bool(V3),?int16(V4),?int8(V5),?string(V6),?int64(V7),?int16(V8),?int32(V9),?int8(V10)];
encode_def(sc_pvp_get_list, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?list_tuple(V3),?int16(V4)];
encode_def(cs_pvp_fight, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_pvp_fight, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4),?int32(V5),?int32(V6),?int32(V7)];
encode_def(cs_pvp_get_first_eight_replays, R)->
	{_}=R,
	[];
encode_def(p_pvp_replay_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?string(V3),?int16(V4),?int16(V5),?int64(V6),?int32(V7)];
encode_def(sc_pvp_get_first_eight_replays, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_pvp_eight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_pvp_eight_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_pvp_free_fight, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_pvp_free_fight, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4)];
encode_def(cs_pvp_get_free_fight_level, R)->
	{_}=R,
	[];
encode_def(sc_pvp_get_free_fight_level, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_shop_buy_num, R)->
	{_}=R,
	[];
encode_def(p_shop_num, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5),?int8(V6)];
encode_def(sc_shop_buy_num, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_shop_buy, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int8(V4)];
encode_def(sc_shop_buy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_shop_encounter, R)->
	{_}=R,
	[];
encode_def(p_shop_random, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int32(V3),?list_int16(V4)];
encode_def(sc_shop_encounter, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int32(V3)];
encode_def(sc_shop_new, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_shop_refresh, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_shop_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_shop_auto_refresh, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_shop_family_limit_info, R)->
	{_}=R,
	[];
encode_def(p_shop_family_limit, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5),?int16(V6),?tuple(V7)];
encode_def(sc_shop_family_limit_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_shop_family_limit_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_shop_family_limit_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_shop_seed_info, R)->
	{_}=R,
	[];
encode_def(seed_sell_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(sc_shop_seed_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_bag, R)->
	{_}=R,
	[];
encode_def(p_item, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int8(V5),?int16(V6),?int32(V7),?int16(V8),?int8(V9),?int8(V10),?int8(V11)];
encode_def(sc_item_bag, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_item_bag2, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_item_equip, R)->
	{_}=R,
	[];
encode_def(sc_item_equip, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_sell, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_item_sell, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_item_down_equip, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_item_down_equip, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(cs_item_up_equip, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int8(V3),?int64(V4),?int64(V5)];
encode_def(sc_item_up_equip, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int64(V5)];
encode_def(sc_item_new, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_item_num_update, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int16(V3)];
encode_def(sc_item_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_use, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_item_use, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(sc_item_delete_notify, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(cs_item_reinforce, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_reinforce, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int16(V4)];
encode_def(cs_item_max_reinforce, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_max_reinforce, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?list_int16(V4)];
encode_def(sc_item_update_rank, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?int32(V4)];
encode_def(cs_item_up_rank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int64(V3),?int64(V4),?int64(V5)];
encode_def(sc_item_up_rank, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int64(V4),?int16(V5),?int8(V6)];
encode_def(cs_item_compound, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_item_compound, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_item_eat, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int64(V3),?list_int64(V4)];
encode_def(sc_item_eat, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int16(V5)];
encode_def(p_all_equipment, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_int32(V3)];
encode_def(sc_item_all_equipment, R)->
	{_,V2,V3}=R,
	[?list_int64(V2),?list_tuple(V3)];
encode_def(cs_item_use_info, R)->
	{_}=R,
	[];
encode_def(p_item_use_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_item_use_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_auto_up_equip, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_item_auto_up_equip, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_item_stone_eat, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int64(V3),?list_int64(V4)];
encode_def(sc_item_stone_eat, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?int64(V4),?int8(V5)];
encode_def(p_item_decompose_unit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_item_decompose, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_item_decompose, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?list_tuple(V4)];
encode_def(cs_item_decompose_again, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_item_decompose_again, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?list_tuple(V4)];
encode_def(cs_item_decompose_by_money_cost, R)->
	{_}=R,
	[];
encode_def(sc_item_decompose_by_money_cost, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_item_decompose_again_cost, R)->
	{_}=R,
	[];
encode_def(sc_item_decompose_again_cost, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_item_enchant, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(p_equip2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int8(V5),?int64(V6),?int8(V7),?int32(V8),?int16(V9),?int8(V10),?int8(V11)];
encode_def(sc_item_enchant, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_item_x_get, R)->
	{_,V2,V3}=R,
	[?int16(V2),?list_int64(V3)];
encode_def(sc_item_x_get, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_item_down_rank, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_down_rank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int64(V4),?int8(V5)];
encode_def(cs_item_pft_uprank, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int64(V3),?list_int64(V4)];
encode_def(sc_item_pft_uprank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int64(V4),?list_int64(V5)];
encode_def(cs_item_stone_uprank, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int64(V3),?int64(V4)];
encode_def(sc_item_stone_uprank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?int64(V4),?int64(V5)];
encode_def(sc_item_dtl_update, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_item_max_reinforce_for_ger, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(p_reinforce_result, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int16(V3),?int8(V4),?list_int16(V5)];
encode_def(sc_item_max_reinforce_for_ger, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_item_make_legend, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_make_legend, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int16(V4)];
encode_def(cs_item_legend_uprank, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_legend_uprank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(cs_item_stone_legend, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_stone_legend, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int64(V4),?int8(V5),?int64(V6)];
encode_def(cs_explore_one, R)->
	{_}=R,
	[];
encode_def(p_echapter, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int32(V3),?int32(V4),?bool(V5)];
encode_def(sc_explore_one, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int32(V6),?int8(V7)];
encode_def(cs_explore_dungeon_list, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_edungeon, R)->
	{_,V2,V3}=R,
	[?int16(V2),?bool(V3)];
encode_def(sc_explore_dungeon_list, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?list_tuple(V3),?int8(V4)];
encode_def(cs_explore_challenge_encounter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_challenge_encounter, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?int8(V5),?int8(V6)];
encode_def(sc_explore_delete_encounter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_explore_giveup_encounter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_giveup_encounter, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_list, R)->
	{_}=R,
	[];
encode_def(sc_explore_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_explore_collect, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_collect, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(cs_explore_force_collect, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_explore_force_collect, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int8(V3),?int8(V4)];
encode_def(cs_explore_auto_explore_check, R)->
	{_}=R,
	[];
encode_def(sc_explore_auto_explore_check, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int8(V4),?int16(V5)];
encode_def(cs_explore_encounter_pass_reward, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_encounter_pass_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_encounter_dungeon_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_encounter_dungeon_state, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int8(V5)];
encode_def(cs_explore_free, R)->
	{_}=R,
	[];
encode_def(sc_explore_free, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_all, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_explore_reward, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int8(V5)];
encode_def(sc_explore_all, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?tuple(V4)];
encode_def(cs_ger_info, R)->
	{_}=R,
	[];
encode_def(sc_ger_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_ger_info2, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(p_ger_pos_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int8(V3),?list_int64(V4),?list_tuple(V5)];
encode_def(sc_ger_update, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int32(V5),?int64(V6),?int64(V7),?int64(V8),?int64(V9),?list_tuple(V10),?int16(V11)];
encode_def(sc_ger_new, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_ger_standup, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_standup, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int64(V4)];
encode_def(cs_ger_move_pos, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_move_pos, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(cs_ger_pos_list, R)->
	{_}=R,
	[];
encode_def(p_ger_icon_unit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_pos_list, R)->
	{_,V2,V3,V4,V5}=R,
	[?list_tuple(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_ger_sell, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_ger_sell, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_ger_detail, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_ger_detail, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int16(V5),?int16(V6),?int16(V7),?int16(V8),?int16(V9),?int16(V10),?int16(V11),?int16(V12),?int16(V13),?int16(V14),?int16(V15),?int16(V16),?int16(V17),?int32(V18)];
encode_def(cs_ger_view_other, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_ger_view_other, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6)];
encode_def(cs_ger_view_other_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_ger_view_other_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16}=R,
	[?int32(V2),?string(V3),?bool(V4),?int16(V5),?int64(V6),?list_tuple(V7),?list_tuple(V8),?list_tuple(V9),?list_tuple(V10),?int32(V11),?int8(V12),?int32(V13),?int32(V14),?tuple(V15),?list_tuple(V16)];
encode_def(sc_ger_update_exp, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(cs_ger_eat, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int32(V3),?int32(V4)];
encode_def(sc_ger_eat, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_ger_up_rank, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_ger_up_rank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(sc_ger_update_standlist, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_ger_del, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(p_ger_power, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_refresh_power, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_ger_save_item, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_ger_save_dtl, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(p_ger_tag_dtl, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_ger_saved_tag, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_saved_tag, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_set_icon, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_ger_set_icon, R)->
	{_}=R,
	[];
encode_def(cs_ger_buy_tag, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_buy_tag, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_set_tag_data, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(sc_ger_set_tag_data, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_change_tag, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_change_tag, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_ger_lieu_pos_list, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_pos_list, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int8(V3)];
encode_def(cs_ger_lieu_standup, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_lieu_standup, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int64(V4)];
encode_def(cs_ger_lieu_dequeue, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_lieu_dequeue, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int64(V4)];
encode_def(cs_ger_lieu_untie, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_ger_lieu_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?int8(V4),?int16(V5),?int8(V6),?int16(V7),?int8(V8)];
encode_def(sc_ger_lieu_untie, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_ger_lieu_info_list, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_info_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_lieu_move_pos, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_lieu_move_pos, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(cs_ger_lieu_lock_clo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_lieu_lock_clo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_lieu_unlock_clo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_lieu_unlock_clo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_lieu_refresh_clo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_lieu_refresh_clo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_ger_lieu_tie_info, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_tie_info, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_ger_lieu_refresh_freeTimes, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_refresh_freeTimes, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_ger_new_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_down_rank, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_ger_down_rank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int64(V4)];
encode_def(cs_ger_unload, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_unload, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_ger_mirror_info, R)->
	{_}=R,
	[];
encode_def(sc_ger_mirror_info, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int16(V3),?list_int32(V4)];
encode_def(cs_ger_mirror_convert, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_ger_mirror_convert, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_ger_mirror_commit, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_mirror_commit, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_ger_set_body, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_ger_set_body, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_second_uprank, R)->
	{_,V2,V3}=R,
	[?int64(V2),?list_int64(V3)];
encode_def(sc_ger_second_uprank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_ger_batch_uprank, R)->
	{_,V2,V3}=R,
	[?int64(V2),?list_int64(V3)];
encode_def(sc_ger_batch_uprank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_ger_second_transmigration, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_ger_second_transmigration, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_transform_blink, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_ger_transform_blink, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_message_notice, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_notice, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?string(V4)];
encode_def(sc_message_notice, R)->
	{_,V2,V3,V4}=R,
	[?list_int32(V2),?list_tuple(V3),?string(V4)];
encode_def(cs_message_certain_notice, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_message_certain_notice, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_message_bc, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_message_bc_id, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_message_bc_id2, R)->
	{_,V2,V3}=R,
	[?int16(V2),?list_any(V3)];
encode_def(cs_message_test, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_message_test, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(sc_message_best_card, R)->
	{_,V2,V3,V4}=R,
	[?string(V2),?int8(V3),?int32(V4)];
encode_def(sc_message_ger_upLevel, R)->
	{_,V2,V3}=R,
	[?string(V2),?tuple(V3)];
encode_def(sc_message_item_uprank, R)->
	{_,V2,V3}=R,
	[?string(V2),?tuple(V3)];
encode_def(cs_battle_progress, R)->
	{_}=R,
	[];
encode_def(p_battle_chapter_star_reward, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int8(V5)];
encode_def(p_battle_progress, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int16(V5)];
encode_def(sc_battle_progress, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_int16(V3),?list_tuple(V4)];
encode_def(cs_battle_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(p_dungeon, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int16(V5)];
encode_def(sc_battle_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int16(V3),?bool(V4),?list_tuple(V5),?int16(V6),?int16(V7)];
encode_def(cs_battle_challenge, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_battle_challenge, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?int8(V5)];
encode_def(cs_battle_perfect_reward, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_battle_perfect_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_battle_broadcast_get_item, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?string(V2),?int32(V3),?int8(V4),?int16(V5),?int16(V6)];
encode_def(cs_battle_dungeon_raids, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_battle_dungeon_raids, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_battle_star_reward, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(sc_battle_star_reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?list_tuple(V4)];
encode_def(cs_battle_reset_dungeon, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_battle_reset_dungeon, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_battle_battle_fail, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(cs_battle_world_level, R)->
	{_}=R,
	[];
encode_def(sc_battle_world_level, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int16(V4)];
encode_def(cs_battle_obtain_boss_reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_battle_obtain_boss_reward, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5),?list_tuple(V6)];
encode_def(cs_battle_get_boss_reward_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_battle_get_boss_reward_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5)];
encode_def(cs_battle_dojang_info, R)->
	{_}=R,
	[];
encode_def(p_dojang_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_battle_dojang_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4),?int32(V5)];
encode_def(cs_battle_dojang_fight, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_battle_dojang_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?tuple(V3),?list_tuple(V4),?list_tuple(V5),?list_tuple(V6)];
encode_def(cs_battle_dojang_harvest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_battle_dojang_harvest, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_battle_dojang_buy, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_battle_dojang_buy, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int32(V4),?int32(V5)];
encode_def(cs_role_info, R)->
	{_}=R,
	[];
encode_def(sc_role_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V24,V25,V26,V27,V28,V29,V30,V31,V32,V33,V34,V35,V36,V37,V38,V39,V40,V41,V42,V43,V44,V45,V46,V47,V48,V49,V50,V51,V52,V53,V54,V55,V56,V57,V58}=R,
	[?int32(V2),?string(V3),?bool(V4),?string(V5),?int32(V6),?int16(V7),?int64(V8),?int64(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?int8(V14),?int32(V15),?int16(V16),?int16(V17),?int32(V18),?int16(V19),?int32(V20),?int16(V21),?int16(V22),?int8(V23),?int8(V24),?int8(V25),?int8(V26),?bool(V27),?bool(V28),?int16(V29),?int16(V30),?int16(V31),?int16(V32),?int8(V33),?int32(V34),?int32(V35),?int16(V36),?int16(V37),?int32(V38),?int32(V39),?int32(V40),?bool(V41),?int8(V42),?int32(V43),?int32(V44),?int16(V45),?int32(V46),?int32(V47),?int32(V48),?int16(V49),?int16(V50),?int32(V51),?int32(V52),?int16(V53),?int32(V54),?int32(V55),?int32(V56),?int32(V57),?int8(V58)];
encode_def(sc_role_update_level, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_update_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_role_update_unioncoin, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_role_buy_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_buy_energy, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5),?int8(V6)];
encode_def(sc_role_update_exp, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_role_update_coin, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_role_update_reputation, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_gold, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_goldBonus, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_vipLevel, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_role_update_energy, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(sc_role_update_discoveryTimes, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(sc_role_update_pvpTimes, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(sc_role_update_plunderTimes, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(sc_role_update_randomPVPTimes, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_singlePVPTimes, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_goldUsed, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_title, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_encounterFreeNum, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_weiboCount, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_setting, R)->
	{_}=R,
	[];
encode_def(sc_role_setting, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_role_get_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_get_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_buy_coin_value, R)->
	{_}=R,
	[];
encode_def(sc_role_buy_coin_value, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(cs_role_weixin_share, R)->
	{_}=R,
	[];
encode_def(sc_role_update_pay_ext, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_role_suggest_open, R)->
	{_}=R,
	[];
encode_def(sc_role_suggest_open, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(cs_role_suggest, R)->
	{_,V2,V3}=R,
	[?string(V2),?string(V3)];
encode_def(sc_role_suggest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_log_guide_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_talent_remains_point, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_role_multi_buy_energy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(p_multi_buy_energy, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_role_multi_buy_energy, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5),?list_tuple(V6)];
encode_def(cs_role_is_under_examine, R)->
	{_}=R,
	[];
encode_def(sc_role_is_under_examine, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(cs_role_push_setting, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_role_push_setting, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(cs_role_get_guide_state, R)->
	{_}=R,
	[];
encode_def(sc_role_get_guide_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_role_set_guide_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_set_guide_state, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_change_head, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_change_head, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_role_change_location, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_role_update_teamPkTimes, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(cs_role_teamPkTimes_info, R)->
	{_}=R,
	[];
encode_def(sc_role_teamPkTimes_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?list_int16(V4)];
encode_def(cs_role_token, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_role_token, R)->
	{_}=R,
	[];
encode_def(cs_role_select_ger, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_select_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_demo_fight, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_demo_fight, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_role_base_config, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_role_pay_ios, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?string(V2),?int32(V3),?string(V4),?string(V5),?int8(V6)];
encode_def(sc_role_pay_ios, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?int32(V4),?bool(V5)];
encode_def(cs_role_pay_91, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?int32(V3),?string(V4),?string(V5)];
encode_def(sc_role_pay_91, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?int32(V4),?bool(V5)];
encode_def(sc_role_pay_uc, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_dl, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_zz, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_360, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_wdj, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_dk, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_mi, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_az, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_update_profoundCrystal, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_honor, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_pvppoint, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_home_resource, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_ticket, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_laputastone, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_role_do_transmigration, R)->
	{_}=R,
	[];
encode_def(sc_role_do_transmigration, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_role_can_transmigration, R)->
	{_}=R,
	[];
encode_def(sc_role_can_transmigration, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int32(V8)];
encode_def(sc_role_update_svip, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_account_login, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?string(V4),?string(V5),?string(V6),?int16(V7),?string(V8),?int16(V9)];
encode_def(sc_account_login, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?bool(V3),?bool(V4),?int8(V5)];
encode_def(cs_account_create, R)->
	{_,V2,V3}=R,
	[?string(V2),?int8(V3)];
encode_def(sc_account_create, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_account_enter_game, R)->
	{_}=R,
	[];
encode_def(sc_account_enter_game, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_account_kick, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_account_heart, R)->
	{_}=R,
	[];
encode_def(sc_account_heart, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_account_logout, R)->
	{_}=R,
	[];
encode_def(cs_account_check_rolename, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_account_check_rolename, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_account_demo, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(sc_account_demo, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(_, _) -> <<>>.

decode_def(21701)->
	[cs_trainingRoom_info];
decode_def(21702)->
	[sc_trainingRoom_info,int16,int64];
decode_def(21703)->
	[cs_trainingRoom_start_training,int16];
decode_def(10418)->
	[p_ger_view,int16,int16,int16];
decode_def(10616)->
	[p_item_view,int16,int16,int8,int16];
decode_def(11930)->
	[p_reward_info,int32,int32,int32,int32,int32,{list, 10616},{list, 10418}];
decode_def(21704)->
	[p_trainingRoom_reward,int8,int16,int32,int32,{list, 11930},{list, 11930},int16];
decode_def(21705)->
	[sc_trainingRoom_start_training,int8,{list, 21704}];
decode_def(21601)->
	[p_diamond,int8,int16];
decode_def(21602)->
	[cs_diamond_combine,int16,int16];
decode_def(21603)->
	[sc_diamond_combine,int8];
decode_def(21604)->
	[cs_diamond_equip,int64,int16,int8];
decode_def(21605)->
	[sc_diamond_equip,int8,int64,int8,int16,int8];
decode_def(21606)->
	[cs_diamond_demount,int64,int16,int8];
decode_def(21607)->
	[sc_diamond_demount,int8,int64,int8,int16,int8];
decode_def(21608)->
	[cs_diamond_demount_multi,int16];
decode_def(21610)->
	[p_diamond_location,int64,int8,int8];
decode_def(21609)->
	[sc_diamond_demount_multi,int8,{list, 21610}];
decode_def(21611)->
	[cs_diamond_holygrail_info,int64];
decode_def(21612)->
	[sc_diamond_holygrail_info,int8,int8,bool];
decode_def(21613)->
	[cs_diamond_holygrail_sacrifice,int64,int64];
decode_def(21614)->
	[sc_diamond_holygrail_sacrifice,int8];
decode_def(21615)->
	[cs_diamond_holygrail_uplevel,int64];
decode_def(21616)->
	[sc_diamond_holygrail_uplevel,int8,int64,int8];
decode_def(21501)->
	[cs_conquerisland_sign];
decode_def(21502)->
	[sc_conquerisland_sign,int8,int32];
decode_def(21503)->
	[cs_conquerisland_buy];
decode_def(21504)->
	[sc_conquerisland_buy,int8,int16,int16];
decode_def(21505)->
	[cs_conquerisland_info];
decode_def(21506)->
	[sc_conquerisland_info,int16,int16,int16,int16,{list, int32},int32,int8,int32];
decode_def(21507)->
	[cs_conquerisland_unrequest];
decode_def(21508)->
	[sc_conquerisland_unrequest,int8];
decode_def(21509)->
	[sc_conquerisland_times_update,int16];
decode_def(21510)->
	[cs_conquerisland_war_base_info];
decode_def(21529)->
	[p_pos,int16,int16];
decode_def(21530)->
	[p_centre,int8,int8,int8,int8,int8,21529,int32];
decode_def(21531)->
	[p_player,int32,int32,string,int16,int8,int64,int8,int8,int32,21529,21529,int8,int32,int16,int64,int16];
decode_def(21532)->
	[p_conquerisland_boss,int8,21529,int16,int16,int16,int16,int64,int64,int16,int8];
decode_def(21511)->
	[sc_conquerisland_war_base_info,int8,int32,21529,21529,{list, 21530},{list, 21531},{list, 21532}];
decode_def(21512)->
	[cs_conquerisland_role_dtl,int32,int32];
decode_def(14835)->
	[p_carlos_replay_dtl,int8,int64,string,string];
decode_def(21533)->
	[p_conquerisland_ger,int32,int16,int16,int64,int64];
decode_def(21513)->
	[sc_conquerisland_role_dtl,int8,{list, 21533},{list, 14835},int16];
decode_def(21514)->
	[cs_conquerisland_mov,int8];
decode_def(21515)->
	[sc_conquerisland_mov,int8];
decode_def(21516)->
	[cs_conquerisland_stop];
decode_def(21517)->
	[sc_conquerisland_stop,int8];
decode_def(21518)->
	[cs_conquerisland_centre_dtl,int8];
decode_def(21519)->
	[sc_conquerisland_centre_dtl,int8,21530,{list, 21531},{list, 21531}];
decode_def(21520)->
	[cs_conquerisland_attack,int32,int32,int8];
decode_def(20004)->
	[p_action,int8,int16,{list, int8},int8,int64,int64,int16];
decode_def(20003)->
	[p_fighter,int64,int16,int8,int64,int64,int64,int64,int16,int16,int8,int8,int8,int8,int64];
decode_def(20002)->
	[sc_fight_request,{list, 20003},{list, 20004},bool];
decode_def(21521)->
	[sc_conquerisland_attack,int8,{list, 20002}];
decode_def(21522)->
	[cs_conquerisland_centre_occupy,int8];
decode_def(21523)->
	[sc_conquerisland_centre_occupy,int8,int32];
decode_def(21524)->
	[cs_conquerisland_reborn];
decode_def(21525)->
	[sc_conquerisland_reborn,int8];
decode_def(21526)->
	[cs_conquerisland_replay,int64];
decode_def(21527)->
	[sc_conquerisland_replay,int8,{list, 20002}];
decode_def(21528)->
	[sc_conquerisland_update_war,{list, 21531},{list, 21530},{list, 21532}];
decode_def(21534)->
	[cs_conquerisland_self];
decode_def(21535)->
	[sc_conquerisland_self,21531];
decode_def(21536)->
	[cs_conquerisland_rank];
decode_def(21538)->
	[p_fight_rank,int32,int32,string,int16,int32,int32,int64,int32];
decode_def(21537)->
	[sc_conquerisland_rank,{list, 21538}];
decode_def(21539)->
	[cs_conquerisland_talk,string];
decode_def(21540)->
	[cs_conquerisland_get_talk];
decode_def(14837)->
	[p_carlos_talk,int32,string,string,int8];
decode_def(21541)->
	[sc_conquerisland_get_talk,{list, 14837}];
decode_def(14836)->
	[p_carlos_rank_dtl,int32,int32,int16,int16,int16,int16,string,int8];
decode_def(21542)->
	[sc_conquerisland_end_war,{list, 14836}];
decode_def(21543)->
	[sc_conquerisland_talk,14837];
decode_def(21401)->
	[cs_recycle_restore,int8,int64,int64,int8];
decode_def(21402)->
	[sc_recycle_restore,int8,int8,int64,int64,{list, 11930}];
decode_def(21301)->
	[cs_crystal_uplevel,int8,int64,int64];
decode_def(21305)->
	[p_crystalinfo,int8,int8,int16,int64,int64];
decode_def(21302)->
	[sc_crystal_uplevel,int8,int8,int64,int8,21305];
decode_def(21303)->
	[cs_crystal_uprank,int8,int64,int64];
decode_def(21304)->
	[sc_crystal_uprank,int8,int8,int64,int8,21305];
decode_def(21306)->
	[cs_crystal_info,int64];
decode_def(21307)->
	[sc_crystal_info,int8,int64,int8,{list, 21305}];
decode_def(21308)->
	[p_crystalinfo_brief,int8,int8,int16];
decode_def(21309)->
	[ger_crystalinfo_brief,int64,int8,{list, 21308}];
decode_def(21203)->
	[cs_doublematch_lineup];
decode_def(21206)->
	[p_doublematch_team_trainer_info,int32,string,bool,int8,int32,int32,int16,int32];
decode_def(10606)->
	[p_equip,int64,int16,int16,int8,int64,int8,int32,int16,int8,int8,int8];
decode_def(10485)->
	[p_demage_rate_unit,int8,int32,int32];
decode_def(20607)->
	[p_awake_info,int8,int32,int8,{list, int32}];
decode_def(10403)->
	[p_ger,int64,int16,int16,int16,int32,int64,int64,int64,int64,{list, 20607},{list, 10485},int8,int16,int8];
decode_def(21205)->
	[p_doublematch_team_ger_info,int32,int32,10403,{list, 10606}];
decode_def(21207)->
	[p_doublematch_team_member,21206,{list, 21205}];
decode_def(21204)->
	[sc_doublematch_lineup,int8,{list, 21207}];
decode_def(21210)->
	[p_ger_pos_unit,int64,int32];
decode_def(21208)->
	[cs_doublematch_update_lineup,{list, 21210}];
decode_def(21209)->
	[sc_doublematch_update_lineup,int8,{list, 21207}];
decode_def(21211)->
	[cs_doublematch_sign];
decode_def(21212)->
	[sc_doublematch_sign,int8];
decode_def(21213)->
	[cs_doublematch_sign_cancel];
decode_def(21214)->
	[sc_doublematch_sign_cancel,int8];
decode_def(21215)->
	[cs_doublematch_times_buy];
decode_def(21216)->
	[sc_doublematch_times_buy,int8,int16,int32];
decode_def(21217)->
	[cs_doublematch_roleinfo];
decode_def(21218)->
	[sc_doublematch_roleinfo,int8,int8,int16,int32,int32,int32,int32,int32,int32,int32,int32,int8,int16,int32];
decode_def(21219)->
	[cs_doublematch_rankinfo,int8,int32,int32];
decode_def(21221)->
	[p_doublematch_rank_unit,int32,string,bool,int8,int32,int16,int32,int8,int16,int32,int32,int32,int8];
decode_def(21220)->
	[sc_doublematch_rankinfo,int8,int8,int32,{list, 21221},int32];
decode_def(21222)->
	[sc_doublematch_quit,int8];
decode_def(21223)->
	[cs_doublematch_record_list,int32,int8,int32];
decode_def(21227)->
	[p_record_unit,int64,{list, 21221},int32];
decode_def(21224)->
	[sc_doublematch_record_list,int8,{list, 21227},int32,int8,int32];
decode_def(21225)->
	[cs_doublematch_replay,int64];
decode_def(20005)->
	[sc_fight_double_request,{list, 20003},{list, 20004},bool];
decode_def(21226)->
	[sc_doublematch_replay,int8,20005,int8,int8];
decode_def(21228)->
	[sc_doublematch_fight,{list, 21221},int8,int8,20005,int32];
decode_def(21101)->
	[cs_goldenegg_use_item,int32];
decode_def(21102)->
	[sc_goldenegg_use_item,int8,int32,{list, int32}];
decode_def(21103)->
	[cs_goldenegg_smash,int32,{list, int32}];
decode_def(12005)->
	[p_reward_view,int8,int32];
decode_def(21104)->
	[sc_goldenegg_smash,int8,int32,{list, 12005}];
decode_def(21107)->
	[cs_goldenegg_roleinfo];
decode_def(21108)->
	[sc_goldenegg_roleinfo,int32,int32,{list, int32}];
decode_def(21109)->
	[cs_goldenegg_shop];
decode_def(21111)->
	[p_goods_unit,int32,11930,int32,int8,int32,int32,int32];
decode_def(21110)->
	[sc_goldenegg_shop,int8,{list, 21111},int32];
decode_def(21112)->
	[cs_goldenegg_open];
decode_def(21113)->
	[sc_goldenegg_open,int8,int32];
decode_def(21114)->
	[sc_goldenegg_update_score,int32,int32];
decode_def(21001)->
	[p_member_info,int32,int32,int8,int8,bool,int8,int32];
decode_def(21002)->
	[sc_matchRoom_init,int32,{list, 21001},int32];
decode_def(21003)->
	[sc_matchRoom_ready,int32,int32];
decode_def(21004)->
	[sc_matchRoom_cancel,int32,int32];
decode_def(21005)->
	[sc_matchRoom_kick,int32,int32];
decode_def(21006)->
	[sc_matchRoom_exit,int32,{list, int32}];
decode_def(21007)->
	[sc_matchRoom_new,int32,{list, 21001}];
decode_def(21009)->
	[cs_matchRoom_ready,int32];
decode_def(21010)->
	[cs_matchRoom_cancel,int32];
decode_def(21011)->
	[cs_matchRoom_exit,int32];
decode_def(21012)->
	[sc_matchRoom_close,int32,int8];
decode_def(20901)->
	[cs_skin_compose,int32];
decode_def(20902)->
	[sc_skin_compose,int8,bool];
decode_def(20903)->
	[cs_skin_activate,int32,int8];
decode_def(20904)->
	[sc_skin_activate,int8];
decode_def(20905)->
	[cs_skin_equip,int32];
decode_def(20906)->
	[sc_skin_equip,int8];
decode_def(20907)->
	[cs_skin_demount];
decode_def(20908)->
	[sc_skin_demount,int8];
decode_def(20910)->
	[p_skin,int32,int8];
decode_def(20911)->
	[cs_skin_info];
decode_def(20914)->
	[p_skin_buff,int32,int32,int32,int8];
decode_def(20912)->
	[sc_skin_info,{list, 20910},int32,20914];
decode_def(20913)->
	[p_skin_info,int32];
decode_def(20801)->
	[cs_home_info];
decode_def(20807)->
	[c_info,int32,int32,int32];
decode_def(20802)->
	[sc_home_info,int8,{list, 20807},int32];
decode_def(20803)->
	[cs_home_build,int32];
decode_def(20804)->
	[sc_home_build,int8,{list, 20807}];
decode_def(20805)->
	[cs_home_up_stage];
decode_def(20806)->
	[sc_home_up_stage,int8];
decode_def(20808)->
	[c_need,int32,int32];
decode_def(20809)->
	[cs_home_get_friend,int32];
decode_def(20810)->
	[sc_home_get_friend,int8,{list, 20807},int32];
decode_def(20811)->
	[cs_home_task_info];
decode_def(20815)->
	[home_task,int32,int8,int32,{list, int32},int32,int32,int32,11930,int8,int32,{list, int64},{list, int32},{list, int8},{list, int32},int32,int32,int32,string,string];
decode_def(20812)->
	[sc_home_task_info,{list, 20815}];
decode_def(20813)->
	[cs_home_task_operate,int8,int32,{list, int64},int32];
decode_def(20814)->
	[sc_home_task_operate,int8];
decode_def(20816)->
	[cs_home_bounty_task_info];
decode_def(20817)->
	[sc_home_bounty_task_info,{list, 20815},int32,int32];
decode_def(20818)->
	[cs_home_cirrus_info,int32];
decode_def(20824)->
	[cirrus_node,int32,int32,int32,int8,int32,int64,int8,11930,int32,int32,int32,int32];
decode_def(20819)->
	[sc_home_cirrus_info,int32,{list, 20824}];
decode_def(20820)->
	[cs_home_cirrus_fight,int32];
decode_def(20821)->
	[sc_home_cirrus_fight,int8,{list, 20002}];
decode_def(20822)->
	[cs_home_cirrus_operate,int32,int32,int64];
decode_def(20823)->
	[sc_home_cirrus_operate,int8];
decode_def(20825)->
	[cs_home_exchange,int32,int32];
decode_def(20826)->
	[sc_home_exchange,int8];
decode_def(20701)->
	[cs_sign_reward_info];
decode_def(20703)->
	[p_sign_reward,int8,{list, 12005},int8];
decode_def(20702)->
	[sc_sign_reward_info,int8,{list, 20703},{list, 20703},int8,int8];
decode_def(20704)->
	[cs_sign_reward,int8,int8];
decode_def(20705)->
	[sc_sign_reward,int8,int8,int8,{list, 12005}];
decode_def(20601)->
	[cs_awake_ger,int64,int8];
decode_def(20602)->
	[sc_awake_ger,int8,int32,int64,int8];
decode_def(20603)->
	[cs_awake_recast_ger,int64,int8,int8];
decode_def(20604)->
	[sc_awake_recast_ger,int8,{list, int32},int64,int8];
decode_def(20605)->
	[cs_awake_exchange_skill,int64,int8,int32];
decode_def(20606)->
	[sc_awake_exchange_skill,int8,int64,int8,int32];
decode_def(20608)->
	[cs_awake_cancel_skill,int64,int8];
decode_def(20609)->
	[sc_awake_cancel_skill,int8,int64,int8];
decode_def(20501)->
	[cs_generalteam_create,int32,int8];
decode_def(20502)->
	[sc_generalteam_create,int8];
decode_def(20503)->
	[cs_generalteam_invite,int32];
decode_def(20504)->
	[sc_generalteam_invite,int8];
decode_def(20508)->
	[p_team_member_info,int32,string,bool,int32,int8,int16,int64,int32];
decode_def(20505)->
	[sc_generalteam_invite_request,20508,int8,int8,int64,int64];
decode_def(20506)->
	[cs_generalteam_invite_response,int8,int64,int32,int8,int8];
decode_def(20507)->
	[sc_generalteam_invite_response,int8,int64,int32,20508];
decode_def(20510)->
	[p_team_info,int8,int64,int32,{list, int32},{list, 20508}];
decode_def(20509)->
	[update_generalteam_info,int8,20510];
decode_def(20511)->
	[cs_generalteam_leaveteam];
decode_def(20512)->
	[sc_generalteam_leaveteam,int8,int32];
decode_def(20513)->
	[cs_generalteam_disbandteam];
decode_def(20514)->
	[sc_generalteam_disbandteam,int8];
decode_def(20515)->
	[cs_generalteam_kick,int32];
decode_def(20516)->
	[sc_generalteam_kick,int8,int32,int32];
decode_def(20517)->
	[cs_generalteam_change_authority,int8,int32];
decode_def(20518)->
	[sc_generalteam_change_authority,int8,int8,int32,int32];
decode_def(20519)->
	[cs_generalteam_talk,string];
decode_def(20520)->
	[sc_generalteam_talk,int8];
decode_def(20521)->
	[update_generalteam_talk,20508,string];
decode_def(20522)->
	[cs_generalteam_info];
decode_def(20523)->
	[sc_generalteam_info,int8,20510];
decode_def(20524)->
	[cs_generalteam_change_teamtype,int8,int8];
decode_def(20525)->
	[sc_generalteam_change_teamtype,int8,int8];
decode_def(20401)->
	[cs_panicbuy_info];
decode_def(20309)->
	[p_ger_unit,int32,int32,int32,int32];
decode_def(20308)->
	[p_item_unit,int32,int32,int32,int32];
decode_def(20307)->
	[p_sell_reward_unit,int64,int64,int64,{list, 20308},{list, 20309}];
decode_def(20403)->
	[p_panic_buy_config,int32,20307,20307,int32,int32,int32,int64,int64];
decode_def(20402)->
	[sc_panicbuy_info,int8,{list, 20403},string];
decode_def(20404)->
	[cs_panicbuy_once,int32];
decode_def(20405)->
	[sc_panicbuy_once,int8,{list, 12005}];
decode_def(20301)->
	[cs_discount_activity_list];
decode_def(20303)->
	[p_activity_unit,int32,int32,int64,int64,int8,int32];
decode_def(20302)->
	[sc_discount_activity_list,int8,{list, 20303}];
decode_def(20304)->
	[cs_discount_exchange_info];
decode_def(20306)->
	[p_exchange_config_unit,int32,20307,20307,int32,int32];
decode_def(20305)->
	[sc_discount_exchange_info,int8,string,{list, 20306},int32];
decode_def(20310)->
	[cs_discount_exchange,int32,int32];
decode_def(20311)->
	[sc_discount_exchange,int8,int32,int32,{list, 12005}];
decode_def(20312)->
	[cs_discount_pay_activity_info];
decode_def(20314)->
	[p_pay_config_unit,int32,int32,20307,int32,int32];
decode_def(20313)->
	[sc_discount_pay_activity_info,int8,string,{list, 20314},int32];
decode_def(20315)->
	[cs_discount_pay_activity,int32,int32];
decode_def(20316)->
	[sc_discount_pay_activity,int8,int32,int32,{list, 12005}];
decode_def(20201)->
	[p_homestead_log,string,int8,int8,string,int8,int32];
decode_def(20202)->
	[p_homestead_machine,int8,int16,int32,int32,int32,int32,int8,int32];
decode_def(20203)->
	[p_homestead,string,int8,int8,int32,int8,int16,int64,int16,int16,int32];
decode_def(20204)->
	[sc_homestead_error,int8];
decode_def(20205)->
	[cs_homestead_get_info];
decode_def(20206)->
	[sc_homestead_get_info,20203,{list, 20202}];
decode_def(20207)->
	[cs_homestead_get_friend_info,int32];
decode_def(20208)->
	[sc_homestead_get_friend_info,int32,20203,{list, 20202}];
decode_def(20209)->
	[cs_homestead_unlock_machine,int8];
decode_def(20210)->
	[sc_homestead_unlock_machine,20202];
decode_def(20211)->
	[cs_homestead_uproot_seed,int8];
decode_def(20212)->
	[sc_homestead_uproot_seed,int8];
decode_def(20213)->
	[cs_homestead_harvest,int8];
decode_def(20214)->
	[sc_homestead_harvest,20202,12005];
decode_def(20215)->
	[cs_homestead_seeding,int8,int16];
decode_def(20216)->
	[sc_homestead_seeding,20202];
decode_def(20217)->
	[sc_homestead_update_machine,20202];
decode_def(20218)->
	[cs_homestead_change_ger,int64];
decode_def(20219)->
	[sc_homestead_change_ger,int64];
decode_def(20220)->
	[cs_homestead_mating,int32];
decode_def(20221)->
	[sc_homestead_mating,int8,{list, 12005},int32,int32,int8];
decode_def(20222)->
	[sc_homestead_mating_to_friend,20201,int32,int8];
decode_def(20223)->
	[cs_homestead_addenergy,int32,int8];
decode_def(20224)->
	[sc_homestead_addenergy,int32,int8,int8,int32,int32,{list, 12005}];
decode_def(20225)->
	[sc_homestead_addenergy_to_friend,int8,20201,int32,int32];
decode_def(20226)->
	[cs_homestead_get_log];
decode_def(20227)->
	[sc_homestead_get_log,{list, 20201}];
decode_def(20228)->
	[cs_homestead_get_friend_log,int32];
decode_def(20229)->
	[sc_homestead_get_friend_log,int32,{list, 20201}];
decode_def(20230)->
	[sc_homestead_sync_mating_cool_second,int32,int32];
decode_def(20231)->
	[sc_homestead_sync_ger,int32,int32,int16];
decode_def(20232)->
	[sc_homestead_sync_add_enagy,int32,int32,int32,int32,int32];
decode_def(20233)->
	[cs_homestead_compose,int16,int16];
decode_def(20234)->
	[sc_homestead_compose,int8];
decode_def(20235)->
	[cs_homestead_compose_list];
decode_def(20237)->
	[p_compose_info,int16,int16,int16,int16];
decode_def(20236)->
	[sc_homestead_compose_list,{list, 20237}];
decode_def(20101)->
	[p_task,int32,int8,int32];
decode_def(20103)->
	[cs_task_get_info];
decode_def(20104)->
	[sc_task_get_info,{list, 20101},{list, 20101},{list, 20101},{list, 20101},{list, 20101},{list, 20101}];
decode_def(20105)->
	[cs_task_operate,int8,int32];
decode_def(20106)->
	[sc_task_operate,int8,int32,int8,{list, 12005}];
decode_def(20107)->
	[sc_task_error,int8];
decode_def(20108)->
	[sc_task_notify_change,int8,{list, 20101},{list, int32}];
decode_def(20001)->
	[cs_fight_request];
decode_def(17101)->
	[cs_consumerank_info];
decode_def(17102)->
	[sc_consumerank_info,int8,int32];
decode_def(17103)->
	[cs_consumerank_list,int8];
decode_def(17105)->
	[p_consumerank_unit,int32,bool,int16,int8,string,int64,int16,int32,int8,int32];
decode_def(17104)->
	[sc_consumerank_list,int8,{list, 17105},17105];
decode_def(17001)->
	[cs_buddy_partner_insert,int64,int64,int8];
decode_def(17002)->
	[sc_buddy_partner_insert,int8,int8,int64,int64];
decode_def(17003)->
	[cs_buddy_partner_insert_batch,int8,{list, int64}];
decode_def(17004)->
	[sc_buddy_partner_insert_batch,int8,int8,{list, int64}];
decode_def(17005)->
	[cs_buddy_partner_remove,int8,int64];
decode_def(17006)->
	[sc_buddy_partner_remove,int8,int64];
decode_def(16901)->
	[p_dSign_unit,int32,int32,int16,int8];
decode_def(16902)->
	[cs_dSign_info];
decode_def(16903)->
	[sc_dSign_info,int32,int32,int32,{list, 16901},int8,int32,{list, 12005}];
decode_def(16904)->
	[cs_dSign_sign];
decode_def(16905)->
	[sc_dSign_sign,int8,{list, 12005}];
decode_def(16906)->
	[cs_dSign_reSign,int32,int16];
decode_def(16907)->
	[sc_dSign_reSign,int8,{list, 12005}];
decode_def(16908)->
	[cs_dSign_get_sevenReward];
decode_def(16909)->
	[sc_dSign_get_sevenReward,int8,{list, 12005}];
decode_def(16910)->
	[cs_dSign_get_monReward];
decode_def(16911)->
	[sc_dSign_get_monReward,int8,{list, 12005}];
decode_def(16912)->
	[cs_dSign_mark_mon];
decode_def(16913)->
	[sc_dSign_mark_mon,int8,{list, 12005}];
decode_def(16801)->
	[cs_trainerRear_brief];
decode_def(16802)->
	[sc_trainerRear_brief,int8,int8];
decode_def(16803)->
	[cs_trainerRear_info,int8];
decode_def(16812)->
	[p_object_unit,int32,int8];
decode_def(16805)->
	[p_rear_machine,int8,int8,{list, 16812},int32,int32];
decode_def(16804)->
	[sc_trainerRear_info,int8,int8,{list, 16805}];
decode_def(16806)->
	[cs_trainerRear_insert,int8,int8,{list, int64}];
decode_def(16807)->
	[sc_trainerRear_insert,int8,16805];
decode_def(16808)->
	[cs_trainerRear_mature,int8,int8];
decode_def(16809)->
	[sc_trainerRear_mature,int8,int8,int8,{list, 12005}];
decode_def(16810)->
	[cs_trainerRear_accelerate,int8,int8,int16];
decode_def(16811)->
	[sc_trainerRear_accelerate,int8,int8,int8,int32];
decode_def(16701)->
	[cs_dojangrank_info];
decode_def(16702)->
	[sc_dojangrank_info,int32,int32,int32,int32];
decode_def(16703)->
	[cs_dojangrank_rank,int32];
decode_def(16707)->
	[p_dojangrank_rank,int32,int64,bool,int8,int32,int16,string,int8,int32,int32];
decode_def(16704)->
	[sc_dojangrank_rank,int32,16707,{list, 16707}];
decode_def(16705)->
	[cs_dojangrank_buy,int32];
decode_def(16706)->
	[sc_dojangrank_buy,int32,int32,int32,int32];
decode_def(16710)->
	[cs_dojangrank_select_ger_type,int32,int32];
decode_def(16711)->
	[sc_dojangrank_select_ger_type,int32,int32,int32];
decode_def(16712)->
	[cs_dojangrank_fight,int32,int32,int32];
decode_def(16713)->
	[sc_dojangrank_fight,int8,16707,{list, 16707},{list, 20002},11930];
decode_def(16714)->
	[cs_dojangrank_replay_list];
decode_def(16718)->
	[p_dojang_replay_info,string,string,int16,int16,int64,int32,int32,int32];
decode_def(16715)->
	[sc_dojangrank_replay_list,{list, 16718}];
decode_def(16716)->
	[cs_dojangrank_replay_detail,int64];
decode_def(16717)->
	[sc_dojangrank_replay_detail,int8,{list, 20002}];
decode_def(16719)->
	[cs_dojangrank_ger_view_other,int32,int32];
decode_def(16720)->
	[sc_dojangrank_ger_view_other,int32,string,int16,int64,{list, 10403}];
decode_def(16721)->
	[cs_dojangrank_self_rank];
decode_def(16722)->
	[sc_dojangrank_self_rank,{list, int32}];
decode_def(16741)->
	[cs_dojangrank_world_info];
decode_def(16742)->
	[sc_dojangrank_world_info,{list, int32},int32,{list, int32},{list, int32},int32,int32,int32,int32];
decode_def(16743)->
	[cs_dojangrank_world_rank,int32,int32,int32,int32];
decode_def(16765)->
	[dr_ger_info,int32,int32,int32];
decode_def(16747)->
	[p_dr_world_rank,int32,int64,bool,int8,int32,int16,string,int8,int32,int32,int32,int32,{list, 16765},int32,int32,int32,int32];
decode_def(16744)->
	[sc_dojangrank_world_rank,int32,16747,{list, 16747}];
decode_def(16745)->
	[cs_dojangrank_world_buy,int32,int32];
decode_def(16746)->
	[sc_dojangrank_world_buy,int32,int32,int32,int32,int32];
decode_def(16748)->
	[cs_dojangrank_world_refresh_enemy,int32,int32];
decode_def(16749)->
	[sc_dojangrank_world_refresh_enemy,{list, 16747},int32,16747,int32];
decode_def(16750)->
	[cs_dojangrank_world_select_ger_type,int32,int32];
decode_def(16751)->
	[sc_dojangrank_world_select_ger_type,int32,int32,int32];
decode_def(16752)->
	[cs_dojangrank_world_fight,int32,int32,int32];
decode_def(16753)->
	[sc_dojangrank_world_fight,int8,16747,{list, 16747},{list, 20002},11930];
decode_def(16754)->
	[cs_dojangrank_world_replay_list,int32];
decode_def(16758)->
	[p_dr_dojang_replay_info,16747,16747,int64,int32,int32,int32];
decode_def(16755)->
	[sc_dojangrank_world_replay_list,{list, 16758}];
decode_def(16756)->
	[cs_dojangrank_world_replay_detail,int64,int32];
decode_def(16757)->
	[sc_dojangrank_world_replay_detail,int8,{list, 20002}];
decode_def(16759)->
	[cs_dojangrank_world_ger_view_other,int32,int32,int32];
decode_def(16760)->
	[sc_dojangrank_world_ger_view_other,16747,{list, 16765}];
decode_def(16763)->
	[cs_dojangrank_world_top_replay_list,int32];
decode_def(16764)->
	[sc_dojangrank_world_top_replay_list,{list, 16758}];
decode_def(16601)->
	[cs_trainerProf_info];
decode_def(16603)->
	[p_trainerProf_unit,int8,int8];
decode_def(16602)->
	[sc_trainerProf_info,{list, 16603}];
decode_def(16604)->
	[cs_trainerProf_uplevel,int8];
decode_def(16605)->
	[sc_trainerProf_uplevel,int8,int8,int8];
decode_def(16651)->
	[cs_trainerProf_battle_info];
decode_def(16653)->
	[p_trainerProf_battle_unit,int8,int16];
decode_def(16652)->
	[sc_trainerProf_battle_info,{list, 16653}];
decode_def(16654)->
	[cs_trainerProf_battle_uplevel,int8];
decode_def(16655)->
	[sc_trainerProf_battle_uplevel,int8,int8];
decode_def(16656)->
	[cs_trainerProf_battle_unclock,int8];
decode_def(16657)->
	[sc_trainerProf_battle_unclock,int8,int8];
decode_def(16501)->
	[cs_treasurebowl_info];
decode_def(16504)->
	[p_treasurebowl_draw,int32,int8,int8,11930];
decode_def(16503)->
	[p_treasurebowl_activity,int32,int8,{list, 16504},int8,int32];
decode_def(16502)->
	[sc_treasurebowl_info,int8,{list, 16503},int8,{list, int32},int32,int32,string];
decode_def(16505)->
	[sc_treasurebowl_update,int8];
decode_def(16506)->
	[cs_treasurebowl_exchange,int32];
decode_def(16507)->
	[sc_treasurebowl_exchange,int8];
decode_def(16508)->
	[cs_treasurebowl_draw,int32,int32];
decode_def(16509)->
	[sc_treasurebowl_draw,int8,{list, 11930}];
decode_def(16510)->
	[sc_treasurebowl_open,int8];
decode_def(16401)->
	[cs_maintask_info];
decode_def(15810)->
	[p_task_unit,int32,int64,int8];
decode_def(16402)->
	[sc_maintask_info,15810];
decode_def(16403)->
	[cs_maintask_draw,int32];
decode_def(16404)->
	[sc_maintask_draw,int8,{list, 12005}];
decode_def(16301)->
	[cs_xbattle_info];
decode_def(16302)->
	[sc_xbattle_info,int16,int32,int32,int8,int32,int32,int16];
decode_def(16303)->
	[cs_xbattle_challenge,int16];
decode_def(16304)->
	[sc_xbattle_challenge,int8,{list, 11930},int16,{list, 20002}];
decode_def(16305)->
	[cs_xbattle_raid,int16,int16];
decode_def(16320)->
	[p_xbattle_raid,int8,{list, 11930},int16];
decode_def(16306)->
	[sc_xbattle_raid,int8,{list, 16320}];
decode_def(16307)->
	[cs_xbattle_offline_reward];
decode_def(16308)->
	[sc_xbattle_offline_reward,int32,{list, 11930},int16];
decode_def(16309)->
	[sc_xbattle_tri,int16,{list, 11930}];
decode_def(16310)->
	[cs_xbattle_use_elixir,int64,int8];
decode_def(16311)->
	[sc_xbattle_use_elixir,int8,int16,{list, 11930}];
decode_def(16312)->
	[cs_xbattle_buy_quick];
decode_def(16313)->
	[sc_xbattle_buy_quick,int8,int32,int32,{list, 11930}];
decode_def(16314)->
	[cs_xbattle_get_pass_reward];
decode_def(16315)->
	[sc_xbattle_get_pass_reward,int8,{list, 11930},int16];
decode_def(16316)->
	[cs_xbattle_chapter_info,int16];
decode_def(16323)->
	[p_xbattle_dungeon,int16,int8];
decode_def(16317)->
	[sc_xbattle_chapter_info,int8,{list, 16323},int32,int32];
decode_def(16318)->
	[cs_xbattle_set_reward_chapter,int16];
decode_def(16319)->
	[sc_xbattle_set_reward_chapter,int8];
decode_def(16321)->
	[cs_xbattle_start_reward];
decode_def(16322)->
	[sc_xbattle_start_reward,int8];
decode_def(16324)->
	[cs_xbattle_challenge_boss];
decode_def(16325)->
	[sc_xbattle_challenge_boss,int8,{list, 20002}];
decode_def(16398)->
	[cs_xbattle_package_full];
decode_def(16399)->
	[cs_xbattle_package_ok];
decode_def(16201)->
	[cs_exBoss_get_info];
decode_def(16204)->
	[p_exBoss_dtl,int16,int16,int8,int64,int64,11930];
decode_def(16203)->
	[p_exBoss_times_dtl,int16,int16,int32,int8];
decode_def(16205)->
	[p_exBoss_hit_list,int16,int64];
decode_def(16202)->
	[sc_exBoss_get_info,16204,{list, 16205},16203,int16,int16,int64];
decode_def(16206)->
	[cs_exBoss_hit];
decode_def(16207)->
	[sc_exBoss_hit,int8];
decode_def(16208)->
	[cs_exBoss_buy_times];
decode_def(16209)->
	[sc_exBoss_buy_times,int8,16203];
decode_def(16210)->
	[cs_exBoss_get_reward];
decode_def(16211)->
	[sc_exBoss_get_reward,int8,{list, 11930},{list, 16202}];
decode_def(16212)->
	[sc_exBoss_update_times,int16];
decode_def(16213)->
	[cs_exBoss_refresh];
decode_def(16214)->
	[sc_exBoss_refresh,int8,{list, 16202}];
decode_def(16215)->
	[cs_exBoss_buy_cost];
decode_def(16216)->
	[sc_exBoss_buy_cost,int16,int16,int16,int16];
decode_def(16217)->
	[cs_exBoss_oneKey];
decode_def(16218)->
	[sc_exBoss_oneKey,int8];
decode_def(16101)->
	[cs_tasklink_get_info];
decode_def(16133)->
	[p_level_info,int32,int32,{list, 12005}];
decode_def(16102)->
	[sc_tasklink_get_info,int32,int32,int32,int32,{list, 16133},int32,int32,int32];
decode_def(16103)->
	[cs_tasklink_get_progress];
decode_def(16131)->
	[p_point_info,int32,int32,int32,{list, int32}];
decode_def(16104)->
	[sc_tasklink_get_progress,int32,int32,int32,int32,{list, 16131},int32,{list, 21001},int32];
decode_def(16105)->
	[cs_tasklink_get_reward_log];
decode_def(16132)->
	[p_reward_log,int32,{list, string},int32,{list, 12005}];
decode_def(16106)->
	[sc_tasklink_get_reward_log,{list, 16132}];
decode_def(16107)->
	[cs_tasklink_sign,int32];
decode_def(16108)->
	[sc_tasklink_sign,int32,string];
decode_def(16109)->
	[cs_tasklink_buy_time];
decode_def(16110)->
	[sc_tasklink_buy_time,int32,int32];
decode_def(16111)->
	[cs_tasklink_ready_notice];
decode_def(16112)->
	[sc_tasklink_ready_notice,{list, 21001},int32,int32];
decode_def(16113)->
	[cs_tasklink_ready_opt,int32];
decode_def(16114)->
	[sc_tasklink_ready_opt,int32];
decode_def(16115)->
	[cs_tasklink_get_reward,int32];
decode_def(16116)->
	[sc_tasklink_get_reward,int32,{list, 12005}];
decode_def(16001)->
	[cs_activityFestival_info];
decode_def(16003)->
	[p_activityFestival_data,int8,11930,int16];
decode_def(16005)->
	[p_activityFestival_box,int8,int8,11930];
decode_def(16002)->
	[sc_activityFestival_info,int32,int32,int16,int16,int8,{list, 16003},{list, 16005}];
decode_def(16004)->
	[p_activityFestival_self,int8,int8];
decode_def(16006)->
	[p_activityFestival_box_get,int8,int8];
decode_def(16007)->
	[cs_activityFestival_self];
decode_def(16008)->
	[sc_activityFestival_self,int16,{list, 16004},{list, 16006}];
decode_def(16009)->
	[cs_activityFestival_sign];
decode_def(16010)->
	[sc_activityFestival_sign,int8,{list, 11930}];
decode_def(16011)->
	[cs_activityFestival_box_get,int8];
decode_def(16012)->
	[sc_activityFestival_box_get,int8,{list, 11930}];
decode_def(16013)->
	[cs_activityFestival_sign2,int8];
decode_def(16014)->
	[sc_activityFestival_sign2,int8,{list, 11930}];
decode_def(15901)->
	[cs_familycross_info];
decode_def(15916)->
	[p_familycross_enermy,int16,string];
decode_def(15905)->
	[p_familycross_info_dtl,int8,int8,int8,int32,int32,int32,int32,{list, 15916}];
decode_def(15902)->
	[sc_familycross_info,int8,{list, 15905}];
decode_def(15903)->
	[cs_familycross_sign];
decode_def(15904)->
	[sc_familycross_sign,int8];
decode_def(15906)->
	[p_familycross_car,int32,int32,int32,int32,int8,int8];
decode_def(15907)->
	[cs_familycross_displayer,{list, 15906}];
decode_def(15908)->
	[sc_familycross_displayer,int8];
decode_def(15909)->
	[cs_familycross_displayer_info];
decode_def(15910)->
	[sc_familycross_displayer_info,{list, 15906}];
decode_def(15911)->
	[p_familycross_player_fly,int32,int8];
decode_def(15912)->
	[cs_familycross_player_fly];
decode_def(15913)->
	[sc_familycross_player_fly,{list, 15911}];
decode_def(15914)->
	[cs_familycross_enermy_info];
decode_def(15915)->
	[sc_familycross_enermy_info,string,string,int16,int16];
decode_def(15920)->
	[p_familycross_war_site3,int8,int8,int8];
decode_def(15921)->
	[p_city_pos,int32,int32];
decode_def(15922)->
	[p_site_pos,int8,int32,int32];
decode_def(15923)->
	[p_born_pos,int8,int8,int32,int32];
decode_def(15924)->
	[p_familycross_war_car,int8,int8,15921,int8,int32,int32,{list, int32},int32];
decode_def(15953)->
	[p_familycross_fighter,int16,int8,int64,int64];
decode_def(15925)->
	[p_familycross_war_fly,int8,int8,int8,int8,15922,int8,int32,int32,int32,int32,{list, 15953},string,int32,int16];
decode_def(15928)->
	[p_familycross_war_fly2,int32,int32];
decode_def(15926)->
	[p_familycross_war_site,int8,int8,15922,{list, 15928},{list, 15928},{list, 15928},int8,int32];
decode_def(15927)->
	[p_familycross_war_city,int8,15921,{list, 15920}];
decode_def(15929)->
	[p_familycross_war_car2,int8,int8];
decode_def(15930)->
	[p_familycross_war_family_dtl,int32,int8,string];
decode_def(15931)->
	[cs_familycross_war_info];
decode_def(15978)->
	[p_familycross_city_des,int8,int16,int16,int16];
decode_def(15976)->
	[p_familycross_head_des,int32,int16,int16,int32];
decode_def(15932)->
	[sc_familycross_war_info,int32,int32,int32,{list, 15930},{list, 15927},{list, 15924},{list, 15923},{list, 15976},{list, 15978}];
decode_def(15933)->
	[cs_familycross_self_info];
decode_def(15934)->
	[sc_familycross_self_info,15925];
decode_def(15935)->
	[cs_familycross_drive_car,int8,int8];
decode_def(15936)->
	[sc_familycross_drive_car,int8];
decode_def(15937)->
	[cs_familycross_mov,int8,int8];
decode_def(15938)->
	[sc_familycross_mov,int8];
decode_def(15939)->
	[cs_familycross_attack,int8,int8,int32,int32];
decode_def(15940)->
	[sc_familycross_attack,int8,{list, 20002}];
decode_def(15941)->
	[cs_familycross_city_dtl,int8];
decode_def(15942)->
	[sc_familycross_city_dtl,int8,15921,{list, 15929},{list, 15925},{list, 15926},{list, 15923}];
decode_def(15943)->
	[cs_familycross_site_dtl,int8,int8];
decode_def(15944)->
	[sc_familycross_site_dtl,int8,15922,{list, 15925},int8,int32];
decode_def(15945)->
	[cs_familycross_fly_dtl,int32,int32];
decode_def(15962)->
	[p_familycross_replay_dtl,int8,int64,string,string];
decode_def(15946)->
	[sc_familycross_fly_dtl,15925,{list, 15962},int16];
decode_def(15947)->
	[sc_familycross_map_update,{list, 15924},{list, 15920}];
decode_def(15948)->
	[sc_familycross_city_update,{list, 15925},{list, 15926}];
decode_def(15949)->
	[sc_familycross_site_update,{list, 15926}];
decode_def(15950)->
	[sc_familycross_fly_update,{list, 15925}];
decode_def(15951)->
	[cs_familycross_be_driver,int8,int8];
decode_def(15952)->
	[sc_familycross_be_driver,int8];
decode_def(15954)->
	[cs_familycross_drive_car_stop,int8];
decode_def(15955)->
	[sc_familycross_drive_car_stop,int8];
decode_def(15956)->
	[cs_familycross_mov_stop];
decode_def(15957)->
	[sc_familycross_mov_stop,int8];
decode_def(15958)->
	[cs_familycross_mov_back,int8,int8];
decode_def(15959)->
	[sc_familycross_mov_back,int8];
decode_def(15960)->
	[cs_familycross_reborn];
decode_def(15961)->
	[sc_familycross_reborn,int8];
decode_def(15963)->
	[cs_familycross_replay,int64];
decode_def(15964)->
	[sc_familycross_replay,int8,{list, 20002}];
decode_def(15965)->
	[sc_familycross_attack_update,{list, 15925}];
decode_def(15966)->
	[cs_familycross_own_site,int8,int8];
decode_def(15967)->
	[sc_familycross_own_site,int8,int32];
decode_def(15968)->
	[cs_familycross_season_rank,int32,int16];
decode_def(15970)->
	[p_anubis_family,int32,string,int16,string,int32,int32];
decode_def(15969)->
	[sc_familycross_season_rank,int8,int32,int16,int8,{list, 15970},{list, 15970}];
decode_def(15971)->
	[cs_familycross_seasoninfo];
decode_def(15972)->
	[sc_familycross_seasoninfo,int8,int32,{list, int32}];
decode_def(15973)->
	[cs_familycross_family_rank,int32];
decode_def(15975)->
	[p_anubis_family_member,int32,string,int32,int32,int16];
decode_def(15974)->
	[sc_familycross_family_rank,int8,{list, 15975}];
decode_def(15977)->
	[sc_familycross_des_update,{list, 15976}];
decode_def(15979)->
	[p_familycross_battle_rank_dtl,int32,int32,int16,int16,int16,int16,string,int8];
decode_def(15980)->
	[cs_familycross_battle_get_rank];
decode_def(15981)->
	[sc_familycross_battle_get_rank,{list, 15979}];
decode_def(15982)->
	[sc_familycross_battle_end,{list, 15979}];
decode_def(15983)->
	[cs_familycross_be_driver2,int8,int8];
decode_def(15984)->
	[sc_familycross_be_driver2,int8];
decode_def(15985)->
	[cs_familycross_self_car];
decode_def(15986)->
	[sc_familycross_self_car,int8,{list, 15924},{list, 15925}];
decode_def(15801)->
	[cs_payGuide_info];
decode_def(15803)->
	[p_payGuide_unit,int16,int16,int16,int8,int8,int16,{list, 11930},{list, 11930}];
decode_def(15802)->
	[sc_payGuide_info,int16,15803];
decode_def(15804)->
	[cs_payGuide_get_reward,int16];
decode_def(15805)->
	[sc_payGuide_get_reward,int8,{list, 11930}];
decode_def(15806)->
	[cs_payGuide_seven_info];
decode_def(15814)->
	[period_state,int8,int8];
decode_def(15807)->
	[sc_payGuide_seven_info,int8,int32,int32,int32,int8,{list, 15814},int8];
decode_def(15808)->
	[cs_payGuide_seven_period_info,int8];
decode_def(15809)->
	[sc_payGuide_seven_period_info,int8,int8,{list, 15810}];
decode_def(15811)->
	[cs_payGuide_seven_draw,int32];
decode_def(15812)->
	[sc_payGuide_seven_draw,int8,{list, 12005}];
decode_def(15813)->
	[sc_payGuide_seven_task_update,{list, 15810}];
decode_def(15815)->
	[sc_payGuide_seven_period_summary,{list, 15814}];
decode_def(15701)->
	[cs_homeBoss_info,int32];
decode_def(15703)->
	[p_homeBoss,int32,int32,int32,int8,int8,11930,int8,int8,int64,int64,string];
decode_def(15702)->
	[sc_homeBoss_info,15703,15703];
decode_def(15704)->
	[p_boss,int8,int64,int64];
decode_def(15705)->
	[cs_homeBoss_attack,int32,int8];
decode_def(15706)->
	[sc_homeBoss_attack,int8,{list, 20005},{list, 15703},{list, 11930}];
decode_def(15707)->
	[cs_homeBoss_self];
decode_def(15708)->
	[sc_homeBoss_self,int8,int8,{list, int16}];
decode_def(15709)->
	[cs_homeBoss_buy_times,int8];
decode_def(15710)->
	[sc_homeBoss_buy_times,int8,int8];
decode_def(15711)->
	[cs_homeBoss_get_reward];
decode_def(15712)->
	[sc_homeBoss_get_reward,int8];
decode_def(15713)->
	[cs_homeBoss_read,int32];
decode_def(15714)->
	[sc_homeBoss_read];
decode_def(15601)->
	[p_lvlSgAttr_attr,int32,int64,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int16,int16,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32];
decode_def(15602)->
	[sc_lvlSgAttr_inc,int8,int16,15601];
decode_def(15501)->
	[cs_galactica_info];
decode_def(15502)->
	[sc_galactica_info,int16,int16,int16,int16,{list, int32},int32,int8,int32];
decode_def(15503)->
	[cs_galactica_sign];
decode_def(15504)->
	[sc_galactica_sign,int8,int32];
decode_def(15505)->
	[cs_galactica_buy];
decode_def(15506)->
	[sc_galactica_buy,int8,int16,int16];
decode_def(15507)->
	[sc_galactica_times_update,int16];
decode_def(15508)->
	[p_galactica_p_s,int32,int16];
decode_def(15509)->
	[p_galactica_pos,int16,int16];
decode_def(15510)->
	[p_galactica_mine,int8,int8,int8,15509,{list, 15508},{list, 15508},int32,int32,{list, 15508},{list, 15508}];
decode_def(15511)->
	[p_galactica_mine_s,int8,int8,15509,{list, 15508},{list, 15508},int32,{list, 15508},{list, 15508}];
decode_def(15512)->
	[p_galactica_player,int32,int16,int8,int8,int8,int8,int32,string,int64,int16,15509,int32,int16];
decode_def(15513)->
	[p_galactica_player_s,int32,int16,int8,int8,15509,int32,int32];
decode_def(15514)->
	[p_galactica_fairy,int32,int32,int32,int8];
decode_def(15515)->
	[p_galactica_home,int8,int8,{list, int8},15509];
decode_def(15516)->
	[sc_galactica_gas,int32,int32];
decode_def(15518)->
	[cs_galactica_war_base_info];
decode_def(15519)->
	[sc_galactica_war_base_info,int8,int32,15509,15509,{list, 15510},{list, 15512},{list, 15515},int32,int32];
decode_def(15520)->
	[cs_galactica_self];
decode_def(15521)->
	[sc_galactica_self,15513];
decode_def(15522)->
	[cs_galactica_mov,int8];
decode_def(15523)->
	[sc_galactica_mov,int8];
decode_def(15524)->
	[cs_galactica_attack,int32,int32,int8];
decode_def(15525)->
	[sc_galactica_attack,int8,{list, 20002}];
decode_def(15527)->
	[p_galactica_home_s,int8,{list, int8}];
decode_def(15526)->
	[sc_galactica_home_s,{list, 15527}];
decode_def(15528)->
	[sc_galactica_update,{list, 15513},{list, 15511}];
decode_def(15529)->
	[cs_galactica_role_dtl,int32,int32];
decode_def(15535)->
	[p_galactica_replay_dtl,int8,int64,string,string];
decode_def(15530)->
	[sc_galactica_role_dtl,{list, 15514},{list, 15535},int16];
decode_def(15531)->
	[cs_galactica_replay,int64];
decode_def(15532)->
	[sc_galactica_replay,int8,{list, 20002}];
decode_def(15533)->
	[cs_galactica_mov_stop];
decode_def(15534)->
	[sc_galactica_mov_stop,int8];
decode_def(15536)->
	[p_galactica_rank_dtl,int32,int32,int16,int16,int16,string,int8];
decode_def(15537)->
	[p_galactica_talk,int32,string,string,int8];
decode_def(15538)->
	[cs_galactica_talk,string];
decode_def(15539)->
	[cs_galactica_get_talk];
decode_def(15540)->
	[sc_galactica_get_talk,{list, 15537}];
decode_def(15541)->
	[cs_galactica_get_rank];
decode_def(15542)->
	[sc_galactica_get_rank,{list, 15536}];
decode_def(15543)->
	[sc_galactica_talk,15537];
decode_def(15544)->
	[sc_galactica_end_war,int8,int8,{list, 15536}];
decode_def(15545)->
	[cs_galactica_reborn];
decode_def(15546)->
	[sc_galactica_reborn,int8];
decode_def(15547)->
	[cs_galactica_unrequest];
decode_def(15548)->
	[sc_galactica_unrequest,int8];
decode_def(15549)->
	[cs_galactica_mov_in,int8];
decode_def(15550)->
	[sc_galactica_mov_in,int8];
decode_def(15401)->
	[cs_twins_info];
decode_def(15436)->
	[p_twins_rank_dtl,int32,int32,int16,string,int64,int8];
decode_def(15445)->
	[p_twins_box_info,int8,int8,int8];
decode_def(15402)->
	[sc_twins_info,int16,int16,int16,int16,{list, int32},int8,int8,{list, 15445},{list, 15436},int8,int32];
decode_def(15403)->
	[cs_twins_sign,int8];
decode_def(15404)->
	[sc_twins_sign,int8,int32];
decode_def(15405)->
	[cs_twins_buy];
decode_def(15406)->
	[sc_twins_buy,int8,int16,int16];
decode_def(15407)->
	[sc_twins_times_update,int16];
decode_def(15408)->
	[p_twins_p_s,int32,int16];
decode_def(15409)->
	[p_twins_pos,int16,int16];
decode_def(15410)->
	[p_twins_mine,int8,int8,int8,int8,int16,int16,int32,int64,int64,15409,{list, 15408},int8];
decode_def(15411)->
	[p_twins_mine_s,int8,int8,int8,int8,int32,int64,15409,{list, 15408}];
decode_def(15412)->
	[p_twins_player,int32,int16,int8,int8,int8,int8,int32,int64,int64,15409,string,int32,int16];
decode_def(15413)->
	[p_twins_player_s,int32,int16,int8,int8,15409,int32,int64];
decode_def(15414)->
	[p_twins_fairy,int32,int32,int32,int8];
decode_def(15415)->
	[p_twins_home,int8,15409];
decode_def(15416)->
	[cs_twins_unrequest];
decode_def(15417)->
	[sc_twins_unrequest,int8];
decode_def(15418)->
	[cs_twins_war_base_info];
decode_def(15419)->
	[sc_twins_war_base_info,int8,int8,int32,15409,{list, 15415},{list, 15410},{list, 15412}];
decode_def(15420)->
	[cs_twins_self];
decode_def(15421)->
	[sc_twins_self,15413,int16];
decode_def(15422)->
	[cs_twins_mov,int8];
decode_def(15423)->
	[sc_twins_mov,int8];
decode_def(15424)->
	[cs_twins_attack,int8];
decode_def(15425)->
	[sc_twins_attack,int8,{list, 20002},{list, 11930}];
decode_def(15428)->
	[sc_twins_update,{list, 15413},{list, 15411}];
decode_def(15429)->
	[cs_twins_role_dtl,int32,int32];
decode_def(15430)->
	[sc_twins_role_dtl,{list, 15414},int16];
decode_def(15433)->
	[cs_twins_mov_stop];
decode_def(15434)->
	[sc_twins_mov_stop,int8];
decode_def(15437)->
	[p_twins_talk,int32,string,string];
decode_def(15438)->
	[cs_twins_talk,string];
decode_def(15439)->
	[cs_twins_get_talk];
decode_def(15440)->
	[sc_twins_get_talk,{list, 15437}];
decode_def(15441)->
	[cs_twins_get_rank];
decode_def(15442)->
	[sc_twins_get_rank,{list, 15436}];
decode_def(15443)->
	[sc_twins_talk,15437];
decode_def(15444)->
	[sc_twins_end_war,int8,int8,{list, 15436}];
decode_def(15446)->
	[cs_twins_open_reward_box,int8];
decode_def(15447)->
	[sc_twins_open_reward_box,int8,{list, 11930}];
decode_def(15303)->
	[cs_bounty_self_info];
decode_def(15305)->
	[p_bounty_unit,int16,int8,int8,int8];
decode_def(15312)->
	[p_bounty_data,int8,int8,int8,{list, 15305}];
decode_def(15314)->
	[p_bounty_chapter_blood,int16,int64,int64];
decode_def(15304)->
	[sc_bounty_self_info,{list, 15312},{list, int16},int32,{list, 15314}];
decode_def(15306)->
	[cs_bounty_challenge,int16,int8];
decode_def(15307)->
	[sc_bounty_challenge,int8,{list, 20002},15314];
decode_def(15308)->
	[cs_bounty_buy_times,int8,int8];
decode_def(15309)->
	[sc_bounty_buy_times,int8,int8,int32,int32];
decode_def(15310)->
	[cs_bounty_get_reward,int16,int8];
decode_def(15311)->
	[sc_bounty_get_reward,int8,{list, 11930}];
decode_def(15201)->
	[cs_trSpecial_info];
decode_def(15202)->
	[sc_trSpecial_info,int16,int32,11930];
decode_def(15203)->
	[cs_trSpecial_select,int16];
decode_def(15204)->
	[sc_trSpecial_select,int8];
decode_def(15205)->
	[cs_trSpecial_clear];
decode_def(15206)->
	[sc_trSpecial_clear,int8];
decode_def(15207)->
	[sc_trSpecial_fightPower,int64];
decode_def(15101)->
	[cs_tvcard_info];
decode_def(15103)->
	[p_tvcard_info,int8,11930];
decode_def(15102)->
	[sc_tvcard_info,int32,int32,{list, 15103},{list, 11930}];
decode_def(15104)->
	[cs_tvcard_select,int8];
decode_def(15105)->
	[sc_tvcard_select,int8,{list, 15103},15103];
decode_def(15106)->
	[cs_tvcard_rand];
decode_def(15107)->
	[sc_tvcard_rand,{list, 15103}];
decode_def(15001)->
	[cs_magicBook_swallow_ger,int8,int16,int64,int8];
decode_def(15002)->
	[sc_magicBook_swallow_ger,int8];
decode_def(15003)->
	[p_magicBook_summary,int16,int8,int8];
decode_def(15004)->
	[cs_magicBook_summary];
decode_def(15005)->
	[sc_magicBook_summary,{list, 15003}];
decode_def(15006)->
	[cs_magicBook_picture_reward,int8,int8];
decode_def(15007)->
	[sc_magicBook_picture_reward,int8];
decode_def(15008)->
	[cs_magicBook_book_reward,int8];
decode_def(15009)->
	[sc_magicBook_book_reward,int8];
decode_def(15010)->
	[p_magicBook_attr,int32,int64,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int16,int16,int32,int32,int32,int32];
decode_def(15011)->
	[cs_magicBook_book_info,int8];
decode_def(15012)->
	[sc_magicBook_book_info,15010,string];
decode_def(15013)->
	[sc_magicBook_book_detial,15010,string,int8,int8];
decode_def(14901)->
	[cs_luckyRoll_get_list];
decode_def(14904)->
	[p_luckyRoll_card_inner,int8,int8];
decode_def(14906)->
	[p_baseBoxInfo,int8,int8,int32,11930];
decode_def(14905)->
	[p_lucky_role_card_p,int8,int8];
decode_def(14903)->
	[p_luckyRoll_card_outer,int8,int8,int32,int32];
decode_def(14902)->
	[sc_luckyRoll_get_list,int8,{list, 14903},{list, 14904},14905,int8,{list, 14906},int32,int32,int16,int16,int32,int32];
decode_def(14907)->
	[cs_luckyRoll_explore_one];
decode_def(14909)->
	[p_luckyRoll_card_oneTime,{list, 14903},{list, 14903},int8];
decode_def(14908)->
	[sc_luckyRoll_explore_one,int8,int32,{list, 14909}];
decode_def(14911)->
	[cs_luckyRoll_refresh];
decode_def(14912)->
	[sc_luckyRoll_refresh,int8,{list, 14903}];
decode_def(14913)->
	[cs_luckyRoll_open_base_box,int8];
decode_def(14914)->
	[sc_luckyRoll_open_base_box,int8,{list, 14906}];
decode_def(14915)->
	[p_luckyRoll_ranker,int8,int16,int32,string,11930];
decode_def(14916)->
	[cs_luckyRoll_get_rankInfo];
decode_def(14917)->
	[sc_luckyRoll_get_rankInfo,int8,int8,14915,{list, 14915}];
decode_def(14918)->
	[cs_luckyRoll_get_rank_Reward];
decode_def(14919)->
	[sc_luckyRoll_get_rank_Reward,int8,int8,11930];
decode_def(14924)->
	[sc_luckyRoll_change_state];
decode_def(14925)->
	[cs_luckyRoll_explore_ten];
decode_def(14926)->
	[sc_luckyRoll_explore_ten,int8,int32,{list, 14909}];
decode_def(14801)->
	[cs_carlos_sign];
decode_def(14802)->
	[sc_carlos_sign,int8,int32];
decode_def(14803)->
	[cs_carlos_plane_uplevel];
decode_def(14804)->
	[sc_carlos_plane_uplevel,int8,int8];
decode_def(14805)->
	[cs_carlos_buy];
decode_def(14806)->
	[sc_carlos_buy,int8,int16,int16];
decode_def(14807)->
	[cs_carlos_info];
decode_def(14808)->
	[sc_carlos_info,int16,int16,int16,int16,{list, int32},int32,int8,int32];
decode_def(14809)->
	[sc_carlos_times_update,int16];
decode_def(14811)->
	[p_carlos_pos,int16,int16];
decode_def(14812)->
	[p_carlos_mine,int8,int8,int8,int8,int32,int32,int32,int32,int32,14811];
decode_def(14813)->
	[p_carlos_player,int32,int32,14811,14811,int8,int8,int8,int8,int32,string,int32,int64,int16,int16];
decode_def(14814)->
	[p_carlos_fairy,int32,int32,int32,int8];
decode_def(14815)->
	[cs_carlos_war_base_info];
decode_def(14816)->
	[sc_carlos_war_base_info,int8,int32,14811,14811,{list, 14812},{list, 14813}];
decode_def(14817)->
	[sc_carlos_war_update,{list, 14813}];
decode_def(14818)->
	[cs_carlos_mine_detail,int8];
decode_def(14819)->
	[sc_carlos_mine_detail,14812,int8,{list, 14813},{list, 14813}];
decode_def(14820)->
	[cs_carlos_self];
decode_def(14821)->
	[sc_carlos_self,14813];
decode_def(14822)->
	[cs_carlos_mov,int8];
decode_def(14823)->
	[sc_carlos_mov,int8];
decode_def(14824)->
	[cs_carlos_attack,int32,int32,int8];
decode_def(14825)->
	[sc_carlos_attack,int8,{list, 20002}];
decode_def(14826)->
	[cs_carlos_ownMine,int8];
decode_def(14827)->
	[sc_carlos_ownMine,int8,int32];
decode_def(14828)->
	[sc_carlos_update,{list, 14813},{list, 14812}];
decode_def(14829)->
	[cs_carlos_role_dtl,int32,int32];
decode_def(14830)->
	[sc_carlos_role_dtl,{list, 14814},{list, 14835},int16];
decode_def(14831)->
	[cs_carlos_replay,int64];
decode_def(14832)->
	[sc_carlos_replay,int8,{list, 20002}];
decode_def(14833)->
	[cs_carlos_mov_stop];
decode_def(14834)->
	[sc_carlos_mov_stop,int8];
decode_def(14838)->
	[cs_carlos_talk,string];
decode_def(14839)->
	[cs_carlos_get_talk];
decode_def(14840)->
	[sc_carlos_get_talk,{list, 14837}];
decode_def(14841)->
	[cs_carlos_get_rank];
decode_def(14842)->
	[sc_carlos_get_rank,{list, 14836}];
decode_def(14843)->
	[sc_carlos_talk,14837];
decode_def(14844)->
	[sc_carlos_end_war,int8,int8,{list, 14836}];
decode_def(14845)->
	[cs_carlos_reborn];
decode_def(14846)->
	[sc_carlos_reborn,int8];
decode_def(14847)->
	[cs_carlos_unrequest];
decode_def(14848)->
	[sc_carlos_unrequest,int8];
decode_def(14849)->
	[cs_carlos_rank_list,int8];
decode_def(14851)->
	[player_rank_info,int32,string,bool,int32,int8,int16,int64,int32,int32,int32,int32,int32,int8,int8];
decode_def(14850)->
	[sc_carlos_rank_list,int8,int8,int32,int32,{list, 14851},14851];
decode_def(14852)->
	[cs_carlos_season_info];
decode_def(14853)->
	[sc_carlos_season_info,int8,int32];
decode_def(14854)->
	[cs_carlos_plane_select,int8];
decode_def(14855)->
	[sc_carlos_plane_select,int8];
decode_def(14861)->
	[cs_carlos_relic_sign,int8];
decode_def(14862)->
	[sc_carlos_relic_sign,int8];
decode_def(14863)->
	[cs_carlos_relic_info];
decode_def(14864)->
	[sc_carlos_relic_info,int16,int32,int32,int8,int16,int8,{list, string},{list, int32},{list, int8},int8,int32];
decode_def(14865)->
	[cs_carlos_relic_war_base_info];
decode_def(14896)->
	[relic_role_other,int32,int32,int64,int16,bool,int8,int32,int64];
decode_def(14878)->
	[relic_island,int32,int32,int8,int64,int64,14811];
decode_def(14866)->
	[sc_carlos_relic_war_base_info,int8,int32,{list, 14878},{list, 14813},{list, 14896},int32,int16,int16];
decode_def(14867)->
	[sc_carlos_relic_war_update,{list, 14813},int16,int16];
decode_def(14868)->
	[cs_carlos_relic_mov,int32];
decode_def(14869)->
	[sc_carlos_relic_mov,int8];
decode_def(14870)->
	[cs_carlos_relic_mov_stop];
decode_def(14871)->
	[sc_carlos_relic_mov_stop,int8];
decode_def(14872)->
	[cs_carlos_relic_attack,int32];
decode_def(14873)->
	[sc_carlos_relic_attack,int8,{list, 20002},14878,11930,int8];
decode_def(14874)->
	[cs_carlos_relic_active,int32];
decode_def(14875)->
	[sc_carlos_relic_active,int8,14878];
decode_def(14876)->
	[cs_carlos_relic_open_reward_box,int8];
decode_def(14877)->
	[sc_carlos_relic_open_reward_box,int8,{list, 11930}];
decode_def(14879)->
	[cs_carlos_relic_buy];
decode_def(14880)->
	[sc_carlos_relic_buy,int8,int16,int16,int32];
decode_def(14881)->
	[cs_carlos_relic_sign_cancel];
decode_def(14882)->
	[sc_carlos_relic_sign_cancel,int8];
decode_def(14883)->
	[cs_carlos_relic_self];
decode_def(14884)->
	[sc_carlos_relic_self,14813];
decode_def(14885)->
	[cs_carlos_relic_role_dtl,int32,int32];
decode_def(14886)->
	[sc_carlos_relic_role_dtl,{list, 14814},int8,int16];
decode_def(14887)->
	[sc_carlos_relic_times_update,int16];
decode_def(14888)->
	[sc_carlos_relic_end_war,int8,int8,{list, string},{list, int32},{list, 14896}];
decode_def(14889)->
	[cs_carlos_relic_island_detail,int32];
decode_def(14890)->
	[sc_carlos_relic_island_detail,14878,{list, 14813}];
decode_def(14891)->
	[cs_carlos_relic_talk,string];
decode_def(14892)->
	[cs_carlos_relic_get_talk];
decode_def(14893)->
	[sc_carlos_relic_get_talk,{list, 14837}];
decode_def(14894)->
	[sc_carlos_relic_talk,14837];
decode_def(14895)->
	[sc_carlos_relic_update,{list, 14813},{list, 14878},int32,int16,int16];
decode_def(14897)->
	[cs_carlos_change_plane,int64];
decode_def(14898)->
	[sc_carlos_change_plane,int8];
decode_def(14860)->
	[p_carlos_plane_dtl,int8,int32];
decode_def(14899)->
	[sc_carlos_plane_use_info,{list, 14860},int8];
decode_def(14701)->
	[cs_monthVIP_info];
decode_def(14702)->
	[sc_monthVIP_info,int16,int16,int32,int32,int32,int32,int32,int32,int32,int32,int8,int8,int16,int16,int16,11930,11930,int8,int8];
decode_def(14703)->
	[sc_monthVip_success,int8,int16,11930];
decode_def(14704)->
	[cs_monthVIP_get_reward,int8];
decode_def(14705)->
	[sc_monthVIP_get_reward,int8,int16];
decode_def(14706)->
	[cs_monthVIP_buy,int8];
decode_def(14707)->
	[sc_monthVIP_buy,int8,int16,{list, 11930}];
decode_def(14708)->
	[cs_monthVIP_get_growth_fund_info];
decode_def(14714)->
	[p_growth_fund_state,int32,int8,11930];
decode_def(14709)->
	[sc_monthVIP_get_growth_fund_info,int8,{list, 14714}];
decode_def(14710)->
	[cs_monthVIP_buy_growth_fund];
decode_def(14711)->
	[sc_monthVIP_buy_growth_fund,int8];
decode_def(14712)->
	[cs_monthVIP_get_growth_reward,int16];
decode_def(14713)->
	[sc_monthVIP_get_growth_reward,int8];
decode_def(14601)->
	[cs_talent_get_info];
decode_def(14603)->
	[p_talent,int32,int32];
decode_def(14602)->
	[sc_talent_get_info,{list, 14603},int16,int32];
decode_def(14604)->
	[cs_talent_study,int32];
decode_def(14605)->
	[sc_talent_study,int8,14603,int32,int16];
decode_def(14606)->
	[cs_talent_undo,int32];
decode_def(14607)->
	[sc_talent_undo,int8];
decode_def(14608)->
	[cs_talent_cooldown,int32];
decode_def(14609)->
	[sc_talent_cooldown,int8];
decode_def(14501)->
	[cs_trumpet_message,int8,int16,string,bool];
decode_def(14502)->
	[sc_trumpet_message,int8,int8,int32];
decode_def(14503)->
	[sc_trumpet_message_info,int8,int16,string,int16,string,int8,int64,int32,int32,bool,int32];
decode_def(14504)->
	[cs_trumpet_recent_list,int8];
decode_def(14509)->
	[sc_trumpet_recent_list,int8,{list, 14503}];
decode_def(14510)->
	[cs_trumpet_get_bonus,int8,int32];
decode_def(14514)->
	[p_get_info,int16,int32,string,int32];
decode_def(14513)->
	[p_bonus_info,int32,int32,int16,int16,int32,{list, 14514}];
decode_def(14511)->
	[sc_trumpet_get_bonus_failed,int8,int32,int8,14513];
decode_def(14512)->
	[sc_trumpet_get_bonus,int8,int32,int32,14513];
decode_def(14521)->
	[cs_trumpet_redpacket_status];
decode_def(14522)->
	[sc_trumpet_redpacket_status,int32,int32,int32,int8,int32];
decode_def(14523)->
	[sc_trumpet_new_redpacket_status,string,int32];
decode_def(14524)->
	[cs_trumpet_get_all_publishers];
decode_def(14531)->
	[redpacket_info,int8,int32,int32];
decode_def(14530)->
	[p_publisher_info,int32,string,int32,int8,bool,int32,20913,int32,{list, 14531},int32,int32];
decode_def(14525)->
	[sc_trumpet_get_all_publishers,{list, 14530}];
decode_def(14526)->
	[sc_trumpet_notice_publisher,int8,14530];
decode_def(14527)->
	[cs_trumpet_redpacket_openclose,int32];
decode_def(14532)->
	[sc_trumpet_redpacket_openclose,int32];
decode_def(14528)->
	[cs_trumpet_redpacket_get_reward,int32,int8];
decode_def(14529)->
	[sc_trumpet_redpacket_get_reward,int32,int32];
decode_def(14533)->
	[cs_trumpet_get_world_publishers];
decode_def(14534)->
	[sc_trumpet_get_world_publishers,int32,int32,{list, 14530}];
decode_def(14400)->
	[cs_changename,int8,string];
decode_def(14401)->
	[sc_changename,int8,int8];
decode_def(14402)->
	[cs_changename_freetimes,int8];
decode_def(14403)->
	[sc_changename_freetimes,int8,int8,{list, int16}];
decode_def(14301)->
	[cs_familyBoss_base_info];
decode_def(14303)->
	[p_family_boss_base_info,int8,int8,int8,int8,int16,int16,int32,int64,int64,int8,int8,int32,int32];
decode_def(14302)->
	[sc_familyBoss_base_info,int8,{list, 14303}];
decode_def(14304)->
	[cs_familyBoss_attack,int8];
decode_def(14305)->
	[sc_familyBoss_attack,int8,{list, 20002}];
decode_def(14306)->
	[cs_familyBoss_hatch_egg,int8];
decode_def(14307)->
	[sc_familyBoss_hatch_egg,int8];
decode_def(14308)->
	[cs_familyBoss_feed_boss,int8];
decode_def(14309)->
	[sc_familyBoss_feed_boss,int8];
decode_def(14310)->
	[cs_familyBoss_set_boss_time,int8,int32];
decode_def(14311)->
	[sc_familyBoss_set_boss_time,int8];
decode_def(14312)->
	[sc_familyBoss_boss_be_boss,int8];
decode_def(14313)->
	[sc_familyBoss_boss_born,int8,int64,int64];
decode_def(14314)->
	[cs_familyBoss_get_rank,int8];
decode_def(14316)->
	[p_family_boss_ranker,int32,int8,int64,string];
decode_def(14315)->
	[sc_familyBoss_get_rank,int8,int8,{list, 14316}];
decode_def(14317)->
	[sc_familyBoss_bc_attack,string,int64,int8];
decode_def(14318)->
	[sc_familyBoss_boss_dead,int8,int8];
decode_def(14319)->
	[sc_familyBoss_bc_set_boss_time,int8,int32];
decode_def(14320)->
	[sc_familyBoss_boss_unlock,int8];
decode_def(14201)->
	[cs_familyTek_info];
decode_def(14213)->
	[p_ger_view2,int32,int16,int16,int16];
decode_def(14212)->
	[p_reward_info2,int32,int32,int32,int32,int32,{list, 10616},{list, 14213}];
decode_def(14203)->
	[p_familyTekDtl,int32,int16,int8,14212,int8];
decode_def(14202)->
	[sc_familyTek_info,{list, 14203}];
decode_def(14204)->
	[cs_familyTek_upLevel,int32];
decode_def(14205)->
	[sc_familyTek_upLevel,int8,int32,int16];
decode_def(14206)->
	[cs_familyTek_cost,int32,int16];
decode_def(14207)->
	[sc_familyTek_cost,int32,int16,12005];
decode_def(14208)->
	[cs_familyTek_wallet,int32];
decode_def(14209)->
	[sc_familyTek_wallet,14212];
decode_def(14210)->
	[p_familyTek_Ger_attr,int32,int64,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int16];
decode_def(14211)->
	[p_familyTek_Generate_buff,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32,int32];
decode_def(14214)->
	[cs_familyTek_donate,int8,int32,int32,int32,int16];
decode_def(14215)->
	[sc_familyTek_donate,int8,int32,int32,int32,int16,14212];
decode_def(14216)->
	[sc_family_levelup,int16];
decode_def(13701)->
	[cs_familyfight_info];
decode_def(13705)->
	[p_familyfight_info_dtl,int8,int8,int16,int16,int32,int32,int32,int32,int32,int32,int16,int16,int16,int16,int16,int32,int16,int16,int16];
decode_def(13702)->
	[sc_familyfight_info,int8,{list, 13705}];
decode_def(13703)->
	[cs_familyfight_sign,{list, int32}];
decode_def(13704)->
	[sc_familyfight_sign,int8];
decode_def(13707)->
	[p_familyfighter_member_info,int32,string,int8,int8,int8,int8,int8,int32,int8,bool,int16,int64];
decode_def(13706)->
	[p_familyfighter_info_dtl,int32,int32,string,int32,int32,{list, 13707}];
decode_def(13708)->
	[cs_familyfight_fighter_info];
decode_def(13709)->
	[sc_familyfight_fighter_info,int8,{list, 13706},{list, 13706}];
decode_def(13710)->
	[cs_familyfight_attack,int32,int32,int32];
decode_def(13718)->
	[p_familyfight_record_dtl,int64,int8,20002,string,string];
decode_def(13711)->
	[sc_familyfight_attack,int8,13718];
decode_def(13712)->
	[cs_familyfight_result];
decode_def(13714)->
	[p_familyfight_result_dtl,int8,int16,int16,int32,int32,int32,string];
decode_def(13713)->
	[sc_familyfight_result,int8,{list, 13714}];
decode_def(13715)->
	[cs_familyfight_get_fight_record_list];
decode_def(13717)->
	[p_familyfight_record_info,int8,int8,string,string,string,int64,int32,int32];
decode_def(13716)->
	[sc_familyfight_get_fight_record_list,int8,{list, 13717}];
decode_def(13719)->
	[cs_familyfight_replay,int64];
decode_def(13720)->
	[sc_familyfight_replay,20002];
decode_def(13721)->
	[cs_familyfight_get_fighter_history,int32];
decode_def(13722)->
	[sc_familyfight_get_fighter_history,{list, 13717}];
decode_def(13723)->
	[sc_familyfight_update_star_info,int32,int32,int8,int16,13717];
decode_def(13724)->
	[sc_familyfight_update_state_info,int8];
decode_def(13725)->
	[cs_familyfight_rankerList];
decode_def(13727)->
	[p_familyfight_ranker,int32,int16,int16,int32,string,string,int64,int16];
decode_def(13726)->
	[sc_familyfight_rankerList,{list, 13727}];
decode_def(13728)->
	[cs_familyfight_instance_open_state];
decode_def(13730)->
	[family_instance_open_state,int32,int32];
decode_def(13729)->
	[sc_familyfight_instance_open_state,{list, 13730},int32,int32,int32,int32,int32,int8];
decode_def(13731)->
	[cs_familyfight_instance_boss_info];
decode_def(13733)->
	[instance_boss_info,int32,int64,int64];
decode_def(13732)->
	[sc_familyfight_instance_boss_info,int32,{list, 13733}];
decode_def(13734)->
	[cs_familyfight_attack_boss,int32];
decode_def(13735)->
	[sc_familyfight_attack_boss,int8,{list, 20002},int8,11930];
decode_def(13736)->
	[cs_familyfight_select_instance,int32];
decode_def(13737)->
	[sc_familyfight_select_instance,int8];
decode_def(13738)->
	[cs_familyfight_instance_reward_info];
decode_def(13745)->
	[instance_damage_info,int32,string,int32,int64];
decode_def(13742)->
	[instance_reward,string,int32,11930,int8];
decode_def(13739)->
	[sc_familyfight_instance_reward_info,int8,{list, 13742},11930,{list, 13745}];
decode_def(13740)->
	[cs_familyfight_instance_get_reward,int32];
decode_def(13741)->
	[sc_familyfight_instance_get_reward,int8,13742];
decode_def(13743)->
	[cs_familyfight_bug_attack_time,int8];
decode_def(13744)->
	[sc_familyfight_bug_attack_time,int8,int32];
decode_def(13746)->
	[cs_familyfight_get_fighter];
decode_def(13747)->
	[sc_familyfight_get_fighter,{list, int32}];
decode_def(13748)->
	[cs_familyfight_select_fighter,{list, int32}];
decode_def(13749)->
	[sc_familyfight_select_fighter,int8];
decode_def(13601)->
	[cs_alien_info];
decode_def(13606)->
	[p_alien_fighter,int32,int64,bool,int8,int32,int16,string,int32,int16,int8,int32,int16,int8];
decode_def(13602)->
	[sc_alien_info,int8,int32,int8,bool,int8,{list, 13606},int32,int16,int8,int8,int16];
decode_def(13603)->
	[sc_alien_sign_info,int8,int16,bool,int32];
decode_def(13604)->
	[cs_alien_first_five,int16];
decode_def(13605)->
	[sc_alien_first_five,{list, 13606}];
decode_def(13607)->
	[cs_alien_kill_num_rank,int8,int8];
decode_def(13609)->
	[p_alien_fighter2,int32,int64,bool,int8,int32,int16,string,int32,int16,int16,int8];
decode_def(13608)->
	[sc_alien_kill_num_rank,{list, 13609}];
decode_def(13610)->
	[cs_alien_kill_continuous_rank,int8,int8];
decode_def(13612)->
	[p_alien_fighter3,int32,int64,bool,int8,int32,int16,string,int32,int16,int16,bool,int8];
decode_def(13611)->
	[sc_alien_kill_continuous_rank,{list, 13612}];
decode_def(13613)->
	[cs_alien_guess_info];
decode_def(13614)->
	[sc_alien_guess_info,int32,bool,int32,int32,{list, int32}];
decode_def(13615)->
	[cs_alien_guess,int32,bool];
decode_def(13616)->
	[sc_alien_guess,int8];
decode_def(13617)->
	[cs_alien_reset];
decode_def(13618)->
	[sc_alien_reset,int8,int32,{list, 13606}];
decode_def(13619)->
	[cs_alien_fight,int32,int16];
decode_def(13620)->
	[sc_alien_fight,int8,{list, 20002},int16,int32,{list, 13606},int16,int16];
decode_def(13621)->
	[cs_alien_sign];
decode_def(13622)->
	[sc_alien_sign,int8];
decode_def(13623)->
	[cs_alien_self_record,int8,int8];
decode_def(13627)->
	[p_alien_self_record,bool,bool,string,int16,int64,int32];
decode_def(13624)->
	[sc_alien_self_record,{list, 13627}];
decode_def(13625)->
	[cs_alien_record,int8,int8];
decode_def(11403)->
	[p_id_num,int16,int32];
decode_def(11404)->
	[p_mail_reward,{list, 11403},{list, 11403}];
decode_def(13662)->
	[p_alien_record3,int8,bool,string,string,int16,int16,int64,int32,11404];
decode_def(13626)->
	[sc_alien_record,{list, 13662}];
decode_def(13628)->
	[p_alien_record,int8,string,string,int16,int16,int64,int32];
decode_def(13629)->
	[sc_alien_update_times,int8,int32];
decode_def(13630)->
	[cs_alien_self_fight_replay,int64];
decode_def(13631)->
	[sc_alien_self_fight_replay,20002];
decode_def(13632)->
	[cs_alien_fight_replay,int64];
decode_def(13633)->
	[sc_alien_fight_repaly,20002];
decode_def(13634)->
	[sc_alien_new_fighter_list,{list, 13606}];
decode_def(13635)->
	[cs_alien_leave];
decode_def(13636)->
	[sc_alien_new_self_record];
decode_def(13637)->
	[cs_alien_view_other,int32];
decode_def(13638)->
	[sc_alien_view_other,int32,string,int16,int64,{list, 10403},int32,string,int16,int64,{list, 10403}];
decode_def(13639)->
	[cs_alien_view_other_dtl,int32];
decode_def(13640)->
	[cs_alien_buy_times,int8];
decode_def(13641)->
	[sc_alien_buy_times,int8,int8];
decode_def(13642)->
	[cs_alien_active];
decode_def(13643)->
	[sc_alien_active,int8];
decode_def(13644)->
	[p_alien_record2,int8,string,string,int16,int16,int64,int32,11404];
decode_def(13645)->
	[p_alien_finals_record,string,int32,string,int32,bool,int64];
decode_def(13646)->
	[p_alien_finals_round_record,int8,{list, 13645}];
decode_def(13647)->
	[cs_alien_finals_records,int8,int8];
decode_def(13648)->
	[sc_alien_finals_records,int8,int8,{list, 13646}];
decode_def(13649)->
	[p_alien_finals_role_info,int32,string,int64,int16,bool,int8,int32,int8];
decode_def(13650)->
	[cs_alien_finals_list,int8,int8];
decode_def(13651)->
	[sc_alien_finals_list,int8,int8,{list, 13649}];
decode_def(13652)->
	[cs_alien_finals_guess,int32,int8];
decode_def(13653)->
	[sc_alien_finals_guess,int8];
decode_def(13654)->
	[cs_alien_finals_fight_replay,int64];
decode_def(13655)->
	[sc_alien_finals_fight_replay,20002];
decode_def(13656)->
	[cs_alien_finals_info];
decode_def(13657)->
	[sc_alien_finals_info,int8,int8,int32];
decode_def(13658)->
	[cs_alien_finals_stake_list];
decode_def(13659)->
	[sc_alien_finals_stake_list,{list, int32}];
decode_def(13660)->
	[cs_alien_finals_self_info];
decode_def(13661)->
	[sc_alien_finals_self_info,int32,int8,int8];
decode_def(13663)->
	[cs_alien_self_rank];
decode_def(13664)->
	[sc_alien_self_rank,bool,int32,int32];
decode_def(13501)->
	[cs_team_pk_info];
decode_def(13503)->
	[p_team_member,int32,int64,bool,int8,int32,int16,string,int8];
decode_def(13502)->
	[sc_team_pk_open,int64,int32,int32,int8,int8,{list, 13503},{list, 13503},int32];
decode_def(13504)->
	[sc_team_pk_close,int64,int32,int32,int32,{list, 13503}];
decode_def(13505)->
	[cs_team_refresh,int8];
decode_def(13506)->
	[sc_team_refresh,int8,int8,{list, 13503}];
decode_def(13507)->
	[cs_team_fight];
decode_def(13509)->
	[p_team_member2,int32,int64,bool,int8,int32,int16,string,int8,bool];
decode_def(13508)->
	[sc_team_fight_result,bool,int32,int32,int32,int32,int32,int8,int8,{list, 13509},{list, 13509},{list, 20002},{list, 13503},{list, 13503}];
decode_def(13510)->
	[cs_team_rank];
decode_def(13512)->
	[p_team_member3,int32,int64,bool,int8,int32,int16,string,int32,int32,int8];
decode_def(13511)->
	[sc_team_rank,int32,{list, 13512}];
decode_def(13513)->
	[cs_team_record];
decode_def(13515)->
	[p_team_record,bool,int32,string,string,{list, int64}];
decode_def(13514)->
	[sc_team_record,{list, 13515}];
decode_def(13516)->
	[cs_team_self_record];
decode_def(13518)->
	[p_team_self_record,int32,bool,int32,int32,int32,{list, string},{list, string},{list, int64}];
decode_def(13517)->
	[sc_team_self_record,{list, 13518}];
decode_def(13519)->
	[cs_team_move,int8,int8];
decode_def(13520)->
	[sc_team_move,int8];
decode_def(13521)->
	[sc_team_pk_not_open,int16];
decode_def(13522)->
	[sc_team_fight_error,int8];
decode_def(13523)->
	[cs_team_fight_replay,{list, int64}];
decode_def(13524)->
	[sc_team_fight_replay,int8,{list, 20002},{list, 13509},{list, 13509}];
decode_def(13525)->
	[cs_team_self_fight_replay,{list, int64}];
decode_def(13526)->
	[sc_team_self_fight_replay,int8,{list, 20002},{list, 13509},{list, 13509}];
decode_def(13527)->
	[cs_team_view_other,int32];
decode_def(13528)->
	[sc_team_view_other,int32,string,int16,int64,{list, 10418}];
decode_def(13529)->
	[cs_team_view_other_dtl,int32];
decode_def(10471)->
	[p_lieu_view,int16,int16,int8,int16];
decode_def(10413)->
	[p_ger_pos,int64,int8];
decode_def(13530)->
	[sc_team_view_other_dtl,int32,string,bool,int16,int64,{list, 10403},{list, 10606},{list, 10413},int16,int16,{list, 10471},int32,int8];
decode_def(13531)->
	[cs_team_new_status];
decode_def(13532)->
	[sc_team_new_status,bool];
decode_def(13402)->
	[p_race_rec,string,string,{list, int64},int32,int32,int64,int64,int8,int8,bool,bool,int8,int8,int32,int32,{list, bool}];
decode_def(13401)->
	[sc_race_new_fight,13402];
decode_def(13403)->
	[p_race_fighter,int32,string,int64,int16,bool,int8,int32];
decode_def(13404)->
	[cs_race_history,int8,int8,int16,int16];
decode_def(13405)->
	[sc_race_history,int8,int8,{list, 13402}];
decode_def(13406)->
	[cs_race_replay,int64];
decode_def(13407)->
	[sc_race_replay,int8,20002];
decode_def(13408)->
	[cs_race_fight_list,int8];
decode_def(13409)->
	[sc_race_fight_list,int8,{list, 13403}];
decode_def(13410)->
	[cs_race_sign];
decode_def(13411)->
	[sc_race_sign,int8];
decode_def(13412)->
	[cs_race_info];
decode_def(13416)->
	[p_race_pos,int32,string,bool,int8,int32,int8];
decode_def(13413)->
	[sc_race_info,int8,int32,int16,bool,{list, 13416},string,int32,int8,bool];
decode_def(13414)->
	[cs_race_enter];
decode_def(13415)->
	[cs_race_leave];
decode_def(13417)->
	[cs_race_pos_history,int8];
decode_def(13418)->
	[sc_race_pos_history,int8,13402];
decode_def(13419)->
	[sc_race_new_first,13416];
decode_def(13420)->
	[sc_race_new_status,int8,int32];
decode_def(13421)->
	[cs_race_is_open];
decode_def(13422)->
	[sc_race_is_open,bool];
decode_def(13423)->
	[cs_race_auto_sign];
decode_def(13424)->
	[sc_race_auto_sign,int8];
decode_def(13425)->
	[cs_race_auto_unsign];
decode_def(13426)->
	[sc_race_auto_unsign,int8];
decode_def(13427)->
	[cs_race_self_history];
decode_def(13428)->
	[sc_race_self_history,{list, 13402}];
decode_def(13429)->
	[cs_race_guess_info];
decode_def(13430)->
	[sc_race_guess_info,int32,int32,{list, int32}];
decode_def(13431)->
	[cs_race_guess,int32,int32];
decode_def(13432)->
	[sc_race_guess,int8];
decode_def(13441)->
	[cs_race2_info];
decode_def(13459)->
	[arena_fighter,int32,string,bool,int8,int32,int64,int8,int32,int32,int16,int32];
decode_def(13442)->
	[sc_race2_info,int8,{list, int32},int8,int8,{list, 13459},int32];
decode_def(13443)->
	[cs_race2_sign,int8];
decode_def(13444)->
	[sc_race2_sign,int8];
decode_def(13445)->
	[cs_race2_openclose,int8];
decode_def(13446)->
	[sc_race2_openclose,int8];
decode_def(13447)->
	[cs_race2_arena_fighter];
decode_def(13448)->
	[sc_race2_arena_fighter,{list, 13459}];
decode_def(13449)->
	[cs_race2_fight,int32,int8];
decode_def(13450)->
	[sc_race2_fight,int8,int8,int8,{list, 20002}];
decode_def(13451)->
	[cs_race2_knockout_fighter,int32];
decode_def(13460)->
	[race2_fight_info,{list, 13459},int32,int8,int8,int32,int32,int32,int8,int8];
decode_def(13452)->
	[sc_race2_knockout_fighter,{list, 13460}];
decode_def(13453)->
	[cs_race2_get_gamble];
decode_def(13461)->
	[gamble_info,int32,int32,int32,int32,int8];
decode_def(13454)->
	[sc_race2_get_gamble,{list, 13461},{list, int32}];
decode_def(13455)->
	[cs_race2_do_gamble,int32,int32,int32];
decode_def(13456)->
	[sc_race2_do_gamble,int8];
decode_def(13457)->
	[cs_race2_final_info];
decode_def(13458)->
	[sc_race2_final_info,{list, 13459}];
decode_def(13462)->
	[cs_race2_get_fight_rec,int32];
decode_def(13463)->
	[sc_race2_get_fight_rec,int32,int8,int8,{list, 20002}];
decode_def(13464)->
	[cs_race2_cancel_sign];
decode_def(13465)->
	[sc_race2_cancel_sign,int8];
decode_def(13302)->
	[p_family_member_info,int32,string,int32,int32,int32,int8,bool,bool,int16,int64,int8,int32,int32,int8,int32,int32];
decode_def(13301)->
	[p_family_info,int32,string,int16,int32,string,int32,string,int16,int32,string,{list, 13302},int32,int32,int32,int32,string,int64,int32,string];
decode_def(13303)->
	[p_family_request,int32,string,int16,int64,int32,int32,bool,int32,int32,int8,bool];
decode_def(13304)->
	[p_family_summary,int32,string,string,int16,int16,int32,string,bool,int32,int32,string,int64,int32,int32,int16];
decode_def(13305)->
	[cs_family_get_list,int8,int16,int16];
decode_def(13306)->
	[sc_family_get_list,int8,int8,{list, 13304}];
decode_def(13307)->
	[cs_family_create,string,bool];
decode_def(13308)->
	[sc_family_create,int8,13301,int32];
decode_def(13309)->
	[cs_family_request_join,int32];
decode_def(13310)->
	[sc_family_request_join,int8,bool,int32,int32];
decode_def(13311)->
	[cs_family_cancel_join,int32];
decode_def(13312)->
	[sc_family_cancel_join,int8,bool,int32];
decode_def(13313)->
	[cs_family_agree_join,int32];
decode_def(13314)->
	[sc_family_agree_join,int8,bool,13301];
decode_def(13315)->
	[cs_family_refuse_join,int32];
decode_def(13316)->
	[sc_family_refuse_join,int8,bool];
decode_def(13317)->
	[cs_family_get_info];
decode_def(13318)->
	[sc_family_get_info,int8,13301,int32];
decode_def(13319)->
	[cs_family_kick,int32];
decode_def(13320)->
	[sc_family_kick,int8,bool,13301];
decode_def(13321)->
	[cs_family_create_consume];
decode_def(13322)->
	[sc_family_create_consume,int32,int32];
decode_def(13323)->
	[cs_family_leave];
decode_def(13324)->
	[sc_family_leave,int8,bool,13301];
decode_def(13325)->
	[cs_family_change_notice,string];
decode_def(13326)->
	[sc_family_change_notice,int8,bool,string];
decode_def(13327)->
	[cs_family_request_list];
decode_def(13328)->
	[sc_family_request_list,int8,{list, 13303}];
decode_def(13329)->
	[sc_family_del_request,int32];
decode_def(13330)->
	[cs_family_get_log_list];
decode_def(13354)->
	[p_family_log_dtl,int32,string,int32,int32,int32,int32,string,int8,bool];
decode_def(13331)->
	[sc_family_get_log_list,int8,{list, 13354}];
decode_def(13332)->
	[cs_family_get_contribute_info];
decode_def(13335)->
	[p_familyContributeLog,string,int16];
decode_def(13334)->
	[p_familyContributeType,int16,int8,int32,int32,int32,11930];
decode_def(13333)->
	[sc_family_get_contribute_info,int8,int32,{list, 13334},{list, 13335}];
decode_def(13336)->
	[cs_family_do_contribute,int16];
decode_def(13337)->
	[sc_family_do_contribute,int8];
decode_def(13338)->
	[sc_family_update_exp,int32,int16];
decode_def(13339)->
	[cs_family_search_by_family_name,string];
decode_def(13340)->
	[sc_family_search_by_family_name,int8,{list, 13304}];
decode_def(13341)->
	[cs_family_change_member_power,int32,int16];
decode_def(13342)->
	[sc_family_change_member_power,int8];
decode_def(13343)->
	[sc_family_update_family_info,13301];
decode_def(13344)->
	[cs_family_send_role_energy,int32];
decode_def(13345)->
	[sc_family_send_role_energy,int8];
decode_def(13346)->
	[cs_family_get_role_energy,string];
decode_def(13347)->
	[sc_family_get_role_energy,int8];
decode_def(13348)->
	[cs_family_get_role_send_energy_list];
decode_def(13349)->
	[sc_family_get_role_send_energy_list,{list, int32}];
decode_def(13350)->
	[cs_family_get_member_power];
decode_def(13351)->
	[sc_family_get_member_power,{list, int8}];
decode_def(13352)->
	[cs_family_get_send_role_energy_list];
decode_def(13353)->
	[sc_family_get_send_role_energy_list,{list, string}];
decode_def(13355)->
	[sc_family_update_contribute_log,{list, 13335}];
decode_def(13356)->
	[p_family_storage,int64,int16,int8,{list, int32}];
decode_def(13357)->
	[cs_family_storage_info];
decode_def(13358)->
	[sc_family_storage_info,int8,int8,int16,int32,{list, 13356}];
decode_def(13359)->
	[cs_family_storage_req,int64];
decode_def(13360)->
	[sc_family_storage_req,int8];
decode_def(13361)->
	[cs_family_storage_assign,int64,int32];
decode_def(13362)->
	[sc_family_storage_assign,int8];
decode_def(13363)->
	[sc_family_storage_update,int64,int8,{list, int32}];
decode_def(13364)->
	[cs_family_owner_impeach];
decode_def(13365)->
	[sc_family_owner_impeach,int8];
decode_def(13366)->
	[cs_family_wallet];
decode_def(13367)->
	[sc_family_wallet,int64];
decode_def(13368)->
	[cs_family_change_slogan,string];
decode_def(13369)->
	[sc_family_change_slogan,int8,bool,string];
decode_def(13370)->
	[cs_family_worship,int8];
decode_def(13371)->
	[sc_family_worship,int8,int8,int32];
decode_def(13372)->
	[cs_family_worship_fight];
decode_def(13373)->
	[sc_family_worship_fight,int8,{list, 20002},int32];
decode_def(13374)->
	[cs_family_worship_info];
decode_def(13375)->
	[sc_family_worship_info,int8,int8,int32];
decode_def(13376)->
	[sc_family_worship_self_push,int8];
decode_def(13377)->
	[sc_family_worship_refresh_fight_reward,int32];
decode_def(13378)->
	[sc_family_worship_family_push,int8,int32];
decode_def(13379)->
	[cs_family_worship_limit];
decode_def(13380)->
	[sc_family_worship_limit,int8,int8,int16];
decode_def(13381)->
	[cs_family_get_member_list,int32];
decode_def(13382)->
	[sc_family_get_member_list,int32,{list, 13302}];
decode_def(13383)->
	[cs_family_get_donate_contribute_list,int32];
decode_def(13386)->
	[p_ger_donate_record_unit,int8,int64];
decode_def(13387)->
	[p_item_donate_record_unit,int8,int64];
decode_def(13385)->
	[p_donate_info,int64,int64,int64,{list, 13386},{list, 13387}];
decode_def(13384)->
	[sc_family_get_donate_contribute_list,int8,int32,13385];
decode_def(13388)->
	[cs_family_impeach_list];
decode_def(13389)->
	[sc_family_impeach_list,int8,{list, int32}];
decode_def(13390)->
	[cs_family_donate_contribution_summary];
decode_def(13392)->
	[p_family_memeber_donate_info_summary,int32,int8,string,bool,int16,int64,int32,int8,int8];
decode_def(13391)->
	[sc_family_donate_contribution_summary,int8,{list, 13392}];
decode_def(13393)->
	[cs_family_invite_request,int32];
decode_def(13394)->
	[sc_family_invite_request,int8];
decode_def(13201)->
	[cs_combine_do,int8,int16,int8,{list, int64}];
decode_def(13202)->
	[sc_combine_fail,int8];
decode_def(13205)->
	[p_newGer,int16,int16,int8];
decode_def(13203)->
	[sc_combine_ger,{list, 13205},{list, int32}];
decode_def(13206)->
	[p_newEquip,int16,int16,int16,int8];
decode_def(13204)->
	[sc_combine_equip,{list, 13206},{list, int32}];
decode_def(13207)->
	[cs_combine_info];
decode_def(13208)->
	[sc_combine_info,int32,string,{list, int8},{list, int8}];
decode_def(13209)->
	[cs_combine_mage_info];
decode_def(13211)->
	[p_mage_config,int16,20307];
decode_def(13210)->
	[sc_combine_mage_info,{list, 13211}];
decode_def(13212)->
	[cs_combine_do_mage,int16,int64];
decode_def(13213)->
	[sc_combine_do_mage,int8,int8,int64,11930];
decode_def(13101)->
	[cs_firecracker_open];
decode_def(13103)->
	[p_discount,int32,int8];
decode_def(13102)->
	[sc_firecracker_open,int8,string,string,string,int32,int32,int32,int32,int8,int8,int32,int8,int8,int32,{list, 13103},int16,int16];
decode_def(13104)->
	[sc_firecracker_info_sync,int32,int8,int32];
decode_def(13105)->
	[cs_firecracker_close];
decode_def(13106)->
	[cs_firecracker_setoff,int8];
decode_def(13107)->
	[sc_firecracker_setoff,int8,int32,int32,int8,{list, 11930}];
decode_def(13108)->
	[cs_firecracker_rank];
decode_def(13110)->
	[p_firecracker_rank,int8,string,int32,{list, 11930},int16];
decode_def(13109)->
	[sc_firecracker_rank,{list, 13110}];
decode_def(13111)->
	[cs_firecracker_get_reward];
decode_def(13112)->
	[sc_firecracker_get_reward,int8,{list, 11930}];
decode_def(13113)->
	[cs_firecracker_rewardList];
decode_def(13114)->
	[sc_firecracker_rewardList,11930];
decode_def(13001)->
	[cs_treaHouse_get_list];
decode_def(13002)->
	[p_treaHouse_card,int8,int8,int8,int8,int32];
decode_def(13022)->
	[p_baseBoxOpenInfo,int8,int8];
decode_def(13003)->
	[sc_treaHouse_get_list,int8,int8,int8,{list, 13022},int32,{list, 13002},int32,int32,int32,int8];
decode_def(13004)->
	[cs_treaHouse_is_open];
decode_def(13005)->
	[sc_treaHouse_is_open,int8,int8,int16];
decode_def(13006)->
	[cs_treaHouse_explore_one];
decode_def(13008)->
	[p_treaHouse_card_oneTime,{list, 13002},{list, 13002}];
decode_def(13007)->
	[sc_treaHouse_explore_one,int8,int32,int8,{list, 13008}];
decode_def(13009)->
	[cs_treaHouse_explore_ten];
decode_def(13010)->
	[sc_treaHouse_explore_ten,int8,int32,int8,int8,{list, 13008}];
decode_def(13011)->
	[cs_treaHouse_refresh];
decode_def(13012)->
	[sc_treaHouse_refresh,int8,{list, 13002}];
decode_def(13013)->
	[cs_treaHouse_open_base_box,int8];
decode_def(13014)->
	[sc_treaHouse_open_base_box,int8,{list, 13022}];
decode_def(13015)->
	[p_treaHouse_ranker,int8,int16,int32,string,11930];
decode_def(13016)->
	[cs_treaHouse_get_rankInfo];
decode_def(13017)->
	[sc_treaHouse_get_rankInfo,int8,int8,13015,{list, 13015}];
decode_def(13018)->
	[cs_treaHouse_get_rank_Reward];
decode_def(13019)->
	[sc_treaHouse_get_rank_Reward,int8,int8,11930];
decode_def(13020)->
	[cs_treaHouse_get_baseBoxRewardInfo];
decode_def(13023)->
	[p_treaHouse_BaseReward_Info,int8,int32,11930];
decode_def(13021)->
	[sc_treaHouse_get_baseBoxRewardInfo,{list, 13023}];
decode_def(13024)->
	[sc_treaHouse_change_state];
decode_def(12902)->
	[p_cross_rec,string,string,{list, int64},int32,int32,int64,int64,int8,bool,bool,int8,int8,int16,int16,{list, bool}];
decode_def(12901)->
	[sc_cross_new_fight,12902];
decode_def(12903)->
	[cs_cross_history,int8,int16,int16];
decode_def(12904)->
	[sc_cross_history,int8,{list, 12902}];
decode_def(12905)->
	[cs_cross_replay,int64,bool];
decode_def(12906)->
	[sc_cross_replay,int8,20002];
decode_def(12907)->
	[sc_cross_fight_list,{list, string},{list, string}];
decode_def(12910)->
	[cs_cross_sign];
decode_def(12911)->
	[sc_cross_sign,int8];
decode_def(12920)->
	[cs_cross_info,int8];
decode_def(12921)->
	[sc_cross_info,int8,int32,int16,bool,{list, 12902},string,int32,int32,int8,bool,bool,{list, string},{list, string}];
decode_def(12922)->
	[cs_cross_enter,int8];
decode_def(12923)->
	[cs_cross_leave];
decode_def(12924)->
	[cs_cross_view_list,int8];
decode_def(12926)->
	[p_view_data,string,int16];
decode_def(12925)->
	[sc_cross_view_list,int8,{list, 12926}];
decode_def(12930)->
	[cs_cross_support,int32,int8];
decode_def(12931)->
	[sc_cross_support,int8,int32];
decode_def(12932)->
	[cs_cross_price_list,int16,int16,int8];
decode_def(12934)->
	[p_price_info,int32,string,bool,int16,int64,int64,int8,int16];
decode_def(12933)->
	[sc_cross_price_list,{list, 12934},int32];
decode_def(12801)->
	[cs_emperor_get_open_time];
decode_def(12802)->
	[sc_emperor_get_open_time,int64];
decode_def(12803)->
	[cs_emperor_enter];
decode_def(12805)->
	[p_emp_fighter,int32,string,int8,int8,int8];
decode_def(12804)->
	[sc_emperor_enter,int8,int8,int8,int8,int16,int32,int32,int32,{list, 12805},int64];
decode_def(12806)->
	[sc_emperor_broadcast_fightInfo,string,string,int8,int8,int8,int64];
decode_def(12807)->
	[cs_emperor_replay,int8];
decode_def(12819)->
	[p_emperor_replayInfo,string,string,int8,int64];
decode_def(12808)->
	[sc_emperor_replay,{list, 12819}];
decode_def(12809)->
	[cs_emperor_quit];
decode_def(12810)->
	[cs_emperor_get_bet_info];
decode_def(12812)->
	[p_bet,string,string,string,int32,int8];
decode_def(12811)->
	[sc_emperor_get_bet_info,int8,{list, 12812}];
decode_def(12813)->
	[cs_emperor_role_bet,int8,int8,int32];
decode_def(12814)->
	[sc_emperor_role_bet,int8];
decode_def(12815)->
	[cs_emperor_bet_info,int8];
decode_def(12816)->
	[sc_emperor_bet_info,int8,int8,int32,int8,int32,int8,int32,int32,int32,int32,int32];
decode_def(12817)->
	[cs_emperor_get_replay,int64];
decode_def(12818)->
	[sc_emperor_get_replay,20002];
decode_def(12820)->
	[sc_emperor_bc_fight_end];
decode_def(12701)->
	[cs_challengeGod_info];
decode_def(12702)->
	[sc_challengeGod_info,int16,int16,int8,int8];
decode_def(12703)->
	[cs_challengeGod_select_ger,int8];
decode_def(12704)->
	[sc_challengeGod_select_ger,int8];
decode_def(12705)->
	[cs_challengeGod_challenge_dungeon_one,int16];
decode_def(10211)->
	[p_ger_add_exp,int8,int32,bool];
decode_def(10210)->
	[p_reward,int32,int32,{list, 10211},int32,{list, 10616},{list, 10418},int32];
decode_def(12706)->
	[sc_challengeGod_challenge_dungeon_one,int8,{list, 20002},{list, 10210}];
decode_def(12707)->
	[cs_challengeGod_challenge_dungeon_ten,int16];
decode_def(12708)->
	[p_challengeGod_result,int8,10210];
decode_def(12709)->
	[sc_challengeGod_challenge_ten,int8,{list, 12708}];
decode_def(12601)->
	[cs_talk_world,int8,string];
decode_def(12602)->
	[sc_talk_world,int8,int8];
decode_def(12603)->
	[sc_talk_world_message,int8,string,string,int8,int64,int32,int8,string,int32,bool,int8,int8,int32];
decode_def(12604)->
	[cs_talk_gag_one,string];
decode_def(12605)->
	[cs_talk_ungag_one,string];
decode_def(12606)->
	[cs_talk_get_gag_list];
decode_def(12616)->
	[gag_info,string,{list, int32}];
decode_def(12607)->
	[sc_talk_get_gag_list,{list, 12616}];
decode_def(12608)->
	[cs_talk_recent_list,int8];
decode_def(12609)->
	[sc_talk_recent_list,int8,{list, 12603}];
decode_def(12610)->
	[cs_talk_send_whisper,int32,string];
decode_def(12611)->
	[sc_talk_send_whisper,int8];
decode_def(12612)->
	[cs_talk_get_whisper];
decode_def(12614)->
	[p_whisper_record,int32,string,int64];
decode_def(12613)->
	[sc_talk_get_whisper,{list, 12614}];
decode_def(12615)->
	[sc_talk_whisper_notice];
decode_def(12501)->
	[sc_push_highlight_Info,int8,int8];
decode_def(12401)->
	[cs_nanm_open];
decode_def(12402)->
	[sc_nanm_open,int8,int32,int64,int16,bool,int16,bool,int32,int8];
decode_def(12403)->
	[sc_nanm_init_state,int64,int64,int32,int32];
decode_def(12404)->
	[cs_nanm_close];
decode_def(12405)->
	[cs_nanm_buff,int8];
decode_def(12406)->
	[sc_nanm_buff,int8,int8];
decode_def(12411)->
	[cs_nanm_last_info,int32];
decode_def(12412)->
	[sc_nanm_last_info_ignore];
decode_def(12415)->
	[p_nanm_info,string,int64];
decode_def(12413)->
	[sc_nanm_last_info_win,int16,int32,int64,{list, 12415},{list, string},int32];
decode_def(12414)->
	[sc_nanm_last_info_fail,int16,int32,int32];
decode_def(12416)->
	[cs_nanm_cur_info];
decode_def(12417)->
	[sc_nanm_cur_info_ignore];
decode_def(12418)->
	[sc_nanm_cur_info,{list, 12415}];
decode_def(12420)->
	[sc_nanm_hp_sync,int64];
decode_def(12422)->
	[p_nanm_harm,string,int64];
decode_def(12421)->
	[sc_nanm_harm_broadcast,{list, 12422}];
decode_def(12424)->
	[sc_nanm_buff_sync,int16];
decode_def(11255)->
	[p_role_stastic,int64,int32,int32];
decode_def(12425)->
	[sc_nanm_stop,int8,11255];
decode_def(12426)->
	[cs_nanm_rank_sync];
decode_def(12427)->
	[sc_nanm_rank_sync,int16];
decode_def(12431)->
	[cs_nanm_fight];
decode_def(12432)->
	[sc_nanm_fight,int8,{list, 20002},int32,int32,int32];
decode_def(12441)->
	[cs_nanm_reborn];
decode_def(12442)->
	[sc_nanm_reborn,int8];
decode_def(12443)->
	[cs_nanm_offline_play,bool];
decode_def(12444)->
	[sc_nanm_offline_play,int8,bool];
decode_def(12450)->
	[cs_nanm_open_time];
decode_def(12451)->
	[sc_nanm_open_time,int32];
decode_def(12452)->
	[cs_nanm_reward];
decode_def(12453)->
	[sc_nanm_reward,int8,{list, 12005}];
decode_def(12301)->
	[cs_version,string];
decode_def(12302)->
	[sc_version,int8];
decode_def(12201)->
	[cs_gift_request,string];
decode_def(12202)->
	[sc_gift_request,int8,{list, 11930}];
decode_def(12101)->
	[cs_king_enter];
decode_def(12102)->
	[sc_king_enter,int32,int8,int8,int16,int32,string,int16,int8,bool,int16];
decode_def(12103)->
	[sc_king_enter_wait_first_session,int32];
decode_def(12106)->
	[cs_king_quit];
decode_def(12110)->
	[cs_king_sign];
decode_def(12111)->
	[sc_king_sign,int8];
decode_def(12120)->
	[cs_king_buff,int8];
decode_def(12121)->
	[sc_king_buff,int8,int8,int8];
decode_def(12131)->
	[p_rec,string,string,int8,int64];
decode_def(12130)->
	[sc_king_new_fight,12131];
decode_def(12140)->
	[cs_king_replay,int64];
decode_def(12141)->
	[sc_king_replay,20002];
decode_def(12150)->
	[cs_king_history,int8,int8,int16,int8];
decode_def(12151)->
	[sc_king_history,{list, 12131}];
decode_def(12160)->
	[cs_king_rank,int8,int16,int8];
decode_def(12162)->
	[p_king_rank,int32,{list, int16},string];
decode_def(12161)->
	[sc_king_rank,int8,{list, 12162}];
decode_def(12001)->
	[cs_box_item,int16];
decode_def(12002)->
	[sc_box_item,int8,{list, 12005},int16];
decode_def(12003)->
	[cs_box_shop,int32,int8];
decode_def(12004)->
	[sc_box_shop,int8,{list, 12005}];
decode_def(12006)->
	[p_reward_view2,int8,int16,int16];
decode_def(12007)->
	[cs_box_shop_info];
decode_def(12009)->
	[p_shop_box_info,int16,int32,int32,int8,int32,int32];
decode_def(12008)->
	[sc_box_shop_info,{list, 12009}];
decode_def(12010)->
	[p_reward_view3,int8,int16,int32];
decode_def(12011)->
	[cs_box_get_spirit_equip_count];
decode_def(12012)->
	[sc_box_get_spirit_equip_count,int32,int32,int32,int32,int32,int32,int32,int32,int32];
decode_def(12013)->
	[cs_box_item_multi,int16,int8];
decode_def(12014)->
	[sc_box_item_multi,int8,{list, 12005},int16];
decode_def(12015)->
	[cs_box_free_info];
decode_def(12016)->
	[sc_box_free_info,int8,int8,int8,int8,int8,int8,int8,int8];
decode_def(12017)->
	[cs_box_free_open,int8];
decode_def(12018)->
	[sc_box_free_open,int8,{list, 12005}];
decode_def(12019)->
	[sc_box_mystery_notice,string,string,{list, 12005}];
decode_def(12020)->
	[cs_box_shopBoxDtl];
decode_def(12021)->
	[sc_box_shopBoxDtl,int32,int32,int32,int32,int32,int32,int32,int32,int32];
decode_def(12022)->
	[cs_box_shop_view,int32,int8];
decode_def(12023)->
	[sc_box_shop_view,int8,{list, 12005}];
decode_def(12024)->
	[cs_box_shop_refresh,int32,int8];
decode_def(12025)->
	[sc_box_shop_refresh,int8,{list, 12005},int32];
decode_def(12026)->
	[cs_box_shop_get,int32,int8];
decode_def(12027)->
	[sc_box_shop_get,int8];
decode_def(12028)->
	[cs_box_free_get,int8];
decode_def(12029)->
	[sc_box_free_get,int8];
decode_def(12030)->
	[cs_box_ticket_shop,int8];
decode_def(12031)->
	[sc_box_ticket_shop,int8,{list, 12005}];
decode_def(12032)->
	[cs_box_ticket_info];
decode_def(12033)->
	[sc_box_ticket_info,int16,int16];
decode_def(11901)->
	[cs_activity_get_list];
decode_def(11903)->
	[p_activity_icon,int16,string,string,int16];
decode_def(11902)->
	[sc_activity_get_list,{list, 11903}];
decode_def(11910)->
	[cs_activity_info,int16];
decode_def(11912)->
	[p_activity_draw,int16,string,int16,int16,int16,11930,{list, 12005},int8,int8];
decode_def(11911)->
	[sc_activity_info,int16,int8,string,{list, 11912},int32,int32,{list, int32},int8,int8];
decode_def(11920)->
	[cs_activity_draw,int16,int16,11930];
decode_def(11921)->
	[sc_activity_draw,int8,int16,int16,int16,int16];
decode_def(11940)->
	[sc_activity_update,int16,int16,int16];
decode_def(11941)->
	[sc_activity_record_update,int16,{list, int32}];
decode_def(11942)->
	[p_energy_activity,int32,int32,int8,int8];
decode_def(11943)->
	[cs_activity_energy];
decode_def(11944)->
	[sc_activity_energy,{list, 11942}];
decode_def(11945)->
	[cs_activity_sign_emperor_info];
decode_def(11946)->
	[sc_activity_sign_emperor_info,int8,int8,int8,int8,string];
decode_def(11947)->
	[cs_activity_sign_get_reward];
decode_def(11948)->
	[sc_activity_sign_get_reward,int8,{list, 12005}];
decode_def(11949)->
	[cs_activity_sign_up];
decode_def(11950)->
	[sc_activity_sign_up,int8,{list, 12005}];
decode_def(11951)->
	[cs_activity_rebate_info];
decode_def(11954)->
	[p_rebate_info,int8,int8,int32];
decode_def(11953)->
	[p_rebate_list,int8,string,int8,int32,int32,int32,{list, 11954}];
decode_def(11952)->
	[sc_rebate_info,int8,string,string,string,int32,int32,{list, 11953}];
decode_def(11955)->
	[cs_activity_rebate_get_reward,int8];
decode_def(11957)->
	[p_rebate_reward,int32,int32,int32];
decode_def(11956)->
	[sc_rebate_get_reward,int8,{list, 11957}];
decode_def(11958)->
	[sc_rebate_update];
decode_def(11959)->
	[cs_activity_levelRank_open];
decode_def(11961)->
	[levelRank_rankerInfo,int64,int16,int8,int32,string,11930,bool,int8,int32];
decode_def(11960)->
	[sc_activity_levelRank_open,int32,int32,{list, 11961},int32,int32];
decode_def(11962)->
	[cs_activity_levelRank_refresh];
decode_def(11963)->
	[sc_activity_levelRank_refresh,{list, 11961}];
decode_def(11964)->
	[cs_activity_get_payExt_info];
decode_def(11965)->
	[cs_activity_vip_shop];
decode_def(11967)->
	[p_activity_vip_shop,int8,int8,int8,int32,int32,int32,int32,11930,int16,string];
decode_def(11966)->
	[sc_activity_vip_shop,int32,{list, 11967}];
decode_def(11968)->
	[cs_activity_vip_shop_buy,int8,int8];
decode_def(11969)->
	[sc_activity_vip_shop_buy,int8];
decode_def(11970)->
	[p_activity_pay_reward,int8,11930,string];
decode_def(11971)->
	[cs_activity_first_pay];
decode_def(11972)->
	[sc_activity_first_pay,int8,{list, 11970}];
decode_def(11973)->
	[sc_activity_firstPay_update,int8,11930];
decode_def(11974)->
	[cs_activity_consume_reback];
decode_def(11976)->
	[p_consume_unit,int32,int64,int64,int32,string];
decode_def(11975)->
	[sc_activity_consume_reback,int8,int8,{list, 11976},int64,int64,int8,int8,int32,int32,int8];
decode_def(11977)->
	[cs_activity_consume_state];
decode_def(11978)->
	[sc_activity_consume_state,int8];
decode_def(11979)->
	[cs_activity_energy_pac_info];
decode_def(11980)->
	[sc_activity_energy_pac_info,int16,int16];
decode_def(11981)->
	[cs_activity_energy_pac_use];
decode_def(11982)->
	[sc_activity_energy_pac_use,int8];
decode_def(11801)->
	[cs_invite_info];
decode_def(11802)->
	[sc_invite_info,bool,bool,int16,string,int16];
decode_def(11810)->
	[cs_invite_bind_weibo];
decode_def(11811)->
	[sc_invite_bind_weibo,int8];
decode_def(11812)->
	[cs_invite_weibo_share_levelup,int16];
decode_def(11813)->
	[sc_invite_weibo_share_levelup,int8];
decode_def(11820)->
	[cs_invite_input_invite_code,string];
decode_def(11821)->
	[sc_invite_input_invite_code,int8,string];
decode_def(11830)->
	[cs_invite_list];
decode_def(11832)->
	[p_invite,int32,bool,int16,int8,string,bool];
decode_def(11831)->
	[sc_invite_list,{list, 11832}];
decode_def(11701)->
	[cs_friend_get_list,int8];
decode_def(11703)->
	[p_friend,int32,bool,int16,int8,string,int64,int32,string,int32,int32,int32,int16,int8,int8,int32,int32,int32,int32,int32,int16];
decode_def(11702)->
	[sc_friend_get_list,int8,{list, 11703},int8,int8];
decode_def(11704)->
	[cs_friend_more,{list, int32}];
decode_def(11705)->
	[sc_friend_more,{list, 11703}];
decode_def(11706)->
	[cs_friend_get_add_list];
decode_def(11722)->
	[p_stranger,int32,bool,int16,int8,string,int64,int32,string,int32,int8];
decode_def(11707)->
	[sc_friend_get_add_list,{list, 11722}];
decode_def(11708)->
	[cs_friend_add,int32];
decode_def(11709)->
	[sc_friend_add,int8];
decode_def(11720)->
	[cs_friend_explore,string];
decode_def(11721)->
	[sc_friend_explore,{list, 11722}];
decode_def(11730)->
	[cs_friend_delete,int8,int32];
decode_def(11731)->
	[sc_friend_delete,int8,int8,int32];
decode_def(11732)->
	[sc_friend_notify_delete,int8,int32];
decode_def(11740)->
	[sc_friend_new,int8,11703];
decode_def(11741)->
	[cs_friend_send_enargy,int32];
decode_def(11742)->
	[sc_friend_send_enargy,int8,int32];
decode_def(11743)->
	[sc_friend_send_enargy_me,int32];
decode_def(11744)->
	[cs_friend_give_enargy,{list, int32}];
decode_def(11745)->
	[sc_friend_give_enargy,int8,{list, int32},int8];
decode_def(11746)->
	[sc_frend_give_enargy_me,int32,int8];
decode_def(11747)->
	[cs_friend_give_all_enargy];
decode_def(11748)->
	[sc_friend_remove_request];
decode_def(11601)->
	[cs_gather_get_list,int8];
decode_def(11602)->
	[sc_gather_get_list,int8,{list, int32}];
decode_def(11603)->
	[sc_gather_new,int8,{list, int32}];
decode_def(11604)->
	[cs_gather_manual_info_for_tag,int8];
decode_def(11606)->
	[p_ger_manual_unit,int32,int8,int8];
decode_def(11608)->
	[p_collect_unit,int32,int32,int8,int8,int8];
decode_def(11605)->
	[sc_gather_manual_info_for_tag,int8,{list, 11606},{list, 11608}];
decode_def(11607)->
	[sc_gather_manual_update,{list, 11606},{list, 11608}];
decode_def(11609)->
	[cs_gather_manual_info];
decode_def(11610)->
	[sc_gather_manual_info,{list, 11608},{list, int8}];
decode_def(11611)->
	[cs_gather_manual_collect_draw,int32];
decode_def(11612)->
	[sc_gather_manual_collect_draw,int8,{list, 12005}];
decode_def(11501)->
	[cs_hist_get_list,int8,int64];
decode_def(11503)->
	[p_hist,int64,int8,string,int32,int32,int16];
decode_def(11502)->
	[sc_hist_get_list,int8,bool,{list, 11503},int16];
decode_def(11510)->
	[cs_hist_more,int64,int8];
decode_def(11511)->
	[sc_hist_more,int8,{list, 11503},int16];
decode_def(11520)->
	[cs_hist_replay,int64,int8];
decode_def(11521)->
	[sc_hist_replay,int8,{list, 20002}];
decode_def(11531)->
	[sc_hist_unreadNum,int8,int16];
decode_def(11401)->
	[cs_mail_info,int8,int64];
decode_def(11405)->
	[p_mail,int64,int8,int32,string,string,int32,int16,{list, any},{list, 11404},int32,bool];
decode_def(11402)->
	[sc_mail_info,int8,int8,bool,{list, 11405},{list, int8}];
decode_def(11406)->
	[cs_mail_draw_reward,int64];
decode_def(11407)->
	[sc_mail_draw_reward,int8];
decode_def(11408)->
	[cs_mail_delete,int64,int8];
decode_def(11409)->
	[sc_mail_delete,int8];
decode_def(11410)->
	[cs_mail_new,int32,string,string];
decode_def(11411)->
	[sc_mail_new,int8];
decode_def(11420)->
	[cs_mail_unread_num];
decode_def(11421)->
	[sc_mail_unread_num,{list, int8}];
decode_def(11430)->
	[cs_mail_more,int8,int64];
decode_def(11431)->
	[sc_mail_more,int8,int8,{list, 11405},{list, int8}];
decode_def(11440)->
	[cs_mail_agree_friend,int64];
decode_def(11441)->
	[sc_mail_agree_friend,int8,int64];
decode_def(11442)->
	[cs_mail_del_spec_mail,int32];
decode_def(11443)->
	[sc_mail_del_spec_mail,{list, int64}];
decode_def(11444)->
	[cs_mail_invite_operate,int64,int8];
decode_def(11445)->
	[sc_mail_invite_operate,int8];
decode_def(11446)->
	[cs_mail_draw_reward_all];
decode_def(11447)->
	[sc_mail_draw_reward_all,{list, 11930},int8];
decode_def(11448)->
	[cs_mail_canvass_info];
decode_def(11449)->
	[sc_mail_canvass_info,int32,int32,int32,int32,int32];
decode_def(11450)->
	[cs_mail_get_question,int32];
decode_def(11451)->
	[sc_mail_get_question,int32,string,{list, string}];
decode_def(11452)->
	[cs_mail_do_select,int32,int32];
decode_def(11453)->
	[sc_mail_do_select,int8,int32];
decode_def(11301)->
	[cs_hron_info];
decode_def(11302)->
	[sc_hron_info_wait,int8,int16,int8,int8,int32];
decode_def(11303)->
	[sc_hron_info_stop,int32];
decode_def(11304)->
	[cs_hron_info_on];
decode_def(11305)->
	[sc_hron_info_on,int16,int16,int16,int32,int16,int16,int16,int16,int8,int8,int16,int32];
decode_def(11306)->
	[sc_hron_info_on_fail,int8];
decode_def(11311)->
	[cs_hron_last_rank_list,int8];
decode_def(11315)->
	[p_hron_role,string,int16,int32];
decode_def(11312)->
	[sc_hron_last_rank_list,int8,{list, 11315}];
decode_def(11313)->
	[cs_hron_cur_rank_list,int8];
decode_def(11314)->
	[sc_hron_cur_rank_list,int8,{list, 11315}];
decode_def(11321)->
	[cs_hron_buy,int8,int16];
decode_def(11322)->
	[sc_hron_buy,int8,int8,int32,int16,int16];
decode_def(11331)->
	[cs_hron_fight,int8];
decode_def(11332)->
	[sc_hron_fight,int8,{list, 20002},{list, 10210}];
decode_def(11341)->
	[sc_hron_stop,int16,int16];
decode_def(11351)->
	[cs_hron_rank];
decode_def(11352)->
	[sc_hron_rank,int16,int16];
decode_def(11360)->
	[cs_hron_open_time];
decode_def(11361)->
	[sc_hron_open_time,int32,int8,int8];
decode_def(11370)->
	[cs_hron_succ_reward];
decode_def(11371)->
	[sc_hron_succ_reward,int8,11930];
decode_def(11372)->
	[p_hron_succ_reward,int16,int32,int16,bool];
decode_def(11373)->
	[cs_hron_select,int8];
decode_def(11374)->
	[sc_hron_select,int8];
decode_def(11375)->
	[cs_hron_pass];
decode_def(11376)->
	[sc_hron_pass,int8,int16];
decode_def(11377)->
	[cs_hron_reward_view,int8,int16];
decode_def(11378)->
	[sc_hron_reward_view,int8,{list, 11930}];
decode_def(11379)->
	[cs_hron_raids];
decode_def(11380)->
	[sc_hron_raids,int8,int16,int16,{list, 11930}];
decode_def(11381)->
	[sc_hron_update_history,int8,int16];
decode_def(11201)->
	[cs_hula_open];
decode_def(11202)->
	[sc_hula_open,bool,int64,int16,bool,int16,bool,int32];
decode_def(11203)->
	[sc_hula_init_state,int64,int64,int32,int32];
decode_def(11204)->
	[cs_hula_close];
decode_def(11205)->
	[cs_hula_buff,int8];
decode_def(11206)->
	[sc_hula_buff,int8,int8];
decode_def(11211)->
	[cs_hula_last_info,int32];
decode_def(11212)->
	[sc_hula_last_info_ignore];
decode_def(11215)->
	[p_hula_info,string,int64];
decode_def(11213)->
	[sc_hula_last_info_win,int16,int32,int64,{list, 11215},{list, string}];
decode_def(11214)->
	[sc_hula_last_info_fail,int16,int32];
decode_def(11216)->
	[cs_hula_cur_info];
decode_def(11217)->
	[sc_hula_cur_info_ignore];
decode_def(11218)->
	[sc_hula_cur_info,{list, 11215}];
decode_def(11220)->
	[sc_hula_hp_sync,int64];
decode_def(11222)->
	[p_hula_harm,string,int64];
decode_def(11221)->
	[sc_hula_harm_broadcast,{list, 11222}];
decode_def(11224)->
	[sc_hula_buff_sync,int16];
decode_def(11225)->
	[sc_hula_stop,int8,11255];
decode_def(11226)->
	[cs_hula_rank_sync];
decode_def(11227)->
	[sc_hula_rank_sync,int16];
decode_def(11231)->
	[cs_hula_fight];
decode_def(11232)->
	[sc_hula_fight,int8,{list, 20002},int32,int32,int32];
decode_def(11241)->
	[cs_hula_reborn];
decode_def(11242)->
	[sc_hula_reborn,int8];
decode_def(11243)->
	[cs_hula_offline_play,bool];
decode_def(11244)->
	[sc_hula_offline_play,int8,bool];
decode_def(11250)->
	[cs_hula_open_time];
decode_def(11251)->
	[sc_hula_open_time,int32];
decode_def(11101)->
	[cs_card_get_list];
decode_def(11104)->
	[p_card,int8,int32];
decode_def(11103)->
	[p_opened_card,int8,int8,int32];
decode_def(11102)->
	[sc_card_get_list,{list, 11103},{list, 11104},int32,int32];
decode_def(11105)->
	[cs_card_draw,int8];
decode_def(11106)->
	[sc_card_draw,int8,int8,{list, 11104},int32,int32];
decode_def(11107)->
	[cs_card_refresh];
decode_def(11108)->
	[sc_card_refresh,int8,{list, 11104},int32,int32];
decode_def(11120)->
	[cs_card_onekey];
decode_def(11121)->
	[sc_card_onekey,int8,{list, 11103}];
decode_def(11122)->
	[cs_card_activity_info];
decode_def(11123)->
	[sc_card_activity_card,int32,int32,int16,int32,int32];
decode_def(11001)->
	[cs_daily_get_list];
decode_def(11003)->
	[p_daily,int8,int8,bool];
decode_def(11002)->
	[sc_daily_get_list,{list, 11003}];
decode_def(11005)->
	[cs_daily_draw,int8];
decode_def(11006)->
	[sc_daily_draw,int8,11003];
decode_def(11004)->
	[sc_daily_family_info,int8,int8];
decode_def(10901)->
	[cs_plunder_info];
decode_def(10903)->
	[p_stonechip,int32,int16,int16,int16,int16];
decode_def(10902)->
	[sc_plunder_info,bool,int32,int16,int16,int32,int16,{list, 10903},int16];
decode_def(10934)->
	[cs_plunder_compose,int32];
decode_def(10935)->
	[sc_plunder_compose,int8,{list, 10903}];
decode_def(10936)->
	[cs_plunder_get_target,int32,int32];
decode_def(10938)->
	[p_plunder_tar,int32,bool,int16,string,int8,int32,int64,int16,bool];
decode_def(10937)->
	[sc_plunder_get_target,int8,{list, 10938}];
decode_def(10939)->
	[cs_plunder_fight,int32,int16,int8];
decode_def(10940)->
	[sc_plunder_fight,int32,int8,{list, 20002},int16,int16,int16,int16,int8];
decode_def(10941)->
	[cs_plunder_use_protect];
decode_def(10942)->
	[sc_plunder_use_protect,int8];
decode_def(10943)->
	[cs_plunder_buy_attacktime];
decode_def(10944)->
	[sc_plunder_buy_attacktime,int8];
decode_def(10945)->
	[sc_plunder_notice_attacktime,int16,int32];
decode_def(10946)->
	[cs_plunder_fight_ten,int32,int16,int8];
decode_def(10948)->
	[p_plunder_fight_mystery,{list, 12005}];
decode_def(10947)->
	[sc_plunder_fight_ten,int32,int8,{list, bool},int16,int16,int16,int16,int8,{list, 10948}];
decode_def(10949)->
	[cs_plunder_protect_time];
decode_def(10950)->
	[sc_plunder_protect_time,int8,int8];
decode_def(10951)->
	[cs_plunder_multi_compose,int32,int8];
decode_def(10952)->
	[sc_plunder_multi_compose,int8,{list, 10903}];
decode_def(10801)->
	[cs_pvp_get_list];
decode_def(10803)->
	[p_pvp,int32,bool,int16,int8,string,int64,int16,int32,int8];
decode_def(10802)->
	[sc_pvp_get_list,int16,{list, 10803},int16];
decode_def(10804)->
	[cs_pvp_fight,int32,int16];
decode_def(10805)->
	[sc_pvp_fight,int8,{list, 20002},int16,int32,int32,int32];
decode_def(10806)->
	[cs_pvp_get_first_eight_replays];
decode_def(10808)->
	[p_pvp_replay_info,string,string,int16,int16,int64,int32];
decode_def(10807)->
	[sc_pvp_get_first_eight_replays,{list, 10808}];
decode_def(10809)->
	[cs_pvp_eight_replay,int64];
decode_def(10810)->
	[sc_pvp_eight_replay,int8,20002];
decode_def(10811)->
	[cs_pvp_free_fight,int32];
decode_def(10812)->
	[sc_pvp_free_fight,int8,20002,{list, 12005}];
decode_def(10813)->
	[cs_pvp_get_free_fight_level];
decode_def(10814)->
	[sc_pvp_get_free_fight_level,int32];
decode_def(10701)->
	[cs_shop_buy_num];
decode_def(10703)->
	[p_shop_num,int16,int16,int16,int16,int8];
decode_def(10702)->
	[sc_shop_buy_num,{list, 10703}];
decode_def(10704)->
	[cs_shop_buy,int16,int16,int8];
decode_def(10705)->
	[sc_shop_buy,int8,{list, 12005}];
decode_def(10710)->
	[cs_shop_encounter];
decode_def(10712)->
	[p_shop_random,int16,int32,{list, int16}];
decode_def(10711)->
	[sc_shop_encounter,{list, 10712},int32];
decode_def(10713)->
	[sc_shop_new,10712];
decode_def(10720)->
	[cs_shop_refresh,int16];
decode_def(10721)->
	[sc_shop_refresh,int8,{list, 10712}];
decode_def(10730)->
	[sc_shop_auto_refresh,10712];
decode_def(10722)->
	[cs_shop_family_limit_info];
decode_def(10726)->
	[p_shop_family_limit,int8,int8,int16,int16,int16,11930];
decode_def(10723)->
	[sc_shop_family_limit_info,int8,{list, 10726}];
decode_def(10724)->
	[cs_shop_family_limit_buy,int8];
decode_def(10725)->
	[sc_shop_family_limit_buy,int8];
decode_def(10727)->
	[cs_shop_seed_info];
decode_def(10729)->
	[seed_sell_info,int16,int32,int32,int32];
decode_def(10728)->
	[sc_shop_seed_info,{list, 10729}];
decode_def(10601)->
	[cs_item_bag];
decode_def(10603)->
	[p_item,int64,int16,int16,int8,int16,int32,int16,int8,int8,int8];
decode_def(10602)->
	[sc_item_bag,{list, 10603}];
decode_def(10628)->
	[sc_item_bag2,int8,int8,{list, 10603}];
decode_def(10604)->
	[cs_item_equip];
decode_def(10605)->
	[sc_item_equip,{list, 10606}];
decode_def(10607)->
	[cs_item_sell,{list, int64}];
decode_def(10608)->
	[sc_item_sell,int8,{list, 12010},int32];
decode_def(10609)->
	[cs_item_down_equip,int64,int8];
decode_def(10610)->
	[sc_item_down_equip,int8,int64,int8];
decode_def(10611)->
	[cs_item_up_equip,int64,int8,int64,int64];
decode_def(10612)->
	[sc_item_up_equip,int8,int64,int8,int64];
decode_def(10613)->
	[sc_item_new,{list, 10603}];
decode_def(10615)->
	[p_item_num_update,int64,int16];
decode_def(10614)->
	[sc_item_update,{list, 10615}];
decode_def(10617)->
	[cs_item_use,int64,int8];
decode_def(10618)->
	[sc_item_use,int8,int64,int8];
decode_def(10619)->
	[sc_item_delete_notify,{list, int64}];
decode_def(10620)->
	[cs_item_reinforce,int64,int64];
decode_def(10621)->
	[sc_item_reinforce,int8,int64,int16];
decode_def(10622)->
	[cs_item_max_reinforce,int64,int64];
decode_def(10623)->
	[sc_item_max_reinforce,int8,int64,{list, int16}];
decode_def(10624)->
	[sc_item_update_rank,int64,int8,int32];
decode_def(10625)->
	[cs_item_up_rank,int64,int64,int64,int64];
decode_def(10626)->
	[sc_item_up_rank,int8,int64,int64,int16,int8];
decode_def(10631)->
	[cs_item_compound,int16];
decode_def(10632)->
	[sc_item_compound,int8,int16];
decode_def(10633)->
	[cs_item_eat,int64,int64,{list, int64}];
decode_def(10634)->
	[sc_item_eat,int8,int64,int8,int16];
decode_def(10636)->
	[p_all_equipment,int32,{list, int32}];
decode_def(10635)->
	[sc_item_all_equipment,{list, int64},{list, 10636}];
decode_def(10637)->
	[cs_item_use_info];
decode_def(10639)->
	[p_item_use_info,int16,int8];
decode_def(10638)->
	[sc_item_use_info,{list, 10639}];
decode_def(10640)->
	[cs_item_auto_up_equip,int64,int8];
decode_def(10641)->
	[sc_item_auto_up_equip,int8];
decode_def(10642)->
	[cs_item_stone_eat,int64,int64,{list, int64}];
decode_def(10643)->
	[sc_item_stone_eat,int8,10603,int64,int8];
decode_def(10646)->
	[p_item_decompose_unit,int8,int64];
decode_def(10644)->
	[cs_item_decompose,{list, 10646}];
decode_def(10645)->
	[sc_item_decompose,int8,int64,{list, 12005}];
decode_def(10649)->
	[cs_item_decompose_again,int64];
decode_def(10650)->
	[sc_item_decompose_again,int8,int64,{list, 12005}];
decode_def(10651)->
	[cs_item_decompose_by_money_cost];
decode_def(10652)->
	[sc_item_decompose_by_money_cost,int8,int32];
decode_def(10653)->
	[cs_item_decompose_again_cost];
decode_def(10654)->
	[sc_item_decompose_again_cost,int8,{list, 12005}];
decode_def(10655)->
	[cs_item_enchant,int8,int64,int64];
decode_def(10657)->
	[p_equip2,int64,int16,int16,int8,int64,int8,int32,int16,int8,int8];
decode_def(10656)->
	[sc_item_enchant,int8,10657];
decode_def(10658)->
	[cs_item_x_get,int16,{list, int64}];
decode_def(10659)->
	[sc_item_x_get,int8];
decode_def(10660)->
	[cs_item_down_rank,int64,int64];
decode_def(10661)->
	[sc_item_down_rank,int8,{list, 12005},int64,int8];
decode_def(10662)->
	[cs_item_pft_uprank,int64,int64,{list, int64}];
decode_def(10663)->
	[sc_item_pft_uprank,int8,int8,int64,{list, int64}];
decode_def(10664)->
	[cs_item_stone_uprank,int64,int64,int64];
decode_def(10665)->
	[sc_item_stone_uprank,int8,10603,int64,int64];
decode_def(10666)->
	[sc_item_dtl_update,10657];
decode_def(10667)->
	[cs_item_max_reinforce_for_ger,int64,int8];
decode_def(10668)->
	[p_reinforce_result,int64,int16,int8,{list, int16}];
decode_def(10669)->
	[sc_item_max_reinforce_for_ger,int64,int8,{list, 10668}];
decode_def(10670)->
	[cs_item_make_legend,int64,int64];
decode_def(10671)->
	[sc_item_make_legend,int8,int64,int16];
decode_def(10672)->
	[cs_item_legend_uprank,int64,int64];
decode_def(10673)->
	[sc_item_legend_uprank,int8,int64,int8];
decode_def(10674)->
	[cs_item_stone_legend,int64,int64];
decode_def(10675)->
	[sc_item_stone_legend,int8,int64,int64,int8,int64];
decode_def(10501)->
	[cs_explore_one];
decode_def(10503)->
	[p_echapter,int16,int32,int32,bool];
decode_def(10502)->
	[sc_explore_one,int8,{list, 10503},int32,int32,int32,int8];
decode_def(10504)->
	[cs_explore_dungeon_list,int16];
decode_def(10506)->
	[p_edungeon,int16,bool];
decode_def(10505)->
	[sc_explore_dungeon_list,int16,{list, 10506},int8];
decode_def(10507)->
	[cs_explore_challenge_encounter,int16];
decode_def(10508)->
	[sc_explore_challenge_encounter,int8,{list, 20002},{list, 10210},int8,int8];
decode_def(10509)->
	[sc_explore_delete_encounter,int16];
decode_def(10510)->
	[cs_explore_giveup_encounter,int16];
decode_def(10511)->
	[sc_explore_giveup_encounter,int8];
decode_def(10512)->
	[cs_explore_list];
decode_def(10513)->
	[sc_explore_list,{list, 10503}];
decode_def(10514)->
	[cs_explore_collect,int16];
decode_def(10515)->
	[sc_explore_collect,int16,int8];
decode_def(10516)->
	[cs_explore_force_collect,int16,int8];
decode_def(10517)->
	[sc_explore_force_collect,int16,int8,int8];
decode_def(10518)->
	[cs_explore_auto_explore_check];
decode_def(10519)->
	[sc_explore_auto_explore_check,int8,int16,int8,int16];
decode_def(10520)->
	[cs_explore_encounter_pass_reward,int16];
decode_def(10521)->
	[sc_explore_encounter_pass_reward,int8];
decode_def(10522)->
	[cs_explore_encounter_dungeon_state,int16];
decode_def(10523)->
	[sc_explore_encounter_dungeon_state,int8,int16,int16,int8];
decode_def(10530)->
	[cs_explore_free];
decode_def(10531)->
	[sc_explore_free,int8];
decode_def(10532)->
	[cs_explore_all,int8];
decode_def(10533)->
	[p_explore_reward,int32,int32,int32,int8];
decode_def(10534)->
	[sc_explore_all,int8,int32,11930];
decode_def(10401)->
	[cs_ger_info];
decode_def(10402)->
	[sc_ger_info,{list, 10403}];
decode_def(10427)->
	[sc_ger_info2,int8,int8,{list, 10403}];
decode_def(10404)->
	[p_ger_pos_info,int64,int8,{list, int64},{list, 21601}];
decode_def(10405)->
	[sc_ger_update,int64,int16,int16,int32,int64,int64,int64,int64,{list, 10485},int16];
decode_def(10406)->
	[sc_ger_new,10403];
decode_def(10407)->
	[cs_ger_standup,int8,int64];
decode_def(10408)->
	[sc_ger_standup,int8,int8,int64];
decode_def(10409)->
	[cs_ger_move_pos,int8,int8];
decode_def(10410)->
	[sc_ger_move_pos,int8,int8,int8];
decode_def(10411)->
	[cs_ger_pos_list];
decode_def(10431)->
	[p_ger_icon_unit,int8,int8];
decode_def(10412)->
	[sc_ger_pos_list,{list, 10404},int8,int8,{list, 10431}];
decode_def(10414)->
	[cs_ger_sell,{list, int64}];
decode_def(10415)->
	[sc_ger_sell,int8,{list, 12010}];
decode_def(10416)->
	[cs_ger_detail,int64];
decode_def(10417)->
	[sc_ger_detail,int64,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int32];
decode_def(10419)->
	[cs_ger_view_other,int32,int16];
decode_def(10420)->
	[sc_ger_view_other,int32,string,int16,int64,{list, 10403}];
decode_def(10442)->
	[cs_ger_view_other_dtl,int32,int16];
decode_def(10443)->
	[sc_ger_view_other_dtl,int32,string,bool,int16,int64,{list, 10403},{list, 10606},{list, 10413},{list, 10471},int32,int8,int32,int32,20913,{list, 21309}];
decode_def(10421)->
	[sc_ger_update_exp,int64,int64];
decode_def(10422)->
	[cs_ger_eat,int64,int32,int32];
decode_def(10423)->
	[sc_ger_eat,int8,int64];
decode_def(10424)->
	[cs_ger_up_rank,int64,int64];
decode_def(10425)->
	[sc_ger_up_rank,int8,int64,int64];
decode_def(10426)->
	[sc_ger_update_standlist,{list, 10413}];
decode_def(10430)->
	[sc_ger_del,{list, int64}];
decode_def(10441)->
	[p_ger_power,int8,int64];
decode_def(10440)->
	[sc_ger_refresh_power,{list, 10441}];
decode_def(10444)->
	[p_ger_save_item,int8,int64];
decode_def(10445)->
	[p_ger_save_dtl,int8,int64,{list, 10444},{list, 10444}];
decode_def(10446)->
	[p_ger_tag_dtl,int8,{list, 10445},{list, 10445}];
decode_def(10447)->
	[cs_ger_saved_tag,int8];
decode_def(10448)->
	[sc_ger_saved_tag,{list, 10446}];
decode_def(10432)->
	[cs_ger_set_icon,{list, 10431}];
decode_def(10433)->
	[sc_ger_set_icon];
decode_def(10434)->
	[cs_ger_buy_tag,int8];
decode_def(10435)->
	[sc_ger_buy_tag,int8];
decode_def(10436)->
	[cs_ger_set_tag_data,int8,{list, 10445},{list, 10445}];
decode_def(10437)->
	[sc_ger_set_tag_data,int8];
decode_def(10438)->
	[cs_ger_change_tag,int8];
decode_def(10439)->
	[sc_ger_change_tag,int8,{list, 10404},{list, 10404}];
decode_def(10450)->
	[cs_ger_lieu_pos_list];
decode_def(10451)->
	[sc_ger_lieu_pos_list,{list, 10404},int8];
decode_def(10452)->
	[cs_ger_lieu_standup,int8,int64];
decode_def(10453)->
	[sc_ger_lieu_standup,int8,int8,int64];
decode_def(10454)->
	[cs_ger_lieu_dequeue,int8,int64];
decode_def(10455)->
	[sc_ger_lieu_dequeue,int8,int8,int64];
decode_def(10456)->
	[cs_ger_lieu_untie,int8];
decode_def(10458)->
	[p_ger_lieu_info,int8,int16,int8,int16,int8,int16,int8];
decode_def(10457)->
	[sc_ger_lieu_untie,int8,{list, 10458}];
decode_def(10459)->
	[cs_ger_lieu_info_list];
decode_def(10460)->
	[sc_ger_lieu_info_list,{list, 10458}];
decode_def(10461)->
	[cs_ger_lieu_move_pos,int8,int8];
decode_def(10462)->
	[sc_ger_lieu_move_pos,int8,int8,int8];
decode_def(10463)->
	[cs_ger_lieu_lock_clo,int8,int8];
decode_def(10464)->
	[sc_ger_lieu_lock_clo,int8];
decode_def(10465)->
	[cs_ger_lieu_unlock_clo,int8,int8];
decode_def(10466)->
	[sc_ger_lieu_unlock_clo,int8];
decode_def(10467)->
	[cs_ger_lieu_refresh_clo,int8];
decode_def(10468)->
	[sc_ger_lieu_refresh_clo,int8,10458];
decode_def(10469)->
	[cs_ger_lieu_tie_info];
decode_def(10470)->
	[sc_ger_lieu_tie_info,{list, int8}];
decode_def(10472)->
	[cs_ger_lieu_refresh_freeTimes];
decode_def(10473)->
	[sc_ger_lieu_refresh_freeTimes,int16];
decode_def(10474)->
	[sc_ger_new_list,{list, 10403}];
decode_def(10475)->
	[cs_ger_down_rank,int64];
decode_def(10476)->
	[sc_ger_down_rank,int8,{list, 12005},int64];
decode_def(10477)->
	[cs_ger_unload,int8];
decode_def(10478)->
	[sc_ger_unload,int8,int8];
decode_def(10479)->
	[cs_ger_mirror_info];
decode_def(10480)->
	[sc_ger_mirror_info,int64,int16,{list, int32}];
decode_def(10481)->
	[cs_ger_mirror_convert,int64];
decode_def(10482)->
	[sc_ger_mirror_convert,int8,int16];
decode_def(10483)->
	[cs_ger_mirror_commit,int8];
decode_def(10484)->
	[sc_ger_mirror_commit,int8,int64];
decode_def(10486)->
	[cs_ger_set_body,int64,int8];
decode_def(10487)->
	[sc_ger_set_body,int8];
decode_def(10488)->
	[cs_ger_second_uprank,int64,{list, int64}];
decode_def(10489)->
	[sc_ger_second_uprank,int8,int64];
decode_def(10490)->
	[cs_ger_batch_uprank,int64,{list, int64}];
decode_def(10491)->
	[sc_ger_batch_uprank,int8,int64];
decode_def(10492)->
	[cs_ger_second_transmigration,int64];
decode_def(10493)->
	[sc_ger_second_transmigration,int8];
decode_def(10494)->
	[cs_ger_transform_blink,int64,int64];
decode_def(10495)->
	[sc_ger_transform_blink,int8,10403];
decode_def(10302)->
	[cs_message_notice,int32];
decode_def(10304)->
	[p_notice,int32,string,string];
decode_def(10303)->
	[sc_message_notice,{list, int32},{list, 10304},string];
decode_def(10305)->
	[cs_message_certain_notice,int32];
decode_def(10306)->
	[sc_message_certain_notice,{list, 10304}];
decode_def(10301)->
	[sc_message_bc,string];
decode_def(10307)->
	[sc_message_bc_id,int16];
decode_def(10308)->
	[sc_message_bc_id2,int16,{list, any}];
decode_def(10330)->
	[cs_message_test,string];
decode_def(10331)->
	[sc_message_test,int8,string];
decode_def(30001)->
	[sc_message_best_card,string,int8,int32];
decode_def(30002)->
	[sc_message_ger_upLevel,string,10418];
decode_def(30003)->
	[sc_message_item_uprank,string,10616];
decode_def(10201)->
	[cs_battle_progress];
decode_def(10222)->
	[p_battle_chapter_star_reward,int16,int16,int16,int8];
decode_def(10216)->
	[p_battle_progress,int8,int16,int16,int16];
decode_def(10202)->
	[sc_battle_progress,{list, 10216},{list, int16},{list, 10222}];
decode_def(10203)->
	[cs_battle_info,int8,int16];
decode_def(10205)->
	[p_dungeon,int16,int16,int8,int16];
decode_def(10204)->
	[sc_battle_info,int8,int16,bool,{list, 10205},int16,int16];
decode_def(10206)->
	[cs_battle_challenge,int8,int16];
decode_def(10207)->
	[sc_battle_challenge,int8,{list, 20002},{list, 10210},int8];
decode_def(10208)->
	[cs_battle_perfect_reward,int16];
decode_def(10209)->
	[sc_battle_perfect_reward,int8];
decode_def(10212)->
	[sc_battle_broadcast_get_item,string,int32,int8,int16,int16];
decode_def(10214)->
	[cs_battle_dungeon_raids,int16];
decode_def(10215)->
	[sc_battle_dungeon_raids,int8,int8,{list, 10210}];
decode_def(10217)->
	[cs_battle_star_reward,int16,int16];
decode_def(10218)->
	[sc_battle_star_reward,int8,int16,{list, 11930}];
decode_def(10219)->
	[cs_battle_reset_dungeon,int8,int16];
decode_def(10220)->
	[sc_battle_reset_dungeon,int8,int16];
decode_def(10221)->
	[sc_battle_battle_fail,int8,int16,int16];
decode_def(10223)->
	[cs_battle_world_level];
decode_def(10224)->
	[sc_battle_world_level,int16,int16,int16];
decode_def(10225)->
	[cs_battle_obtain_boss_reward,int8,int16,int16];
decode_def(10226)->
	[sc_battle_obtain_boss_reward,int8,int8,int16,int16,{list, 11930}];
decode_def(10227)->
	[cs_battle_get_boss_reward_info,int8,int16,int16];
decode_def(10228)->
	[sc_battle_get_boss_reward_info,int8,int8,int16,int16];
decode_def(10229)->
	[cs_battle_dojang_info];
decode_def(10237)->
	[p_dojang_info,int8,int8];
decode_def(10230)->
	[sc_battle_dojang_info,int32,int32,{list, 10237},int32];
decode_def(10231)->
	[cs_battle_dojang_fight,int8];
decode_def(10232)->
	[sc_battle_dojang_fight,int8,10237,{list, 11930},{list, 20002},{list, 20005}];
decode_def(10233)->
	[cs_battle_dojang_harvest,int8];
decode_def(10234)->
	[sc_battle_dojang_harvest,int8,{list, 11930}];
decode_def(10235)->
	[cs_battle_dojang_buy,int32];
decode_def(10236)->
	[sc_battle_dojang_buy,int8,int32,int32,int32];
decode_def(10101)->
	[cs_role_info];
decode_def(10102)->
	[sc_role_info,int32,string,bool,string,int32,int16,int64,int64,int32,int32,int32,int32,int8,int32,int16,int16,int32,int16,int32,int16,int16,int8,int8,int8,int8,bool,bool,int16,int16,int16,int16,int8,int32,int32,int16,int16,int32,int32,int32,bool,int8,int32,int32,int16,int32,int32,int32,int16,int16,int32,int32,int16,int32,int32,int32,int32,int8];
decode_def(10107)->
	[sc_role_update_level,int16];
decode_def(10103)->
	[sc_role_update_list,{list, 10107}];
decode_def(10104)->
	[sc_role_update_unioncoin,int32];
decode_def(10105)->
	[cs_role_buy_energy,int8];
decode_def(10106)->
	[sc_role_buy_energy,int8,int8,int16,int16,int8];
decode_def(10108)->
	[sc_role_update_exp,int64];
decode_def(10109)->
	[sc_role_update_coin,int64];
decode_def(10110)->
	[sc_role_update_reputation,int32];
decode_def(10111)->
	[sc_role_update_gold,int32];
decode_def(10112)->
	[sc_role_update_goldBonus,int32];
decode_def(10113)->
	[sc_role_update_vipLevel,int8,int16,int16];
decode_def(10114)->
	[sc_role_update_energy,int16,int32];
decode_def(10115)->
	[sc_role_update_discoveryTimes,int16,int32];
decode_def(10116)->
	[sc_role_update_pvpTimes,int16,int32];
decode_def(10117)->
	[sc_role_update_plunderTimes,int16,int32];
decode_def(10118)->
	[sc_role_update_randomPVPTimes,int8];
decode_def(10119)->
	[sc_role_update_singlePVPTimes,int8];
decode_def(10120)->
	[sc_role_update_goldUsed,int32];
decode_def(10121)->
	[sc_role_update_title,int8];
decode_def(10122)->
	[sc_role_update_encounterFreeNum,int8];
decode_def(10123)->
	[sc_role_update_weiboCount,int8];
decode_def(10124)->
	[cs_role_setting];
decode_def(10125)->
	[sc_role_setting,{list, int8}];
decode_def(10126)->
	[cs_role_get_energy,int8];
decode_def(10127)->
	[sc_role_get_energy,int8];
decode_def(10128)->
	[cs_role_buy_coin_value];
decode_def(10129)->
	[sc_role_buy_coin_value,int32,int16];
decode_def(10130)->
	[cs_role_weixin_share];
decode_def(10131)->
	[sc_role_update_pay_ext,int32];
decode_def(10132)->
	[cs_role_suggest_open];
decode_def(10133)->
	[sc_role_suggest_open,bool];
decode_def(10134)->
	[cs_role_suggest,string,string];
decode_def(10135)->
	[sc_role_suggest,int8];
decode_def(10136)->
	[cs_role_log_guide_state,int16];
decode_def(10137)->
	[sc_role_talent_remains_point,int16];
decode_def(10138)->
	[cs_role_multi_buy_energy,int8,int16];
decode_def(10139)->
	[p_multi_buy_energy,int32,int16];
decode_def(10140)->
	[sc_role_multi_buy_energy,int8,int8,int16,int16,{list, 10139}];
decode_def(10141)->
	[cs_role_is_under_examine];
decode_def(10142)->
	[sc_role_is_under_examine,bool];
decode_def(10150)->
	[cs_role_push_setting,int8,int16];
decode_def(10151)->
	[sc_role_push_setting,int8,int8,int16];
decode_def(10153)->
	[cs_role_get_guide_state];
decode_def(10154)->
	[sc_role_get_guide_state,int16];
decode_def(10155)->
	[cs_role_set_guide_state,int16];
decode_def(10156)->
	[sc_role_set_guide_state,int8];
decode_def(10157)->
	[cs_role_change_head,int32];
decode_def(10158)->
	[sc_role_change_head,int8,int32];
decode_def(10159)->
	[cs_role_change_location,string];
decode_def(10160)->
	[sc_role_update_teamPkTimes,int16,int32];
decode_def(10161)->
	[cs_role_teamPkTimes_info];
decode_def(10162)->
	[sc_role_teamPkTimes_info,int8,int16,{list, int16}];
decode_def(10180)->
	[cs_role_token,string];
decode_def(10181)->
	[sc_role_token];
decode_def(10182)->
	[cs_role_select_ger,int16];
decode_def(10183)->
	[sc_role_select_ger,int8];
decode_def(10184)->
	[cs_role_demo_fight,int8];
decode_def(10185)->
	[sc_role_demo_fight,int8,20002];
decode_def(10186)->
	[sc_role_base_config,int8,int8];
decode_def(10190)->
	[cs_role_pay_ios,string,int32,string,string,int8];
decode_def(10191)->
	[sc_role_pay_ios,int8,string,int32,bool];
decode_def(10192)->
	[cs_role_pay_91,string,int32,string,string];
decode_def(10193)->
	[sc_role_pay_91,int8,string,int32,bool];
decode_def(10194)->
	[sc_role_pay_uc,int8,int32,bool];
decode_def(10195)->
	[sc_role_pay_dl,int8,int32,bool];
decode_def(10196)->
	[sc_role_pay_zz,int8,int32,bool];
decode_def(10197)->
	[sc_role_pay_360,int8,int32,bool];
decode_def(10198)->
	[sc_role_pay_wdj,int8,int32,bool];
decode_def(10199)->
	[sc_role_pay_dk,int8,int32,bool];
decode_def(10189)->
	[sc_role_pay_mi,int8,int32,bool];
decode_def(10188)->
	[sc_role_pay_az,int8,int32,bool];
decode_def(10200)->
	[sc_role_update_profoundCrystal,int32];
decode_def(10187)->
	[sc_role_update_honor,int32];
decode_def(10179)->
	[sc_role_update_pvppoint,int32];
decode_def(10143)->
	[sc_role_update_home_resource,int32];
decode_def(10163)->
	[sc_role_update_ticket,int32];
decode_def(10164)->
	[sc_role_update_laputastone,int32];
decode_def(10165)->
	[cs_role_do_transmigration];
decode_def(10166)->
	[sc_role_do_transmigration,int32];
decode_def(10167)->
	[cs_role_can_transmigration];
decode_def(10168)->
	[sc_role_can_transmigration,int32,int32,int32,int32,int32,int32,int32];
decode_def(10169)->
	[sc_role_update_svip,int8,int32];
decode_def(10001)->
	[cs_account_login,int32,int32,string,string,string,int16,string,int16];
decode_def(10002)->
	[sc_account_login,int16,bool,bool,int8];
decode_def(10003)->
	[cs_account_create,string,int8];
decode_def(10004)->
	[sc_account_create,int8];
decode_def(10005)->
	[cs_account_enter_game];
decode_def(10006)->
	[sc_account_enter_game,int8];
decode_def(10007)->
	[sc_account_kick,int8];
decode_def(10013)->
	[cs_account_heart];
decode_def(10014)->
	[sc_account_heart,int32];
decode_def(10015)->
	[cs_account_logout];
decode_def(10016)->
	[cs_account_check_rolename,string];
decode_def(10017)->
	[sc_account_check_rolename,int8];
decode_def(10018)->
	[cs_account_demo,int8,int8,int16];
decode_def(10019)->
	[sc_account_demo,20005];
decode_def(_) -> [].

get_id(cs_trainingRoom_info)->21701;
get_id(sc_trainingRoom_info)->21702;
get_id(cs_trainingRoom_start_training)->21703;
get_id(p_ger_view)->10418;
get_id(p_item_view)->10616;
get_id(p_reward_info)->11930;
get_id(p_trainingRoom_reward)->21704;
get_id(sc_trainingRoom_start_training)->21705;
get_id(p_diamond)->21601;
get_id(cs_diamond_combine)->21602;
get_id(sc_diamond_combine)->21603;
get_id(cs_diamond_equip)->21604;
get_id(sc_diamond_equip)->21605;
get_id(cs_diamond_demount)->21606;
get_id(sc_diamond_demount)->21607;
get_id(cs_diamond_demount_multi)->21608;
get_id(p_diamond_location)->21610;
get_id(sc_diamond_demount_multi)->21609;
get_id(cs_diamond_holygrail_info)->21611;
get_id(sc_diamond_holygrail_info)->21612;
get_id(cs_diamond_holygrail_sacrifice)->21613;
get_id(sc_diamond_holygrail_sacrifice)->21614;
get_id(cs_diamond_holygrail_uplevel)->21615;
get_id(sc_diamond_holygrail_uplevel)->21616;
get_id(cs_conquerisland_sign)->21501;
get_id(sc_conquerisland_sign)->21502;
get_id(cs_conquerisland_buy)->21503;
get_id(sc_conquerisland_buy)->21504;
get_id(cs_conquerisland_info)->21505;
get_id(sc_conquerisland_info)->21506;
get_id(cs_conquerisland_unrequest)->21507;
get_id(sc_conquerisland_unrequest)->21508;
get_id(sc_conquerisland_times_update)->21509;
get_id(cs_conquerisland_war_base_info)->21510;
get_id(p_pos)->21529;
get_id(p_centre)->21530;
get_id(p_player)->21531;
get_id(p_conquerisland_boss)->21532;
get_id(sc_conquerisland_war_base_info)->21511;
get_id(cs_conquerisland_role_dtl)->21512;
get_id(p_carlos_replay_dtl)->14835;
get_id(p_conquerisland_ger)->21533;
get_id(sc_conquerisland_role_dtl)->21513;
get_id(cs_conquerisland_mov)->21514;
get_id(sc_conquerisland_mov)->21515;
get_id(cs_conquerisland_stop)->21516;
get_id(sc_conquerisland_stop)->21517;
get_id(cs_conquerisland_centre_dtl)->21518;
get_id(sc_conquerisland_centre_dtl)->21519;
get_id(cs_conquerisland_attack)->21520;
get_id(p_action)->20004;
get_id(p_fighter)->20003;
get_id(sc_fight_request)->20002;
get_id(sc_conquerisland_attack)->21521;
get_id(cs_conquerisland_centre_occupy)->21522;
get_id(sc_conquerisland_centre_occupy)->21523;
get_id(cs_conquerisland_reborn)->21524;
get_id(sc_conquerisland_reborn)->21525;
get_id(cs_conquerisland_replay)->21526;
get_id(sc_conquerisland_replay)->21527;
get_id(sc_conquerisland_update_war)->21528;
get_id(cs_conquerisland_self)->21534;
get_id(sc_conquerisland_self)->21535;
get_id(cs_conquerisland_rank)->21536;
get_id(p_fight_rank)->21538;
get_id(sc_conquerisland_rank)->21537;
get_id(cs_conquerisland_talk)->21539;
get_id(cs_conquerisland_get_talk)->21540;
get_id(p_carlos_talk)->14837;
get_id(sc_conquerisland_get_talk)->21541;
get_id(p_carlos_rank_dtl)->14836;
get_id(sc_conquerisland_end_war)->21542;
get_id(sc_conquerisland_talk)->21543;
get_id(cs_recycle_restore)->21401;
get_id(sc_recycle_restore)->21402;
get_id(cs_crystal_uplevel)->21301;
get_id(p_crystalinfo)->21305;
get_id(sc_crystal_uplevel)->21302;
get_id(cs_crystal_uprank)->21303;
get_id(sc_crystal_uprank)->21304;
get_id(cs_crystal_info)->21306;
get_id(sc_crystal_info)->21307;
get_id(p_crystalinfo_brief)->21308;
get_id(ger_crystalinfo_brief)->21309;
get_id(cs_doublematch_lineup)->21203;
get_id(p_doublematch_team_trainer_info)->21206;
get_id(p_equip)->10606;
get_id(p_demage_rate_unit)->10485;
get_id(p_awake_info)->20607;
get_id(p_ger)->10403;
get_id(p_doublematch_team_ger_info)->21205;
get_id(p_doublematch_team_member)->21207;
get_id(sc_doublematch_lineup)->21204;
get_id(p_ger_pos_unit)->21210;
get_id(cs_doublematch_update_lineup)->21208;
get_id(sc_doublematch_update_lineup)->21209;
get_id(cs_doublematch_sign)->21211;
get_id(sc_doublematch_sign)->21212;
get_id(cs_doublematch_sign_cancel)->21213;
get_id(sc_doublematch_sign_cancel)->21214;
get_id(cs_doublematch_times_buy)->21215;
get_id(sc_doublematch_times_buy)->21216;
get_id(cs_doublematch_roleinfo)->21217;
get_id(sc_doublematch_roleinfo)->21218;
get_id(cs_doublematch_rankinfo)->21219;
get_id(p_doublematch_rank_unit)->21221;
get_id(sc_doublematch_rankinfo)->21220;
get_id(sc_doublematch_quit)->21222;
get_id(cs_doublematch_record_list)->21223;
get_id(p_record_unit)->21227;
get_id(sc_doublematch_record_list)->21224;
get_id(cs_doublematch_replay)->21225;
get_id(sc_fight_double_request)->20005;
get_id(sc_doublematch_replay)->21226;
get_id(sc_doublematch_fight)->21228;
get_id(cs_goldenegg_use_item)->21101;
get_id(sc_goldenegg_use_item)->21102;
get_id(cs_goldenegg_smash)->21103;
get_id(p_reward_view)->12005;
get_id(sc_goldenegg_smash)->21104;
get_id(cs_goldenegg_roleinfo)->21107;
get_id(sc_goldenegg_roleinfo)->21108;
get_id(cs_goldenegg_shop)->21109;
get_id(p_goods_unit)->21111;
get_id(sc_goldenegg_shop)->21110;
get_id(cs_goldenegg_open)->21112;
get_id(sc_goldenegg_open)->21113;
get_id(sc_goldenegg_update_score)->21114;
get_id(p_member_info)->21001;
get_id(sc_matchRoom_init)->21002;
get_id(sc_matchRoom_ready)->21003;
get_id(sc_matchRoom_cancel)->21004;
get_id(sc_matchRoom_kick)->21005;
get_id(sc_matchRoom_exit)->21006;
get_id(sc_matchRoom_new)->21007;
get_id(cs_matchRoom_ready)->21009;
get_id(cs_matchRoom_cancel)->21010;
get_id(cs_matchRoom_exit)->21011;
get_id(sc_matchRoom_close)->21012;
get_id(cs_skin_compose)->20901;
get_id(sc_skin_compose)->20902;
get_id(cs_skin_activate)->20903;
get_id(sc_skin_activate)->20904;
get_id(cs_skin_equip)->20905;
get_id(sc_skin_equip)->20906;
get_id(cs_skin_demount)->20907;
get_id(sc_skin_demount)->20908;
get_id(p_skin)->20910;
get_id(cs_skin_info)->20911;
get_id(p_skin_buff)->20914;
get_id(sc_skin_info)->20912;
get_id(p_skin_info)->20913;
get_id(cs_home_info)->20801;
get_id(c_info)->20807;
get_id(sc_home_info)->20802;
get_id(cs_home_build)->20803;
get_id(sc_home_build)->20804;
get_id(cs_home_up_stage)->20805;
get_id(sc_home_up_stage)->20806;
get_id(c_need)->20808;
get_id(cs_home_get_friend)->20809;
get_id(sc_home_get_friend)->20810;
get_id(cs_home_task_info)->20811;
get_id(home_task)->20815;
get_id(sc_home_task_info)->20812;
get_id(cs_home_task_operate)->20813;
get_id(sc_home_task_operate)->20814;
get_id(cs_home_bounty_task_info)->20816;
get_id(sc_home_bounty_task_info)->20817;
get_id(cs_home_cirrus_info)->20818;
get_id(cirrus_node)->20824;
get_id(sc_home_cirrus_info)->20819;
get_id(cs_home_cirrus_fight)->20820;
get_id(sc_home_cirrus_fight)->20821;
get_id(cs_home_cirrus_operate)->20822;
get_id(sc_home_cirrus_operate)->20823;
get_id(cs_home_exchange)->20825;
get_id(sc_home_exchange)->20826;
get_id(cs_sign_reward_info)->20701;
get_id(p_sign_reward)->20703;
get_id(sc_sign_reward_info)->20702;
get_id(cs_sign_reward)->20704;
get_id(sc_sign_reward)->20705;
get_id(cs_awake_ger)->20601;
get_id(sc_awake_ger)->20602;
get_id(cs_awake_recast_ger)->20603;
get_id(sc_awake_recast_ger)->20604;
get_id(cs_awake_exchange_skill)->20605;
get_id(sc_awake_exchange_skill)->20606;
get_id(cs_awake_cancel_skill)->20608;
get_id(sc_awake_cancel_skill)->20609;
get_id(cs_generalteam_create)->20501;
get_id(sc_generalteam_create)->20502;
get_id(cs_generalteam_invite)->20503;
get_id(sc_generalteam_invite)->20504;
get_id(p_team_member_info)->20508;
get_id(sc_generalteam_invite_request)->20505;
get_id(cs_generalteam_invite_response)->20506;
get_id(sc_generalteam_invite_response)->20507;
get_id(p_team_info)->20510;
get_id(update_generalteam_info)->20509;
get_id(cs_generalteam_leaveteam)->20511;
get_id(sc_generalteam_leaveteam)->20512;
get_id(cs_generalteam_disbandteam)->20513;
get_id(sc_generalteam_disbandteam)->20514;
get_id(cs_generalteam_kick)->20515;
get_id(sc_generalteam_kick)->20516;
get_id(cs_generalteam_change_authority)->20517;
get_id(sc_generalteam_change_authority)->20518;
get_id(cs_generalteam_talk)->20519;
get_id(sc_generalteam_talk)->20520;
get_id(update_generalteam_talk)->20521;
get_id(cs_generalteam_info)->20522;
get_id(sc_generalteam_info)->20523;
get_id(cs_generalteam_change_teamtype)->20524;
get_id(sc_generalteam_change_teamtype)->20525;
get_id(cs_panicbuy_info)->20401;
get_id(p_ger_unit)->20309;
get_id(p_item_unit)->20308;
get_id(p_sell_reward_unit)->20307;
get_id(p_panic_buy_config)->20403;
get_id(sc_panicbuy_info)->20402;
get_id(cs_panicbuy_once)->20404;
get_id(sc_panicbuy_once)->20405;
get_id(cs_discount_activity_list)->20301;
get_id(p_activity_unit)->20303;
get_id(sc_discount_activity_list)->20302;
get_id(cs_discount_exchange_info)->20304;
get_id(p_exchange_config_unit)->20306;
get_id(sc_discount_exchange_info)->20305;
get_id(cs_discount_exchange)->20310;
get_id(sc_discount_exchange)->20311;
get_id(cs_discount_pay_activity_info)->20312;
get_id(p_pay_config_unit)->20314;
get_id(sc_discount_pay_activity_info)->20313;
get_id(cs_discount_pay_activity)->20315;
get_id(sc_discount_pay_activity)->20316;
get_id(p_homestead_log)->20201;
get_id(p_homestead_machine)->20202;
get_id(p_homestead)->20203;
get_id(sc_homestead_error)->20204;
get_id(cs_homestead_get_info)->20205;
get_id(sc_homestead_get_info)->20206;
get_id(cs_homestead_get_friend_info)->20207;
get_id(sc_homestead_get_friend_info)->20208;
get_id(cs_homestead_unlock_machine)->20209;
get_id(sc_homestead_unlock_machine)->20210;
get_id(cs_homestead_uproot_seed)->20211;
get_id(sc_homestead_uproot_seed)->20212;
get_id(cs_homestead_harvest)->20213;
get_id(sc_homestead_harvest)->20214;
get_id(cs_homestead_seeding)->20215;
get_id(sc_homestead_seeding)->20216;
get_id(sc_homestead_update_machine)->20217;
get_id(cs_homestead_change_ger)->20218;
get_id(sc_homestead_change_ger)->20219;
get_id(cs_homestead_mating)->20220;
get_id(sc_homestead_mating)->20221;
get_id(sc_homestead_mating_to_friend)->20222;
get_id(cs_homestead_addenergy)->20223;
get_id(sc_homestead_addenergy)->20224;
get_id(sc_homestead_addenergy_to_friend)->20225;
get_id(cs_homestead_get_log)->20226;
get_id(sc_homestead_get_log)->20227;
get_id(cs_homestead_get_friend_log)->20228;
get_id(sc_homestead_get_friend_log)->20229;
get_id(sc_homestead_sync_mating_cool_second)->20230;
get_id(sc_homestead_sync_ger)->20231;
get_id(sc_homestead_sync_add_enagy)->20232;
get_id(cs_homestead_compose)->20233;
get_id(sc_homestead_compose)->20234;
get_id(cs_homestead_compose_list)->20235;
get_id(p_compose_info)->20237;
get_id(sc_homestead_compose_list)->20236;
get_id(p_task)->20101;
get_id(cs_task_get_info)->20103;
get_id(sc_task_get_info)->20104;
get_id(cs_task_operate)->20105;
get_id(sc_task_operate)->20106;
get_id(sc_task_error)->20107;
get_id(sc_task_notify_change)->20108;
get_id(cs_fight_request)->20001;
get_id(cs_consumerank_info)->17101;
get_id(sc_consumerank_info)->17102;
get_id(cs_consumerank_list)->17103;
get_id(p_consumerank_unit)->17105;
get_id(sc_consumerank_list)->17104;
get_id(cs_buddy_partner_insert)->17001;
get_id(sc_buddy_partner_insert)->17002;
get_id(cs_buddy_partner_insert_batch)->17003;
get_id(sc_buddy_partner_insert_batch)->17004;
get_id(cs_buddy_partner_remove)->17005;
get_id(sc_buddy_partner_remove)->17006;
get_id(p_dSign_unit)->16901;
get_id(cs_dSign_info)->16902;
get_id(sc_dSign_info)->16903;
get_id(cs_dSign_sign)->16904;
get_id(sc_dSign_sign)->16905;
get_id(cs_dSign_reSign)->16906;
get_id(sc_dSign_reSign)->16907;
get_id(cs_dSign_get_sevenReward)->16908;
get_id(sc_dSign_get_sevenReward)->16909;
get_id(cs_dSign_get_monReward)->16910;
get_id(sc_dSign_get_monReward)->16911;
get_id(cs_dSign_mark_mon)->16912;
get_id(sc_dSign_mark_mon)->16913;
get_id(cs_trainerRear_brief)->16801;
get_id(sc_trainerRear_brief)->16802;
get_id(cs_trainerRear_info)->16803;
get_id(p_object_unit)->16812;
get_id(p_rear_machine)->16805;
get_id(sc_trainerRear_info)->16804;
get_id(cs_trainerRear_insert)->16806;
get_id(sc_trainerRear_insert)->16807;
get_id(cs_trainerRear_mature)->16808;
get_id(sc_trainerRear_mature)->16809;
get_id(cs_trainerRear_accelerate)->16810;
get_id(sc_trainerRear_accelerate)->16811;
get_id(cs_dojangrank_info)->16701;
get_id(sc_dojangrank_info)->16702;
get_id(cs_dojangrank_rank)->16703;
get_id(p_dojangrank_rank)->16707;
get_id(sc_dojangrank_rank)->16704;
get_id(cs_dojangrank_buy)->16705;
get_id(sc_dojangrank_buy)->16706;
get_id(cs_dojangrank_select_ger_type)->16710;
get_id(sc_dojangrank_select_ger_type)->16711;
get_id(cs_dojangrank_fight)->16712;
get_id(sc_dojangrank_fight)->16713;
get_id(cs_dojangrank_replay_list)->16714;
get_id(p_dojang_replay_info)->16718;
get_id(sc_dojangrank_replay_list)->16715;
get_id(cs_dojangrank_replay_detail)->16716;
get_id(sc_dojangrank_replay_detail)->16717;
get_id(cs_dojangrank_ger_view_other)->16719;
get_id(sc_dojangrank_ger_view_other)->16720;
get_id(cs_dojangrank_self_rank)->16721;
get_id(sc_dojangrank_self_rank)->16722;
get_id(cs_dojangrank_world_info)->16741;
get_id(sc_dojangrank_world_info)->16742;
get_id(cs_dojangrank_world_rank)->16743;
get_id(dr_ger_info)->16765;
get_id(p_dr_world_rank)->16747;
get_id(sc_dojangrank_world_rank)->16744;
get_id(cs_dojangrank_world_buy)->16745;
get_id(sc_dojangrank_world_buy)->16746;
get_id(cs_dojangrank_world_refresh_enemy)->16748;
get_id(sc_dojangrank_world_refresh_enemy)->16749;
get_id(cs_dojangrank_world_select_ger_type)->16750;
get_id(sc_dojangrank_world_select_ger_type)->16751;
get_id(cs_dojangrank_world_fight)->16752;
get_id(sc_dojangrank_world_fight)->16753;
get_id(cs_dojangrank_world_replay_list)->16754;
get_id(p_dr_dojang_replay_info)->16758;
get_id(sc_dojangrank_world_replay_list)->16755;
get_id(cs_dojangrank_world_replay_detail)->16756;
get_id(sc_dojangrank_world_replay_detail)->16757;
get_id(cs_dojangrank_world_ger_view_other)->16759;
get_id(sc_dojangrank_world_ger_view_other)->16760;
get_id(cs_dojangrank_world_top_replay_list)->16763;
get_id(sc_dojangrank_world_top_replay_list)->16764;
get_id(cs_trainerProf_info)->16601;
get_id(p_trainerProf_unit)->16603;
get_id(sc_trainerProf_info)->16602;
get_id(cs_trainerProf_uplevel)->16604;
get_id(sc_trainerProf_uplevel)->16605;
get_id(cs_trainerProf_battle_info)->16651;
get_id(p_trainerProf_battle_unit)->16653;
get_id(sc_trainerProf_battle_info)->16652;
get_id(cs_trainerProf_battle_uplevel)->16654;
get_id(sc_trainerProf_battle_uplevel)->16655;
get_id(cs_trainerProf_battle_unclock)->16656;
get_id(sc_trainerProf_battle_unclock)->16657;
get_id(cs_treasurebowl_info)->16501;
get_id(p_treasurebowl_draw)->16504;
get_id(p_treasurebowl_activity)->16503;
get_id(sc_treasurebowl_info)->16502;
get_id(sc_treasurebowl_update)->16505;
get_id(cs_treasurebowl_exchange)->16506;
get_id(sc_treasurebowl_exchange)->16507;
get_id(cs_treasurebowl_draw)->16508;
get_id(sc_treasurebowl_draw)->16509;
get_id(sc_treasurebowl_open)->16510;
get_id(cs_maintask_info)->16401;
get_id(p_task_unit)->15810;
get_id(sc_maintask_info)->16402;
get_id(cs_maintask_draw)->16403;
get_id(sc_maintask_draw)->16404;
get_id(cs_xbattle_info)->16301;
get_id(sc_xbattle_info)->16302;
get_id(cs_xbattle_challenge)->16303;
get_id(sc_xbattle_challenge)->16304;
get_id(cs_xbattle_raid)->16305;
get_id(p_xbattle_raid)->16320;
get_id(sc_xbattle_raid)->16306;
get_id(cs_xbattle_offline_reward)->16307;
get_id(sc_xbattle_offline_reward)->16308;
get_id(sc_xbattle_tri)->16309;
get_id(cs_xbattle_use_elixir)->16310;
get_id(sc_xbattle_use_elixir)->16311;
get_id(cs_xbattle_buy_quick)->16312;
get_id(sc_xbattle_buy_quick)->16313;
get_id(cs_xbattle_get_pass_reward)->16314;
get_id(sc_xbattle_get_pass_reward)->16315;
get_id(cs_xbattle_chapter_info)->16316;
get_id(p_xbattle_dungeon)->16323;
get_id(sc_xbattle_chapter_info)->16317;
get_id(cs_xbattle_set_reward_chapter)->16318;
get_id(sc_xbattle_set_reward_chapter)->16319;
get_id(cs_xbattle_start_reward)->16321;
get_id(sc_xbattle_start_reward)->16322;
get_id(cs_xbattle_challenge_boss)->16324;
get_id(sc_xbattle_challenge_boss)->16325;
get_id(cs_xbattle_package_full)->16398;
get_id(cs_xbattle_package_ok)->16399;
get_id(cs_exBoss_get_info)->16201;
get_id(p_exBoss_dtl)->16204;
get_id(p_exBoss_times_dtl)->16203;
get_id(p_exBoss_hit_list)->16205;
get_id(sc_exBoss_get_info)->16202;
get_id(cs_exBoss_hit)->16206;
get_id(sc_exBoss_hit)->16207;
get_id(cs_exBoss_buy_times)->16208;
get_id(sc_exBoss_buy_times)->16209;
get_id(cs_exBoss_get_reward)->16210;
get_id(sc_exBoss_get_reward)->16211;
get_id(sc_exBoss_update_times)->16212;
get_id(cs_exBoss_refresh)->16213;
get_id(sc_exBoss_refresh)->16214;
get_id(cs_exBoss_buy_cost)->16215;
get_id(sc_exBoss_buy_cost)->16216;
get_id(cs_exBoss_oneKey)->16217;
get_id(sc_exBoss_oneKey)->16218;
get_id(cs_tasklink_get_info)->16101;
get_id(p_level_info)->16133;
get_id(sc_tasklink_get_info)->16102;
get_id(cs_tasklink_get_progress)->16103;
get_id(p_point_info)->16131;
get_id(sc_tasklink_get_progress)->16104;
get_id(cs_tasklink_get_reward_log)->16105;
get_id(p_reward_log)->16132;
get_id(sc_tasklink_get_reward_log)->16106;
get_id(cs_tasklink_sign)->16107;
get_id(sc_tasklink_sign)->16108;
get_id(cs_tasklink_buy_time)->16109;
get_id(sc_tasklink_buy_time)->16110;
get_id(cs_tasklink_ready_notice)->16111;
get_id(sc_tasklink_ready_notice)->16112;
get_id(cs_tasklink_ready_opt)->16113;
get_id(sc_tasklink_ready_opt)->16114;
get_id(cs_tasklink_get_reward)->16115;
get_id(sc_tasklink_get_reward)->16116;
get_id(cs_activityFestival_info)->16001;
get_id(p_activityFestival_data)->16003;
get_id(p_activityFestival_box)->16005;
get_id(sc_activityFestival_info)->16002;
get_id(p_activityFestival_self)->16004;
get_id(p_activityFestival_box_get)->16006;
get_id(cs_activityFestival_self)->16007;
get_id(sc_activityFestival_self)->16008;
get_id(cs_activityFestival_sign)->16009;
get_id(sc_activityFestival_sign)->16010;
get_id(cs_activityFestival_box_get)->16011;
get_id(sc_activityFestival_box_get)->16012;
get_id(cs_activityFestival_sign2)->16013;
get_id(sc_activityFestival_sign2)->16014;
get_id(cs_familycross_info)->15901;
get_id(p_familycross_enermy)->15916;
get_id(p_familycross_info_dtl)->15905;
get_id(sc_familycross_info)->15902;
get_id(cs_familycross_sign)->15903;
get_id(sc_familycross_sign)->15904;
get_id(p_familycross_car)->15906;
get_id(cs_familycross_displayer)->15907;
get_id(sc_familycross_displayer)->15908;
get_id(cs_familycross_displayer_info)->15909;
get_id(sc_familycross_displayer_info)->15910;
get_id(p_familycross_player_fly)->15911;
get_id(cs_familycross_player_fly)->15912;
get_id(sc_familycross_player_fly)->15913;
get_id(cs_familycross_enermy_info)->15914;
get_id(sc_familycross_enermy_info)->15915;
get_id(p_familycross_war_site3)->15920;
get_id(p_city_pos)->15921;
get_id(p_site_pos)->15922;
get_id(p_born_pos)->15923;
get_id(p_familycross_war_car)->15924;
get_id(p_familycross_fighter)->15953;
get_id(p_familycross_war_fly)->15925;
get_id(p_familycross_war_fly2)->15928;
get_id(p_familycross_war_site)->15926;
get_id(p_familycross_war_city)->15927;
get_id(p_familycross_war_car2)->15929;
get_id(p_familycross_war_family_dtl)->15930;
get_id(cs_familycross_war_info)->15931;
get_id(p_familycross_city_des)->15978;
get_id(p_familycross_head_des)->15976;
get_id(sc_familycross_war_info)->15932;
get_id(cs_familycross_self_info)->15933;
get_id(sc_familycross_self_info)->15934;
get_id(cs_familycross_drive_car)->15935;
get_id(sc_familycross_drive_car)->15936;
get_id(cs_familycross_mov)->15937;
get_id(sc_familycross_mov)->15938;
get_id(cs_familycross_attack)->15939;
get_id(sc_familycross_attack)->15940;
get_id(cs_familycross_city_dtl)->15941;
get_id(sc_familycross_city_dtl)->15942;
get_id(cs_familycross_site_dtl)->15943;
get_id(sc_familycross_site_dtl)->15944;
get_id(cs_familycross_fly_dtl)->15945;
get_id(p_familycross_replay_dtl)->15962;
get_id(sc_familycross_fly_dtl)->15946;
get_id(sc_familycross_map_update)->15947;
get_id(sc_familycross_city_update)->15948;
get_id(sc_familycross_site_update)->15949;
get_id(sc_familycross_fly_update)->15950;
get_id(cs_familycross_be_driver)->15951;
get_id(sc_familycross_be_driver)->15952;
get_id(cs_familycross_drive_car_stop)->15954;
get_id(sc_familycross_drive_car_stop)->15955;
get_id(cs_familycross_mov_stop)->15956;
get_id(sc_familycross_mov_stop)->15957;
get_id(cs_familycross_mov_back)->15958;
get_id(sc_familycross_mov_back)->15959;
get_id(cs_familycross_reborn)->15960;
get_id(sc_familycross_reborn)->15961;
get_id(cs_familycross_replay)->15963;
get_id(sc_familycross_replay)->15964;
get_id(sc_familycross_attack_update)->15965;
get_id(cs_familycross_own_site)->15966;
get_id(sc_familycross_own_site)->15967;
get_id(cs_familycross_season_rank)->15968;
get_id(p_anubis_family)->15970;
get_id(sc_familycross_season_rank)->15969;
get_id(cs_familycross_seasoninfo)->15971;
get_id(sc_familycross_seasoninfo)->15972;
get_id(cs_familycross_family_rank)->15973;
get_id(p_anubis_family_member)->15975;
get_id(sc_familycross_family_rank)->15974;
get_id(sc_familycross_des_update)->15977;
get_id(p_familycross_battle_rank_dtl)->15979;
get_id(cs_familycross_battle_get_rank)->15980;
get_id(sc_familycross_battle_get_rank)->15981;
get_id(sc_familycross_battle_end)->15982;
get_id(cs_familycross_be_driver2)->15983;
get_id(sc_familycross_be_driver2)->15984;
get_id(cs_familycross_self_car)->15985;
get_id(sc_familycross_self_car)->15986;
get_id(cs_payGuide_info)->15801;
get_id(p_payGuide_unit)->15803;
get_id(sc_payGuide_info)->15802;
get_id(cs_payGuide_get_reward)->15804;
get_id(sc_payGuide_get_reward)->15805;
get_id(cs_payGuide_seven_info)->15806;
get_id(period_state)->15814;
get_id(sc_payGuide_seven_info)->15807;
get_id(cs_payGuide_seven_period_info)->15808;
get_id(sc_payGuide_seven_period_info)->15809;
get_id(cs_payGuide_seven_draw)->15811;
get_id(sc_payGuide_seven_draw)->15812;
get_id(sc_payGuide_seven_task_update)->15813;
get_id(sc_payGuide_seven_period_summary)->15815;
get_id(cs_homeBoss_info)->15701;
get_id(p_homeBoss)->15703;
get_id(sc_homeBoss_info)->15702;
get_id(p_boss)->15704;
get_id(cs_homeBoss_attack)->15705;
get_id(sc_homeBoss_attack)->15706;
get_id(cs_homeBoss_self)->15707;
get_id(sc_homeBoss_self)->15708;
get_id(cs_homeBoss_buy_times)->15709;
get_id(sc_homeBoss_buy_times)->15710;
get_id(cs_homeBoss_get_reward)->15711;
get_id(sc_homeBoss_get_reward)->15712;
get_id(cs_homeBoss_read)->15713;
get_id(sc_homeBoss_read)->15714;
get_id(p_lvlSgAttr_attr)->15601;
get_id(sc_lvlSgAttr_inc)->15602;
get_id(cs_galactica_info)->15501;
get_id(sc_galactica_info)->15502;
get_id(cs_galactica_sign)->15503;
get_id(sc_galactica_sign)->15504;
get_id(cs_galactica_buy)->15505;
get_id(sc_galactica_buy)->15506;
get_id(sc_galactica_times_update)->15507;
get_id(p_galactica_p_s)->15508;
get_id(p_galactica_pos)->15509;
get_id(p_galactica_mine)->15510;
get_id(p_galactica_mine_s)->15511;
get_id(p_galactica_player)->15512;
get_id(p_galactica_player_s)->15513;
get_id(p_galactica_fairy)->15514;
get_id(p_galactica_home)->15515;
get_id(sc_galactica_gas)->15516;
get_id(cs_galactica_war_base_info)->15518;
get_id(sc_galactica_war_base_info)->15519;
get_id(cs_galactica_self)->15520;
get_id(sc_galactica_self)->15521;
get_id(cs_galactica_mov)->15522;
get_id(sc_galactica_mov)->15523;
get_id(cs_galactica_attack)->15524;
get_id(sc_galactica_attack)->15525;
get_id(p_galactica_home_s)->15527;
get_id(sc_galactica_home_s)->15526;
get_id(sc_galactica_update)->15528;
get_id(cs_galactica_role_dtl)->15529;
get_id(p_galactica_replay_dtl)->15535;
get_id(sc_galactica_role_dtl)->15530;
get_id(cs_galactica_replay)->15531;
get_id(sc_galactica_replay)->15532;
get_id(cs_galactica_mov_stop)->15533;
get_id(sc_galactica_mov_stop)->15534;
get_id(p_galactica_rank_dtl)->15536;
get_id(p_galactica_talk)->15537;
get_id(cs_galactica_talk)->15538;
get_id(cs_galactica_get_talk)->15539;
get_id(sc_galactica_get_talk)->15540;
get_id(cs_galactica_get_rank)->15541;
get_id(sc_galactica_get_rank)->15542;
get_id(sc_galactica_talk)->15543;
get_id(sc_galactica_end_war)->15544;
get_id(cs_galactica_reborn)->15545;
get_id(sc_galactica_reborn)->15546;
get_id(cs_galactica_unrequest)->15547;
get_id(sc_galactica_unrequest)->15548;
get_id(cs_galactica_mov_in)->15549;
get_id(sc_galactica_mov_in)->15550;
get_id(cs_twins_info)->15401;
get_id(p_twins_rank_dtl)->15436;
get_id(p_twins_box_info)->15445;
get_id(sc_twins_info)->15402;
get_id(cs_twins_sign)->15403;
get_id(sc_twins_sign)->15404;
get_id(cs_twins_buy)->15405;
get_id(sc_twins_buy)->15406;
get_id(sc_twins_times_update)->15407;
get_id(p_twins_p_s)->15408;
get_id(p_twins_pos)->15409;
get_id(p_twins_mine)->15410;
get_id(p_twins_mine_s)->15411;
get_id(p_twins_player)->15412;
get_id(p_twins_player_s)->15413;
get_id(p_twins_fairy)->15414;
get_id(p_twins_home)->15415;
get_id(cs_twins_unrequest)->15416;
get_id(sc_twins_unrequest)->15417;
get_id(cs_twins_war_base_info)->15418;
get_id(sc_twins_war_base_info)->15419;
get_id(cs_twins_self)->15420;
get_id(sc_twins_self)->15421;
get_id(cs_twins_mov)->15422;
get_id(sc_twins_mov)->15423;
get_id(cs_twins_attack)->15424;
get_id(sc_twins_attack)->15425;
get_id(sc_twins_update)->15428;
get_id(cs_twins_role_dtl)->15429;
get_id(sc_twins_role_dtl)->15430;
get_id(cs_twins_mov_stop)->15433;
get_id(sc_twins_mov_stop)->15434;
get_id(p_twins_talk)->15437;
get_id(cs_twins_talk)->15438;
get_id(cs_twins_get_talk)->15439;
get_id(sc_twins_get_talk)->15440;
get_id(cs_twins_get_rank)->15441;
get_id(sc_twins_get_rank)->15442;
get_id(sc_twins_talk)->15443;
get_id(sc_twins_end_war)->15444;
get_id(cs_twins_open_reward_box)->15446;
get_id(sc_twins_open_reward_box)->15447;
get_id(cs_bounty_self_info)->15303;
get_id(p_bounty_unit)->15305;
get_id(p_bounty_data)->15312;
get_id(p_bounty_chapter_blood)->15314;
get_id(sc_bounty_self_info)->15304;
get_id(cs_bounty_challenge)->15306;
get_id(sc_bounty_challenge)->15307;
get_id(cs_bounty_buy_times)->15308;
get_id(sc_bounty_buy_times)->15309;
get_id(cs_bounty_get_reward)->15310;
get_id(sc_bounty_get_reward)->15311;
get_id(cs_trSpecial_info)->15201;
get_id(sc_trSpecial_info)->15202;
get_id(cs_trSpecial_select)->15203;
get_id(sc_trSpecial_select)->15204;
get_id(cs_trSpecial_clear)->15205;
get_id(sc_trSpecial_clear)->15206;
get_id(sc_trSpecial_fightPower)->15207;
get_id(cs_tvcard_info)->15101;
get_id(p_tvcard_info)->15103;
get_id(sc_tvcard_info)->15102;
get_id(cs_tvcard_select)->15104;
get_id(sc_tvcard_select)->15105;
get_id(cs_tvcard_rand)->15106;
get_id(sc_tvcard_rand)->15107;
get_id(cs_magicBook_swallow_ger)->15001;
get_id(sc_magicBook_swallow_ger)->15002;
get_id(p_magicBook_summary)->15003;
get_id(cs_magicBook_summary)->15004;
get_id(sc_magicBook_summary)->15005;
get_id(cs_magicBook_picture_reward)->15006;
get_id(sc_magicBook_picture_reward)->15007;
get_id(cs_magicBook_book_reward)->15008;
get_id(sc_magicBook_book_reward)->15009;
get_id(p_magicBook_attr)->15010;
get_id(cs_magicBook_book_info)->15011;
get_id(sc_magicBook_book_info)->15012;
get_id(sc_magicBook_book_detial)->15013;
get_id(cs_luckyRoll_get_list)->14901;
get_id(p_luckyRoll_card_inner)->14904;
get_id(p_baseBoxInfo)->14906;
get_id(p_lucky_role_card_p)->14905;
get_id(p_luckyRoll_card_outer)->14903;
get_id(sc_luckyRoll_get_list)->14902;
get_id(cs_luckyRoll_explore_one)->14907;
get_id(p_luckyRoll_card_oneTime)->14909;
get_id(sc_luckyRoll_explore_one)->14908;
get_id(cs_luckyRoll_refresh)->14911;
get_id(sc_luckyRoll_refresh)->14912;
get_id(cs_luckyRoll_open_base_box)->14913;
get_id(sc_luckyRoll_open_base_box)->14914;
get_id(p_luckyRoll_ranker)->14915;
get_id(cs_luckyRoll_get_rankInfo)->14916;
get_id(sc_luckyRoll_get_rankInfo)->14917;
get_id(cs_luckyRoll_get_rank_Reward)->14918;
get_id(sc_luckyRoll_get_rank_Reward)->14919;
get_id(sc_luckyRoll_change_state)->14924;
get_id(cs_luckyRoll_explore_ten)->14925;
get_id(sc_luckyRoll_explore_ten)->14926;
get_id(cs_carlos_sign)->14801;
get_id(sc_carlos_sign)->14802;
get_id(cs_carlos_plane_uplevel)->14803;
get_id(sc_carlos_plane_uplevel)->14804;
get_id(cs_carlos_buy)->14805;
get_id(sc_carlos_buy)->14806;
get_id(cs_carlos_info)->14807;
get_id(sc_carlos_info)->14808;
get_id(sc_carlos_times_update)->14809;
get_id(p_carlos_pos)->14811;
get_id(p_carlos_mine)->14812;
get_id(p_carlos_player)->14813;
get_id(p_carlos_fairy)->14814;
get_id(cs_carlos_war_base_info)->14815;
get_id(sc_carlos_war_base_info)->14816;
get_id(sc_carlos_war_update)->14817;
get_id(cs_carlos_mine_detail)->14818;
get_id(sc_carlos_mine_detail)->14819;
get_id(cs_carlos_self)->14820;
get_id(sc_carlos_self)->14821;
get_id(cs_carlos_mov)->14822;
get_id(sc_carlos_mov)->14823;
get_id(cs_carlos_attack)->14824;
get_id(sc_carlos_attack)->14825;
get_id(cs_carlos_ownMine)->14826;
get_id(sc_carlos_ownMine)->14827;
get_id(sc_carlos_update)->14828;
get_id(cs_carlos_role_dtl)->14829;
get_id(sc_carlos_role_dtl)->14830;
get_id(cs_carlos_replay)->14831;
get_id(sc_carlos_replay)->14832;
get_id(cs_carlos_mov_stop)->14833;
get_id(sc_carlos_mov_stop)->14834;
get_id(cs_carlos_talk)->14838;
get_id(cs_carlos_get_talk)->14839;
get_id(sc_carlos_get_talk)->14840;
get_id(cs_carlos_get_rank)->14841;
get_id(sc_carlos_get_rank)->14842;
get_id(sc_carlos_talk)->14843;
get_id(sc_carlos_end_war)->14844;
get_id(cs_carlos_reborn)->14845;
get_id(sc_carlos_reborn)->14846;
get_id(cs_carlos_unrequest)->14847;
get_id(sc_carlos_unrequest)->14848;
get_id(cs_carlos_rank_list)->14849;
get_id(player_rank_info)->14851;
get_id(sc_carlos_rank_list)->14850;
get_id(cs_carlos_season_info)->14852;
get_id(sc_carlos_season_info)->14853;
get_id(cs_carlos_plane_select)->14854;
get_id(sc_carlos_plane_select)->14855;
get_id(cs_carlos_relic_sign)->14861;
get_id(sc_carlos_relic_sign)->14862;
get_id(cs_carlos_relic_info)->14863;
get_id(sc_carlos_relic_info)->14864;
get_id(cs_carlos_relic_war_base_info)->14865;
get_id(relic_role_other)->14896;
get_id(relic_island)->14878;
get_id(sc_carlos_relic_war_base_info)->14866;
get_id(sc_carlos_relic_war_update)->14867;
get_id(cs_carlos_relic_mov)->14868;
get_id(sc_carlos_relic_mov)->14869;
get_id(cs_carlos_relic_mov_stop)->14870;
get_id(sc_carlos_relic_mov_stop)->14871;
get_id(cs_carlos_relic_attack)->14872;
get_id(sc_carlos_relic_attack)->14873;
get_id(cs_carlos_relic_active)->14874;
get_id(sc_carlos_relic_active)->14875;
get_id(cs_carlos_relic_open_reward_box)->14876;
get_id(sc_carlos_relic_open_reward_box)->14877;
get_id(cs_carlos_relic_buy)->14879;
get_id(sc_carlos_relic_buy)->14880;
get_id(cs_carlos_relic_sign_cancel)->14881;
get_id(sc_carlos_relic_sign_cancel)->14882;
get_id(cs_carlos_relic_self)->14883;
get_id(sc_carlos_relic_self)->14884;
get_id(cs_carlos_relic_role_dtl)->14885;
get_id(sc_carlos_relic_role_dtl)->14886;
get_id(sc_carlos_relic_times_update)->14887;
get_id(sc_carlos_relic_end_war)->14888;
get_id(cs_carlos_relic_island_detail)->14889;
get_id(sc_carlos_relic_island_detail)->14890;
get_id(cs_carlos_relic_talk)->14891;
get_id(cs_carlos_relic_get_talk)->14892;
get_id(sc_carlos_relic_get_talk)->14893;
get_id(sc_carlos_relic_talk)->14894;
get_id(sc_carlos_relic_update)->14895;
get_id(cs_carlos_change_plane)->14897;
get_id(sc_carlos_change_plane)->14898;
get_id(p_carlos_plane_dtl)->14860;
get_id(sc_carlos_plane_use_info)->14899;
get_id(cs_monthVIP_info)->14701;
get_id(sc_monthVIP_info)->14702;
get_id(sc_monthVip_success)->14703;
get_id(cs_monthVIP_get_reward)->14704;
get_id(sc_monthVIP_get_reward)->14705;
get_id(cs_monthVIP_buy)->14706;
get_id(sc_monthVIP_buy)->14707;
get_id(cs_monthVIP_get_growth_fund_info)->14708;
get_id(p_growth_fund_state)->14714;
get_id(sc_monthVIP_get_growth_fund_info)->14709;
get_id(cs_monthVIP_buy_growth_fund)->14710;
get_id(sc_monthVIP_buy_growth_fund)->14711;
get_id(cs_monthVIP_get_growth_reward)->14712;
get_id(sc_monthVIP_get_growth_reward)->14713;
get_id(cs_talent_get_info)->14601;
get_id(p_talent)->14603;
get_id(sc_talent_get_info)->14602;
get_id(cs_talent_study)->14604;
get_id(sc_talent_study)->14605;
get_id(cs_talent_undo)->14606;
get_id(sc_talent_undo)->14607;
get_id(cs_talent_cooldown)->14608;
get_id(sc_talent_cooldown)->14609;
get_id(cs_trumpet_message)->14501;
get_id(sc_trumpet_message)->14502;
get_id(sc_trumpet_message_info)->14503;
get_id(cs_trumpet_recent_list)->14504;
get_id(sc_trumpet_recent_list)->14509;
get_id(cs_trumpet_get_bonus)->14510;
get_id(p_get_info)->14514;
get_id(p_bonus_info)->14513;
get_id(sc_trumpet_get_bonus_failed)->14511;
get_id(sc_trumpet_get_bonus)->14512;
get_id(cs_trumpet_redpacket_status)->14521;
get_id(sc_trumpet_redpacket_status)->14522;
get_id(sc_trumpet_new_redpacket_status)->14523;
get_id(cs_trumpet_get_all_publishers)->14524;
get_id(redpacket_info)->14531;
get_id(p_publisher_info)->14530;
get_id(sc_trumpet_get_all_publishers)->14525;
get_id(sc_trumpet_notice_publisher)->14526;
get_id(cs_trumpet_redpacket_openclose)->14527;
get_id(sc_trumpet_redpacket_openclose)->14532;
get_id(cs_trumpet_redpacket_get_reward)->14528;
get_id(sc_trumpet_redpacket_get_reward)->14529;
get_id(cs_trumpet_get_world_publishers)->14533;
get_id(sc_trumpet_get_world_publishers)->14534;
get_id(cs_changename)->14400;
get_id(sc_changename)->14401;
get_id(cs_changename_freetimes)->14402;
get_id(sc_changename_freetimes)->14403;
get_id(cs_familyBoss_base_info)->14301;
get_id(p_family_boss_base_info)->14303;
get_id(sc_familyBoss_base_info)->14302;
get_id(cs_familyBoss_attack)->14304;
get_id(sc_familyBoss_attack)->14305;
get_id(cs_familyBoss_hatch_egg)->14306;
get_id(sc_familyBoss_hatch_egg)->14307;
get_id(cs_familyBoss_feed_boss)->14308;
get_id(sc_familyBoss_feed_boss)->14309;
get_id(cs_familyBoss_set_boss_time)->14310;
get_id(sc_familyBoss_set_boss_time)->14311;
get_id(sc_familyBoss_boss_be_boss)->14312;
get_id(sc_familyBoss_boss_born)->14313;
get_id(cs_familyBoss_get_rank)->14314;
get_id(p_family_boss_ranker)->14316;
get_id(sc_familyBoss_get_rank)->14315;
get_id(sc_familyBoss_bc_attack)->14317;
get_id(sc_familyBoss_boss_dead)->14318;
get_id(sc_familyBoss_bc_set_boss_time)->14319;
get_id(sc_familyBoss_boss_unlock)->14320;
get_id(cs_familyTek_info)->14201;
get_id(p_ger_view2)->14213;
get_id(p_reward_info2)->14212;
get_id(p_familyTekDtl)->14203;
get_id(sc_familyTek_info)->14202;
get_id(cs_familyTek_upLevel)->14204;
get_id(sc_familyTek_upLevel)->14205;
get_id(cs_familyTek_cost)->14206;
get_id(sc_familyTek_cost)->14207;
get_id(cs_familyTek_wallet)->14208;
get_id(sc_familyTek_wallet)->14209;
get_id(p_familyTek_Ger_attr)->14210;
get_id(p_familyTek_Generate_buff)->14211;
get_id(cs_familyTek_donate)->14214;
get_id(sc_familyTek_donate)->14215;
get_id(sc_family_levelup)->14216;
get_id(cs_familyfight_info)->13701;
get_id(p_familyfight_info_dtl)->13705;
get_id(sc_familyfight_info)->13702;
get_id(cs_familyfight_sign)->13703;
get_id(sc_familyfight_sign)->13704;
get_id(p_familyfighter_member_info)->13707;
get_id(p_familyfighter_info_dtl)->13706;
get_id(cs_familyfight_fighter_info)->13708;
get_id(sc_familyfight_fighter_info)->13709;
get_id(cs_familyfight_attack)->13710;
get_id(p_familyfight_record_dtl)->13718;
get_id(sc_familyfight_attack)->13711;
get_id(cs_familyfight_result)->13712;
get_id(p_familyfight_result_dtl)->13714;
get_id(sc_familyfight_result)->13713;
get_id(cs_familyfight_get_fight_record_list)->13715;
get_id(p_familyfight_record_info)->13717;
get_id(sc_familyfight_get_fight_record_list)->13716;
get_id(cs_familyfight_replay)->13719;
get_id(sc_familyfight_replay)->13720;
get_id(cs_familyfight_get_fighter_history)->13721;
get_id(sc_familyfight_get_fighter_history)->13722;
get_id(sc_familyfight_update_star_info)->13723;
get_id(sc_familyfight_update_state_info)->13724;
get_id(cs_familyfight_rankerList)->13725;
get_id(p_familyfight_ranker)->13727;
get_id(sc_familyfight_rankerList)->13726;
get_id(cs_familyfight_instance_open_state)->13728;
get_id(family_instance_open_state)->13730;
get_id(sc_familyfight_instance_open_state)->13729;
get_id(cs_familyfight_instance_boss_info)->13731;
get_id(instance_boss_info)->13733;
get_id(sc_familyfight_instance_boss_info)->13732;
get_id(cs_familyfight_attack_boss)->13734;
get_id(sc_familyfight_attack_boss)->13735;
get_id(cs_familyfight_select_instance)->13736;
get_id(sc_familyfight_select_instance)->13737;
get_id(cs_familyfight_instance_reward_info)->13738;
get_id(instance_damage_info)->13745;
get_id(instance_reward)->13742;
get_id(sc_familyfight_instance_reward_info)->13739;
get_id(cs_familyfight_instance_get_reward)->13740;
get_id(sc_familyfight_instance_get_reward)->13741;
get_id(cs_familyfight_bug_attack_time)->13743;
get_id(sc_familyfight_bug_attack_time)->13744;
get_id(cs_familyfight_get_fighter)->13746;
get_id(sc_familyfight_get_fighter)->13747;
get_id(cs_familyfight_select_fighter)->13748;
get_id(sc_familyfight_select_fighter)->13749;
get_id(cs_alien_info)->13601;
get_id(p_alien_fighter)->13606;
get_id(sc_alien_info)->13602;
get_id(sc_alien_sign_info)->13603;
get_id(cs_alien_first_five)->13604;
get_id(sc_alien_first_five)->13605;
get_id(cs_alien_kill_num_rank)->13607;
get_id(p_alien_fighter2)->13609;
get_id(sc_alien_kill_num_rank)->13608;
get_id(cs_alien_kill_continuous_rank)->13610;
get_id(p_alien_fighter3)->13612;
get_id(sc_alien_kill_continuous_rank)->13611;
get_id(cs_alien_guess_info)->13613;
get_id(sc_alien_guess_info)->13614;
get_id(cs_alien_guess)->13615;
get_id(sc_alien_guess)->13616;
get_id(cs_alien_reset)->13617;
get_id(sc_alien_reset)->13618;
get_id(cs_alien_fight)->13619;
get_id(sc_alien_fight)->13620;
get_id(cs_alien_sign)->13621;
get_id(sc_alien_sign)->13622;
get_id(cs_alien_self_record)->13623;
get_id(p_alien_self_record)->13627;
get_id(sc_alien_self_record)->13624;
get_id(cs_alien_record)->13625;
get_id(p_id_num)->11403;
get_id(p_mail_reward)->11404;
get_id(p_alien_record3)->13662;
get_id(sc_alien_record)->13626;
get_id(p_alien_record)->13628;
get_id(sc_alien_update_times)->13629;
get_id(cs_alien_self_fight_replay)->13630;
get_id(sc_alien_self_fight_replay)->13631;
get_id(cs_alien_fight_replay)->13632;
get_id(sc_alien_fight_repaly)->13633;
get_id(sc_alien_new_fighter_list)->13634;
get_id(cs_alien_leave)->13635;
get_id(sc_alien_new_self_record)->13636;
get_id(cs_alien_view_other)->13637;
get_id(sc_alien_view_other)->13638;
get_id(cs_alien_view_other_dtl)->13639;
get_id(cs_alien_buy_times)->13640;
get_id(sc_alien_buy_times)->13641;
get_id(cs_alien_active)->13642;
get_id(sc_alien_active)->13643;
get_id(p_alien_record2)->13644;
get_id(p_alien_finals_record)->13645;
get_id(p_alien_finals_round_record)->13646;
get_id(cs_alien_finals_records)->13647;
get_id(sc_alien_finals_records)->13648;
get_id(p_alien_finals_role_info)->13649;
get_id(cs_alien_finals_list)->13650;
get_id(sc_alien_finals_list)->13651;
get_id(cs_alien_finals_guess)->13652;
get_id(sc_alien_finals_guess)->13653;
get_id(cs_alien_finals_fight_replay)->13654;
get_id(sc_alien_finals_fight_replay)->13655;
get_id(cs_alien_finals_info)->13656;
get_id(sc_alien_finals_info)->13657;
get_id(cs_alien_finals_stake_list)->13658;
get_id(sc_alien_finals_stake_list)->13659;
get_id(cs_alien_finals_self_info)->13660;
get_id(sc_alien_finals_self_info)->13661;
get_id(cs_alien_self_rank)->13663;
get_id(sc_alien_self_rank)->13664;
get_id(cs_team_pk_info)->13501;
get_id(p_team_member)->13503;
get_id(sc_team_pk_open)->13502;
get_id(sc_team_pk_close)->13504;
get_id(cs_team_refresh)->13505;
get_id(sc_team_refresh)->13506;
get_id(cs_team_fight)->13507;
get_id(p_team_member2)->13509;
get_id(sc_team_fight_result)->13508;
get_id(cs_team_rank)->13510;
get_id(p_team_member3)->13512;
get_id(sc_team_rank)->13511;
get_id(cs_team_record)->13513;
get_id(p_team_record)->13515;
get_id(sc_team_record)->13514;
get_id(cs_team_self_record)->13516;
get_id(p_team_self_record)->13518;
get_id(sc_team_self_record)->13517;
get_id(cs_team_move)->13519;
get_id(sc_team_move)->13520;
get_id(sc_team_pk_not_open)->13521;
get_id(sc_team_fight_error)->13522;
get_id(cs_team_fight_replay)->13523;
get_id(sc_team_fight_replay)->13524;
get_id(cs_team_self_fight_replay)->13525;
get_id(sc_team_self_fight_replay)->13526;
get_id(cs_team_view_other)->13527;
get_id(sc_team_view_other)->13528;
get_id(cs_team_view_other_dtl)->13529;
get_id(p_lieu_view)->10471;
get_id(p_ger_pos)->10413;
get_id(sc_team_view_other_dtl)->13530;
get_id(cs_team_new_status)->13531;
get_id(sc_team_new_status)->13532;
get_id(p_race_rec)->13402;
get_id(sc_race_new_fight)->13401;
get_id(p_race_fighter)->13403;
get_id(cs_race_history)->13404;
get_id(sc_race_history)->13405;
get_id(cs_race_replay)->13406;
get_id(sc_race_replay)->13407;
get_id(cs_race_fight_list)->13408;
get_id(sc_race_fight_list)->13409;
get_id(cs_race_sign)->13410;
get_id(sc_race_sign)->13411;
get_id(cs_race_info)->13412;
get_id(p_race_pos)->13416;
get_id(sc_race_info)->13413;
get_id(cs_race_enter)->13414;
get_id(cs_race_leave)->13415;
get_id(cs_race_pos_history)->13417;
get_id(sc_race_pos_history)->13418;
get_id(sc_race_new_first)->13419;
get_id(sc_race_new_status)->13420;
get_id(cs_race_is_open)->13421;
get_id(sc_race_is_open)->13422;
get_id(cs_race_auto_sign)->13423;
get_id(sc_race_auto_sign)->13424;
get_id(cs_race_auto_unsign)->13425;
get_id(sc_race_auto_unsign)->13426;
get_id(cs_race_self_history)->13427;
get_id(sc_race_self_history)->13428;
get_id(cs_race_guess_info)->13429;
get_id(sc_race_guess_info)->13430;
get_id(cs_race_guess)->13431;
get_id(sc_race_guess)->13432;
get_id(cs_race2_info)->13441;
get_id(arena_fighter)->13459;
get_id(sc_race2_info)->13442;
get_id(cs_race2_sign)->13443;
get_id(sc_race2_sign)->13444;
get_id(cs_race2_openclose)->13445;
get_id(sc_race2_openclose)->13446;
get_id(cs_race2_arena_fighter)->13447;
get_id(sc_race2_arena_fighter)->13448;
get_id(cs_race2_fight)->13449;
get_id(sc_race2_fight)->13450;
get_id(cs_race2_knockout_fighter)->13451;
get_id(race2_fight_info)->13460;
get_id(sc_race2_knockout_fighter)->13452;
get_id(cs_race2_get_gamble)->13453;
get_id(gamble_info)->13461;
get_id(sc_race2_get_gamble)->13454;
get_id(cs_race2_do_gamble)->13455;
get_id(sc_race2_do_gamble)->13456;
get_id(cs_race2_final_info)->13457;
get_id(sc_race2_final_info)->13458;
get_id(cs_race2_get_fight_rec)->13462;
get_id(sc_race2_get_fight_rec)->13463;
get_id(cs_race2_cancel_sign)->13464;
get_id(sc_race2_cancel_sign)->13465;
get_id(p_family_member_info)->13302;
get_id(p_family_info)->13301;
get_id(p_family_request)->13303;
get_id(p_family_summary)->13304;
get_id(cs_family_get_list)->13305;
get_id(sc_family_get_list)->13306;
get_id(cs_family_create)->13307;
get_id(sc_family_create)->13308;
get_id(cs_family_request_join)->13309;
get_id(sc_family_request_join)->13310;
get_id(cs_family_cancel_join)->13311;
get_id(sc_family_cancel_join)->13312;
get_id(cs_family_agree_join)->13313;
get_id(sc_family_agree_join)->13314;
get_id(cs_family_refuse_join)->13315;
get_id(sc_family_refuse_join)->13316;
get_id(cs_family_get_info)->13317;
get_id(sc_family_get_info)->13318;
get_id(cs_family_kick)->13319;
get_id(sc_family_kick)->13320;
get_id(cs_family_create_consume)->13321;
get_id(sc_family_create_consume)->13322;
get_id(cs_family_leave)->13323;
get_id(sc_family_leave)->13324;
get_id(cs_family_change_notice)->13325;
get_id(sc_family_change_notice)->13326;
get_id(cs_family_request_list)->13327;
get_id(sc_family_request_list)->13328;
get_id(sc_family_del_request)->13329;
get_id(cs_family_get_log_list)->13330;
get_id(p_family_log_dtl)->13354;
get_id(sc_family_get_log_list)->13331;
get_id(cs_family_get_contribute_info)->13332;
get_id(p_familyContributeLog)->13335;
get_id(p_familyContributeType)->13334;
get_id(sc_family_get_contribute_info)->13333;
get_id(cs_family_do_contribute)->13336;
get_id(sc_family_do_contribute)->13337;
get_id(sc_family_update_exp)->13338;
get_id(cs_family_search_by_family_name)->13339;
get_id(sc_family_search_by_family_name)->13340;
get_id(cs_family_change_member_power)->13341;
get_id(sc_family_change_member_power)->13342;
get_id(sc_family_update_family_info)->13343;
get_id(cs_family_send_role_energy)->13344;
get_id(sc_family_send_role_energy)->13345;
get_id(cs_family_get_role_energy)->13346;
get_id(sc_family_get_role_energy)->13347;
get_id(cs_family_get_role_send_energy_list)->13348;
get_id(sc_family_get_role_send_energy_list)->13349;
get_id(cs_family_get_member_power)->13350;
get_id(sc_family_get_member_power)->13351;
get_id(cs_family_get_send_role_energy_list)->13352;
get_id(sc_family_get_send_role_energy_list)->13353;
get_id(sc_family_update_contribute_log)->13355;
get_id(p_family_storage)->13356;
get_id(cs_family_storage_info)->13357;
get_id(sc_family_storage_info)->13358;
get_id(cs_family_storage_req)->13359;
get_id(sc_family_storage_req)->13360;
get_id(cs_family_storage_assign)->13361;
get_id(sc_family_storage_assign)->13362;
get_id(sc_family_storage_update)->13363;
get_id(cs_family_owner_impeach)->13364;
get_id(sc_family_owner_impeach)->13365;
get_id(cs_family_wallet)->13366;
get_id(sc_family_wallet)->13367;
get_id(cs_family_change_slogan)->13368;
get_id(sc_family_change_slogan)->13369;
get_id(cs_family_worship)->13370;
get_id(sc_family_worship)->13371;
get_id(cs_family_worship_fight)->13372;
get_id(sc_family_worship_fight)->13373;
get_id(cs_family_worship_info)->13374;
get_id(sc_family_worship_info)->13375;
get_id(sc_family_worship_self_push)->13376;
get_id(sc_family_worship_refresh_fight_reward)->13377;
get_id(sc_family_worship_family_push)->13378;
get_id(cs_family_worship_limit)->13379;
get_id(sc_family_worship_limit)->13380;
get_id(cs_family_get_member_list)->13381;
get_id(sc_family_get_member_list)->13382;
get_id(cs_family_get_donate_contribute_list)->13383;
get_id(p_ger_donate_record_unit)->13386;
get_id(p_item_donate_record_unit)->13387;
get_id(p_donate_info)->13385;
get_id(sc_family_get_donate_contribute_list)->13384;
get_id(cs_family_impeach_list)->13388;
get_id(sc_family_impeach_list)->13389;
get_id(cs_family_donate_contribution_summary)->13390;
get_id(p_family_memeber_donate_info_summary)->13392;
get_id(sc_family_donate_contribution_summary)->13391;
get_id(cs_family_invite_request)->13393;
get_id(sc_family_invite_request)->13394;
get_id(cs_combine_do)->13201;
get_id(sc_combine_fail)->13202;
get_id(p_newGer)->13205;
get_id(sc_combine_ger)->13203;
get_id(p_newEquip)->13206;
get_id(sc_combine_equip)->13204;
get_id(cs_combine_info)->13207;
get_id(sc_combine_info)->13208;
get_id(cs_combine_mage_info)->13209;
get_id(p_mage_config)->13211;
get_id(sc_combine_mage_info)->13210;
get_id(cs_combine_do_mage)->13212;
get_id(sc_combine_do_mage)->13213;
get_id(cs_firecracker_open)->13101;
get_id(p_discount)->13103;
get_id(sc_firecracker_open)->13102;
get_id(sc_firecracker_info_sync)->13104;
get_id(cs_firecracker_close)->13105;
get_id(cs_firecracker_setoff)->13106;
get_id(sc_firecracker_setoff)->13107;
get_id(cs_firecracker_rank)->13108;
get_id(p_firecracker_rank)->13110;
get_id(sc_firecracker_rank)->13109;
get_id(cs_firecracker_get_reward)->13111;
get_id(sc_firecracker_get_reward)->13112;
get_id(cs_firecracker_rewardList)->13113;
get_id(sc_firecracker_rewardList)->13114;
get_id(cs_treaHouse_get_list)->13001;
get_id(p_treaHouse_card)->13002;
get_id(p_baseBoxOpenInfo)->13022;
get_id(sc_treaHouse_get_list)->13003;
get_id(cs_treaHouse_is_open)->13004;
get_id(sc_treaHouse_is_open)->13005;
get_id(cs_treaHouse_explore_one)->13006;
get_id(p_treaHouse_card_oneTime)->13008;
get_id(sc_treaHouse_explore_one)->13007;
get_id(cs_treaHouse_explore_ten)->13009;
get_id(sc_treaHouse_explore_ten)->13010;
get_id(cs_treaHouse_refresh)->13011;
get_id(sc_treaHouse_refresh)->13012;
get_id(cs_treaHouse_open_base_box)->13013;
get_id(sc_treaHouse_open_base_box)->13014;
get_id(p_treaHouse_ranker)->13015;
get_id(cs_treaHouse_get_rankInfo)->13016;
get_id(sc_treaHouse_get_rankInfo)->13017;
get_id(cs_treaHouse_get_rank_Reward)->13018;
get_id(sc_treaHouse_get_rank_Reward)->13019;
get_id(cs_treaHouse_get_baseBoxRewardInfo)->13020;
get_id(p_treaHouse_BaseReward_Info)->13023;
get_id(sc_treaHouse_get_baseBoxRewardInfo)->13021;
get_id(sc_treaHouse_change_state)->13024;
get_id(p_cross_rec)->12902;
get_id(sc_cross_new_fight)->12901;
get_id(cs_cross_history)->12903;
get_id(sc_cross_history)->12904;
get_id(cs_cross_replay)->12905;
get_id(sc_cross_replay)->12906;
get_id(sc_cross_fight_list)->12907;
get_id(cs_cross_sign)->12910;
get_id(sc_cross_sign)->12911;
get_id(cs_cross_info)->12920;
get_id(sc_cross_info)->12921;
get_id(cs_cross_enter)->12922;
get_id(cs_cross_leave)->12923;
get_id(cs_cross_view_list)->12924;
get_id(p_view_data)->12926;
get_id(sc_cross_view_list)->12925;
get_id(cs_cross_support)->12930;
get_id(sc_cross_support)->12931;
get_id(cs_cross_price_list)->12932;
get_id(p_price_info)->12934;
get_id(sc_cross_price_list)->12933;
get_id(cs_emperor_get_open_time)->12801;
get_id(sc_emperor_get_open_time)->12802;
get_id(cs_emperor_enter)->12803;
get_id(p_emp_fighter)->12805;
get_id(sc_emperor_enter)->12804;
get_id(sc_emperor_broadcast_fightInfo)->12806;
get_id(cs_emperor_replay)->12807;
get_id(p_emperor_replayInfo)->12819;
get_id(sc_emperor_replay)->12808;
get_id(cs_emperor_quit)->12809;
get_id(cs_emperor_get_bet_info)->12810;
get_id(p_bet)->12812;
get_id(sc_emperor_get_bet_info)->12811;
get_id(cs_emperor_role_bet)->12813;
get_id(sc_emperor_role_bet)->12814;
get_id(cs_emperor_bet_info)->12815;
get_id(sc_emperor_bet_info)->12816;
get_id(cs_emperor_get_replay)->12817;
get_id(sc_emperor_get_replay)->12818;
get_id(sc_emperor_bc_fight_end)->12820;
get_id(cs_challengeGod_info)->12701;
get_id(sc_challengeGod_info)->12702;
get_id(cs_challengeGod_select_ger)->12703;
get_id(sc_challengeGod_select_ger)->12704;
get_id(cs_challengeGod_challenge_dungeon_one)->12705;
get_id(p_ger_add_exp)->10211;
get_id(p_reward)->10210;
get_id(sc_challengeGod_challenge_dungeon_one)->12706;
get_id(cs_challengeGod_challenge_dungeon_ten)->12707;
get_id(p_challengeGod_result)->12708;
get_id(sc_challengeGod_challenge_ten)->12709;
get_id(cs_talk_world)->12601;
get_id(sc_talk_world)->12602;
get_id(sc_talk_world_message)->12603;
get_id(cs_talk_gag_one)->12604;
get_id(cs_talk_ungag_one)->12605;
get_id(cs_talk_get_gag_list)->12606;
get_id(gag_info)->12616;
get_id(sc_talk_get_gag_list)->12607;
get_id(cs_talk_recent_list)->12608;
get_id(sc_talk_recent_list)->12609;
get_id(cs_talk_send_whisper)->12610;
get_id(sc_talk_send_whisper)->12611;
get_id(cs_talk_get_whisper)->12612;
get_id(p_whisper_record)->12614;
get_id(sc_talk_get_whisper)->12613;
get_id(sc_talk_whisper_notice)->12615;
get_id(sc_push_highlight_Info)->12501;
get_id(cs_nanm_open)->12401;
get_id(sc_nanm_open)->12402;
get_id(sc_nanm_init_state)->12403;
get_id(cs_nanm_close)->12404;
get_id(cs_nanm_buff)->12405;
get_id(sc_nanm_buff)->12406;
get_id(cs_nanm_last_info)->12411;
get_id(sc_nanm_last_info_ignore)->12412;
get_id(p_nanm_info)->12415;
get_id(sc_nanm_last_info_win)->12413;
get_id(sc_nanm_last_info_fail)->12414;
get_id(cs_nanm_cur_info)->12416;
get_id(sc_nanm_cur_info_ignore)->12417;
get_id(sc_nanm_cur_info)->12418;
get_id(sc_nanm_hp_sync)->12420;
get_id(p_nanm_harm)->12422;
get_id(sc_nanm_harm_broadcast)->12421;
get_id(sc_nanm_buff_sync)->12424;
get_id(p_role_stastic)->11255;
get_id(sc_nanm_stop)->12425;
get_id(cs_nanm_rank_sync)->12426;
get_id(sc_nanm_rank_sync)->12427;
get_id(cs_nanm_fight)->12431;
get_id(sc_nanm_fight)->12432;
get_id(cs_nanm_reborn)->12441;
get_id(sc_nanm_reborn)->12442;
get_id(cs_nanm_offline_play)->12443;
get_id(sc_nanm_offline_play)->12444;
get_id(cs_nanm_open_time)->12450;
get_id(sc_nanm_open_time)->12451;
get_id(cs_nanm_reward)->12452;
get_id(sc_nanm_reward)->12453;
get_id(cs_version)->12301;
get_id(sc_version)->12302;
get_id(cs_gift_request)->12201;
get_id(sc_gift_request)->12202;
get_id(cs_king_enter)->12101;
get_id(sc_king_enter)->12102;
get_id(sc_king_enter_wait_first_session)->12103;
get_id(cs_king_quit)->12106;
get_id(cs_king_sign)->12110;
get_id(sc_king_sign)->12111;
get_id(cs_king_buff)->12120;
get_id(sc_king_buff)->12121;
get_id(p_rec)->12131;
get_id(sc_king_new_fight)->12130;
get_id(cs_king_replay)->12140;
get_id(sc_king_replay)->12141;
get_id(cs_king_history)->12150;
get_id(sc_king_history)->12151;
get_id(cs_king_rank)->12160;
get_id(p_king_rank)->12162;
get_id(sc_king_rank)->12161;
get_id(cs_box_item)->12001;
get_id(sc_box_item)->12002;
get_id(cs_box_shop)->12003;
get_id(sc_box_shop)->12004;
get_id(p_reward_view2)->12006;
get_id(cs_box_shop_info)->12007;
get_id(p_shop_box_info)->12009;
get_id(sc_box_shop_info)->12008;
get_id(p_reward_view3)->12010;
get_id(cs_box_get_spirit_equip_count)->12011;
get_id(sc_box_get_spirit_equip_count)->12012;
get_id(cs_box_item_multi)->12013;
get_id(sc_box_item_multi)->12014;
get_id(cs_box_free_info)->12015;
get_id(sc_box_free_info)->12016;
get_id(cs_box_free_open)->12017;
get_id(sc_box_free_open)->12018;
get_id(sc_box_mystery_notice)->12019;
get_id(cs_box_shopBoxDtl)->12020;
get_id(sc_box_shopBoxDtl)->12021;
get_id(cs_box_shop_view)->12022;
get_id(sc_box_shop_view)->12023;
get_id(cs_box_shop_refresh)->12024;
get_id(sc_box_shop_refresh)->12025;
get_id(cs_box_shop_get)->12026;
get_id(sc_box_shop_get)->12027;
get_id(cs_box_free_get)->12028;
get_id(sc_box_free_get)->12029;
get_id(cs_box_ticket_shop)->12030;
get_id(sc_box_ticket_shop)->12031;
get_id(cs_box_ticket_info)->12032;
get_id(sc_box_ticket_info)->12033;
get_id(cs_activity_get_list)->11901;
get_id(p_activity_icon)->11903;
get_id(sc_activity_get_list)->11902;
get_id(cs_activity_info)->11910;
get_id(p_activity_draw)->11912;
get_id(sc_activity_info)->11911;
get_id(cs_activity_draw)->11920;
get_id(sc_activity_draw)->11921;
get_id(sc_activity_update)->11940;
get_id(sc_activity_record_update)->11941;
get_id(p_energy_activity)->11942;
get_id(cs_activity_energy)->11943;
get_id(sc_activity_energy)->11944;
get_id(cs_activity_sign_emperor_info)->11945;
get_id(sc_activity_sign_emperor_info)->11946;
get_id(cs_activity_sign_get_reward)->11947;
get_id(sc_activity_sign_get_reward)->11948;
get_id(cs_activity_sign_up)->11949;
get_id(sc_activity_sign_up)->11950;
get_id(cs_activity_rebate_info)->11951;
get_id(p_rebate_info)->11954;
get_id(p_rebate_list)->11953;
get_id(sc_rebate_info)->11952;
get_id(cs_activity_rebate_get_reward)->11955;
get_id(p_rebate_reward)->11957;
get_id(sc_rebate_get_reward)->11956;
get_id(sc_rebate_update)->11958;
get_id(cs_activity_levelRank_open)->11959;
get_id(levelRank_rankerInfo)->11961;
get_id(sc_activity_levelRank_open)->11960;
get_id(cs_activity_levelRank_refresh)->11962;
get_id(sc_activity_levelRank_refresh)->11963;
get_id(cs_activity_get_payExt_info)->11964;
get_id(cs_activity_vip_shop)->11965;
get_id(p_activity_vip_shop)->11967;
get_id(sc_activity_vip_shop)->11966;
get_id(cs_activity_vip_shop_buy)->11968;
get_id(sc_activity_vip_shop_buy)->11969;
get_id(p_activity_pay_reward)->11970;
get_id(cs_activity_first_pay)->11971;
get_id(sc_activity_first_pay)->11972;
get_id(sc_activity_firstPay_update)->11973;
get_id(cs_activity_consume_reback)->11974;
get_id(p_consume_unit)->11976;
get_id(sc_activity_consume_reback)->11975;
get_id(cs_activity_consume_state)->11977;
get_id(sc_activity_consume_state)->11978;
get_id(cs_activity_energy_pac_info)->11979;
get_id(sc_activity_energy_pac_info)->11980;
get_id(cs_activity_energy_pac_use)->11981;
get_id(sc_activity_energy_pac_use)->11982;
get_id(cs_invite_info)->11801;
get_id(sc_invite_info)->11802;
get_id(cs_invite_bind_weibo)->11810;
get_id(sc_invite_bind_weibo)->11811;
get_id(cs_invite_weibo_share_levelup)->11812;
get_id(sc_invite_weibo_share_levelup)->11813;
get_id(cs_invite_input_invite_code)->11820;
get_id(sc_invite_input_invite_code)->11821;
get_id(cs_invite_list)->11830;
get_id(p_invite)->11832;
get_id(sc_invite_list)->11831;
get_id(cs_friend_get_list)->11701;
get_id(p_friend)->11703;
get_id(sc_friend_get_list)->11702;
get_id(cs_friend_more)->11704;
get_id(sc_friend_more)->11705;
get_id(cs_friend_get_add_list)->11706;
get_id(p_stranger)->11722;
get_id(sc_friend_get_add_list)->11707;
get_id(cs_friend_add)->11708;
get_id(sc_friend_add)->11709;
get_id(cs_friend_explore)->11720;
get_id(sc_friend_explore)->11721;
get_id(cs_friend_delete)->11730;
get_id(sc_friend_delete)->11731;
get_id(sc_friend_notify_delete)->11732;
get_id(sc_friend_new)->11740;
get_id(cs_friend_send_enargy)->11741;
get_id(sc_friend_send_enargy)->11742;
get_id(sc_friend_send_enargy_me)->11743;
get_id(cs_friend_give_enargy)->11744;
get_id(sc_friend_give_enargy)->11745;
get_id(sc_frend_give_enargy_me)->11746;
get_id(cs_friend_give_all_enargy)->11747;
get_id(sc_friend_remove_request)->11748;
get_id(cs_gather_get_list)->11601;
get_id(sc_gather_get_list)->11602;
get_id(sc_gather_new)->11603;
get_id(cs_gather_manual_info_for_tag)->11604;
get_id(p_ger_manual_unit)->11606;
get_id(p_collect_unit)->11608;
get_id(sc_gather_manual_info_for_tag)->11605;
get_id(sc_gather_manual_update)->11607;
get_id(cs_gather_manual_info)->11609;
get_id(sc_gather_manual_info)->11610;
get_id(cs_gather_manual_collect_draw)->11611;
get_id(sc_gather_manual_collect_draw)->11612;
get_id(cs_hist_get_list)->11501;
get_id(p_hist)->11503;
get_id(sc_hist_get_list)->11502;
get_id(cs_hist_more)->11510;
get_id(sc_hist_more)->11511;
get_id(cs_hist_replay)->11520;
get_id(sc_hist_replay)->11521;
get_id(sc_hist_unreadNum)->11531;
get_id(cs_mail_info)->11401;
get_id(p_mail)->11405;
get_id(sc_mail_info)->11402;
get_id(cs_mail_draw_reward)->11406;
get_id(sc_mail_draw_reward)->11407;
get_id(cs_mail_delete)->11408;
get_id(sc_mail_delete)->11409;
get_id(cs_mail_new)->11410;
get_id(sc_mail_new)->11411;
get_id(cs_mail_unread_num)->11420;
get_id(sc_mail_unread_num)->11421;
get_id(cs_mail_more)->11430;
get_id(sc_mail_more)->11431;
get_id(cs_mail_agree_friend)->11440;
get_id(sc_mail_agree_friend)->11441;
get_id(cs_mail_del_spec_mail)->11442;
get_id(sc_mail_del_spec_mail)->11443;
get_id(cs_mail_invite_operate)->11444;
get_id(sc_mail_invite_operate)->11445;
get_id(cs_mail_draw_reward_all)->11446;
get_id(sc_mail_draw_reward_all)->11447;
get_id(cs_mail_canvass_info)->11448;
get_id(sc_mail_canvass_info)->11449;
get_id(cs_mail_get_question)->11450;
get_id(sc_mail_get_question)->11451;
get_id(cs_mail_do_select)->11452;
get_id(sc_mail_do_select)->11453;
get_id(cs_hron_info)->11301;
get_id(sc_hron_info_wait)->11302;
get_id(sc_hron_info_stop)->11303;
get_id(cs_hron_info_on)->11304;
get_id(sc_hron_info_on)->11305;
get_id(sc_hron_info_on_fail)->11306;
get_id(cs_hron_last_rank_list)->11311;
get_id(p_hron_role)->11315;
get_id(sc_hron_last_rank_list)->11312;
get_id(cs_hron_cur_rank_list)->11313;
get_id(sc_hron_cur_rank_list)->11314;
get_id(cs_hron_buy)->11321;
get_id(sc_hron_buy)->11322;
get_id(cs_hron_fight)->11331;
get_id(sc_hron_fight)->11332;
get_id(sc_hron_stop)->11341;
get_id(cs_hron_rank)->11351;
get_id(sc_hron_rank)->11352;
get_id(cs_hron_open_time)->11360;
get_id(sc_hron_open_time)->11361;
get_id(cs_hron_succ_reward)->11370;
get_id(sc_hron_succ_reward)->11371;
get_id(p_hron_succ_reward)->11372;
get_id(cs_hron_select)->11373;
get_id(sc_hron_select)->11374;
get_id(cs_hron_pass)->11375;
get_id(sc_hron_pass)->11376;
get_id(cs_hron_reward_view)->11377;
get_id(sc_hron_reward_view)->11378;
get_id(cs_hron_raids)->11379;
get_id(sc_hron_raids)->11380;
get_id(sc_hron_update_history)->11381;
get_id(cs_hula_open)->11201;
get_id(sc_hula_open)->11202;
get_id(sc_hula_init_state)->11203;
get_id(cs_hula_close)->11204;
get_id(cs_hula_buff)->11205;
get_id(sc_hula_buff)->11206;
get_id(cs_hula_last_info)->11211;
get_id(sc_hula_last_info_ignore)->11212;
get_id(p_hula_info)->11215;
get_id(sc_hula_last_info_win)->11213;
get_id(sc_hula_last_info_fail)->11214;
get_id(cs_hula_cur_info)->11216;
get_id(sc_hula_cur_info_ignore)->11217;
get_id(sc_hula_cur_info)->11218;
get_id(sc_hula_hp_sync)->11220;
get_id(p_hula_harm)->11222;
get_id(sc_hula_harm_broadcast)->11221;
get_id(sc_hula_buff_sync)->11224;
get_id(sc_hula_stop)->11225;
get_id(cs_hula_rank_sync)->11226;
get_id(sc_hula_rank_sync)->11227;
get_id(cs_hula_fight)->11231;
get_id(sc_hula_fight)->11232;
get_id(cs_hula_reborn)->11241;
get_id(sc_hula_reborn)->11242;
get_id(cs_hula_offline_play)->11243;
get_id(sc_hula_offline_play)->11244;
get_id(cs_hula_open_time)->11250;
get_id(sc_hula_open_time)->11251;
get_id(cs_card_get_list)->11101;
get_id(p_card)->11104;
get_id(p_opened_card)->11103;
get_id(sc_card_get_list)->11102;
get_id(cs_card_draw)->11105;
get_id(sc_card_draw)->11106;
get_id(cs_card_refresh)->11107;
get_id(sc_card_refresh)->11108;
get_id(cs_card_onekey)->11120;
get_id(sc_card_onekey)->11121;
get_id(cs_card_activity_info)->11122;
get_id(sc_card_activity_card)->11123;
get_id(cs_daily_get_list)->11001;
get_id(p_daily)->11003;
get_id(sc_daily_get_list)->11002;
get_id(cs_daily_draw)->11005;
get_id(sc_daily_draw)->11006;
get_id(sc_daily_family_info)->11004;
get_id(cs_plunder_info)->10901;
get_id(p_stonechip)->10903;
get_id(sc_plunder_info)->10902;
get_id(cs_plunder_compose)->10934;
get_id(sc_plunder_compose)->10935;
get_id(cs_plunder_get_target)->10936;
get_id(p_plunder_tar)->10938;
get_id(sc_plunder_get_target)->10937;
get_id(cs_plunder_fight)->10939;
get_id(sc_plunder_fight)->10940;
get_id(cs_plunder_use_protect)->10941;
get_id(sc_plunder_use_protect)->10942;
get_id(cs_plunder_buy_attacktime)->10943;
get_id(sc_plunder_buy_attacktime)->10944;
get_id(sc_plunder_notice_attacktime)->10945;
get_id(cs_plunder_fight_ten)->10946;
get_id(p_plunder_fight_mystery)->10948;
get_id(sc_plunder_fight_ten)->10947;
get_id(cs_plunder_protect_time)->10949;
get_id(sc_plunder_protect_time)->10950;
get_id(cs_plunder_multi_compose)->10951;
get_id(sc_plunder_multi_compose)->10952;
get_id(cs_pvp_get_list)->10801;
get_id(p_pvp)->10803;
get_id(sc_pvp_get_list)->10802;
get_id(cs_pvp_fight)->10804;
get_id(sc_pvp_fight)->10805;
get_id(cs_pvp_get_first_eight_replays)->10806;
get_id(p_pvp_replay_info)->10808;
get_id(sc_pvp_get_first_eight_replays)->10807;
get_id(cs_pvp_eight_replay)->10809;
get_id(sc_pvp_eight_replay)->10810;
get_id(cs_pvp_free_fight)->10811;
get_id(sc_pvp_free_fight)->10812;
get_id(cs_pvp_get_free_fight_level)->10813;
get_id(sc_pvp_get_free_fight_level)->10814;
get_id(cs_shop_buy_num)->10701;
get_id(p_shop_num)->10703;
get_id(sc_shop_buy_num)->10702;
get_id(cs_shop_buy)->10704;
get_id(sc_shop_buy)->10705;
get_id(cs_shop_encounter)->10710;
get_id(p_shop_random)->10712;
get_id(sc_shop_encounter)->10711;
get_id(sc_shop_new)->10713;
get_id(cs_shop_refresh)->10720;
get_id(sc_shop_refresh)->10721;
get_id(sc_shop_auto_refresh)->10730;
get_id(cs_shop_family_limit_info)->10722;
get_id(p_shop_family_limit)->10726;
get_id(sc_shop_family_limit_info)->10723;
get_id(cs_shop_family_limit_buy)->10724;
get_id(sc_shop_family_limit_buy)->10725;
get_id(cs_shop_seed_info)->10727;
get_id(seed_sell_info)->10729;
get_id(sc_shop_seed_info)->10728;
get_id(cs_item_bag)->10601;
get_id(p_item)->10603;
get_id(sc_item_bag)->10602;
get_id(sc_item_bag2)->10628;
get_id(cs_item_equip)->10604;
get_id(sc_item_equip)->10605;
get_id(cs_item_sell)->10607;
get_id(sc_item_sell)->10608;
get_id(cs_item_down_equip)->10609;
get_id(sc_item_down_equip)->10610;
get_id(cs_item_up_equip)->10611;
get_id(sc_item_up_equip)->10612;
get_id(sc_item_new)->10613;
get_id(p_item_num_update)->10615;
get_id(sc_item_update)->10614;
get_id(cs_item_use)->10617;
get_id(sc_item_use)->10618;
get_id(sc_item_delete_notify)->10619;
get_id(cs_item_reinforce)->10620;
get_id(sc_item_reinforce)->10621;
get_id(cs_item_max_reinforce)->10622;
get_id(sc_item_max_reinforce)->10623;
get_id(sc_item_update_rank)->10624;
get_id(cs_item_up_rank)->10625;
get_id(sc_item_up_rank)->10626;
get_id(cs_item_compound)->10631;
get_id(sc_item_compound)->10632;
get_id(cs_item_eat)->10633;
get_id(sc_item_eat)->10634;
get_id(p_all_equipment)->10636;
get_id(sc_item_all_equipment)->10635;
get_id(cs_item_use_info)->10637;
get_id(p_item_use_info)->10639;
get_id(sc_item_use_info)->10638;
get_id(cs_item_auto_up_equip)->10640;
get_id(sc_item_auto_up_equip)->10641;
get_id(cs_item_stone_eat)->10642;
get_id(sc_item_stone_eat)->10643;
get_id(p_item_decompose_unit)->10646;
get_id(cs_item_decompose)->10644;
get_id(sc_item_decompose)->10645;
get_id(cs_item_decompose_again)->10649;
get_id(sc_item_decompose_again)->10650;
get_id(cs_item_decompose_by_money_cost)->10651;
get_id(sc_item_decompose_by_money_cost)->10652;
get_id(cs_item_decompose_again_cost)->10653;
get_id(sc_item_decompose_again_cost)->10654;
get_id(cs_item_enchant)->10655;
get_id(p_equip2)->10657;
get_id(sc_item_enchant)->10656;
get_id(cs_item_x_get)->10658;
get_id(sc_item_x_get)->10659;
get_id(cs_item_down_rank)->10660;
get_id(sc_item_down_rank)->10661;
get_id(cs_item_pft_uprank)->10662;
get_id(sc_item_pft_uprank)->10663;
get_id(cs_item_stone_uprank)->10664;
get_id(sc_item_stone_uprank)->10665;
get_id(sc_item_dtl_update)->10666;
get_id(cs_item_max_reinforce_for_ger)->10667;
get_id(p_reinforce_result)->10668;
get_id(sc_item_max_reinforce_for_ger)->10669;
get_id(cs_item_make_legend)->10670;
get_id(sc_item_make_legend)->10671;
get_id(cs_item_legend_uprank)->10672;
get_id(sc_item_legend_uprank)->10673;
get_id(cs_item_stone_legend)->10674;
get_id(sc_item_stone_legend)->10675;
get_id(cs_explore_one)->10501;
get_id(p_echapter)->10503;
get_id(sc_explore_one)->10502;
get_id(cs_explore_dungeon_list)->10504;
get_id(p_edungeon)->10506;
get_id(sc_explore_dungeon_list)->10505;
get_id(cs_explore_challenge_encounter)->10507;
get_id(sc_explore_challenge_encounter)->10508;
get_id(sc_explore_delete_encounter)->10509;
get_id(cs_explore_giveup_encounter)->10510;
get_id(sc_explore_giveup_encounter)->10511;
get_id(cs_explore_list)->10512;
get_id(sc_explore_list)->10513;
get_id(cs_explore_collect)->10514;
get_id(sc_explore_collect)->10515;
get_id(cs_explore_force_collect)->10516;
get_id(sc_explore_force_collect)->10517;
get_id(cs_explore_auto_explore_check)->10518;
get_id(sc_explore_auto_explore_check)->10519;
get_id(cs_explore_encounter_pass_reward)->10520;
get_id(sc_explore_encounter_pass_reward)->10521;
get_id(cs_explore_encounter_dungeon_state)->10522;
get_id(sc_explore_encounter_dungeon_state)->10523;
get_id(cs_explore_free)->10530;
get_id(sc_explore_free)->10531;
get_id(cs_explore_all)->10532;
get_id(p_explore_reward)->10533;
get_id(sc_explore_all)->10534;
get_id(cs_ger_info)->10401;
get_id(sc_ger_info)->10402;
get_id(sc_ger_info2)->10427;
get_id(p_ger_pos_info)->10404;
get_id(sc_ger_update)->10405;
get_id(sc_ger_new)->10406;
get_id(cs_ger_standup)->10407;
get_id(sc_ger_standup)->10408;
get_id(cs_ger_move_pos)->10409;
get_id(sc_ger_move_pos)->10410;
get_id(cs_ger_pos_list)->10411;
get_id(p_ger_icon_unit)->10431;
get_id(sc_ger_pos_list)->10412;
get_id(cs_ger_sell)->10414;
get_id(sc_ger_sell)->10415;
get_id(cs_ger_detail)->10416;
get_id(sc_ger_detail)->10417;
get_id(cs_ger_view_other)->10419;
get_id(sc_ger_view_other)->10420;
get_id(cs_ger_view_other_dtl)->10442;
get_id(sc_ger_view_other_dtl)->10443;
get_id(sc_ger_update_exp)->10421;
get_id(cs_ger_eat)->10422;
get_id(sc_ger_eat)->10423;
get_id(cs_ger_up_rank)->10424;
get_id(sc_ger_up_rank)->10425;
get_id(sc_ger_update_standlist)->10426;
get_id(sc_ger_del)->10430;
get_id(p_ger_power)->10441;
get_id(sc_ger_refresh_power)->10440;
get_id(p_ger_save_item)->10444;
get_id(p_ger_save_dtl)->10445;
get_id(p_ger_tag_dtl)->10446;
get_id(cs_ger_saved_tag)->10447;
get_id(sc_ger_saved_tag)->10448;
get_id(cs_ger_set_icon)->10432;
get_id(sc_ger_set_icon)->10433;
get_id(cs_ger_buy_tag)->10434;
get_id(sc_ger_buy_tag)->10435;
get_id(cs_ger_set_tag_data)->10436;
get_id(sc_ger_set_tag_data)->10437;
get_id(cs_ger_change_tag)->10438;
get_id(sc_ger_change_tag)->10439;
get_id(cs_ger_lieu_pos_list)->10450;
get_id(sc_ger_lieu_pos_list)->10451;
get_id(cs_ger_lieu_standup)->10452;
get_id(sc_ger_lieu_standup)->10453;
get_id(cs_ger_lieu_dequeue)->10454;
get_id(sc_ger_lieu_dequeue)->10455;
get_id(cs_ger_lieu_untie)->10456;
get_id(p_ger_lieu_info)->10458;
get_id(sc_ger_lieu_untie)->10457;
get_id(cs_ger_lieu_info_list)->10459;
get_id(sc_ger_lieu_info_list)->10460;
get_id(cs_ger_lieu_move_pos)->10461;
get_id(sc_ger_lieu_move_pos)->10462;
get_id(cs_ger_lieu_lock_clo)->10463;
get_id(sc_ger_lieu_lock_clo)->10464;
get_id(cs_ger_lieu_unlock_clo)->10465;
get_id(sc_ger_lieu_unlock_clo)->10466;
get_id(cs_ger_lieu_refresh_clo)->10467;
get_id(sc_ger_lieu_refresh_clo)->10468;
get_id(cs_ger_lieu_tie_info)->10469;
get_id(sc_ger_lieu_tie_info)->10470;
get_id(cs_ger_lieu_refresh_freeTimes)->10472;
get_id(sc_ger_lieu_refresh_freeTimes)->10473;
get_id(sc_ger_new_list)->10474;
get_id(cs_ger_down_rank)->10475;
get_id(sc_ger_down_rank)->10476;
get_id(cs_ger_unload)->10477;
get_id(sc_ger_unload)->10478;
get_id(cs_ger_mirror_info)->10479;
get_id(sc_ger_mirror_info)->10480;
get_id(cs_ger_mirror_convert)->10481;
get_id(sc_ger_mirror_convert)->10482;
get_id(cs_ger_mirror_commit)->10483;
get_id(sc_ger_mirror_commit)->10484;
get_id(cs_ger_set_body)->10486;
get_id(sc_ger_set_body)->10487;
get_id(cs_ger_second_uprank)->10488;
get_id(sc_ger_second_uprank)->10489;
get_id(cs_ger_batch_uprank)->10490;
get_id(sc_ger_batch_uprank)->10491;
get_id(cs_ger_second_transmigration)->10492;
get_id(sc_ger_second_transmigration)->10493;
get_id(cs_ger_transform_blink)->10494;
get_id(sc_ger_transform_blink)->10495;
get_id(cs_message_notice)->10302;
get_id(p_notice)->10304;
get_id(sc_message_notice)->10303;
get_id(cs_message_certain_notice)->10305;
get_id(sc_message_certain_notice)->10306;
get_id(sc_message_bc)->10301;
get_id(sc_message_bc_id)->10307;
get_id(sc_message_bc_id2)->10308;
get_id(cs_message_test)->10330;
get_id(sc_message_test)->10331;
get_id(sc_message_best_card)->30001;
get_id(sc_message_ger_upLevel)->30002;
get_id(sc_message_item_uprank)->30003;
get_id(cs_battle_progress)->10201;
get_id(p_battle_chapter_star_reward)->10222;
get_id(p_battle_progress)->10216;
get_id(sc_battle_progress)->10202;
get_id(cs_battle_info)->10203;
get_id(p_dungeon)->10205;
get_id(sc_battle_info)->10204;
get_id(cs_battle_challenge)->10206;
get_id(sc_battle_challenge)->10207;
get_id(cs_battle_perfect_reward)->10208;
get_id(sc_battle_perfect_reward)->10209;
get_id(sc_battle_broadcast_get_item)->10212;
get_id(cs_battle_dungeon_raids)->10214;
get_id(sc_battle_dungeon_raids)->10215;
get_id(cs_battle_star_reward)->10217;
get_id(sc_battle_star_reward)->10218;
get_id(cs_battle_reset_dungeon)->10219;
get_id(sc_battle_reset_dungeon)->10220;
get_id(sc_battle_battle_fail)->10221;
get_id(cs_battle_world_level)->10223;
get_id(sc_battle_world_level)->10224;
get_id(cs_battle_obtain_boss_reward)->10225;
get_id(sc_battle_obtain_boss_reward)->10226;
get_id(cs_battle_get_boss_reward_info)->10227;
get_id(sc_battle_get_boss_reward_info)->10228;
get_id(cs_battle_dojang_info)->10229;
get_id(p_dojang_info)->10237;
get_id(sc_battle_dojang_info)->10230;
get_id(cs_battle_dojang_fight)->10231;
get_id(sc_battle_dojang_fight)->10232;
get_id(cs_battle_dojang_harvest)->10233;
get_id(sc_battle_dojang_harvest)->10234;
get_id(cs_battle_dojang_buy)->10235;
get_id(sc_battle_dojang_buy)->10236;
get_id(cs_role_info)->10101;
get_id(sc_role_info)->10102;
get_id(sc_role_update_level)->10107;
get_id(sc_role_update_list)->10103;
get_id(sc_role_update_unioncoin)->10104;
get_id(cs_role_buy_energy)->10105;
get_id(sc_role_buy_energy)->10106;
get_id(sc_role_update_exp)->10108;
get_id(sc_role_update_coin)->10109;
get_id(sc_role_update_reputation)->10110;
get_id(sc_role_update_gold)->10111;
get_id(sc_role_update_goldBonus)->10112;
get_id(sc_role_update_vipLevel)->10113;
get_id(sc_role_update_energy)->10114;
get_id(sc_role_update_discoveryTimes)->10115;
get_id(sc_role_update_pvpTimes)->10116;
get_id(sc_role_update_plunderTimes)->10117;
get_id(sc_role_update_randomPVPTimes)->10118;
get_id(sc_role_update_singlePVPTimes)->10119;
get_id(sc_role_update_goldUsed)->10120;
get_id(sc_role_update_title)->10121;
get_id(sc_role_update_encounterFreeNum)->10122;
get_id(sc_role_update_weiboCount)->10123;
get_id(cs_role_setting)->10124;
get_id(sc_role_setting)->10125;
get_id(cs_role_get_energy)->10126;
get_id(sc_role_get_energy)->10127;
get_id(cs_role_buy_coin_value)->10128;
get_id(sc_role_buy_coin_value)->10129;
get_id(cs_role_weixin_share)->10130;
get_id(sc_role_update_pay_ext)->10131;
get_id(cs_role_suggest_open)->10132;
get_id(sc_role_suggest_open)->10133;
get_id(cs_role_suggest)->10134;
get_id(sc_role_suggest)->10135;
get_id(cs_role_log_guide_state)->10136;
get_id(sc_role_talent_remains_point)->10137;
get_id(cs_role_multi_buy_energy)->10138;
get_id(p_multi_buy_energy)->10139;
get_id(sc_role_multi_buy_energy)->10140;
get_id(cs_role_is_under_examine)->10141;
get_id(sc_role_is_under_examine)->10142;
get_id(cs_role_push_setting)->10150;
get_id(sc_role_push_setting)->10151;
get_id(cs_role_get_guide_state)->10153;
get_id(sc_role_get_guide_state)->10154;
get_id(cs_role_set_guide_state)->10155;
get_id(sc_role_set_guide_state)->10156;
get_id(cs_role_change_head)->10157;
get_id(sc_role_change_head)->10158;
get_id(cs_role_change_location)->10159;
get_id(sc_role_update_teamPkTimes)->10160;
get_id(cs_role_teamPkTimes_info)->10161;
get_id(sc_role_teamPkTimes_info)->10162;
get_id(cs_role_token)->10180;
get_id(sc_role_token)->10181;
get_id(cs_role_select_ger)->10182;
get_id(sc_role_select_ger)->10183;
get_id(cs_role_demo_fight)->10184;
get_id(sc_role_demo_fight)->10185;
get_id(sc_role_base_config)->10186;
get_id(cs_role_pay_ios)->10190;
get_id(sc_role_pay_ios)->10191;
get_id(cs_role_pay_91)->10192;
get_id(sc_role_pay_91)->10193;
get_id(sc_role_pay_uc)->10194;
get_id(sc_role_pay_dl)->10195;
get_id(sc_role_pay_zz)->10196;
get_id(sc_role_pay_360)->10197;
get_id(sc_role_pay_wdj)->10198;
get_id(sc_role_pay_dk)->10199;
get_id(sc_role_pay_mi)->10189;
get_id(sc_role_pay_az)->10188;
get_id(sc_role_update_profoundCrystal)->10200;
get_id(sc_role_update_honor)->10187;
get_id(sc_role_update_pvppoint)->10179;
get_id(sc_role_update_home_resource)->10143;
get_id(sc_role_update_ticket)->10163;
get_id(sc_role_update_laputastone)->10164;
get_id(cs_role_do_transmigration)->10165;
get_id(sc_role_do_transmigration)->10166;
get_id(cs_role_can_transmigration)->10167;
get_id(sc_role_can_transmigration)->10168;
get_id(sc_role_update_svip)->10169;
get_id(cs_account_login)->10001;
get_id(sc_account_login)->10002;
get_id(cs_account_create)->10003;
get_id(sc_account_create)->10004;
get_id(cs_account_enter_game)->10005;
get_id(sc_account_enter_game)->10006;
get_id(sc_account_kick)->10007;
get_id(cs_account_heart)->10013;
get_id(sc_account_heart)->10014;
get_id(cs_account_logout)->10015;
get_id(cs_account_check_rolename)->10016;
get_id(sc_account_check_rolename)->10017;
get_id(cs_account_demo)->10018;
get_id(sc_account_demo)->10019;
get_id(_)->0.
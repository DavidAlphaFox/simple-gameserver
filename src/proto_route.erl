-module(proto_route).
-compile(export_all).

route(cs_trainingRoom_info) ->
	{role,role_trainingRoom};
route(cs_trainingRoom_start_training) ->
	{role,role_trainingRoom};
route(cs_diamond_combine) ->
	{role,role_diamond};
route(cs_diamond_equip) ->
	{role,role_diamond};
route(cs_diamond_demount) ->
	{role,role_diamond};
route(cs_diamond_demount_multi) ->
	{role,role_diamond};
route(cs_diamond_holygrail_info) ->
	{role,role_diamond};
route(cs_diamond_holygrail_sacrifice) ->
	{role,role_diamond};
route(cs_diamond_holygrail_uplevel) ->
	{role,role_diamond};
route(cs_conquerisland_sign) ->
	{role,role_conquerisland};
route(cs_conquerisland_buy) ->
	{role,role_conquerisland};
route(cs_conquerisland_info) ->
	{role,role_conquerisland};
route(cs_conquerisland_unrequest) ->
	{role,role_conquerisland};
route(cs_conquerisland_war_base_info) ->
	{role,role_conquerisland};
route(cs_conquerisland_role_dtl) ->
	{role,role_conquerisland};
route(cs_conquerisland_mov) ->
	{role,role_conquerisland};
route(cs_conquerisland_stop) ->
	{role,role_conquerisland};
route(cs_conquerisland_centre_dtl) ->
	{role,role_conquerisland};
route(cs_conquerisland_attack) ->
	{role,role_conquerisland};
route(cs_conquerisland_centre_occupy) ->
	{role,role_conquerisland};
route(cs_conquerisland_reborn) ->
	{role,role_conquerisland};
route(cs_conquerisland_replay) ->
	{role,role_conquerisland};
route(cs_conquerisland_self) ->
	{role,role_conquerisland};
route(cs_conquerisland_rank) ->
	{role,role_conquerisland};
route(cs_conquerisland_talk) ->
	{role,role_conquerisland};
route(cs_conquerisland_get_talk) ->
	{role,role_conquerisland};
route(cs_recycle_restore) ->
	{role,role_recycle};
route(cs_crystal_uplevel) ->
	{role,role_crystal};
route(cs_crystal_uprank) ->
	{role,role_crystal};
route(cs_crystal_info) ->
	{role,role_crystal};
route(cs_doublematch_lineup) ->
	{role,role_doublematch};
route(cs_doublematch_update_lineup) ->
	{role,role_doublematch};
route(cs_doublematch_sign) ->
	{role,role_doublematch};
route(cs_doublematch_sign_cancel) ->
	{role,role_doublematch};
route(cs_doublematch_times_buy) ->
	{role,role_doublematch};
route(cs_doublematch_roleinfo) ->
	{role,role_doublematch};
route(cs_doublematch_rankinfo) ->
	{role,role_doublematch};
route(cs_doublematch_record_list) ->
	{role,role_doublematch};
route(cs_doublematch_replay) ->
	{role,role_doublematch};
route(cs_goldenegg_use_item) ->
	{role,role_goldenegg};
route(cs_goldenegg_smash) ->
	{role,role_goldenegg};
route(cs_goldenegg_roleinfo) ->
	{role,role_goldenegg};
route(cs_goldenegg_shop) ->
	{role,role_goldenegg};
route(cs_goldenegg_open) ->
	{role,role_goldenegg};
route(cs_matchRoom_ready) ->
	{role,role_matchRoom};
route(cs_matchRoom_cancel) ->
	{role,role_matchRoom};
route(cs_matchRoom_exit) ->
	{role,role_matchRoom};
route(cs_skin_compose) ->
	{role,role_skin};
route(cs_skin_activate) ->
	{role,role_skin};
route(cs_skin_equip) ->
	{role,role_skin};
route(cs_skin_demount) ->
	{role,role_skin};
route(cs_skin_info) ->
	{role,role_skin};
route(cs_home_info) ->
	{role,role_home};
route(cs_home_build) ->
	{role,role_home};
route(cs_home_up_stage) ->
	{role,role_home};
route(cs_home_get_friend) ->
	{role,role_home};
route(cs_home_task_info) ->
	{role,role_home};
route(cs_home_task_operate) ->
	{role,role_home};
route(cs_home_bounty_task_info) ->
	{role,role_home};
route(cs_home_cirrus_info) ->
	{role,role_home};
route(cs_home_cirrus_fight) ->
	{role,role_home};
route(cs_home_cirrus_operate) ->
	{role,role_home};
route(cs_home_exchange) ->
	{role,role_home};
route(cs_sign_reward_info) ->
	{role,role_sign};
route(cs_sign_reward) ->
	{role,role_sign};
route(cs_awake_ger) ->
	{role,role_awake};
route(cs_awake_recast_ger) ->
	{role,role_awake};
route(cs_awake_exchange_skill) ->
	{role,role_awake};
route(cs_awake_cancel_skill) ->
	{role,role_awake};
route(cs_generalteam_create) ->
	{role,role_generalteam};
route(cs_generalteam_invite) ->
	{role,role_generalteam};
route(cs_generalteam_invite_response) ->
	{role,role_generalteam};
route(cs_generalteam_leaveteam) ->
	{role,role_generalteam};
route(cs_generalteam_disbandteam) ->
	{role,role_generalteam};
route(cs_generalteam_kick) ->
	{role,role_generalteam};
route(cs_generalteam_change_authority) ->
	{role,role_generalteam};
route(cs_generalteam_talk) ->
	{role,role_generalteam};
route(cs_generalteam_info) ->
	{role,role_generalteam};
route(cs_generalteam_change_teamtype) ->
	{role,role_generalteam};
route(cs_panicbuy_info) ->
	{role,role_panicbuy};
route(cs_panicbuy_once) ->
	{role,role_panicbuy};
route(cs_discount_activity_list) ->
	{role,role_discount};
route(cs_discount_exchange_info) ->
	{role,role_discount};
route(cs_discount_exchange) ->
	{role,role_discount};
route(cs_discount_pay_activity_info) ->
	{role,role_discount};
route(cs_discount_pay_activity) ->
	{role,role_discount};
route(cs_homestead_get_info) ->
	{role,role_homestead};
route(cs_homestead_get_friend_info) ->
	{friend_server,role_homestead};
route(cs_homestead_unlock_machine) ->
	{role,role_homestead};
route(cs_homestead_uproot_seed) ->
	{role,role_homestead};
route(cs_homestead_harvest) ->
	{role,role_homestead};
route(cs_homestead_seeding) ->
	{role,role_homestead};
route(cs_homestead_change_ger) ->
	{role,role_homestead};
route(cs_homestead_mating) ->
	{friend_server,role_homestead};
route(cs_homestead_addenergy) ->
	{friend_server,role_homestead};
route(cs_homestead_get_log) ->
	{role,role_homestead};
route(cs_homestead_get_friend_log) ->
	{friend_server,role_homestead};
route(cs_homestead_compose) ->
	{role,role_homestead};
route(cs_homestead_compose_list) ->
	{role,role_homestead};
route(cs_task_get_info) ->
	{role,role_task};
route(cs_task_operate) ->
	{role,role_task};
route(cs_fight_request) ->
	{role,role_fight};
route(cs_consumerank_info) ->
	{role,role_consumerank};
route(cs_consumerank_list) ->
	{role,role_consumerank};
route(cs_buddy_partner_insert) ->
	{role,role_buddy};
route(cs_buddy_partner_insert_batch) ->
	{role,role_buddy};
route(cs_buddy_partner_remove) ->
	{role,role_buddy};
route(cs_dSign_info) ->
	{role,role_dSign};
route(cs_dSign_sign) ->
	{role,role_dSign};
route(cs_dSign_reSign) ->
	{role,role_dSign};
route(cs_dSign_get_sevenReward) ->
	{role,role_dSign};
route(cs_dSign_get_monReward) ->
	{role,role_dSign};
route(cs_dSign_mark_mon) ->
	{role,role_dSign};
route(cs_trainerRear_brief) ->
	{role,role_trainerRear};
route(cs_trainerRear_info) ->
	{role,role_trainerRear};
route(cs_trainerRear_insert) ->
	{role,role_trainerRear};
route(cs_trainerRear_mature) ->
	{role,role_trainerRear};
route(cs_trainerRear_accelerate) ->
	{role,role_trainerRear};
route(cs_dojangrank_info) ->
	{role,role_dojangrank};
route(cs_dojangrank_rank) ->
	{role,role_dojangrank};
route(cs_dojangrank_buy) ->
	{role,role_dojangrank};
route(cs_dojangrank_select_ger_type) ->
	{role,role_dojangrank};
route(cs_dojangrank_fight) ->
	{role,role_dojangrank};
route(cs_dojangrank_replay_list) ->
	{role,role_dojangrank};
route(cs_dojangrank_replay_detail) ->
	{role,role_dojangrank};
route(cs_dojangrank_ger_view_other) ->
	{role,role_dojangrank};
route(cs_dojangrank_self_rank) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_info) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_rank) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_buy) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_refresh_enemy) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_select_ger_type) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_fight) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_replay_list) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_replay_detail) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_ger_view_other) ->
	{role,role_dojangrank};
route(cs_dojangrank_world_top_replay_list) ->
	{role,role_dojangrank};
route(cs_trainerProf_info) ->
	{role,role_trainerProf};
route(cs_trainerProf_uplevel) ->
	{role,role_trainerProf};
route(cs_trainerProf_battle_info) ->
	{role,role_trainerProf};
route(cs_trainerProf_battle_uplevel) ->
	{role,role_trainerProf};
route(cs_trainerProf_battle_unclock) ->
	{role,role_trainerProf};
route(cs_treasurebowl_info) ->
	{role,role_treasurebowl};
route(cs_treasurebowl_exchange) ->
	{role,role_treasurebowl};
route(cs_treasurebowl_draw) ->
	{role,role_treasurebowl};
route(cs_maintask_info) ->
	{role,role_maintask};
route(cs_maintask_draw) ->
	{role,role_maintask};
route(cs_xbattle_info) ->
	{role,role_xbattle};
route(cs_xbattle_challenge) ->
	{role,role_xbattle};
route(cs_xbattle_raid) ->
	{role,role_xbattle};
route(cs_xbattle_offline_reward) ->
	{role,role_xbattle};
route(cs_xbattle_use_elixir) ->
	{role,role_xbattle};
route(cs_xbattle_buy_quick) ->
	{role,role_xbattle};
route(cs_xbattle_get_pass_reward) ->
	{role,role_xbattle};
route(cs_xbattle_chapter_info) ->
	{role,role_xbattle};
route(cs_xbattle_set_reward_chapter) ->
	{role,role_xbattle};
route(cs_xbattle_start_reward) ->
	{role,role_xbattle};
route(cs_xbattle_challenge_boss) ->
	{role,role_xbattle};
route(cs_xbattle_package_full) ->
	{role,role_xbattle};
route(cs_xbattle_package_ok) ->
	{role,role_xbattle};
route(cs_exBoss_get_info) ->
	{role,role_exBoss};
route(cs_exBoss_hit) ->
	{role,role_exBoss};
route(cs_exBoss_buy_times) ->
	{role,role_exBoss};
route(cs_exBoss_get_reward) ->
	{role,role_exBoss};
route(cs_exBoss_refresh) ->
	{role,role_exBoss};
route(cs_exBoss_buy_cost) ->
	{role,role_exBoss};
route(cs_exBoss_oneKey) ->
	{role,role_exBoss};
route(cs_tasklink_get_info) ->
	{role,role_tasklink};
route(cs_tasklink_get_progress) ->
	{role,role_tasklink};
route(cs_tasklink_get_reward_log) ->
	{role,role_tasklink};
route(cs_tasklink_sign) ->
	{role,role_tasklink};
route(cs_tasklink_buy_time) ->
	{role,role_tasklink};
route(cs_tasklink_ready_notice) ->
	{role,role_tasklink};
route(cs_tasklink_ready_opt) ->
	{role,role_tasklink};
route(cs_tasklink_get_reward) ->
	{role,role_tasklink};
route(cs_activityFestival_info) ->
	{role,role_activityFestival};
route(cs_activityFestival_self) ->
	{role,role_activityFestival};
route(cs_activityFestival_sign) ->
	{role,role_activityFestival};
route(cs_activityFestival_box_get) ->
	{role,role_activityFestival};
route(cs_activityFestival_sign2) ->
	{role,role_activityFestival};
route(cs_familycross_info) ->
	{role,role_familycross};
route(cs_familycross_sign) ->
	{role,role_familycross};
route(cs_familycross_displayer) ->
	{role,role_familycross};
route(cs_familycross_displayer_info) ->
	{role,role_familycross};
route(cs_familycross_player_fly) ->
	{role,role_familycross};
route(cs_familycross_enermy_info) ->
	{role,role_familycross};
route(cs_familycross_war_info) ->
	{role,role_familycross};
route(cs_familycross_self_info) ->
	{role,role_familycross};
route(cs_familycross_drive_car) ->
	{role,role_familycross};
route(cs_familycross_mov) ->
	{role,role_familycross};
route(cs_familycross_attack) ->
	{role,role_familycross};
route(cs_familycross_city_dtl) ->
	{role,role_familycross};
route(cs_familycross_site_dtl) ->
	{role,role_familycross};
route(cs_familycross_fly_dtl) ->
	{role,role_familycross};
route(cs_familycross_be_driver) ->
	{role,role_familycross};
route(cs_familycross_drive_car_stop) ->
	{role,role_familycross};
route(cs_familycross_mov_stop) ->
	{role,role_familycross};
route(cs_familycross_mov_back) ->
	{role,role_familycross};
route(cs_familycross_reborn) ->
	{role,role_familycross};
route(cs_familycross_replay) ->
	{role,role_familycross};
route(cs_familycross_own_site) ->
	{role,role_familycross};
route(cs_familycross_season_rank) ->
	{role,role_familycross};
route(cs_familycross_seasoninfo) ->
	{role,role_familycross};
route(cs_familycross_family_rank) ->
	{role,role_familycross};
route(cs_familycross_battle_get_rank) ->
	{role,role_familycross};
route(cs_familycross_be_driver2) ->
	{role,role_familycross};
route(cs_familycross_self_car) ->
	{role,role_familycross};
route(cs_payGuide_info) ->
	{role,role_payGuide};
route(cs_payGuide_get_reward) ->
	{role,role_payGuide};
route(cs_payGuide_seven_info) ->
	{role,role_payGuide};
route(cs_payGuide_seven_period_info) ->
	{role,role_payGuide};
route(cs_payGuide_seven_draw) ->
	{role,role_payGuide};
route(cs_homeBoss_info) ->
	{role,role_homeBoss};
route(cs_homeBoss_attack) ->
	{role,role_homeBoss};
route(cs_homeBoss_self) ->
	{role,role_homeBoss};
route(cs_homeBoss_buy_times) ->
	{role,role_homeBoss};
route(cs_homeBoss_get_reward) ->
	{role,role_homeBoss};
route(cs_homeBoss_read) ->
	{role,role_homeBoss};
route(cs_galactica_info) ->
	{role,role_galactica};
route(cs_galactica_sign) ->
	{role,role_galactica};
route(cs_galactica_buy) ->
	{role,role_galactica};
route(cs_galactica_war_base_info) ->
	{role,role_galactica};
route(cs_galactica_self) ->
	{role,role_galactica};
route(cs_galactica_mov) ->
	{role,role_galactica};
route(cs_galactica_attack) ->
	{role,role_galactica};
route(cs_galactica_role_dtl) ->
	{role,role_galactica};
route(cs_galactica_replay) ->
	{role,role_galactica};
route(cs_galactica_mov_stop) ->
	{role,role_galactica};
route(cs_galactica_talk) ->
	{role,role_galactica};
route(cs_galactica_get_talk) ->
	{role,role_galactica};
route(cs_galactica_get_rank) ->
	{role,role_galactica};
route(cs_galactica_reborn) ->
	{role,role_galactica};
route(cs_galactica_unrequest) ->
	{role,role_galactica};
route(cs_galactica_mov_in) ->
	{role,role_galactica};
route(cs_twins_info) ->
	{role,role_twins};
route(cs_twins_sign) ->
	{role,role_twins};
route(cs_twins_buy) ->
	{role,role_twins};
route(cs_twins_unrequest) ->
	{role,role_twins};
route(cs_twins_war_base_info) ->
	{role,role_twins};
route(cs_twins_self) ->
	{role,role_twins};
route(cs_twins_mov) ->
	{role,role_twins};
route(cs_twins_attack) ->
	{role,role_twins};
route(cs_twins_role_dtl) ->
	{role,role_twins};
route(cs_twins_mov_stop) ->
	{role,role_twins};
route(cs_twins_talk) ->
	{role,role_twins};
route(cs_twins_get_talk) ->
	{role,role_twins};
route(cs_twins_get_rank) ->
	{role,role_twins};
route(cs_twins_open_reward_box) ->
	{role,role_twins};
route(cs_bounty_self_info) ->
	{role,role_bounty};
route(cs_bounty_challenge) ->
	{role,role_bounty};
route(cs_bounty_buy_times) ->
	{role,role_bounty};
route(cs_bounty_get_reward) ->
	{role,role_bounty};
route(cs_trSpecial_info) ->
	{role,role_trSpecial};
route(cs_trSpecial_select) ->
	{role,role_trSpecial};
route(cs_trSpecial_clear) ->
	{role,role_trSpecial};
route(cs_tvcard_info) ->
	{role,role_tvcard};
route(cs_tvcard_select) ->
	{role,role_tvcard};
route(cs_tvcard_rand) ->
	{role,role_tvcard};
route(cs_magicBook_swallow_ger) ->
	{role,role_magicBook};
route(cs_magicBook_summary) ->
	{role,role_magicBook};
route(cs_magicBook_picture_reward) ->
	{role,role_magicBook};
route(cs_magicBook_book_reward) ->
	{role,role_magicBook};
route(cs_magicBook_book_info) ->
	{role,role_magicBook};
route(cs_luckyRoll_get_list) ->
	{role,role_luckyRoll};
route(cs_luckyRoll_explore_one) ->
	{role,role_luckyRoll};
route(cs_luckyRoll_refresh) ->
	{role,role_luckyRoll};
route(cs_luckyRoll_open_base_box) ->
	{role,role_luckyRoll};
route(cs_luckyRoll_get_rankInfo) ->
	{role,role_luckyRoll};
route(cs_luckyRoll_get_rank_Reward) ->
	{lucky_roll_server,role_luckyRoll};
route(cs_luckyRoll_explore_ten) ->
	{role,role_luckyRoll};
route(cs_carlos_sign) ->
	{role,role_carlos};
route(cs_carlos_plane_uplevel) ->
	{role,role_carlos};
route(cs_carlos_buy) ->
	{role,role_carlos};
route(cs_carlos_info) ->
	{role,role_carlos};
route(cs_carlos_war_base_info) ->
	{role,role_carlos};
route(cs_carlos_mine_detail) ->
	{role,role_carlos};
route(cs_carlos_self) ->
	{role,role_carlos};
route(cs_carlos_mov) ->
	{role,role_carlos};
route(cs_carlos_attack) ->
	{role,role_carlos};
route(cs_carlos_ownMine) ->
	{role,role_carlos};
route(cs_carlos_role_dtl) ->
	{role,role_carlos};
route(cs_carlos_replay) ->
	{role,role_carlos};
route(cs_carlos_mov_stop) ->
	{role,role_carlos};
route(cs_carlos_talk) ->
	{role,role_carlos};
route(cs_carlos_get_talk) ->
	{role,role_carlos};
route(cs_carlos_get_rank) ->
	{role,role_carlos};
route(cs_carlos_reborn) ->
	{role,role_carlos};
route(cs_carlos_unrequest) ->
	{role,role_carlos};
route(cs_carlos_rank_list) ->
	{role,role_carlos};
route(cs_carlos_season_info) ->
	{role,role_carlos};
route(cs_carlos_plane_select) ->
	{role,role_carlos};
route(cs_carlos_relic_sign) ->
	{role,role_carlos};
route(cs_carlos_relic_info) ->
	{role,role_carlos};
route(cs_carlos_relic_war_base_info) ->
	{role,role_carlos};
route(cs_carlos_relic_mov) ->
	{role,role_carlos};
route(cs_carlos_relic_mov_stop) ->
	{role,role_carlos};
route(cs_carlos_relic_attack) ->
	{role,role_carlos};
route(cs_carlos_relic_active) ->
	{role,role_carlos};
route(cs_carlos_relic_open_reward_box) ->
	{role,role_carlos};
route(cs_carlos_relic_buy) ->
	{role,role_carlos};
route(cs_carlos_relic_sign_cancel) ->
	{role,role_carlos};
route(cs_carlos_relic_self) ->
	{role,role_carlos};
route(cs_carlos_relic_role_dtl) ->
	{role,role_carlos};
route(cs_carlos_relic_island_detail) ->
	{role,role_carlos};
route(cs_carlos_relic_talk) ->
	{role,role_carlos};
route(cs_carlos_relic_get_talk) ->
	{role,role_carlos};
route(cs_carlos_change_plane) ->
	{role,role_carlos};
route(cs_monthVIP_info) ->
	{role,role_monthVIP};
route(cs_monthVIP_get_reward) ->
	{role,role_monthVIP};
route(cs_monthVIP_buy) ->
	{role,role_monthVIP};
route(cs_monthVIP_get_growth_fund_info) ->
	{role,role_monthVIP};
route(cs_monthVIP_buy_growth_fund) ->
	{role,role_monthVIP};
route(cs_monthVIP_get_growth_reward) ->
	{role,role_monthVIP};
route(cs_talent_get_info) ->
	{role,role_talent};
route(cs_talent_study) ->
	{role,role_talent};
route(cs_talent_undo) ->
	{role,role_talent};
route(cs_talent_cooldown) ->
	{role,role_talent};
route(cs_trumpet_message) ->
	{role,role_trumpet};
route(cs_trumpet_recent_list) ->
	{trumpet_server,role_trumpet};
route(cs_trumpet_get_bonus) ->
	{role,role_trumpet};
route(cs_trumpet_redpacket_status) ->
	{role,role_trumpet};
route(cs_trumpet_get_all_publishers) ->
	{redpacket_server,role_trumpet};
route(cs_trumpet_redpacket_openclose) ->
	{redpacket_server,role_trumpet};
route(cs_trumpet_redpacket_get_reward) ->
	{redpacket_server,role_trumpet};
route(cs_trumpet_get_world_publishers) ->
	{redpacket_server,role_trumpet};
route(cs_changename) ->
	{role,role_changename};
route(cs_changename_freetimes) ->
	{role,role_changename};
route(cs_familyBoss_base_info) ->
	{role,role_familyBoss};
route(cs_familyBoss_attack) ->
	{role,role_familyBoss};
route(cs_familyBoss_hatch_egg) ->
	{role,role_familyBoss};
route(cs_familyBoss_feed_boss) ->
	{role,role_familyBoss};
route(cs_familyBoss_set_boss_time) ->
	{role,role_familyBoss};
route(cs_familyBoss_get_rank) ->
	{role,role_familyBoss};
route(cs_familyTek_info) ->
	{role,role_familyTek};
route(cs_familyTek_upLevel) ->
	{role,role_familyTek};
route(cs_familyTek_cost) ->
	{role,role_familyTek};
route(cs_familyTek_wallet) ->
	{role,role_familyTek};
route(cs_familyTek_donate) ->
	{role,role_familyTek};
route(cs_familyfight_info) ->
	{role,role_familyfight};
route(cs_familyfight_sign) ->
	{role,role_familyfight};
route(cs_familyfight_fighter_info) ->
	{role,role_familyfight};
route(cs_familyfight_attack) ->
	{role,role_familyfight};
route(cs_familyfight_result) ->
	{role,role_familyfight};
route(cs_familyfight_get_fight_record_list) ->
	{role,role_familyfight};
route(cs_familyfight_replay) ->
	{role,role_familyfight};
route(cs_familyfight_get_fighter_history) ->
	{role,role_familyfight};
route(cs_familyfight_rankerList) ->
	{family_fight_server,role_familyfight};
route(cs_familyfight_instance_open_state) ->
	{role,role_familyfight};
route(cs_familyfight_instance_boss_info) ->
	{role,role_familyfight};
route(cs_familyfight_attack_boss) ->
	{role,role_familyfight};
route(cs_familyfight_select_instance) ->
	{role,role_familyfight};
route(cs_familyfight_instance_reward_info) ->
	{role,role_familyfight};
route(cs_familyfight_instance_get_reward) ->
	{role,role_familyfight};
route(cs_familyfight_bug_attack_time) ->
	{role,role_familyfight};
route(cs_familyfight_get_fighter) ->
	{role,role_familyfight};
route(cs_familyfight_select_fighter) ->
	{role,role_familyfight};
route(cs_alien_info) ->
	{role,role_alien};
route(cs_alien_first_five) ->
	{alien_server,role_alien};
route(cs_alien_kill_num_rank) ->
	{alien_server,role_alien};
route(cs_alien_kill_continuous_rank) ->
	{alien_server,role_alien};
route(cs_alien_guess_info) ->
	{alien_server,role_alien};
route(cs_alien_guess) ->
	{role,role_alien};
route(cs_alien_reset) ->
	{role,role_alien};
route(cs_alien_fight) ->
	{role,role_alien};
route(cs_alien_sign) ->
	{role,role_alien};
route(cs_alien_self_record) ->
	{alien_server,role_alien};
route(cs_alien_record) ->
	{alien_server,role_alien};
route(cs_alien_self_fight_replay) ->
	{alien_server,role_alien};
route(cs_alien_fight_replay) ->
	{alien_server,role_alien};
route(cs_alien_leave) ->
	{alien_server,role_alien};
route(cs_alien_view_other) ->
	{alien_server,role_alien};
route(cs_alien_view_other_dtl) ->
	{alien_server,role_alien};
route(cs_alien_buy_times) ->
	{role,role_alien};
route(cs_alien_active) ->
	{alien_server,role_alien};
route(cs_alien_finals_records) ->
	{alien_server,role_alien};
route(cs_alien_finals_list) ->
	{alien_server,role_alien};
route(cs_alien_finals_guess) ->
	{role,role_alien};
route(cs_alien_finals_fight_replay) ->
	{alien_server,role_alien};
route(cs_alien_finals_info) ->
	{alien_server,role_alien};
route(cs_alien_finals_stake_list) ->
	{role,role_alien};
route(cs_alien_finals_self_info) ->
	{alien_server,role_alien};
route(cs_alien_self_rank) ->
	{role,role_alien};
route(cs_team_pk_info) ->
	{role,role_team};
route(cs_team_refresh) ->
	{role,role_team};
route(cs_team_fight) ->
	{role,role_team};
route(cs_team_rank) ->
	{role,role_team};
route(cs_team_record) ->
	{team_pk_server,role_team};
route(cs_team_self_record) ->
	{role,role_team};
route(cs_team_move) ->
	{role,role_team};
route(cs_team_fight_replay) ->
	{team_pk_server,role_team};
route(cs_team_self_fight_replay) ->
	{role,role_team};
route(cs_team_view_other) ->
	{role,role_team};
route(cs_team_view_other_dtl) ->
	{role,role_team};
route(cs_team_new_status) ->
	{team_pk_server,role_team};
route(cs_race_history) ->
	{race_server,role_race};
route(cs_race_replay) ->
	{race_server,role_race};
route(cs_race_fight_list) ->
	{race_server,role_race};
route(cs_race_sign) ->
	{role,role_race};
route(cs_race_info) ->
	{race_server,role_race};
route(cs_race_enter) ->
	{race_server,role_race};
route(cs_race_leave) ->
	{race_server,role_race};
route(cs_race_pos_history) ->
	{race_server,role_race};
route(cs_race_is_open) ->
	{race_server,role_race};
route(cs_race_auto_sign) ->
	{role,role_race};
route(cs_race_auto_unsign) ->
	{race_server,role_race};
route(cs_race_self_history) ->
	{race_server,role_race};
route(cs_race_guess_info) ->
	{race_server,role_race};
route(cs_race_guess) ->
	{role,role_race};
route(cs_race2_info) ->
	{race2_server,role_race2};
route(cs_race2_sign) ->
	{race2_server,role_race2};
route(cs_race2_openclose) ->
	{race2_server,role_race2};
route(cs_race2_arena_fighter) ->
	{race2_server,role_race2};
route(cs_race2_fight) ->
	{race2_server,role_race2};
route(cs_race2_knockout_fighter) ->
	{race2_server,role_race2};
route(cs_race2_get_gamble) ->
	{race2_server,role_race2};
route(cs_race2_do_gamble) ->
	{race2_server,role_race2};
route(cs_race2_final_info) ->
	{race2_server,role_race2};
route(cs_race2_get_fight_rec) ->
	{race2_server,role_race2};
route(cs_race2_cancel_sign) ->
	{race2_server,role_race2};
route(cs_family_get_list) ->
	{family_manager_server,role_family};
route(cs_family_create) ->
	{role,role_family};
route(cs_family_request_join) ->
	{role,role_family};
route(cs_family_cancel_join) ->
	{role,role_family};
route(cs_family_agree_join) ->
	{role,role_family};
route(cs_family_refuse_join) ->
	{role,role_family};
route(cs_family_get_info) ->
	{role,role_family};
route(cs_family_kick) ->
	{role,role_family};
route(cs_family_create_consume) ->
	{role,role_family};
route(cs_family_leave) ->
	{role,role_family};
route(cs_family_change_notice) ->
	{role,role_family};
route(cs_family_request_list) ->
	{role,role_family};
route(cs_family_get_log_list) ->
	{role,role_family};
route(cs_family_get_contribute_info) ->
	{role,role_family};
route(cs_family_do_contribute) ->
	{role,role_family};
route(cs_family_search_by_family_name) ->
	{role,role_family};
route(cs_family_change_member_power) ->
	{role,role_family};
route(cs_family_send_role_energy) ->
	{role,role_family};
route(cs_family_get_role_energy) ->
	{role,role_family};
route(cs_family_get_role_send_energy_list) ->
	{role,role_family};
route(cs_family_get_member_power) ->
	{role,role_family};
route(cs_family_get_send_role_energy_list) ->
	{role,role_family};
route(cs_family_storage_info) ->
	{role,role_family};
route(cs_family_storage_req) ->
	{role,role_family};
route(cs_family_storage_assign) ->
	{role,role_family};
route(cs_family_owner_impeach) ->
	{role,role_family};
route(cs_family_wallet) ->
	{role,role_family};
route(cs_family_change_slogan) ->
	{role,role_family};
route(cs_family_worship) ->
	{role,role_family};
route(cs_family_worship_fight) ->
	{role,role_family};
route(cs_family_worship_info) ->
	{role,role_family};
route(cs_family_worship_limit) ->
	{role,role_family};
route(cs_family_get_member_list) ->
	{family_manager_server,role_family};
route(cs_family_get_donate_contribute_list) ->
	{role,role_family};
route(cs_family_impeach_list) ->
	{role,role_family};
route(cs_family_donate_contribution_summary) ->
	{role,role_family};
route(cs_family_invite_request) ->
	{role,role_family};
route(cs_combine_do) ->
	{role,role_combine};
route(cs_combine_info) ->
	{role,role_combine};
route(cs_combine_mage_info) ->
	{role,role_combine};
route(cs_combine_do_mage) ->
	{role,role_combine};
route(cs_firecracker_open) ->
	{role,role_firecracker};
route(cs_firecracker_close) ->
	{fire_server,role_firecracker};
route(cs_firecracker_setoff) ->
	{role,role_firecracker};
route(cs_firecracker_rank) ->
	{fire_server,role_firecracker};
route(cs_firecracker_get_reward) ->
	{role,role_firecracker};
route(cs_firecracker_rewardList) ->
	{role,role_firecracker};
route(cs_treaHouse_get_list) ->
	{role,role_treaHouse};
route(cs_treaHouse_is_open) ->
	{role,role_treaHouse};
route(cs_treaHouse_explore_one) ->
	{role,role_treaHouse};
route(cs_treaHouse_explore_ten) ->
	{role,role_treaHouse};
route(cs_treaHouse_refresh) ->
	{role,role_treaHouse};
route(cs_treaHouse_open_base_box) ->
	{role,role_treaHouse};
route(cs_treaHouse_get_rankInfo) ->
	{role,role_treaHouse};
route(cs_treaHouse_get_rank_Reward) ->
	{activityRank_server,role_treaHouse};
route(cs_treaHouse_get_baseBoxRewardInfo) ->
	{role,role_treaHouse};
route(cs_cross_history) ->
	{cross_server,role_cross};
route(cs_cross_replay) ->
	{cross_server,role_cross};
route(cs_cross_sign) ->
	{role,role_cross};
route(cs_cross_info) ->
	{cross_server,role_cross};
route(cs_cross_enter) ->
	{cross_server,role_cross};
route(cs_cross_leave) ->
	{cross_server,role_cross};
route(cs_cross_view_list) ->
	{cross_server,role_cross};
route(cs_cross_support) ->
	{role,role_cross};
route(cs_cross_price_list) ->
	{cross_server,role_cross};
route(cs_emperor_get_open_time) ->
	{emperor_server,role_emperor};
route(cs_emperor_enter) ->
	{emperor_server,role_emperor};
route(cs_emperor_replay) ->
	{emperor_server,role_emperor};
route(cs_emperor_quit) ->
	{emperor_server,role_emperor};
route(cs_emperor_get_bet_info) ->
	{emperor_server,role_emperor};
route(cs_emperor_role_bet) ->
	{emperor_server,role_emperor};
route(cs_emperor_bet_info) ->
	{emperor_server,role_emperor};
route(cs_emperor_get_replay) ->
	{emperor_server,role_emperor};
route(cs_challengeGod_info) ->
	{role,role_challengeGod};
route(cs_challengeGod_select_ger) ->
	{role,role_challengeGod};
route(cs_challengeGod_challenge_dungeon_one) ->
	{role,role_challengeGod};
route(cs_challengeGod_challenge_dungeon_ten) ->
	{role,role_challengeGod};
route(cs_talk_world) ->
	{role,role_talk};
route(cs_talk_gag_one) ->
	{role,role_talk};
route(cs_talk_ungag_one) ->
	{role,role_talk};
route(cs_talk_get_gag_list) ->
	{role,role_talk};
route(cs_talk_recent_list) ->
	{talk_server,role_talk};
route(cs_talk_send_whisper) ->
	{role,role_talk};
route(cs_talk_get_whisper) ->
	{role,role_talk};
route(cs_nanm_open) ->
	{nanm_server,role_nanm};
route(cs_nanm_close) ->
	{nanm_server,role_nanm};
route(cs_nanm_buff) ->
	{role,role_nanm};
route(cs_nanm_last_info) ->
	{nanm_server,role_nanm};
route(cs_nanm_cur_info) ->
	{nanm_server,role_nanm};
route(cs_nanm_rank_sync) ->
	{nanm_server,role_nanm};
route(cs_nanm_fight) ->
	{role,role_nanm};
route(cs_nanm_reborn) ->
	{role,role_nanm};
route(cs_nanm_offline_play) ->
	{role,role_nanm};
route(cs_nanm_open_time) ->
	{nanm_server,role_nanm};
route(cs_nanm_reward) ->
	{role,role_nanm};
route(cs_version) ->
	{role,role_version};
route(cs_gift_request) ->
	{role,role_gift};
route(cs_king_enter) ->
	{king_server,role_king};
route(cs_king_quit) ->
	{king_server,role_king};
route(cs_king_sign) ->
	{role,role_king};
route(cs_king_buff) ->
	{king_server,role_king};
route(cs_king_replay) ->
	{king_server,role_king};
route(cs_king_history) ->
	{king_server,role_king};
route(cs_king_rank) ->
	{king_server,role_king};
route(cs_box_item) ->
	{role,role_box};
route(cs_box_shop) ->
	{role,role_box};
route(cs_box_shop_info) ->
	{role,role_box};
route(cs_box_get_spirit_equip_count) ->
	{role,role_box};
route(cs_box_item_multi) ->
	{role,role_box};
route(cs_box_free_info) ->
	{role,role_box};
route(cs_box_free_open) ->
	{role,role_box};
route(cs_box_shopBoxDtl) ->
	{role,role_box};
route(cs_box_shop_view) ->
	{role,role_box};
route(cs_box_shop_refresh) ->
	{role,role_box};
route(cs_box_shop_get) ->
	{role,role_box};
route(cs_box_free_get) ->
	{role,role_box};
route(cs_box_ticket_shop) ->
	{role,role_box};
route(cs_box_ticket_info) ->
	{role,role_box};
route(cs_activity_get_list) ->
	{role,role_activity};
route(cs_activity_info) ->
	{role,role_activity};
route(cs_activity_draw) ->
	{role,role_activity};
route(cs_activity_energy) ->
	{activity_server,role_activity};
route(cs_activity_sign_emperor_info) ->
	{role,role_activity};
route(cs_activity_sign_get_reward) ->
	{role,role_activity};
route(cs_activity_sign_up) ->
	{role,role_activity};
route(cs_activity_rebate_info) ->
	{role,role_activity};
route(cs_activity_rebate_get_reward) ->
	{role,role_activity};
route(cs_activity_levelRank_open) ->
	{role,role_activity};
route(cs_activity_levelRank_refresh) ->
	{role,role_activity};
route(cs_activity_get_payExt_info) ->
	{role,role_activity};
route(cs_activity_vip_shop) ->
	{role,role_activity};
route(cs_activity_vip_shop_buy) ->
	{role,role_activity};
route(cs_activity_first_pay) ->
	{role,role_activity};
route(cs_activity_consume_reback) ->
	{role,role_activity};
route(cs_activity_consume_state) ->
	{role,role_activity};
route(cs_activity_energy_pac_info) ->
	{role,role_activity};
route(cs_activity_energy_pac_use) ->
	{role,role_activity};
route(cs_invite_info) ->
	{role,role_invite};
route(cs_invite_bind_weibo) ->
	{role,role_invite};
route(cs_invite_weibo_share_levelup) ->
	{role,role_invite};
route(cs_invite_input_invite_code) ->
	{role,role_invite};
route(cs_invite_list) ->
	{role,role_invite};
route(cs_friend_get_list) ->
	{friend_server,role_friend};
route(cs_friend_more) ->
	{role,role_friend};
route(cs_friend_get_add_list) ->
	{friend_server,role_friend};
route(cs_friend_add) ->
	{friend_server,role_friend};
route(cs_friend_explore) ->
	{friend_server,role_friend};
route(cs_friend_delete) ->
	{friend_server,role_friend};
route(cs_friend_send_enargy) ->
	{friend_server,role_friend};
route(cs_friend_give_enargy) ->
	{role,role_friend};
route(cs_friend_give_all_enargy) ->
	{role,role_friend};
route(cs_gather_get_list) ->
	{role,role_gather};
route(cs_gather_manual_info_for_tag) ->
	{role,role_gather};
route(cs_gather_manual_info) ->
	{role,role_gather};
route(cs_gather_manual_collect_draw) ->
	{role,role_gather};
route(cs_hist_get_list) ->
	{hist_server,role_hist};
route(cs_hist_more) ->
	{hist_server,role_hist};
route(cs_hist_replay) ->
	{hist_server,role_hist};
route(cs_mail_info) ->
	{mail_server,role_mail};
route(cs_mail_draw_reward) ->
	{mail_server,role_mail};
route(cs_mail_delete) ->
	{mail_server,role_mail};
route(cs_mail_new) ->
	{role,role_mail};
route(cs_mail_unread_num) ->
	{mail_server,role_mail};
route(cs_mail_more) ->
	{mail_server,role_mail};
route(cs_mail_agree_friend) ->
	{mail_server,role_mail};
route(cs_mail_del_spec_mail) ->
	{mail_server,role_mail};
route(cs_mail_invite_operate) ->
	{mail_server,role_mail};
route(cs_mail_draw_reward_all) ->
	{mail_server,role_mail};
route(cs_mail_canvass_info) ->
	{role,role_mail};
route(cs_mail_get_question) ->
	{role,role_mail};
route(cs_mail_do_select) ->
	{role,role_mail};
route(cs_hron_info) ->
	{role,role_hron};
route(cs_hron_info_on) ->
	{role,role_hron};
route(cs_hron_last_rank_list) ->
	{hron_server,role_hron};
route(cs_hron_cur_rank_list) ->
	{hron_server,role_hron};
route(cs_hron_buy) ->
	{role,role_hron};
route(cs_hron_fight) ->
	{role,role_hron};
route(cs_hron_rank) ->
	{role,role_hron};
route(cs_hron_open_time) ->
	{role,role_hron};
route(cs_hron_succ_reward) ->
	{role,role_hron};
route(cs_hron_select) ->
	{role,role_hron};
route(cs_hron_pass) ->
	{role,role_hron};
route(cs_hron_reward_view) ->
	{role,role_hron};
route(cs_hron_raids) ->
	{role,role_hron};
route(cs_hula_open) ->
	{hula_server,role_hula};
route(cs_hula_close) ->
	{hula_server,role_hula};
route(cs_hula_buff) ->
	{role,role_hula};
route(cs_hula_last_info) ->
	{hula_server,role_hula};
route(cs_hula_cur_info) ->
	{hula_server,role_hula};
route(cs_hula_rank_sync) ->
	{hula_server,role_hula};
route(cs_hula_fight) ->
	{role,role_hula};
route(cs_hula_reborn) ->
	{role,role_hula};
route(cs_hula_offline_play) ->
	{role,role_hula};
route(cs_hula_open_time) ->
	{hula_server,role_hula};
route(cs_card_get_list) ->
	{role,role_card};
route(cs_card_draw) ->
	{role,role_card};
route(cs_card_refresh) ->
	{role,role_card};
route(cs_card_onekey) ->
	{role,role_card};
route(cs_card_activity_info) ->
	{role,role_card};
route(cs_daily_get_list) ->
	{role,role_daily};
route(cs_daily_draw) ->
	{role,role_daily};
route(cs_plunder_info) ->
	{role,role_plunder};
route(cs_plunder_compose) ->
	{plunder_server,role_plunder};
route(cs_plunder_get_target) ->
	{plunder_server,role_plunder};
route(cs_plunder_fight) ->
	{role,role_plunder};
route(cs_plunder_use_protect) ->
	{role,role_plunder};
route(cs_plunder_buy_attacktime) ->
	{role,role_plunder};
route(cs_plunder_fight_ten) ->
	{role,role_plunder};
route(cs_plunder_protect_time) ->
	{role,role_plunder};
route(cs_plunder_multi_compose) ->
	{plunder_server,role_plunder};
route(cs_pvp_get_list) ->
	{pvp_server,role_pvp};
route(cs_pvp_fight) ->
	{role,role_pvp};
route(cs_pvp_get_first_eight_replays) ->
	{pvp_server,role_pvp};
route(cs_pvp_eight_replay) ->
	{pvp_server,role_pvp};
route(cs_pvp_free_fight) ->
	{role,role_pvp};
route(cs_pvp_get_free_fight_level) ->
	{role,role_pvp};
route(cs_shop_buy_num) ->
	{role,role_shop};
route(cs_shop_buy) ->
	{role,role_shop};
route(cs_shop_encounter) ->
	{role,role_shop};
route(cs_shop_refresh) ->
	{role,role_shop};
route(cs_shop_family_limit_info) ->
	{role,role_shop};
route(cs_shop_family_limit_buy) ->
	{role,role_shop};
route(cs_shop_seed_info) ->
	{role,role_shop};
route(cs_item_bag) ->
	{role,role_item};
route(cs_item_equip) ->
	{role,role_item};
route(cs_item_sell) ->
	{role,role_item};
route(cs_item_down_equip) ->
	{role,role_item};
route(cs_item_up_equip) ->
	{role,role_item};
route(cs_item_use) ->
	{role,role_item};
route(cs_item_reinforce) ->
	{role,role_item};
route(cs_item_max_reinforce) ->
	{role,role_item};
route(cs_item_up_rank) ->
	{role,role_item};
route(cs_item_compound) ->
	{role,role_item};
route(cs_item_eat) ->
	{role,role_item};
route(cs_item_use_info) ->
	{role,role_item};
route(cs_item_auto_up_equip) ->
	{role,role_item};
route(cs_item_stone_eat) ->
	{role,role_item};
route(cs_item_decompose) ->
	{role,role_item};
route(cs_item_decompose_again) ->
	{role,role_item};
route(cs_item_decompose_by_money_cost) ->
	{role,role_item};
route(cs_item_decompose_again_cost) ->
	{role,role_item};
route(cs_item_enchant) ->
	{role,role_item};
route(cs_item_x_get) ->
	{role,role_item};
route(cs_item_down_rank) ->
	{role,role_item};
route(cs_item_pft_uprank) ->
	{role,role_item};
route(cs_item_stone_uprank) ->
	{role,role_item};
route(cs_item_max_reinforce_for_ger) ->
	{role,role_item};
route(cs_item_make_legend) ->
	{role,role_item};
route(cs_item_legend_uprank) ->
	{role,role_item};
route(cs_item_stone_legend) ->
	{role,role_item};
route(cs_explore_one) ->
	{role,role_explore};
route(cs_explore_dungeon_list) ->
	{role,role_explore};
route(cs_explore_challenge_encounter) ->
	{role,role_explore};
route(cs_explore_giveup_encounter) ->
	{role,role_explore};
route(cs_explore_list) ->
	{role,role_explore};
route(cs_explore_collect) ->
	{role,role_explore};
route(cs_explore_force_collect) ->
	{role,role_explore};
route(cs_explore_auto_explore_check) ->
	{role,role_explore};
route(cs_explore_encounter_pass_reward) ->
	{role,role_explore};
route(cs_explore_encounter_dungeon_state) ->
	{role,role_explore};
route(cs_explore_free) ->
	{role,role_explore};
route(cs_explore_all) ->
	{role,role_explore};
route(cs_ger_info) ->
	{role,role_ger};
route(cs_ger_standup) ->
	{role,role_ger};
route(cs_ger_move_pos) ->
	{role,role_ger};
route(cs_ger_pos_list) ->
	{role,role_ger};
route(cs_ger_sell) ->
	{role,role_ger};
route(cs_ger_detail) ->
	{role,role_ger};
route(cs_ger_view_other) ->
	{role,role_ger};
route(cs_ger_view_other_dtl) ->
	{role,role_ger};
route(cs_ger_eat) ->
	{role,role_ger};
route(cs_ger_up_rank) ->
	{role,role_ger};
route(cs_ger_saved_tag) ->
	{role,role_ger};
route(cs_ger_set_icon) ->
	{role,role_ger};
route(cs_ger_buy_tag) ->
	{role,role_ger};
route(cs_ger_set_tag_data) ->
	{role,role_ger};
route(cs_ger_change_tag) ->
	{role,role_ger};
route(cs_ger_lieu_pos_list) ->
	{role,role_ger};
route(cs_ger_lieu_standup) ->
	{role,role_ger};
route(cs_ger_lieu_dequeue) ->
	{role,role_ger};
route(cs_ger_lieu_untie) ->
	{role,role_ger};
route(cs_ger_lieu_info_list) ->
	{role,role_ger};
route(cs_ger_lieu_move_pos) ->
	{role,role_ger};
route(cs_ger_lieu_lock_clo) ->
	{role,role_ger};
route(cs_ger_lieu_unlock_clo) ->
	{role,role_ger};
route(cs_ger_lieu_refresh_clo) ->
	{role,role_ger};
route(cs_ger_lieu_tie_info) ->
	{role,role_ger};
route(cs_ger_lieu_refresh_freeTimes) ->
	{role,role_ger};
route(cs_ger_down_rank) ->
	{role,role_ger};
route(cs_ger_unload) ->
	{role,role_ger};
route(cs_ger_mirror_info) ->
	{role,role_ger};
route(cs_ger_mirror_convert) ->
	{role,role_ger};
route(cs_ger_mirror_commit) ->
	{role,role_ger};
route(cs_ger_set_body) ->
	{role,role_ger};
route(cs_ger_second_uprank) ->
	{role,role_ger};
route(cs_ger_batch_uprank) ->
	{role,role_ger};
route(cs_ger_second_transmigration) ->
	{role,role_ger};
route(cs_ger_transform_blink) ->
	{role,role_ger};
route(cs_message_notice) ->
	{role,role_message};
route(cs_message_certain_notice) ->
	{role,role_message};
route(cs_message_test) ->
	{role,role_message};
route(cs_battle_progress) ->
	{role,role_battle};
route(cs_battle_info) ->
	{role,role_battle};
route(cs_battle_challenge) ->
	{role,role_battle};
route(cs_battle_perfect_reward) ->
	{role,role_battle};
route(cs_battle_dungeon_raids) ->
	{role,role_battle};
route(cs_battle_star_reward) ->
	{role,role_battle};
route(cs_battle_reset_dungeon) ->
	{role,role_battle};
route(cs_battle_world_level) ->
	{role,role_battle};
route(cs_battle_obtain_boss_reward) ->
	{role,role_battle};
route(cs_battle_get_boss_reward_info) ->
	{role,role_battle};
route(cs_battle_dojang_info) ->
	{role,role_battle};
route(cs_battle_dojang_fight) ->
	{role,role_battle};
route(cs_battle_dojang_harvest) ->
	{role,role_battle};
route(cs_battle_dojang_buy) ->
	{role,role_battle};
route(cs_role_info) ->
	{role,role_role};
route(cs_role_buy_energy) ->
	{role,role_role};
route(cs_role_setting) ->
	{role,role_role};
route(cs_role_get_energy) ->
	{role,role_role};
route(cs_role_buy_coin_value) ->
	{role,role_role};
route(cs_role_weixin_share) ->
	{role,role_role};
route(cs_role_suggest_open) ->
	{role,role_role};
route(cs_role_suggest) ->
	{role,role_role};
route(cs_role_log_guide_state) ->
	{role,role_role};
route(cs_role_multi_buy_energy) ->
	{role,role_role};
route(cs_role_is_under_examine) ->
	{role,role_role};
route(cs_role_push_setting) ->
	{push_server,role_role};
route(cs_role_get_guide_state) ->
	{role,role_role};
route(cs_role_set_guide_state) ->
	{role,role_role};
route(cs_role_change_head) ->
	{role,role_role};
route(cs_role_change_location) ->
	{role,role_role};
route(cs_role_teamPkTimes_info) ->
	{role,role_role};
route(cs_role_token) ->
	{role,role_role};
route(cs_role_select_ger) ->
	{role,role_role};
route(cs_role_demo_fight) ->
	{role,role_role};
route(cs_role_pay_ios) ->
	{pay_server,role_role};
route(cs_role_pay_91) ->
	{pay_server,role_role};
route(cs_role_do_transmigration) ->
	{role,role_role};
route(cs_role_can_transmigration) ->
	{role,role_role};
route(cs_account_login) ->
	{role,role_account};
route(cs_account_create) ->
	{role,role_account};
route(cs_account_enter_game) ->
	{role,role_account};
route(cs_account_heart) ->
	{role,role_account};
route(cs_account_logout) ->
	{role,role_account};
route(cs_account_check_rolename) ->
	{role,role_account};
route(cs_account_demo) ->
	{role,role_account};
route(_) ->undefined.

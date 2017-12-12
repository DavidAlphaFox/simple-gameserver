
-define(MAX_FIGHT_NUM, 512).
-define(FIRST_EIGHT, 8).
-define(FIRST_FOUR, 4).
-define(FIRST_TWO, 2).
-define(FIRST_ONE, 1).

%% 华丽大赛活动的状态
-define(STATUS_NOT_OPEN,            0).
-define(STATUS_SIGN,                1).
-define(STATUS_WAIT_PRE_FIHGT1,     2).
-define(STATUS_PRE_FIGHT1,          3).
-define(STATUS_WAIT_PRE_FIHGT2,     4).
-define(STATUS_PRE_FIGHT2,          5).
-define(STATUS_WAIT_PRE_FIHGT3,     6).
-define(STATUS_PRE_FIGHT3,          7).
-define(STATUS_WAIT_PRE_FIHGT4,     8).
-define(STATUS_PRE_FIGHT4,          9).
-define(STATUS_WAIT_PRE_FIHGT5,     10).
-define(STATUS_PRE_FIGHT5,          11).
-define(STATUS_WAIT_PRE_FIHGT6,     12).
-define(STATUS_PRE_FIGHT6,          13).
-define(STATUS_WAIT_PRE_FIHGT7,     14).
-define(STATUS_PRE_FIGHT7,          15).
-define(STATUS_WAIT_PRE_FIHGT8,     16).
-define(STATUS_PRE_FIGHT8,          17).
-define(STATUS_WAIT_FOUR_FIGHT,     18).
-define(STATUS_FOUR_FIGHT,          19).      
-define(STATUS_WAIT_TWO_FIGHT,      20).
-define(STATUS_TWO_FIGHT,           21).
-define(STATUS_WAIT_FINAL_FIGHT,    22).
-define(STATUS_FINAL_FIGHT,         23).


%% 错误返回提示码
-define(REASON_CODE_NOT_SIGN_TIME, 1).
-define(REASON_CODE_ROLE_LEVEL_NOT_ENOUGH, 2).
-define(REASON_CODE_ROLE_ALREADY_SIGN, 3).
-define(REASON_CODE_SIGN_NUM_IS_MAX, 4).
-define(REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN, 5).
-define(REASON_CODE_CAN_NOT_GET_ROLE_INFO_FOR_RACE_SIGN, 6).



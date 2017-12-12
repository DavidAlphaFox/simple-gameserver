

%% =================================
%% dictionary key in role server




%% =================================



%% 关卡信息
-record(dungeon, {
				  dungeonID :: ?int16
				  ,restTimes :: ?int16
				  ,bestScore :: ?int8
				  ,resetTimes :: ?int16
				  }).

%% 章节信息
-record(chapter, {
				  id :: {?int32, ?int8}		%% 以玩家ID加章节ID为key
				  ,perfectRewarded:: boolean()
				  ,dungeonList :: [#dungeon{}]
				  ,curDate:: calendar:date()
				  ,starRewarded :: ?int16
				  }).

-define(MAX_DUNGEON_SCORE, 3).
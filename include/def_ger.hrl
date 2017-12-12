-include("common.hrl").
-include("record.hrl").
-include("data.hrl").
-include("all_proto.hrl").


%% 获取#ger.gerBase的子属性的简写
-define(b(Ger, Field), (Ger#ger.gerBase#gerBase.Field)).

%% 获取#ger.gerAttr的子属性的简写
-define(a(Ger, Field), (Ger#ger.gerAttr#gerAttr.Field)).

					
-module(db_trans).
-compile(export_all).

trans(gActivity, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, ActID, Value, List]) ->
					[RoleID, ActID, db_sql:quote(Value), db_sql:quote(List)] end, DataList),
	{"insert into gActivity values", "(~w, ~w, ~s, ~s)", NewDataList};

trans(gAlien, DataList) ->
	{"insert into gAlien values", "(~w, ~w, ~w, ~w)", DataList};

trans(gBagItem, DataList) ->
	{"insert into gBagItem values", "(~w, ~w, ~w, ~w)", DataList};

trans(gBestPassChapter, DataList) ->
	{"insert into gBestPassChapter values", "(~w, ~w)", DataList};

trans(gCard, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, OpenedCardList, CardList, DrawCount, ActivityID]) ->
					[RoleID, db_sql:quote(OpenedCardList), db_sql:quote(CardList), DrawCount, ActivityID] end, DataList),
	{"insert into gCard values", "(~w, ~s, ~s, ~w, ~w)", NewDataList};

trans(gChapter, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, ChapterID, BestRewarded, {date, CurDate}, StarRewarded]) ->
					[RoleID, ChapterID, BestRewarded, db_sql:date(CurDate), StarRewarded] end, DataList),
	{"insert into gChapter values", "(~w, ~w, ~w, '~s', ~w)", NewDataList};

trans(gDungeon, DataList) ->
	{"insert into gDungeon values", "(~w, ~w, ~w, ~w, ~w, ~w)", DataList};

trans(gEquip, DataList) ->
	{"insert into gEquip values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w)", DataList};

trans(gFamily, DataList) ->
	NewDataList = 
		lists:map(fun([FamilyID, FamilyName, FamilyLevel, CreateRoleID, CreateRoleName, OwnerRoleID, OwnerRoleName, CurMembers, ActivePoints, FamilyScore, Notice, Slogan, Rank, WorldRank, {date, IsZeroRefreshed}, CreateTime, TalkData, ChangeLog, ContributeLog, LeftChgNameTimes, FamilyTask, Cross_rank, TalkRoomID, Boss_info, Fighter_group]) ->
					[FamilyID, db_sql:quote(FamilyName), FamilyLevel, CreateRoleID, db_sql:quote(CreateRoleName), OwnerRoleID, db_sql:quote(OwnerRoleName), CurMembers, ActivePoints, FamilyScore, db_sql:quote(Notice), db_sql:quote(Slogan), Rank, WorldRank, db_sql:date(IsZeroRefreshed), CreateTime, db_sql:quote(""), db_sql:quote(""), db_sql:quote(ContributeLog), LeftChgNameTimes, db_sql:quote(FamilyTask), Cross_rank, db_sql:quote(TalkRoomID), db_sql:quote(Boss_info), db_sql:quote(Fighter_group)] end, DataList),
	{"insert into gFamily values", "(~w, ~s, ~w, ~w, ~s, ~w, ~s, ~w, ~w, ~w, ~s, ~s, ~w, ~w, '~s', ~w, ~s, ~s, ~s, ~w, ~s, ~w, ~s, ~s, ~s)", NewDataList};

trans(gFamilyMember, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, RoleName, FamilyID, FamilyCon, LeftFamilyCon, UseGoldTime, Title, IsMale, RoleLevel, FightPower, FamilyTitle, IsJoinWar, AttackTimes, DefendTimes, WinStar, RewardLevel, JoinTime, WeeklyContributes, {date, LastContributeDate}, LastRecvEnergyList, StorageReqData, Head, OfflineTime, LimitShop]) ->
					[RoleID, db_sql:quote(RoleName), FamilyID, FamilyCon, LeftFamilyCon, UseGoldTime, Title, IsMale, RoleLevel, FightPower, FamilyTitle, IsJoinWar, AttackTimes, DefendTimes, WinStar, RewardLevel, JoinTime, WeeklyContributes, db_sql:date(LastContributeDate), db_sql:quote(LastRecvEnergyList), db_sql:quote(StorageReqData), Head, OfflineTime, db_sql:quote(LimitShop)] end, DataList),
	{"insert into gFamilyMember values", "(~w, ~s, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~s', ~s, ~s, ~w, ~w, ~s)", NewDataList};

trans(gFighterList, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, FighterList, LieuInfoList, LieuAtkAdd, LieuHpAdd, TrSpecial]) ->
					[RoleID, db_sql:quote(FighterList), db_sql:quote(LieuInfoList), LieuAtkAdd, LieuHpAdd, db_sql:quote(TrSpecial)] end, DataList),
	{"insert into gFighterList values", "(~w, ~s, ~s, ~w, ~w, ~s)", NewDataList};

trans(gFriend, DataList) ->
	{"insert into gFriend values", "(~w, ~w, ~w)", DataList};

trans(gGather, DataList) ->
	{"insert into gGather values", "(~w, ~w, ~w)", DataList};

trans(gGer, DataList) ->
	NewDataList = 
		lists:map(fun([GerID, RoleID, GerTypeID, GerLevel, GerExp, GerRank, GerPos, GerAwakeInfo, GerBody]) ->
					[GerID, RoleID, GerTypeID, GerLevel, GerExp, GerRank, GerPos, db_sql:quote(GerAwakeInfo), GerBody] end, DataList),
	{"insert into gGer values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~s, ~w)", NewDataList};

trans(gGift, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Type]) ->
					[RoleID, db_sql:quote(Type)] end, DataList),
	{"insert into gGift values", "(~w, ~s)", NewDataList};

trans(gGuide, DataList) ->
	{"insert into gGuide values", "(~w, ~w)", DataList};

trans(gHron, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, {date, Date}, Star, CurDungeonNum, AttackAdd, HpAdd, Morale, DungeonIDList, BestScore, ChallengeTimes, IsHaveSuccReward, LastFightResult, IsSelect]) ->
					[RoleID, db_sql:date(Date), Star, CurDungeonNum, AttackAdd, HpAdd, Morale, db_sql:quote(DungeonIDList), BestScore, ChallengeTimes, IsHaveSuccReward, LastFightResult, IsSelect] end, DataList),
	{"insert into gHron values", "(~w, '~s', ~w, ~w, ~w, ~w, ~w, ~s, ~w, ~w, ~w, ~w, ~w)", NewDataList};

trans(gInvite, DataList) ->
	{"insert into gInvite values", "(~w, ~w)", DataList};

trans(gInviteRoleList, DataList) ->
	{"insert into gInviteRoleList values", "(~w, ~w)", DataList};

trans(gLimit, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, EncounterNum, IsBindWeibo, InviteRoleID, InviteRoleName, LastShareLevel, SpiritGoldBoxCount, SpiritGoldBonusBoxCount, SpiritItemBonusBoxCount, EquipGoldBoxCount
                      , EquipGoldBonusBoxCount, EquipItemBonusBoxCount, TrainerGoldBonusBoxCount, TrainerGoldBoxCount, TrainerItemBoxCount,VspiritRefresh,VequipRefresh,VtrainerRefresh,VCombine2Value,TicketValue,TicketValue2]) ->
					[RoleID, EncounterNum, IsBindWeibo, InviteRoleID, db_sql:quote(InviteRoleName), LastShareLevel, SpiritGoldBoxCount, SpiritGoldBonusBoxCount, SpiritItemBonusBoxCount, EquipGoldBoxCount, EquipGoldBonusBoxCount, EquipItemBonusBoxCount, TrainerGoldBonusBoxCount, TrainerGoldBoxCount, TrainerItemBoxCount,VspiritRefresh,VequipRefresh,VtrainerRefresh,VCombine2Value,TicketValue,TicketValue2] end, DataList),
	{"insert into gLimit values", "(~w, ~w, ~w, ~w, ~s, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w,~w,~w)", NewDataList};

trans(gMail, DataList) ->
	NewDataList = 
		lists:map(fun([MailUID, RecvID, MailType, SenderID, SenderName, Content, Time, MailTemplateID, ParamList, MailReward, IsRead, Head, IsMale]) ->
					[MailUID, RecvID, MailType, SenderID, db_sql:quote(SenderName), db_sql:quote(Content), Time, MailTemplateID, db_sql:quote(ParamList), db_sql:quote(MailReward), IsRead, Head, IsMale] end, DataList),
	{"insert into gMail values", "(~w, ~w, ~w, ~w, ~s, ~s, ~w, ~w, ~s, ~s, ~w, ~w, ~w)", NewDataList};

trans(gOfflineDeductGold, DataList) ->
	{"insert into gOfflineDeductGold values", "(~w, ~w)", DataList};

trans(gOfflinePayLog, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, PayItemID, Receipt, ReceiptMd5, SrcType]) ->
					[RoleID, PayItemID, db_sql:quote(Receipt), db_sql:quote(ReceiptMd5), SrcType] end, DataList),
	{"insert into gOfflinePayLog values", "(~w, ~w, ~s, ~s, ~w)", NewDataList};

trans(gPay, DataList) ->
	NewDataList = 
		lists:map(fun([ReceiptMd5, RoleID, Receipt, SrcType, {datetime, Time}, PayGold]) ->
					[db_sql:quote(ReceiptMd5), RoleID, db_sql:quote(Receipt), SrcType, db_sql:datetime(Time), PayGold] end, DataList),
	{"insert into gPay values", "(~s, ~w, ~s, ~w, '~s', ~w)", NewDataList};

trans(gPush, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Token, IsPVPPushOpen, IsPushNightMute]) ->
					[RoleID, db_sql:quote(Token), IsPVPPushOpen, IsPushNightMute] end, DataList),
	{"insert into gPush values", "(~w, ~s, ~w, ~w)", NewDataList};

trans(gRole, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Accid, RoleName, IsMale, Level, Exp, Coin, Reputation, Gold, GoldBonus, GoldUsed, Unioncoin, ProfoundCrystal
					  , VipLevel, GoldTotalPaid, Title, FightPower, LastLogoutTime, FamilyID, LastJoinFamily, Head, PayExtReward, ExtRdActTime
					  , Location, IsFailed, Devid, SrcType, LastLoginTime, Tasklevel, Plane_level, Teamid, Honor, Pvppoint, Carloswintime
					  , Carlosequaltime, Carloslosetime, Carlosseason, Carlosprewintime, Carlospreequaltime, Carlosprelosetime, Carlospreseason
					  ,HomeResource,FirstPayStatus,Ticket]) ->
					[RoleID, Accid, db_sql:quote(RoleName), IsMale, Level, Exp, Coin, Reputation, Gold, GoldBonus
                    , GoldUsed, Unioncoin, ProfoundCrystal, VipLevel, GoldTotalPaid, Title, FightPower, LastLogoutTime, FamilyID, LastJoinFamily
                    , Head, PayExtReward, ExtRdActTime, db_sql:quote(Location), IsFailed, db_sql:quote(Devid), SrcType, LastLoginTime, Tasklevel, Plane_level
                    , Teamid, Honor, Pvppoint, Carloswintime, Carlosequaltime, Carloslosetime, Carlosseason, Carlosprewintime, Carlospreequaltime, Carlosprelosetime, Carlospreseason,HomeResource,FirstPayStatus,Ticket] end, DataList),
	{"insert into gRole values", "(~w, ~w, ~s, ~w, ~w, ~w, ~w, ~w, ~w, ~w
                                 , ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w
                                 , ~w, ~w, ~w, ~s, ~w, ~s, ~w, ~w, ~w, ~w
                                 , ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w,~w)", NewDataList};

trans(gRoleExtra, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, BattleProgress, BattleProgressHard, BattleProgressFastHard, Energy, EnergyBuyTimes, ChallengeGodEnergy, ChallengeGodBuyTimes, {date, LastChallengeGodDate}, RefreshLieuTimes
                      , AlreadyPayRefreshLieuTimes, DscvBuyTimes, PvpBuyTimes, PlunderBuyTimes, CoinBuyTimes, FireTimes, {date, LastBuyTimesRefreshDate}, LastEnergyTime, DiscoveryTimes, LastDscvTime
                      , DscvCount, PvpTimes, LastPvpTime, PlunderTimes, LastPlunderTime, WeiboCount, NextWeiboCountRefreshSec, LastWeiXinShareSec, EncounterList, {date, LastTitleRewardDate}
                      , LastDrawTitle, {date, LastLoggedLoginDate}, LastDrawLoginRewardDays, LoginDays, LastDrawLevelUpLevel, RandomShopList, LeftChgNameTimes, TalentStudyBuyTimes, LastPayTime, MagicBookState
                      , TeamPkTimes, TeampPkBuyTimes, LastTeamPkTime, Sign_day_count, Is_get_sign_reward, Last_sign_time, Is_get_acc_sign_reward,BattleBossRewardInfo,MainGerTypeID]) ->
					[RoleID, BattleProgress, BattleProgressHard, BattleProgressFastHard, Energy, EnergyBuyTimes, ChallengeGodEnergy, ChallengeGodBuyTimes, db_sql:date(LastChallengeGodDate), RefreshLieuTimes
                    , AlreadyPayRefreshLieuTimes, DscvBuyTimes, PvpBuyTimes, PlunderBuyTimes, CoinBuyTimes, FireTimes, db_sql:date(LastBuyTimesRefreshDate), LastEnergyTime, DiscoveryTimes, LastDscvTime
                    , DscvCount, PvpTimes, LastPvpTime, PlunderTimes, LastPlunderTime, WeiboCount, NextWeiboCountRefreshSec, LastWeiXinShareSec, db_sql:quote(EncounterList), db_sql:date(LastTitleRewardDate)
                    , LastDrawTitle, db_sql:date(LastLoggedLoginDate), LastDrawLoginRewardDays, LoginDays, LastDrawLevelUpLevel, db_sql:quote(RandomShopList), LeftChgNameTimes, TalentStudyBuyTimes, LastPayTime, db_sql:quote(MagicBookState)
                    , TeamPkTimes, TeampPkBuyTimes, LastTeamPkTime, Sign_day_count, Is_get_sign_reward, Last_sign_time, Is_get_acc_sign_reward,db_sql:quote(BattleBossRewardInfo),MainGerTypeID] end, DataList),
	{"insert into gRoleExtra values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~s', ~w, 
                                        ~w, ~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w, ~w, 
                                        ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~s, '~s', 
                                        ~w, '~s', ~w, ~w, ~w, ~s, ~w, ~w, ~w, ~s, 
                                        ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~s, ~w)", NewDataList};

trans(gShopNum, DataList) ->
	{"insert into gShopNum values", "(~w, ~w, ~w, ~w)", DataList};

trans(gTeamPk, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, TeamPkData]) ->
					[RoleID, db_sql:quote(TeamPkData)] end, DataList),
	{"insert into gTeamPk values", "(~w, ~s)", NewDataList};

trans(gtreasurepatch, DataList) ->
	{"insert into gtreasurepatch values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w)", DataList};

trans(gEncounter, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, MonsterRank, ChapterInfoList]) ->
					[RoleID, MonsterRank, db_sql:quote(ChapterInfoList)] end, DataList),
	{"insert into gEncounter values", "(~w, ~w, ~s)", NewDataList};

trans(gLieuInfo, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, LieuInfo]) ->
					[RoleID, db_sql:quote(LieuInfo)] end, DataList),
	{"insert into gLieuInfo values", "(~w, ~s)", NewDataList};

trans(gOtherRecord, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, {date, LastSignDate}, SignedDays, IsEmperor, IsGetBox]) ->
					[RoleID, db_sql:date(LastSignDate), SignedDays, IsEmperor, IsGetBox] end, DataList),
	{"insert into gOtherRecord values", "(~w, '~s', ~w, ~w, ~w)", NewDataList};

trans(gTask, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, TaskID, Status, TriggerNum, TriggerNotes]) ->
					[RoleID, TaskID, Status, TriggerNum, db_sql:quote(TriggerNotes)] end, DataList),
	{"insert into gTask values", "(~w, ~w, ~w, ~w, ~s)", NewDataList};

trans(gHomestead, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, RoleName, AddEnergyTimes, MatingTimes, MatingCoolSecond, Add4mating, GerID, GerTypeID, Quality, Level, RefreshMatingSecond, MachineList, LogList]) ->
					[RoleID, db_sql:quote(RoleName), AddEnergyTimes, MatingTimes, MatingCoolSecond, Add4mating, GerID, GerTypeID, Quality, Level, RefreshMatingSecond, db_sql:quote(MachineList), db_sql:quote(LogList)] end, DataList),
	{"insert into gHomestead values", "(~w, ~s, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~s, ~s)", NewDataList};

trans(gFriendEnargy, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, {date, RefreshDate}, GiveTimes]) ->
					[RoleID, db_sql:date(RefreshDate), GiveTimes] end, DataList),
	{"insert into gFriendEnargy values", "(~w, '~s', ~w)", NewDataList};

trans(gToFriend, DataList) ->
	{"insert into gToFriend values", "(~w, ~w, ~w, ~w)", DataList};

trans(gToMe, DataList) ->
	{"insert into gToMe values", "(~w, ~w, ~w)", DataList};

trans(gAddFriend, DataList) ->
	{"insert into gAddFriend values", "(~w, ~w, ~w)", DataList};

trans(gFamilyFight, DataList) ->
	NewDataList = 
		lists:map(fun([FamilyID, WarPeriod, IsSign, AttackTimes, DefendTimes, WinStar, MatcherFamilyID, MatcherServerID, MatcherWinStar, MatcherWorldRank, MatcherFamilyName, Result, LastWorldRank]) ->
					[FamilyID, WarPeriod, IsSign, AttackTimes, DefendTimes, WinStar, MatcherFamilyID, MatcherServerID, MatcherWinStar, MatcherWorldRank, db_sql:quote(MatcherFamilyName), Result, LastWorldRank] end, DataList),
	{"insert into gFamilyFight values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~s, ~w, ~w)", NewDataList};

trans(gFamilyStorage, DataList) ->
	NewDataList = 
		lists:map(fun([ItemUID, FamilyID, ItemTypeID, Type, ItemReqBin]) ->
					[ItemUID, FamilyID, ItemTypeID, Type, db_sql:quote(ItemReqBin)] end, DataList),
	{"insert into gFamilyStorage values", "(~w, ~w, ~w, ~w, ~s)", NewDataList};

trans(gBoxInfo, DataList) ->
	{"insert into gBoxInfo values", "(~w, ~w, ~w, ~w)", DataList};

trans(gStoneChip, DataList) ->
	{"insert into gStoneChip values", "(~w, ~w, ~w, ~w, ~w, ~w)", DataList};

trans(gPlunder, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, ProtectEndTime, RestAttackTimes, BuyTimes, LastTimestamp, RecoverTimestamp]) ->
					[RoleID, ProtectEndTime, RestAttackTimes, BuyTimes, db_sql:quote(LastTimestamp), RecoverTimestamp] end, DataList),
	{"insert into gPlunder values", "(~w, ~w, ~w, ~w, ~s, ~w)", NewDataList};

trans(gFamilyTek, DataList) ->
	{"insert into gFamilyTek values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w)", DataList};

trans(gFamilyWallet, DataList) ->
	NewDataList = 
		lists:map(fun([FamilyID, FamilySource, FamilyTekID]) ->
					[FamilyID, db_sql:quote(FamilySource), FamilyTekID] end, DataList),
	{"insert into gFamilyWallet values", "(~w, ~s, ~w)", NewDataList};

trans(gWhisper, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, SendID, Content, Sendtime]) ->
					[RoleID, SendID, db_sql:quote(Content), Sendtime] end, DataList),
	{"insert into gWhisper values", "(~w, ~w, ~s, ~w)", NewDataList};

trans(gTrainer, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Talent]) ->
					[RoleID, db_sql:quote(Talent)] end, DataList),
	{"insert into gTrainer values", "(~w, ~s)", NewDataList};

trans(gFamilyDonate, DataList) ->
	NewDataList = 
		lists:map(fun([FamilyID, RoleID, DiamondNum, CoinNum, ReputationNum, GerInfo, ItemInfo, DonateContribution]) ->
					[FamilyID, RoleID, DiamondNum, CoinNum, ReputationNum, db_sql:quote(GerInfo), db_sql:quote(ItemInfo), DonateContribution] end, DataList),
	{"insert into gFamilyDonate values", "(~w, ~w, ~w, ~w, ~w, ~s, ~s, ~w)", NewDataList};

trans(gDiscountInfo, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, DiscountActivityInfo]) ->
					[RoleID, db_sql:quote(DiscountActivityInfo)] end, DataList),
	{"insert into gDiscountInfo values", "(~w, ~s)", NewDataList};

trans(gDiscountActivityInfo, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, DiscountActivityInfo]) ->
					[RoleID, db_sql:quote(DiscountActivityInfo)] end, DataList),
	{"insert into gDiscountActivityInfo values", "(~w, ~s)", NewDataList};

trans(gPanicBuyActivityInfo, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, PanicBuyActivityInfo]) ->
					[RoleID, db_sql:quote(PanicBuyActivityInfo)] end, DataList),
	{"insert into gPanicBuyActivityInfo values", "(~w, ~s)", NewDataList};

trans(gMonthVIP, DataList) ->
	{"insert into gMonthVIP values", "(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w)", DataList};

trans(gStarRewardChapter, DataList) ->
	{"insert into gStarRewardChapter values", "(~w, ~w, ~w, ~w)", DataList};

trans(gGerMirror, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, GerMirrorInfo]) ->
					[RoleID, db_sql:quote(GerMirrorInfo)] end, DataList),
	{"insert into gGerMirror values", "(~w, ~s)", NewDataList};

trans(gLuckyRoll, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Free_count, Buy_count, Free_times, Mark, IsGetRankReward, BaseBoxInfo, Outer, Inner, Pos, ActivityID]) ->
					[RoleID, Free_count, Buy_count, Free_times, Mark, IsGetRankReward, db_sql:quote(BaseBoxInfo), db_sql:quote(Outer), db_sql:quote(Inner), db_sql:quote(Pos), ActivityID] end, DataList),
	{"insert into gLuckyRoll values", "(~w, ~w, ~w, ~w, ~w, ~w, ~s, ~s, ~s, ~s, ~w)", NewDataList};

trans(gVipActivity, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, ActivityID, Items, Timestamp]) ->
					[RoleID, ActivityID, db_sql:quote(Items), Timestamp] end, DataList),
	{"insert into gVipActivity values", "(~w, ~w, ~s, ~w)", NewDataList};

trans(gTvCard, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Tvcard]) ->
					[RoleID, db_sql:quote(Tvcard)] end, DataList),
	{"insert into gTvCard values", "(~w, ~s)", NewDataList};

trans(gTrSpecial, DataList) ->
	{"insert into gTrSpecial values", "(~w, ~w, ~w)", DataList};

trans(gGrowthFund, DataList) ->
	NewDataList = 
		lists:map(fun([RoleID, Is_buy, Reward_record]) ->
					[RoleID, Is_buy, db_sql:quote(Reward_record)] end, DataList),
	{"insert into gGrowthFund values", "(~w, ~w, ~s)", NewDataList};

trans(gSkin, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID, Has, Equip]) ->
                    [RoleID,  db_sql:quote(Has), Equip] end, DataList),
    {"insert into gSkin values", "(~w, ~s, ~w)", NewDataList};

trans(gHome, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID, Stage, ConstrList, NewTaskTimer, DountAcNum,CirrusInfo]) ->
                    [RoleID, Stage, db_sql:quote(ConstrList), db_sql:quote(NewTaskTimer), db_sql:quote(DountAcNum),db_sql:quote(CirrusInfo)] end, DataList),
    {"insert into gHome values", "(~w, ~w, ~s, ~s, ~s, ~s)", NewDataList};

trans(gBounty, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID, SaveTime, Type, RestTime, AlreadyBuy, DountyData]) ->
                    [RoleID, SaveTime, Type, RestTime, AlreadyBuy, db_sql:quote(DountyData)] end, DataList),
    {"insert into gBounty values", "(~w, ~w, ~w, ~w, ~w, ~s)", NewDataList};

trans(gDoublematch, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID, RemainTime,AlreadyBuy,RfDate,Rank,Score,FightRecBin,Session,RemainTimeBuy,BeforeScore]) ->
                    [RoleID, RemainTime,AlreadyBuy,RfDate,Rank,Score,db_sql:quote(FightRecBin),Session,RemainTimeBuy,BeforeScore] end, DataList),
    {"insert into gDoublematch values", "(~w,~w,~w,\"~s\",~w,~w,~s,~w,~w,~w)", NewDataList};

trans(gCarlosPlaneInfo, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID, Type,Type2, Use,ValidTime,ValidTime2]) ->
                    [RoleID, Type,Type2, Use,ValidTime,ValidTime2] end, DataList),
    {"insert into gCarlosPlaneInfo values", "(~w,~w,~w,~w,~w,~w)", NewDataList};

trans(gGerCrystal, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID, GerID, GerCrystalInfo]) ->
                    [RoleID,GerID,db_sql:quote(GerCrystalInfo)] end, DataList),
    {"insert into gGerCrystal values", "(~w,~w,~s)", NewDataList};

trans(gHomeBossTimes, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID,Total,GoldTimes]) ->
                    [RoleID,Total,GoldTimes] end, DataList),
    {"insert into gHomeBossTimes values", "(~w,~w,~w)", NewDataList};

trans(gShopBoxCard, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID,CDInfo,OpenCardInfo]) ->
                    [RoleID,db_sql:quote(CDInfo),db_sql:quote(OpenCardInfo)] end, DataList),
    {"insert into gShopBoxCard values", "(~w,~s,~s)", NewDataList};

trans(gTreasureHouse, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID,ValueInfo, CardList0, FreeCount, BuyCount, FreeTimes, Mark, BaseBoxGetProcess, IsGetRankReward,{date,LastExploreDate},ActivityID]) ->
                    [RoleID,ValueInfo, db_sql:quote(CardList0), FreeCount, BuyCount, FreeTimes, Mark, db_sql:quote(BaseBoxGetProcess), IsGetRankReward,db_sql:date(LastExploreDate),ActivityID] end, DataList),
    {"insert into gTreasureHouse values", "(~w,~w, ~s,~w,~w,~w,~w,~s,~w,'~s',~w)", NewDataList};

trans(gPayGuide, DataList) ->
    NewDataList = 
        lists:map(fun([RoleID,Unit]) ->
                    [RoleID,db_sql:quote(Unit)] end, DataList),
    {"insert into gPayGuide values", "(~w,~s)", NewDataList};

trans(Name, _) -> erlang:error(io_lib:format("unknown db table:~p.~n", [Name])).


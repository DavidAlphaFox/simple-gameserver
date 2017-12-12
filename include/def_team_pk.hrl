

-record(team_pk_info, {session=0,refreshSelf=0,refreshOther=0,selfTeam=[],otherTeam=[],selfRecordList=[]}).

%%1.4.0在fighterData里面新加了天赋技能列表这一项
-record(team_member, {roleID=0,fightPower=0,isMale=true,title=0,head=0,level=0,roleName= <<"">>,fighterData={[],#add_attr{},[],[],[]},itemList=[],vip=0}).

-record(team_pk_rank, {roleID=0,score=0,timestamp=0,rank=0,fightPower=0,isMale=true,title=0,head=0,level=0,roleName= <<"">>,vip=1}).

-record(team_record,{
                     isWin=false
                     ,timestamp=0
                     ,roleName= <<"">>
                     ,godName= <<"">>
                     ,replayUIDList=[]
                     ,selfList=[]
                     ,otherList=[]}).


-record(team_self_record,{
                          timestamp=0
                          ,isWin=false
                          ,addExp=0
                          ,addCoin=0
                          ,addScore=0
                          ,selfNameList=[]
                          ,otherNameList=[]
                          ,replayUIDList=[]
                          ,selfList=[]
                          ,otherList=[]}).

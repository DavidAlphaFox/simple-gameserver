%% @author lixinglong
%% @doc 按功能分发http请求，为保证礼包与支付功能走不同的处理，可以为支付定义接口handle2来与礼包的handle相区分

-module(web_if_handle).
-export([handle/4]).

handle('POST', "broadcast"++_, Req, _DocRoot)	->
	post_bc_mod:post_bc(Req);
handle('POST', "ban2"++_, Req, _DocRoot)	->
	ban_roleID_list_mod:ban_roleID(Req);
handle('POST', "ban"++_, Req, _DocRoot)	->
	ban_roleID_mod:ban_roleID(Req);
handle('POST',"change_role_data"++_, Req, _)->
    change_role_data:change_role_data(Req);
handle('POST', "gag"++_, Req, _DocRoot)	->
	gag_roleID_mod:gag_roleID(Req);
handle('GET', "mailgift_handle"++_, Req, DocRoot) ->
    mail_gift_mod:handle(Req, DocRoot);
handle('POST', "mailgift_handle"++_, Req, DocRoot) ->
    mail_gift_mod:handle(Req, DocRoot);
handle('GET', "mailgift"++_, Req, DocRoot) ->
	mail_gift_mod:mail_gift(Req, DocRoot);
handle('POST', "mailgift"++_, Req, DocRoot) ->
	mail_gift_mod:mail_gift(Req, DocRoot);
handle('POST', "payzz"++_, Req, _DocRoot) ->
    pay_mod_zz:pay_gold(Req);
handle('POST', "payzr"++_, Req, _DocRoot) ->
    pay_mod_zr:pay_gold(Req);
handle('POST', "pay_txhd2_ard"++_, Req, _DocRoot) ->
    pay_mod_txhd2_ard:pay_gold(Req);
handle('POST', "pay_txhd_ard_ext"++_, Req, _DocRoot) ->
    pay_mod_txhd_ard_ext:pay_gold(Req);
handle('POST', "pay_txhd_ard"++_, Req, _DocRoot) ->
    pay_mod_txhd_ard:pay_gold(Req);
handle('POST', "pay_txhd_ios"++_, Req, _DocRoot) ->
    pay_mod_txhd_ios:pay_gold(Req);
handle('GET', "pay360"++_, Req, _DocRoot) ->
    pay_mod_360:pay_gold(Req);
handle('GET', "paytbt"++_, Req, _DocRoot) ->
    pay_mod_tbt:pay_gold(Req);
handle('POST', "paywdjnew"++_, Req, _DocRoot) ->
    pay_mod_wdj_new:pay_gold(Req);
handle('POST', "paywdj"++_, Req, _DocRoot) ->
    pay_mod_wdj:pay_gold(Req);
handle('POST', "paypp"++_, Req, _DocRoot) ->
    pay_mod_pp:pay_gold(Req);
handle('POST', "paykyand"++_, Req, _DocRoot) ->
    pay_mod_kyand:pay_gold(Req);
handle('POST', "payky"++_, Req, _DocRoot) ->
    pay_mod_ky:pay_gold(Req);
handle('POST', "payhw"++_, Req, _DocRoot) ->
    pay_mod_hw:pay_gold(Req);
handle('POST', "payit"++_, Req, _DocRoot) ->
    pay_mod_it:pay_gold(Req);
handle('GET', "pay91"++_, Req, _DocRoot) ->
    pay_mod_91:pay_gold(Req);
handle('GET', "payzd"++_, Req, _DocRoot) ->
    pay_mod_zd:pay_gold(Req);
handle('GET',"pay_91_new"++_,Req,_DocRoot) ->
	pay_mod_91_new:pay_gold(Req);
handle('GET', "pay_91_sgz15_ios"++_, Req, _DocRoot)->
    pay_mod_91_sgz15_ios:pay_gold(Req);
handle('GET', "pay_91_sgz15_ard"++_, Req, _DocRoot)->
    pay_mod_91_sgz15_ard:pay_gold(Req);
handle('POST',"payucnew"++_,Req,_DocRoot) ->
    pay_mod_uc_new:pay_gold(Req);
handle('POST',"payuc"++_,Req,_DocRoot) ->
    pay_mod_uc:pay_gold(Req);
handle('GET',"paydk"++_,Req,_DocRoot) ->
    pay_mod_dk:pay_gold(Req);
handle('GET',"pay_dk_new"++_,Req,_DocRoot) ->
	pay_mod_dk_new:pay_gold(Req);
handle('GET',"paydl"++_,Req, _DocRoot) ->
    pay_mod_dl:pay_gold(Req);
handle('GET',"paymi"++_,Req, _DocRoot) ->
    pay_mod_mi:pay_gold(Req);
handle('GET',"paysina"++_,Req, _DocRoot) ->
    pay_mod_sina:pay_gold(Req);
handle('GET',"pay_mzw"++_,Req, _DocRoot) ->
    pay_mod_mzw:pay_gold(Req);
handle('GET',"paymz2"++_,Req,_DocRoot) ->
	pay_mod_mz2:pay_gold(Req);
handle('GET', "paymz"++_, Req, _DocRoot) ->
    pay_mod_mz:pay_gold(Req);
handle('GET', "pay37wan"++_, Req, _DocRoot) ->
    pay_mod_37wan:pay_gold(Req);
handle('GET', "payks"++_, Req, _DocRoot) ->
    pay_mod_ks:pay_gold(Req);
handle('GET', "payi4"++_, Req, _DocRoot) ->
    pay_mod_i4:pay_gold(Req);
handle('GET', "payyk"++_, Req, _DocRoot) ->
    pay_mod_yk:pay_gold(Req);
handle('GET', "pay_pps"++_, Req, _DocRoot) ->
    pay_mod_pps:pay_gold(Req);
handle('GET', "pay_4399"++_, Req, _DocRoot) ->
    pay_mod_4399:pay_gold(Req);
handle('GET', "pay_cw"++_, Req, _DocRoot) ->
    pay_mod_cw:pay_gold(Req);
handle('GET', "pay_ld"++_, Req, _DocRoot) ->
    pay_mod_ld:pay_gold(Req);
handle('GET',"paylenovo4"++_,Req, _DocRoot) ->
    pay_mod_lenovo4:pay_gold(Req);
handle('GET',"paylenovo3"++_,Req, _DocRoot) ->
    pay_mod_lenovo3:pay_gold(Req);
handle('GET',"paylenovo2"++_,Req, _DocRoot) ->
    pay_mod_lenovo2:pay_gold(Req);
handle('GET',"paylenovo"++_,Req, _DocRoot) ->
    pay_mod_lenovo:pay_gold(Req);
handle('GET',"pay_lenovo_push"++_,Req, _DocRoot) ->
    pay_mod_lenovo_push:pay_gold(Req);
handle('POST',"payoppo"++_,Req, _DocRoot) ->
    pay_mod_oppo:pay_gold(Req);
handle('POST',"paysogou"++_,Req, _DocRoot) ->
    pay_mod_sogou:pay_gold(Req);
handle('POST',"payjf"++_,Req, _DocRoot) ->
    pay_mod_jf:pay_gold(Req);
handle('GET',"payyyh"++_,Req, _DocRoot) ->
    pay_mod_yyh:pay_gold(Req);
handle('GET',"payyyg2"++_,Req, _DocRoot) ->
    pay_mod_yyg2:pay_gold(Req);
handle('GET',"payyyg"++_,Req, _DocRoot) ->
    pay_mod_yyg:pay_gold(Req);
handle('POST',"payvivo"++_,Req, _DocRoot) ->
    pay_mod_vivo:pay_gold(Req);
handle('POST',"payaz"++_,Req, _DocRoot) ->
    pay_mod_az:pay_gold(Req);
handle('POST', "paydj"++_, Req, _DocRoot) ->
    pay_mod_dj:pay_gold(Req);
handle('GET',"payouw"++_,Req, _DocRoot) ->
    pay_mod_ouw:pay_gold(Req);
handle('POST', "payjl"++_, Req, _DocRoot) ->
    pay_mod_jl:pay_gold(Req);
handle('POST', "payxy"++_, Req, _DocRoot) ->
    pay_mod_xy:pay_gold(Req);
handle('POST', "paygg"++_, Req, _DocRoot) ->
    pay_mod_gg:pay_gold(Req);
handle('GET',"paykw"++_,Req,_DocRoot) ->
    pay_mod_kw:pay_gold(Req);
handle('GET',"payyyb1"++_,Req, _DocRoot) ->
    pay_mod_yyb1:pay_gold(Req,'GET',"/payyyb1");
handle('GET',"pay_yyb_yh3"++_,Req, _DocRoot) ->
    pay_mod_yyb_yh3:pay_gold(Req);
handle('GET',"payyyb3"++_,Req, _DocRoot) ->
    pay_mod_yyb3:pay_gold(Req);
handle('GET',"payyyb"++_,Req, _DocRoot) ->
    pay_mod_yyb:pay_gold(Req,'GET',"/payyyb");
handle('GET', "pay51cm"++_, Req, _DocRoot) ->
	pay_mod_51cm:pay_gold(Req);
handle('GET', "payuu"++_, Req, _DocRoot) ->
    pay_mod_uu:pay_gold(Req);
handle('GET', "pay_qtld_ard"++_, Req, _DocRoot) ->
    pay_mod_qtld_ard:pay_gold(Req);
handle('GET', "pay_qtld_chk_ard"++_, Req, _DocRoot) ->
    pay_mod_qtld_chk_ard:pay_gold(Req);
handle('GET', "pay_qtld_ios"++_, Req, _DocRoot) ->
    pay_mod_qtld_ios:pay_gold(Req);
handle('GET', "paymmy"++_, Req, _DocRoot) ->
	pay_mod_mmy:pay_gold(Req);
handle('GET',"payhm"++_,Req, _DocRoot) ->
	pay_mod_hm:pay_gold(Req);
handle('GET', "pay_pptv"++_, Req, _DocRoot) ->
    pay_mod_pptv:pay_gold(Req);
handle('GET', "paylytx"++_, Req, _DocRoot) ->
    pay_mod_lytx:pay_gold(Req);
handle('POST',"pay3gmh"++_, Req,_DocRoot) ->
    pay_mod_3gmh:pay_gold(Req);
handle('POST', "paymm2"++_,Req,_DocRoot) ->
    pay_mod_mm2:pay_gold(Req);
handle('POST', "paymm"++_,Req,_DocRoot) ->
    pay_mod_mm:pay_gold(Req);
handle('POST', "payaigame"++_,Req,_DocRoot) ->
    pay_mod_aigame:pay_gold(Req);
handle('POST', "pay_aigame2"++_,Req,_DocRoot) ->
    pay_mod_aigame2:pay_gold(Req);
handle('POST', "pay_alipay"++_,Req,_DocRoot) ->
    pay_mod_alipay:pay_gold(Req);
handle('POST', "paylt"++_,Req,_DocRoot) ->
    pay_mod_lt:pay_gold(Req);
handle('GET', "pay_iiapp"++_,Req,_DocRoot) ->
    pay_mod_iiapp:pay_gold(Req);
handle('GET', "payck"++_, Req,_DocRoot) ->
	pay_mod_ck:pay_gold(Req);
handle('GET', "pay49you"++_,Req,_DocRoot) ->
    pay_mod_49you:pay_gold(Req);
handle('GET', "payxxgp"++_,Req,_DocRoot) ->
    pay_mod_xxgp:pay_gold(Req);
handle('GET', "paycgcg"++_,Req,_DocRoot) ->
	pay_mod_cgcg:pay_gold(Req);
handle('GET', "payquickios"++_,Req,_DocRoot) ->
    pay_mod_quick_ios:pay_gold(Req);
handle('GET', "payquick"++_,Req,_DocRoot) ->
    pay_mod_quick:pay_gold(Req);
handle('GET', "payshandou"++_,Req,_DocRoot) ->
    pay_mod_shandou:pay_gold(Req);
handle('GET', "payliebao"++_,Req,_DocRoot) ->
    pay_mod_liebao:pay_gold(Req);
handle('GET', "pay_kaopu"++_,Req,_DocRoot) ->
    pay_mod_kaopu:pay_gold(Req);
handle('GET', "paybaofeng"++_,Req,_DocRoot) ->
    pay_mod_baofeng:pay_gold(Req);
handle('POST',"paykf"++_,Req,_DocRoot) ->
	pay_mod_kf:pay_gold(Req);
handle('POST',"paypyw"++_,Req,_DocRoot) ->
	pay_mod_pyw:pay_gold(Req);
handle('POST',"payttios"++_,Req,_DocRoot) ->
    pay_mod_ttios:pay_gold(Req);
handle('POST',"paytt"++_,Req,_DocRoot) ->
	pay_mod_tt:pay_gold(Req);
handle('GET',"pay17173"++_,Req,_DocRoot) ->
	pay_mod_17173:pay_gold(Req);
handle('GET',"paycaohua2"++_,Req,_DocRoot) ->
    pay_mod_caohua2:pay_gold(Req);
handle('GET',"paycaohua"++_,Req,_DocRoot) ->
    pay_mod_caohua:pay_gold(Req);
handle('GET',"payyoutu"++_,Req,_DocRoot) ->
    pay_mod_youtu:pay_gold(Req);
handle('GET',"payleyou"++_,Req,_DocRoot) ->
    pay_mod_leyou:pay_gold(Req);
handle('GET',"pay_6kwan"++_,Req,_DocRoot) ->
    pay_mod_6kwan:pay_gold(Req);
handle('POST',"payyiyang"++_,Req,_DocRoot) ->
	pay_mod_yiyang:pay_gold(Req);
handle('GET',"payxmw_ard"++_,Req,_DocRoot) ->
	pay_mod_xmw_ard:pay_gold(Req);
handle('GET',"payxmw"++_,Req,_DocRoot) ->
	pay_mod_xmw:pay_gold(Req);
handle('GET',"payx7"++_,Req,_DocRoot) ->
    pay_mod_x7:pay_gold(Req);
handle('GET', Path, Req, DocRoot)   ->
    Req:serve_file(Path, DocRoot).

-module(gmail).
-compile([export_all]).


send2mccq(Title, Content) ->
    send_gmail(Title, Content, "mccq.mainland@gmail.com", "mccqproblemlog").

send_gmail(TitleRaw, ContentRaw, Account, PassWord)  when is_list(Account) andalso is_list(PassWord) ->
    [begin ensure_started(App), ok end || App<-[crypto, public_key, ssl] ],
    Title = io_lib:format("~p", [TitleRaw]),
    Content = io_lib:format("~p", [ContentRaw]),
    {ok, Socket} = ssl:connect("smtp.gmail.com", 465,[{active,false}],100000),
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode(Account))),
    send(Socket, binary_to_list(base64:encode(PassWord))),                                                                                                
    send(Socket, "MAIL FROM: <" ++ Account ++ ">"),
    send(Socket, "RCPT TO:<" ++  Account ++ ">"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: <"++ Account ++ ">"),
    send_no_receive(Socket, "To: <" ++ Account ++ ">"),
    send_no_receive(Socket, "Data:Tue, 15 Jan 2008 16:02:43 +0000"),
    send_no_receive(Socket, "Subject: " ++ Title),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Content),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").

recv(Socket) ->
    case ssl:recv(Socket, 0, 10000) of
        {ok, _Return} ->
            %io:format("~p~n",[Return]);
            ok;
        {error, _Reason}->
           % io:format("ERROR: ~p~n",[Reason])
            error
    end.

send(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

ensure_started(App) ->
    case application:start(App) of
        {error, {not_started, AppRelyOn}} ->
            ensure_started(AppRelyOn),
            ensure_started(App);
        _ ->
            ok
    end.
            

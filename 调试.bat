#:: 修改为脚本自动切换到当前目录
#:: 免得每次都要自己修改路径
cd /d "%~dp0"
start werl -pa ebin config config/app deps/ebin setting  -config config/app/app -mnesia dir data -name dev237@10.10.13.237 -setcookie crimoon

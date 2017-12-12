ulimit -c unlimited
ulimit -SHn 5120
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name a@127.0.0.1 -pa ebin deps/ebin config config/app setting
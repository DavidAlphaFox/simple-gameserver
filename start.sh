#!/bin/bash
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name a@127.0.0.1 -setcookie tkserver1 -pa ebin deps/ebin config config/app setting -s tk start -detached

#!/bin/bash
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name robot@127.0.0.1 -setcookie crimoon -pa ebin deps/ebin config config/app setting 

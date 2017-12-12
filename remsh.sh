#!/bin/bash
erl -name attach@127.0.0.1 -setcookie tkserver1 -pa ebin deps/ebin config config/app setting -remsh a@127.0.0.1

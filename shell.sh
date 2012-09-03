#!/bin/sh
rebar compile skip_deps=true && erl -pa `pwd`/ebin `pwd`/deps/*/ebin -boot start_sasl

@ECHO off
erl -make
erl -pa ebin -eval "app:options()."
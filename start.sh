#!/bin/bash
erl -make
erl -pa ebin -eval "app:options()."
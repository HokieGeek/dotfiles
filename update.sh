#!/bin/sh

git fetch --all
git pull origin master
git submodule foreach --recursive 'git fetch --all && git pull origin master'

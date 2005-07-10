#!/bin/bash
cd $(dirname $0)
for xx in cop robber shared bot-driver ; do (cd src/$xx; make clean); done

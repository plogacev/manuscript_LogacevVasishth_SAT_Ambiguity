#!/bin/bash
SUBJ=2007

cd `dirname $0`

# 1152, 864
RUN="/usr/bin/python2.7 RTExp/runSAT.py -j $SUBJ Experimente"
$RUN Intro
$RUN RaceArg_Block1b
$RUN RaceArg_Block2b
$RUN RaceArg_Block3b
$RUN RaceArg_Block4b

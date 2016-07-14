#!/bin/bash
SUBJ=2011

cd `dirname $0`

# 1152, 864
RUN="/usr/bin/python2.7 RTExp/runSAT.py -j $SUBJ Experimente"
$RUN Intro
$RUN RaceArg_rev_Block4b
$RUN RaceArg_rev_Block3b
$RUN RaceArg_rev_Block2b
$RUN RaceArg_rev_Block1b

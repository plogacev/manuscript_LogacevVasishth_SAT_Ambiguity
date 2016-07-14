#!/bin/bash
SUBJ=2110

cd `dirname $0`

# 1152, 864
RUN="/usr/bin/python2.7 RTExp/runSAT.py -j $SUBJ Experimente"
$RUN Intro
$RUN RaceArg_rev_Block4a
$RUN RaceArg_rev_Block3a
$RUN RaceArg_rev_Block2a
$RUN RaceArg_rev_Block1a

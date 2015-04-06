#!/bin/bash

LATEX="--callpdflatex"
LATEX=""

TP_OPTS="--withSeparator"
TP_OPTS="--daysPerWeek 7 --withOverview"

TODOS="anniversaries.txt appointments2012.txt"

sbt "run $LATEX $TP_OPTS -d -i timeplanDemo.txt -o timeplanDemo.tex $TODOS"

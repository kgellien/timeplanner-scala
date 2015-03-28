#!/bin/bash

TP_OPTS="--withSeparator"
TP_OPTS="--daysPerWeek 5 --withOverview"

TODOS="anniversaries.txt appointments2012.txt"

sbt "run --callpdflatex $TP_OPTS -d -i timeplanDemo.txt -o timeplanDemo.tex $TODOS"

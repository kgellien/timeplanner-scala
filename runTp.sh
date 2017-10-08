#!/bin/bash

TIMEPLAN=timeplanDemo

LATEX="--callpdflatex"
LATEX=""

TP_OPTS="--withSeparator"
TP_OPTS="--withAdditionalTasks --daysPerWeek 7 --withOverview"
TP_OPTS="--daysPerWeek 7 --withOverview"

TODOS="anniversaries.txt appointments2012.txt"

sbt "run $LATEX $TP_OPTS -d -i $TIMEPLAN.txt -o $TIMEPLAN.tex $TODOS"
pdflatex $TIMEPLAN.tex
pdflatex tp.tex

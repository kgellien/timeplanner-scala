#!/bin/bash

TIMEPLAN=timeplanDemo
DAYPLAN=$TIMEPLAN-dp

LATEX="--callpdflatex"
LATEX=""

TP_OPTS="--withSeparator"
TP_OPTS="--withAdditionalTasks --daysPerWeek 7 --dpconfig dayplan-regular.properties --withOverview"
TP_OPTS="--daysPerWeek 7 --withOverview"

TODOS="anniversaries.txt appointments2012.txt"

OUTPUT="--dayplanoutput $DAYPLAN.tex --timeplanoutput $TIMEPLAN.tex"

sbt "run $LATEX $TP_OPTS -d -i $TIMEPLAN.txt $OUTPUT $TODOS"
pdflatex $TIMEPLAN.tex
pdflatex $DAYPLAN.tex

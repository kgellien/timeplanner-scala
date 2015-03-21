#!/bin/bash

TP_OPTS="--withSeparator"
TP_OPTS=""

TODOS="anniversaries.txt appointments2012.txt"

sbt "run --callpdflatex $TP_OPTS -d -i timeplanDemo.txt -o timeplanDemo.tex $TODOS"

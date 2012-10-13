= Timeplanner

Due to the fact that I work on different computers (Win XP / 7, Linux, OS X) inside and outside of a corporate setting, I need a timeplanner which I can use on all these platforms and which provides the possibility to print out timeplans in different granularities.

I started with a simple Python project, about 400 LoC, which had the main functionality I needed.
Subsequently I improved on this and ported a later version to Scala.

Here, the Scala version is distributed.

Both projects can read from multiple input files which will be consolidated into one internal datastructure.

There I differentiate between tasks (keyed by date without associated time), appointment (date with time) and anniversaries (date without year).


Look at anniversaries.txt and appointments2012.txt for example input, whereas timeplan.txt defines which specific plans should be generated.

In pom.xml under launcher use of the commandline is exemplified.

Call mvn scala:run to call this launcher. The above mentioned configuration files will be taken to generate timeplan.pdf


This project expects a functioning (La)TeX installed (pdflatex is called internally).
For this call seemingly the full path to pdflatex is needed. To adjust your personal setting without the need to
change something in the checked in files, copy timeplan.properties.template to timeplan.properties which is listed in .hgignore and change the entries
according to your needs.

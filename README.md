# Timeplanner


Due to the fact that I work on different computers (Win XP / 7, Linux, OS X) inside and outside of a corporate setting,
I need a timeplanner which I can use on all these platforms and which provides the possibility to print out timeplans in different granularities.

I started with a simple Python project, about 400 LoC, which had the main functionality I needed.
Subsequently I improved on this and ported a later version to Scala.

Here, the Scala version is distributed.

Both projects can read from multiple input files which will be consolidated into one internal datastructure.

There I differentiate between tasks (keyed by date without associated time), appointment (date with time) and anniversaries (date without year).

This distinction is not thought through properly.
Currently, everything with either time or date before the info text of an entry is considered an appointment.

In the generated PDF the appointments start at the top, whereas the tasks go to the bottom.

Look at anniversaries.txt and appointments2012.txt for example input, whereas timeplan.txt defines which specific plans should be generated.

Call ./runTp.sh to create example timeplan via SBT.

This project expects a functioning (La)TeX installed (pdflatex is called internally).
For this call seemingly the full path to pdflatex is needed. To adjust your personal setting without the need to
change something in the checked in files, copy `timeplan.properties.template` to `timeplan.properties` which is listed in .hgignore / .gitignore and change the entries
according to your needs.

## Dates and Times

Dates in the first column are basically in ISO-format, i.e. yyyy-MM-dd.
Dates inside an appointment are according to german usage, i.e. dd.(MM.(yyyy))

## Appointments/Tasks

It is possible to enter a date or a date range, e.g.

    2012-W16 [Business] 16. - 17.04. weekly appointment

or

    2012-W16 [Business] 18.04. weekly appointment

To make interpretation easier, in the current version at least one month is required. 

## Prerequisites

- SBT installed (>=0.13.5)
- pdflatex


## Known Problems / Errors

- call to pdflatex sometimes does not return; CTRL-C and explicit call afterwards does work
- Option --withAdditionalTasks currently broken

## To Do

- Make it easy to provide ones own inputfiles *outside* of this project directory
  (see runTp.sh)
- Internationalisation
- use logging instead of println
- try template engine (e.g. scalate) for latex output (DONE: for now not worth the effort)
- set (configurabe) amount of different classifiers displayed in overview; collapse single entries into miscellaneous?
- remove absolute paths
- Documentation!
- make use of ISO8601 calender consistent
- find a way to distinguish between birthdays and anniversaries
- Refactor LatexWeekSchedule; this might constitute a use case for a template engine
- Externalize corresponding configs
- Internationalization
- latin1 vs. utf8 should be configurable
- new week plans and day plans should be configurable
- clean-up generated LaTeX (too many Over-/Underfull \vboxes )

## SBT assembly

can create a fat jar via sbt assembly plugin

```bash
sbt assembly
```

## Revision History


### 0.8.1-SNAPSHOT

- [x] replace joda.time with java.time
- [ ] complete verification of getWeekyear
- [ ] Localisation via comandline parameter
- [ ] update to current SBT (1.3.0)
- [ ] update to Scala 2.13
- [x] Dayplan: change KW to W
- [ ] Localise -ws, -ws24 and -ww too

### 0.8.0

- Added three new week plans (working, but very ad-hoc)
- LaTeX clean up started; try to use same basic structure for all pages

### 0.7.9-SNAPSHOT

 - Update to Scala 2.12 and SBT 1.0 (not yet fully; on my old Mac without Java 8 I can't update yet)
 - Added DayPlans (single entry possible; per WeekPlan automatically daysPerWeek plans will be generated)
 - via 'sbt assembly' you can now generate a fat jar

### 0.7.8-SNAPSHOT

 - removed Scalatra-Template-Engine
 - timespan now accepts single - too
 - single - in timeInfo will be expanded to -- for LaTeX
 - datespan might now just be one date
 - Birthdays now centered between Appointments and Tasks
 - Tasks and Birthdays now also sorted
 - no LaTeX specific code outside of LatexTimePlan
 - --withAdditionalTasks works for Weekly Appointment to Daily Tasks
 - experimental --withAdditionalTasks: Weekly Appointments to additional Daily Appointments
 - appointments sorted
 - special days (e.g. X2012-05-01 Labour Day) introduced


### 0.7.7

 - further experimentation with templating (stopped, because working with different template files was more inconvenient than staying with LatexTimePlan)
 - Tasks now with checkbox
 - some clean-up (Maven-Support removed)


### 0.7.6

 - refactoring (extraction of constants; preparation for cleanup)
 - Week KW-%02d -> W%02d
 - runTp.sh added to demonstrate call via SBT
 - new parameter daysPerWeek, so that instead of 7-day-week e.g. a 5 or 6 day work week is possible
 - Format for timeplan.txt changed: Year, Quarter, Month, Week now analogous to the Task/Appointment entries
 - --withOverview now only as global Option; at the moment not settable at the PeriodPlan level
 - structure made simpler and more regular
 - classifier introduced; these are used for headings in overview


### 0.7.5

## Maven (not any longer actively supported)

In `pom.xml` under launcher use of the commandline is exemplified.

Call `mvn scala:run` to call this launcher. The above mentioned configuration files will be taken to generate timeplan.pdf

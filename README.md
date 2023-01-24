# Choreographic Extended Team Automata

This is a branch of Choreo that focuses on the analysis of choreographies with multiple synchronisation agents, and its projection into team automata.


## How to compile local javascript

This requires JVM and SBT to be installed.

* Pull the git submodules:

> git submodule update --init

* Compile with sbt:

> sbt fastLinkJS

* Run the compiled program by opening the webpage:

> open lib/caos/tool/index.html
> 
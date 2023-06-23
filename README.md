# Choreographic Extended Team Automata

This is a branch of Choreo that focuses on the analysis of choreographies with multiple synchronisation agents, seen as Team Automata. It includes two different approaches to synthesise local views of its the choreography's behaviour, and to detect some cases when these do not exist. A snapshot of this tool can be executed from the link below.

 - http://lmf.di.uminho.pt/ceta


## How to compile local javascript

This requires JVM and SBT to be installed.

* Pull the git submodules:

> git submodule update --init

* Compile with sbt:

> sbt fastLinkJS

* Run the compiled program by opening the webpage:

> open lib/caos/tool/index.html
> 

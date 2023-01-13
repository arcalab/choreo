# Branching Pomsets

This is a branch of Choreo that focuses on the encoding of choreographies into branching pomsets, here also called nested pomsets (NPomsets) for historical reasons.
These analysis of branching pomsets include, among others:

- Visualiser of the encoded pomset (from a choreography);

- Simulator of the reduction rules for branching pomsets, either step-by-step or using a final state machine;

- Verifier of _realisability_, by checking if the given branching pomset is bisimilar to the composed branching pomsets of the view of each participant;

- Verifier of _well-formedness_, which are (sound but incomplete) properties over the graph structure that guarantee realisability, and less complex than verifying bisimilarity.



## Running the tool

This tool is compiled into a stand-alone JavaScript file that can be used from an internet browser. A snapshot is available at:
 
 - http://arcalab.github.io/b-pomset

The input branching pomset is specified in the _Choreograpy_ widget, using the choreographic language Choreo. Many examples are available to illustrate this syntax. Some branching pomsets cannot be specified by choreographies, hence we extended the language to explicitly specify events and causality dependencies; see e.g. example `Non-choreo`.



## How to compile local javascript

This requires JVM and SBT to be installed.

* Pull the git submodules:

> git submodule update --init

* Compile with sbt:

> sbt fastLinkJS

* Run the compiled program by opening the webpage:

> open lib/caos/tool/index.html
> 
# citealign

## What it is

`citealign` is a library for alignments among citable texts in the CITE Environment.

## Use Cases

1. Given a passage (a line, a range of lines, a book) of a poem in the original language of composition, find passages of prose translations that align with it. Do so even if the translation's scheme of citation is radically different (*e.g.* a prose translation of a poem).

1. (Potentially) dis-contiguous alignments of tokens for translation-alignment. (As with the tool http://ugarit.ialigner.com.)

## Overview

This library simply organizes and creates methods for compositions of the basic primitive data-types modeled by the [CITE Architecture](http://cite-architecture.org): texts, collections, and relations. There can be a Collection of "alignment-objects". Each of these is citable with a [Cite2 URN](http://cite-architecture.org/cite2urn/). Passages of text, or textual tokens, are part of [OHCO2 Texts](http://cite-architecture.org/ohco2/), and are citable with [CTS URNs](http://cite-architecture.org/ctsurn/). Text and Relation-objects are associated with [CITE Relations](https://cite-architecture.github.io/citerelations/).

## Current version: 0.4.1

Status: active development.  See [release notes](releases.md)

## License

[GPL 3.0](https://opensource.org/licenses/gpl-3.0.html)


## Documentation

TBD

## Using, building, testing

`citealign` is compiled for both the JVM and ScalaJS using scala versions 2.11 and 2.12.  

Binaries for all platforms are available from jcenter.

If you are using sbt, include Resolver.jcenterRepo in your list of resolvers

    resolvers += Resolver.jcenterRepo

and add this to your library dependencies:

    "edu.furman.classics" %%% "citealign" % VERSION

For maven, ivy or gradle equivalents, refer to https://bintray.com/eumaeus/maven/citealign.

To build from source and test, use normal sbt commands (`compile`, `test` ...).

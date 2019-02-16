# citealign

## What it is

`citealign` is a library for alignments among citable texts in the CITE Environment.

## Use Cases

1. Given a passage of a poem in the original language of composition, find passages of prose translations that align with it.

1. (Potentially) dis-contiguous alignments of tokens for translation-alignment. (As with the tool http://ugarit.ialigner.com.)

## Current version: 0.1.0

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

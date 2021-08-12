# SRTV-test-tweet-collection

## Tweet Examples

This is a collection of JSON examples used for testing in the [SRTV repository](https://github.com/ZavierHenry/SRTV). Each example follows the JSON schema outlined in [schema.json](schema.json). All of the examples can be found in the _tweets_ directory.

## Examples Map

The path to every example relative to the tweets directory can be found in [exampleFilepaths.txt](exampleFilepaths.txt). This file is used by the SRTV tests and is modified automatically by a GitHub action bot whenever there is a change in the tweets directory.

## Type Provider Samples

This repository also contains samples that are used as a source for the [F# JSON Type Provider](http://fsprojects.github.io/FSharp.Data/library/JsonProvider.html), which is used in the SRTV tests. The samples should follow the property types that are in the schema file. This repository also contains a F# script that can generate samples given a schema file. To run the script, run the following command:

``dotnet fsi scripts/SampleGenerator.fsx <schema>``
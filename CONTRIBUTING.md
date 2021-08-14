Contributing To This Repository
========================================

Contributions to this test repository are welcome. There are just a couple of things to keep in mind when doing so.

## Consistent examples

Example tweets should reside in the _tweets_ directory and generally follow the structure in the directory (e.g. the _punctuation_ directory should include tweets about punctuation and the _emojis_ directory should include tweets with emojis).

## Validation of examples

Example tweets should follow the schema in [schema.json](schema.json). There is a GitHub action that validates examples in any pull request to the main branch that contains examples to ensure this.

## Changing the Schema

If your contribution to this repository changes the schema file [schema.json](schema.json), be sure that your changes are reflected in

- All of the examples in the tweets directory
- The type provider samples file [samples.json](samples.json)

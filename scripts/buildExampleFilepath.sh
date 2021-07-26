#!/bin/bash
FILENAME='exampleFilepaths.txt'
TWEET_DIRECTORY="tweets/"

find "$TWEET_DIRECTORY" -type f -name '*.json' | sed "s#$TWEET_DIRECTORY##"
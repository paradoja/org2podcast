[![Haskell CI](https://github.com/paradoja/org2podcast/actions/workflows/haskell.yml/badge.svg)](https://github.com/paradoja/org2podcast/actions/workflows/haskell.yml)

# org2podcast

*org2podcast* is a command-line application that gets an org-mode file with a format like this:

``` org
#+RSS_TITLE: RSS title
#+EMAIL: me@example.net
#+DESCRIPTION: Nice podcast description
#+FILE_LOCATION: /files
#+RSS_FEED_URL: https://podcast.example.net/feed.xml
#+RSS_IMAGE_URL: https://podcast.example.net/icon.png
#+RSS_FILES_URL: https://podcast.example.net/files/

* Title of a nice Podcast entry
:PROPERTIES:
:PUBDATE:  <2024-02-03 Sa 11:31>
:MEDIA:    podcast-episode.mp3
:END:

Some description to be shown.

* Another podcast episode
:PROPERTIES:
:PUBDATE:  <2024-02-01 Do 07:00>
:MEDIA:    another-audio-file.mp3
:END:
This is another descripition.

** Subsection
Something else
```

And builds an atom feed for the podcast.

# Usage

``` sh
org2podcast ORGFILE.org [ATOMFEED.xml]
```

*org2podcast* receives the org file as first parameter. If a second parameter is given the atom feed is written there. Otherwise, it will be printed in *stdout*.

# Development

*org2podcast* is built with Haskell. Uses [*stack*](https://docs.haskellstack.org/) for building, and *HSpec* for testing.

- Building:
``` sh
stack build
```
- Testing:
``` sh
stack test
```

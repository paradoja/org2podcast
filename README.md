[![Haskell CI](https://github.com/paradoja/org2podcast/actions/workflows/haskell.yml/badge.svg)](https://github.com/paradoja/org2podcast/actions/workflows/haskell.yml)

# org2podcast

*org2podcast* is a command-line application that gets an org-mode file with a format like this:

``` org
#+RSS_TITLE: RSS title
#+EMAIL: me@example.net
#+DESCRIPTION: Nice podcast description
#+FILE_LOCATION: files
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

And builds an atom feed for the podcast (this example shown at the end).

For now, relies on having a *file* executable on the path that can give the *MIME* of a file (for the audio files).

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

# Example Feed Result

From the previous org file a XML like this is produced:

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
    <title type="text">
        RSS title
    </title>
    <id>
        https://podcast.example.net/feed.xml
    </id>
    <updated>
        2024-02-10T14:33:40.589797488Z
    </updated>
    <icon>
        https://podcast.example.net/icon.png
    </icon>
    <subtitle type="text">
        Nice podcast description
    </subtitle>
    <entry>
        <id>
            urn:sha1:b75e2b1778aa832483ef2190cb0b4b68a8eb072a
        </id>
        <title type="text">
            Title of a nice Podcast entry
        </title>
        <updated>
            2024-02-03T11:31:00Z
        </updated>
        <author>
            <name>
            </name>
            <email>
                me@example.net
            </email>
        </author>
        <content type="html">
            &lt;p&gt;Some description to be shown.&lt;/p&gt;
        </content>
        <link
          href="https://podcast.example.net/filestestFiles/t1"
          length="30"
          rel="enclosure"
          type="text/plain"/>
    </entry>
    <entry>
        <id>
            urn:sha1:8f0e917d605352cbb150315c8291f6a356e35f1a
        </id>
        <title type="text">
            Another podcast episode
        </title>
        <updated>
            2024-02-01T07:00:00Z
        </updated>
        <author>
            <name>
            </name>
            <email>
                me@example.net
            </email>
        </author>
        <content type="html">
            &lt;p&gt;This is another descripition.&lt;/p&gt;&lt;h2 id="org558c12"&gt;Subsection&lt;/h2&gt;&lt;p&gt;Something else&lt;/p&gt;
        </content>
        <link
          href="https://podcast.example.net/filestestFiles/t2.xml"
          length="90"
          rel="enclosure"
          type="text/xml"/>
    </entry>
</feed>
```

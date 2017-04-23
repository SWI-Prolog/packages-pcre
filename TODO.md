PCRE interface

Useful operations:

  - [ECLiPSe](http://eclipseclp.org/doc/bips/lib/regex/index.html)
    - match    --> bool
    - match    --> matched string
    - matchall --> all matched strings
    - matchsub --> list of register values
    - split    --> matched strings and skipped strings

  - JavaScript
    - match    --> all matched strings
    - replace  --> replaced string
    - search   --> start of match

  - Bagnara
    - regexec(+Compiled, +String, +Pos, +EFlags, -Begin, -Length, -Groups)

Old discussion:

  - http://swi-prolog.996271.n3.nabble.com/Regex-introduction-td13588.html

TODO

  - Capture total match range anyway, so we do not need (?<match_R>...)

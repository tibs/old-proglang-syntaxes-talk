.. ==================================================================
.. (What may now seem) syntax oddities in older programming languages
.. ==================================================================


(What may now seem) syntax oddities in older programming languages
------------------------------------------------------------------

Some notes intended to accompany a talk on (what may now seem strange) syntax
in older programming languages.

...but it's OK to read them by themselves.


By Tibs / Tony Ibbs

Presented at CamPUG_, virtually, 1st September 2020

Written using reStructuredText_.

Converted to PDF slides using pandoc_ and beamer_.

Source and extended notes at https://github.com/tibs/old-proglang-syntaxes-talk

.. _reStructuredText: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _pandoc: https://pandoc.org
.. _beamer: https://github.com/josephwright/beamer

----

Introduction
------------

Only looking at "old" programming languages (remember, Python is from the
early 1990s, so old means "quite old"), because they may be
unfamiliar. Although, strangely, mostly still in use, in one form or another.

Also, only looking at things we'd call a "high level" programming language, so
ignoring things like Autocode (https://en.wikipedia.org/wiki/Autocode) which
is closer to an assembly language.

Ignoring lots of things I think you might already know - so nothing much on
functional languages (ML, Haskell, etc.).

Weighted heavily towards programming languages I know a little bit about, of
course.

----

Timeline?
---------

Maybe just for the notes..

==============  =========
FORTRAN         1954
FLOW-MATIC      1955-1959
LISP and Algol  1958
COBOL           1959
==============  =========

----

Plankalkül
----------

?do I want to mention this?

Designed by Konrad Zuse between 1942 and 1945. It was the first high-level
programming language to be designed for a computer. First implemented in 1975

https://en.wikipedia.org/wiki/Plankalk%C3%BCl

----

FORTRAN IV - an example
-----------------------

This is not valid FORTRAN...

.. code:: fortran

          PRINT 2000
          GOTO 1100
    1000  END
    1100  PRINT 2100
          GOTO 1000
    2000  6HHello
    2100  6HWorld!

----

FORTRAN IV - Leading spaces
---------------------------

The first 6 columns and their uses

----

FORTRAN IV - Computed GOTO
--------------------------


(if/then/else invented by Lisp?)

----

FORTRAN - Functions versus subroutines
--------------------------------------

Are those the right terms?

----

LISP - as we know it
--------------------

...

----

LISP as it was meant to be
--------------------------

The *original* syntax, that never seemed quote worth it

----

My father's parentheses
-----------------------

Franz Lisp (?) and the ``]``

----

COBOL
----

Do I have anything to say?

----

Snobol
------

Double check

``<expression>, <jump if T>, <jump if F>``

----

BCPL
----

``<becomes>`` - ``:=`` because ASCII doesn't have ``←``

  Hmm. Is that true? It's what I remmember, but https://en.wikipedia.org/wiki/EBCDIC
  doesn't seem to show that character.

Similarly, ``!=`` and variants because no ``≠`` (was that in EBCDIC?), and so on

Hmm. https://en.wikipedia.org/wiki/Talk:ALGOL_68 has

"""The American Standards Association (ASA, later to become ANSI) first
published ASCII as a standard in 1963. ASCII-1963 lacked the lowercase
letters, and had an up-arrow (↑) instead of the caret (^) and a left-arrow (←)
instead of the underscore (_)."""

So probably need to lost this slide - it's too confusing, and it appears my
memory is wrong.

----

BCPL
----

Also:

* ``$( .. )$``
* ``IF .. THEN`` and ``TEST .. THEN .. ELSE``

----

Distraction - character sets
----------------------------

* 6 bit, 7, bit, 8 bit, 9 bit characters
* ICL 6 bit - how to represent lower case characters
* ASCII
* EBCDIC
* ISO 646
* ISO 10646 and Unicode (let's not go into details)

and others...

----

ASCII versus EBCDIC
-------------------

Characters that are in one but not the other

* https://www.daytodaygk.com/ascii-vs-ebcdic/
* http://www.dynamoo.com/technical/ascii-ebcdic.htm (opinitionated!)
* https://en.wikipedia.org/wiki/EBCDIC/

----

APL - ancestor of R
-------------------

Give an example of APL versus R

----

Algol 68 - case stropping
-------------------------

Why this was needed.

Other ways of doing it (Algol 68 keywords in CAPS, ??? keywords in single
quotes)

(also, bold stropping in print)

----

Algol 68 - whitespace in variable names
---------------------------------------

.. code:: pascal

   Strictly speaking we do not need this temporary variable but
   the code is clearer if we have it = 3

----

Algol 68 - REFs
---------------

Explain

----

RPG
---

Compare to Snobol

Can I actually make a sensible example for this?

----

JCL - Job Control Language
--------------------------

IBM

----

Smalltalk
---------

Almost no syntax

----

Occam
-----

Signficant indentation!

----

Erlang and Prolog
-----------------

Full stop to end expressions/statements, not semicolon

----

Forth and stack based languages
-------------------------------

(maybe mention PostScript and thus also PDF)

----

Maybe TCL?
----------

Not sure

----

We do not talk about INTERCAL
-----------------------------

Not *really* a language people use

----

...

----

Interesting links
-----------------

Probably more for the notes than for the slides. Not necessarily entirely
pertinent to this exact topic...

* https://www.hillelwayne.com/post/influential-dead-languages/
  10 Most(ly dead) Influential Programming Languages, 2020-03-25, Hillel Wayne

* https://www.vidarholen.net/~vidar/An_Empirical_Investigation_into_Programming_Language_Syntax.pdf
  An Empirical Investigation into Programming Language Syntax, Andreas Stefik
  and Susanna Siebert, 2013

      Stefik, A. and Siebert, S. 2013. An empirical investigation into
      programming language syntax. *ACM Trans.Comput.Educ.* 13, 4, Article 19
      (November 2013), 40 pages.

  I haven't read this yet

* https://en.wikipedia.org/wiki/History_of_programming_languages

* https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(syntax)
  (perhaps too much information)

* http://www.99-bottles-of-beer.net doesn't seem to be working at the moment

* https://web.mit.edu/kenta/www/two/beer.html has Fortran IV, but the pages
  for each language are on ``.net`` and don't seem to work at the moment

* http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/ has Fortran IV
  and seems to work

* https://www.hillelwayne.com/equals-as-assignment/ Why Does "=" Mean
  Assignment? also by Hillel Wayne, from 2018
  

----

Fin
---

Written using reStructuredText_.

Converted to PDF slides using pandoc_ and beamer_.

Source and extended notes at https://github.com/tibs/old-proglang-syntaxes-talk

|cc-attr-sharealike|

This slideshow and its related files are released under a `Creative Commons
Attribution-ShareAlike 4.0 International License`_.

.. |cc-attr-sharealike| image:: images/cc-attribution-sharealike-88x31.png
   :alt: CC-Attribution-ShareAlike image

.. _`Creative Commons Attribution-ShareAlike 4.0 International License`: http://creativecommons.org/licenses/by-sa/4.0/

.. _CamPUG: https://www.meetup.com/CamPUG/
.. _reStructuredText: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _pandoc: https://pandoc.org
.. _beamer: https://github.com/josephwright/beamer

==================================================================
(What may now seem) syntax oddities in older programming languages
==================================================================

A talk on (what may now seem strange) syntax in older programming languages

Written as a talk for CamPUG_

Introduction
============

The intent of these notes is to show that language design is not
obvious. Those writing early programming languages had to design their
languages with relatively little prior art (indeed, in some cases, with no
prior art). This could lead to some choices that seem odd to us
now. Sometimes those choices were led by technical (hardware) limitations, and
sometimes they were an experiment.

It is also worth remembering that at the time, designing and implementing a
new programming language was a very large task. Certainly at the beginning,
the parsing techniques we are now used to did not exist. There wasn't even a
standard syntax for describing a programming language syntax.

All of the languages, except a couple of exceptions toward the end, are ones I
knew about in the 1980s. Most of them are chosen because I have some
familiarity with them, or consider them fascinating for one reason or
another. I'll explain why I've chosen each langauge when discussing it.

There are compilers or interpreters available for more-or-less all of them
still available, and in a good many cases, the language (or its direct
descendant or descendants) is still in use.

There are, of course, many other choices I could have made. If you're at all
interested, I give some links at the end that can start you off on your own
investigations.


.. Programming languages

   FORTRAN IV                     **
   LISP (S and M forms)           **
   COBOL (briefly)                **
   Snobol / Spitbol (probably)    **
   BCPL                           **
   APL                            **
   Algol 68                       ** for stropping at least
   RPG (maybe)
   Smalltalk                      ** ideally
   Occam (briefly)
   Prolog                         ** maybe
   Forth                          ** ideally
   Tcl (maybe)
   ABC                            -- just for appearance
   Python                         -- just for 99 bottles explanation

   That's really too many - even if I only spend 5 minutes on each (and
   FORTRAN IV is longer than that) then that suggests at most 12 for an hour
   speaking, and the above is 15.

   Keeping just the marked items gives me 12, which may be doable.

   Or, looking at it another way, no more than about 40 slides at most for
   60 minutes, depending on how long I talk around each slide.

FORTRAN IV
==========

https://en.wikipedia.org/wiki/Fortran

The first FORTRAN compiler was in 1957, and FORTRAN IV was released in 1962.

* FORTRAN arguable the first high level programming language
* However, I'm choosing FORTRAN IV (also sometimes know as FORTRAN 66 from its
  standardisation)
* Modern Fortran is no longer very much like this - it's continued to evolve
* ...why
* Punched cards, and the use of columns

  * comments
  * labels
  * continuation - marking that a line/card *continues*
  * sequence number

* Spaces in the code are *ignored* (so ``GOTO`` or ``GO TO`` and so on)
* No keywords
* ``IF (<expression>) GOTO <label> ... <label>``
* ``DO <label> <do-expression> ... <label> CONTINUE``
* ``IF (expression) <lt-label>, <eq-label>, <gt-label>``
* FUNCTIONS versus SUBROUTINES
* I remember 6 character names


.. image:: images/FortranCardPROJ039.agr.jpg
   :alt: Fortran punched card. Program text "Z(1) = Y + W(1)". Sequence number "PR0J039"

image source: https://en.wikipedia.org/wiki/Computer_programming_in_the_punched_card_era

LISP
====

https://en.wikipedia.org/wiki/Lisp_(programming_language)

LISP is one year yonger than FORTRAN, and was originally specified in 1958.

M-expressions were meant to be the form of the language, and are used in the
documentaion (for instance in the LISP 1.5 Programmer's Manual).

S-expressions were implemented first (?) and programmers took to them as the
preferred for.

For instance, ``car[cons[A,B]]`` is equivalent to ``(car (cons A B))``

Modern lisps abound, including Common Lisp and a whole host of Schemes.

Many people find S-expressions daunting, although Lisp programmers always
assume that their text editor will just take care of them. I recommend at
least learning a bit more about Lisp -- give a couple of useful references?

Interestingly, Franz Lisp recognised the problem of sometimes needing to type
many closing parentheses in sequence, and allowed the use of ``]`` to mean
"close all outstanding ``)``". I'm not sure how much that feature was used.

If-then-else
============

According to https://en.wikipedia.org/wiki/Lisp_(programming_language

"""A conditional using an if–then–else syntax was invented by McCarthy in a
Fortran context. He proposed its inclusion in ALGOL, but it was not made part
of the Algol 58 specification. For Lisp, McCarthy used the more general
cond-structure. Algol 60 took up if–then–else and popularized it."""

so Algol 60 got "if-then-else" and LISP got ``cond``, which is more like the
``case`` statement we're used to in other programming languages.


COBOL
=====

https://en.wikipedia.org/wiki/COBOL

COBOL was designed in 1959 and first standardised in 1968.

Both FORTRAN (FORmula TRANslation) and LISP (LISt Processing) were seen as
languages for mathematicians and engineers, and there was a need for a
programming language for use in business.

While this may seem strange now, it made a lot of sense at the time - remember
this was all new stuff.

This is where COBOL came in, and why it tried so hard to be more like English.

COBOL is also important because of its innovations on how to specify the
output of text.

    ((See if there's anything usful I can say about that))

(and this is something that people keep trying to reinvent, by the way, either
by making English like programming languages (look at `Inform 7`_ in the text
adventure space, or AppleScript) or by using graphical techniques (consider
all the visual programming languages such as Blockly_, Scratch_ and LabVIEW_))

https://en.wikipedia.org/wiki/Visual_programming_language

.. _`Inform 7`: http://inform7.com/
.. _Applescript: https://en.wikipedia.org/wiki/AppleScript
.. _Blockly: https://en.wikipedia.org/wiki/Visual_programming_language
.. _Scratch: https://en.wikipedia.org/wiki/Scratch_(programming_language)
.. _LabVIEW: https://en.wikipedia.org/wiki/LabVIEW

(paper__ from 2019, giving a good introduction to `Inform 7`_ and its history,
and also talking about the plans to open source it).

__ http://inform7.com/talks/2019/06/14/narrascope.html

SNOBOL4
=======

https://en.wikipedia.org/wiki/SNOBOL

SNOBOL was developed between 1962 and 1967 (SNOBOL4).

Introduced patterns as a first class datatype.

All SNOBOL command lines are of the form::

  <label> <subject> <pattern> = <object> : <transfer>

All parts are optional.

* The <subject> is matched against the <pattern>.
* If <object> is present, any matched portion of <subject> is replaced with <object>
* <transfer> is then an absolute or conditional branch (to a <label>.
* A conditional branch is dependent upon the success/failure of evaluating the
  <subject>, <object> and <pattern>, the pattern match or the final assignment
  (to the <subject>).

So, for instance:

.. code:: snobol

            OUTPUT = "What is your name?"
            Username = INPUT
            Username "J"                                             :S(LOVE)
            Username "K"                                             :S(HATE)
  MEH       OUTPUT = "Hi, " Username                                 :(END)
  LOVE      OUTPUT = "How nice to meet you, " Username               :(END)
  HATE      OUTPUT = "Oh. It's you, " Username
  END

BCPL
====

https://en.wikipedia.org/wiki/BCPL

BCPL was first implemented in 1967.

* Systems level language
* The book of the language includes all the source code for the compiler
* BCPL was the first "brace" programming language, although it historically used
  ``$( .. $)``.
* ``IF .. DO ..`` and ``TEST .. THEN .. ELSE ..``
* The only datatype is the ``word`` - size depends on the computer
* Semicolons separate commands, and a semicolon at the end of a line may be
  omitted. To make this work, infix expression operators (``+``, etc.) may
  never start a line.

  Or, in other words, a command carries on over multiple lines when it ends
  with a character (``+`` or ``,``, for instance) that implies as much.

* Labels are values, and one can do arithmetic on them
* An ancestor of C (CPL begat BCPL which begat B which begat C)

https://www.bell-labs.com/usr/dmr/www/bcpl.html - Martin Richards's BCPL Reference Manual, 1967

https://www.cl.cam.ac.uk/~mr10/bcplman.pdf - the BCPL user guide
from 2020. Note that the examples use ``{ .. }``.

How many bits to a character?
=============================

Well, likely 6, 7 or 8...

* ICL machines used 6 bit characters, and the first BCPL compiler used 6-bit
  BCD. That means you only get one case (so not upper and lowed) and need to
  have some way of "escaping" the other case.

* ASCII is a 7-bit code. In 8-bit bytes (octets), the 8th bit would be used as
  a parity check.

* EBCDIC is/was an 8 bit code.

Nowadays, of course, we have Unicode and UTF-8.
  
APL and related languages
=========================

APL and J
---------

* https://en.wikipedia.org/wiki/APL_(programming_language)
* https://en.wikipedia.org/wiki/J_(programming_language)

Initially designed as a language for thinking about problems, and described in
the book "A Programming Language" in 1962. It was used as a notation for
thinking about problems, such as describing computer systems.

The first use of an implementation using actual APL symbology was in 1966.

------

https://en.wikipedia.org/wiki/APL_(programming_language)#Mathematical_notation

  A mathematical notation for manipulating arrays was developed by
  Kenneth E. Iverson, starting in 1957 at Harvard University. In 1960, he
  began work for IBM where he developed this notation with Adin Falkoff and
  published it in his book A Programming Language in 1962.

Early implementations had to use English reserved words for functions and
operators.

https://en.wikipedia.org/wiki/APL_(programming_language)#Hardware

  A key development in the ability to use APL effectively, before the wide use
  of cathode ray tube (CRT) terminals, was the development of a special IBM
  Selectric typewriter interchangeable typing element with all the special APL
  characters on it. This was used on paper printing terminal workstations
  using the Selectric typewriter and typing element mechanism, such as the IBM
  1050 and IBM 2741 terminal. Keycaps could be placed over the normal keys to
  show which APL characters would be entered and typed when that key was
  struck. For the first time, a programmer could type in and see proper APL
  characters as used in Iverson's notation and not be forced to use awkward
  English keyword representations of them. Falkoff and Iverson had the special
  APL Selectric typing elements, 987 and 988, designed in late 1964, although
  no APL computer system was available to use them. Iverson cited Falkoff
  as the inspiration for the idea of using an IBM Selectric typing element for
  the APL character set.

  Many APL symbols, even with the APL characters on the Selectric typing
  element, still had to be typed in by over-striking two extant element
  characters. An example is the grade up character, which had to be made from
  a delta (shift-H) and a Sheffer stroke (shift-M). This was necessary because
  the APL character set was much larger than the 88 characters allowed on the
  typing element, even when letters were restricted to upper-case (capitals).

APL -> J, using ASCII with digraphs instead of special symbols (basically,
it adds dot and colon to things to make new symbols)

S and R
-------

* https://en.wikipedia.org/wiki/S_%28programming_language%29
* https://en.wikipedia.org/wiki/R_(programming_language)

People here are probably more familiar with R, which is an implementation of S

  APL -> S, a stastical programming language

  R is an implementation of S with some extensions. Much S code should run
  unaltered.

* https://en.wikipedia.org/wiki/R_(programming_language) - initial release 1995
* https://en.wikipedia.org/wiki/S_(programming_language) - first working
  version in 1976

  Richard Becker's `A Brief History of S`_ indicates that they were very well
  aware of APL, but clearly S is not a descendant of APL.

.. _`A Brief History of S`:: https://www.math.uwaterloo.ca/~rwoldfor/software/R-code/historyOfS.pdf

`APL in R`_ by Jan de Leeuw and Masanao Yajima, 2016, is an online book that
presents R code for APL array operations.

.. _`APL in R`: https://bookdown.org/jandeleeuw6/apl/

Algol 68
========

* https://en.wikipedia.org/wiki/ALGOL_68
* https://opensource.com/article/20/6/algol68 - Exploring Algol 68 in the 21st
  century
* http://www.algol68.org/ - a site dedicated to matters related to the programming language
* https://jmvdveer.home.xs4all.nl/en.algol-68-genie.html for modern compilers

The first working compiler was for `ALGOL 68-R` (an extended subset of the
language), in 1970 - although some of the restrictions were adopted into the
revised report on ALGOL 68.

.. _`ALGOL 68-R`: https://en.wikipedia.org/wiki/ALGOL_68-R

----

* Case stropping - what it was and why it was needed and other solutions

  From https://en.wikipedia.org/wiki/ALGOL_68-R#Stropping:

    In ALGOL family languages, it is necessary to distinguish between
    identifiers and basic symbols of the language. In printed texts this was
    usually accomplished by printing basic symbols in boldface or underlined
    (begin or begin for example).

    In source code programs, some stropping technique had to be used. In many
    ALGOL like languages, before ALGOL 68-R, this was accomplished by
    enclosing basic symbols in single quote characters ('begin' for
    example). In 68-R, basic symbols could be distinguished by writing them in
    upper case, lower case being used for identifiers.

    As ALGOL 68-R was implemented on a machine with 6-bit bytes (and hence a
    64 character set) this was quite complex and, at least initially, programs
    had to be composed on paper punched tape using a Friden Flexowriter.

    Partly based on the experience of ALGOL 68-R, the revised report on ALGOL
    68 specified hardware representations for the language, including UPPER
    stropping.

  and https://en.wikipedia.org/wiki/ALGOL_68#Program_representation, which
  gives the following alternative representations for the same code:

      *int* a real int = 3;  # the *strict* language #
    
  .. code:: algol

      'INT'A REAL INT = 3; # QUOTE stropping style #
      .INT A REAL INT = 3; # POINT stropping style #
      INT a real int = 3;  # UPPER stropping style #
      int a_real_int = 3;  # RES stropping style, there are 61 accepted reserved words #

  The inevitable wikipedia page: https://en.wikipedia.org/wiki/Stropping_(syntax)
  
* Whitespace in variable names
* "If it compiles, it runs"
* ``REF``
* Standards arguments (the split in the Algol community) and the difficulty of
  writing a compiler (at the time)

RPG
===

can I say useful stuff?


Smalltalk
=========

https://en.wikipedia.org/wiki/Smalltalk

Smalltalk-80 was made available in 1980.

* Almost no syntax
* Still alive (for instance, Pharo_)
* Influences everywhere
* http://www.jera.com/techinfo/readingSmalltalk.pdf "Reading Smalltalk"

.. _Pharo: https://pharo.org/

Occam
=====

https://en.wikipedia.org/wiki/Occam_(programming_language)

(briefly)

Signficant indentation!

http://concurrency.cc/docs/ - documentation for occam-pi, a superset of occam2
that will run on an arduino. Last blogpost on the site was in 2015.

Designed for parallel programming on a network of transputer chips.


Prolog
======

and maybe a bit about Erlang?

Full stop to end expressions/statements, not semicolon


Forth
=====

A stack based language.

Tcl
===

(maybe)

ABC
===

For old times take

This is the programming language that Guido van Rossum worked on before
inventing Python, and his experiences with ABC were significant in how he
designed Python.

Python
======

Just to show the "99 bottles" solutions, to give an idea of how much / how
little those really convey about a programming language.

One "traditional"

https://rosettacode.org/wiki/99_Bottles_of_Beer/Python

.. code:: python

  def sing(b, end):
      print(b or 'No more','bottle'+('s' if b-1 else ''), end)

  for i in range(99, 0, -1):
      sing(i, 'of beer on the wall,')
      sing(i, 'of beer,')
      print('Take one down, pass it around,')
      sing(i-1, 'of beer on the wall.\n')

(mainly included to show how one should not necessarily judge a language from
the examples given!)

And another that just misses the whole point of the exercise, but is
definitely my favourite:

http://rosettacode.org/wiki/99_Bottles_of_Beer#Python_3

.. code:: python

  """Pythonic 99 beer song (maybe the simplest naive implementation in Python 3)."""

    REGULAR_VERSE = '''\
    {n} bottles of beer on the wall, {n} bottles of beer
    Take one down and pass it around, {n_minus_1} bottles of beer on the wall.

    '''

    ENDING_VERSES = '''\
    2 bottles of beer on the wall, 2 bottles of beer.
    Take one down and pass it around, 1 bottle of beer on the wall.

    1 bottle of beer on the wall, 1 bottle of beer.
    Take one down and pass it around, no more bottles of beer on the wall.

    No more bottles of beer on the wall, no more bottles of beer.
    Go to the store and buy some more, 99 bottles of beer on the wall.

    '''
    for n in range(99, 2, -1):
        print(REGULAR_VERSE.format(n=n, n_minus_1=n - 1))
    print(ENDING_VERSES)


History and Timelines
=====================

* https://www.scriptol.com/programming/history.php
* https://www.scriptol.com/programming/list-programming-languages.php
* https://www.scriptol.com/programming/sieve.php

* https://www.levenez.com/lang/

starts with Plankalkul ! but rather limited on the languages it lists

* https://media.timetoast.com/timelines/programming-languages-b4c706df-fef5-4b23-8d87-2b0a666150df

* http://rigaux.org/language-study/diagram.html - with some links to others

  Has 2 versions - a simplified one, and a more complete one

* http://www.digibarn.com/collections/posters/tongues/ComputerLanguagesChart.png
  from http://www.digibarn.com/collections/posters/tongues/ appears to be
  rather nice at first glance

----------------------------

Written in reStructuredText_.

Converted to PDF using rst2pdf_.

Source and associated slides at https://github.com/tibs/old-proglang-syntaxes-talk

|cc-attr-sharealike| This slideshow and its related files are released under a
`Creative Commons Attribution-ShareAlike 4.0 International License`_.

.. |cc-attr-sharealike| image:: images/cc-attribution-sharealike-88x31.png
   :alt: CC-Attribution-ShareAlike image
   :align: middle

.. _`Creative Commons Attribution-ShareAlike 4.0 International License`: http://creativecommons.org/licenses/by-sa/4.0/

.. _CamPUG: https://www.meetup.com/CamPUG/
.. _reStructuredText: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _rst2pdf: https://rst2pdf.org/

(What may now seem) syntax oddities in older programming languages
==================================================================

.. class:: titleslideinfo

   some intro information

.. relevant available highlighters, from::
..
..     $ pandoc --list-highlight-languages
..
.. abc          (I'm impressed)
.. ada
.. commonlisp
.. default      (presumably if I don't specify)
.. erlang
.. fortran
.. j
.. pascal
.. prolog
.. r
.. scheme
.. tcl
.. 


(What may now seem) syntax oddities in older programming languages
------------------------------------------------------------------

Some notes intended to accompany a talk on (what may now seem strange) syntax
in older programming languages.

...but it's OK to read them by themselves.


By Tibs / Tony Ibbs

Presented at CamPUG_, virtually, 1st September 2020

Written using reStructuredText_.

.. page::

Converted to PDF slides using pandoc_ and beamer_.

Source and extended notes at https://github.com/tibs/old-proglang-syntaxes-talk

.. _reStructuredText: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _pandoc: https://pandoc.org
.. _beamer: https://github.com/josephwright/beamer


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


Timeline?
---------

...Specify a date (or approximate date) for each language.

Maybe provide a table of the languages and their dates in the notes..

==============  =========
FORTRAN         1954
FLOW-MATIC      1955-1959
LISP and Algol  1958
COBOL           1959
==============  =========


Plankalkül
----------

?do I want to mention this?

Designed by Konrad Zuse between 1942 and 1945. It was the first high-level
programming language to be designed for a computer. First implemented in 1975

https://en.wikipedia.org/wiki/Plankalk%C3%BCl


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


FORTRAN IV - bottles
--------------------

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/f.html#FORTRAN-IV

.. code:: fortran

  C Allen Mcintosh
  C mcintosh@bellcore.com 
        integer bottls
        do 50 i = 1, 99
          bottls = 100 - i
          print 10, bottls
  10       format(1x, i2, 31h bottle(s) of beer on the wall.)
          print 20, bottls
  20       format(1x, i2, 19h bottle(s) of beer.)
          print 30
  30       format(34h Take one down and pass it around,)
          bottls = bottls - 1
          print 10, bottls
          print 40
  40       format(1x)
  50    continue
        stop
        end

.. page::

which I amend to give the (to me) more familiar:

.. code:: fortran

  ••••••INTEGER BOTTLS
  ••••••DO 50 I = 1, 99
  ••••••••BOTTLS = 100 - I
  ••••••••PRINT 10, BOTTLS
  ••••••••PRINT 20, BOTTLS
  ••••••••PRINT 30
  ••••••••BOTTLS = BOTTLS - 1
  ••••••••PRINT 10, BOTTLS
  ••••••••PRINT 40
  50••••CONTINUE
  ••••••STOP
  10••••FORMAT(1X, I2, 31H bottle(s) of beer on the wall.)
  20••••FORMAT(1X, I2, 19H bottle(s) of beer.)
  30••••FORMAT(34H Take one down and pass it around,)
  40••••FORMAT(1X)
  ••••••END

(amended from an example by Allen Mcintosh, mcintosh@bellcore.com)



FORTRAN IV - Leading spaces
---------------------------

The first 6 columns and their uses


FORTRAN DATA CARDS
------------------

* Comment Cards

  The first character on the card much be C; all other characters are ignored
  in subsequent processing.
   
* Statement Cards
  
  Statement cards are subdivided into four sections as follows:

::

                  1         2        7            8
    12345 | 6 | 7890123456789 .. .. 9012 | 34567890

.. page::
       
* The first five characters are used for unique statement numbers. Numbers do
  not need to appear in sequence.  Any statement (except the END statement)
  may have a statement number.

* The sixth character is called the "continuation" character.  If more space
  is required from the previous card, include any character (except space
  or 0) in the 6th position of the next card.

  One convention was to put a 0 in the continuation field of the first card
  (the one start was to be continued).

* Positions 7-72 are used for the actual program code.  Often programmers use
  a TAB (8 spaces) rather than type 7 spaces.

* Positions 73-80 are infrequently used, but when they are they are used for
  identification codes which are only of interest to the programmer, they are
  not computed.


(actually, putting a sequence number in that last column is pretty important
in case you drop the deck of cards!)

Note that labels *look* like numbers, but they aren't really. So their order
doesn't make any difference to the compiler. Also, ``▿▿123`` is just as
allowed as ``123▿▿``.

FORTRAN IV
----------

Spaces within program code are ignored. So ``GOTO`` is the same as ``GO TO``
is the same as ``G O T O``.

No reserved words, context gives meaning.

So:

.. code:: fortran

            IF(IF.EQ.PROGRAM)IF=IF*PROGRAM

is legal FORTRAN (of some type)

Also, case is not relevant - although I had a habit of typing all the code in
CAPITALS (after all, that's what your left little finger is for).

I remember that the Fortran compiler we used was limited to 6 character
variable, function and subroutine names, which made writing libraries
interesting...
            

FORTRAN IV - Computed GOTO
--------------------------


Arithmetic IF:
    
        IF (numeric-expression) statement1,statement2,statement3
   
Evaluate the expression, then transfer to statement1 if the result is
negative, to statement2 if zero, to statement3 if positive.  For example,

.. code:: fortran
          
        IF (X/Y*Z) 100,300,50
        
If the result of the computation is negative, transfer to statement number
100, if zero transfer to statement number 300, if positive to statement
number 50.

.. page::

(if/then/else invented by Lisp?)

(no - according to https://en.wikipedia.org/wiki/Lisp_(programming_language)

"""A conditional using an if–then–else syntax was invented by McCarthy in a
Fortran context. He proposed its inclusion in ALGOL, but it was not made part
of the Algol 58 specification. For Lisp, McCarthy used the more general
cond-structure. Algol 60 took up if–then–else and popularized it."""

so Algol 60 got "if-then-else" and LISP got ``cond``)


FORTRAN - Functions versus subroutines
--------------------------------------

Are those the right terms?

function returns a single value (assigned to the function name)

subroutine returns 0 or more values, by modifying the variables in its
parameter list


LISP
----

... present an example of the language as it didn't turn out, first! ...

(? picture of Lisp 1.5 manual ?)

From https://en.wikipedia.org/wiki/Lisp_(programming_language)#History:

"""McCarthy's original notation used bracketed "M-expressions" that would be
translated into S-expressions. As an example, the M-expression car[cons[A,B]]
is equivalent to the S-expression (car (cons A B)). Once Lisp was implemented,
programmers rapidly chose to use S-expressions, and M-expressions were
abandoned. M-expressions surfaced again with short-lived attempts of MLisp[11]
by Horace Enea and CGOL by Vaughan Pratt."""

.. page::

https://en.wikipedia.org/wiki/M-expression

"""McCarthy had planned to develop an automatic Lisp compiler (LISP 2) using
M-expressions as the language syntax and S-expressions to describe the
compiler's internal processes. Stephen B. Russell read the paper and
suggested to him that S-expressions were a more convenient syntax. Although
McCarthy disapproved of the idea, Russell and colleague Daniel J. Edwards
hand-coded an interpreter program that could execute S-expressions.[2] This
program was adopted by McCarthy's research group, establishing S-expressions
as the dominant form of Lisp."""

The Lisp 1.5 manual does, of course, talk about both forms.

.. page::

From
http://www.softwarepreservation.org/projects/LISP/lisp2/SP-2450-SUMSQUARE_LCS.pdf

.. code::

   % SUMSQUARE COMPUTES THE SUM OF THE SQUARES OF THE
   % COMPONENTS OF AN ARBITRARY VECTOR

   REAL SECTION COMPUTE, LISP;

   REAL FUNCTION SUMSQUARE(X(I));
      BEGIN INTEGER J; REAL Y;
              FOR J ← STEP 1 UNTIL I DO
                  Y ← Y + X(J) ↑ 2;
              RETURN Y;
      END;

   SUMSQUARE (2, 7, 4); STOP

giving the result::

  69.0

.. page::

or, of course!

Common Lisp

https://rosettacode.org/wiki/Sum_of_squares#Common_Lisp

.. code:: lisp

  (defun sum-of-squares (vector)
    (loop for x across vector sum (expt x 2)))

Scheme

https://rosettacode.org/wiki/Sum_of_squares#Scheme

.. code:: scheme

  define (sum-of-squares l)
    (apply + (map * l l)))


LISP - as we know it
--------------------

...

Not sure how useful this is:

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/l.html#LISP

.. code:: lisp

	
  ;;; Lisp example of "99 Bottles of beer on the wall"
  ;;;
  ;;; NOTE:  Although my mailer insists on inserting 
  ;;; (at least) one, there is no line break in the 
  ;;; string beginning "~~  (i.e. it should all be on one line).
  ;;;
  ;;; In particular, if it breaks so that the first line
  ;;; ends with "...~~R" and the second line starts "~0@..."
  ;;; they should be put back together with a space between
  ;;; them.  That is, it should read "...~~R ~0@...".
  ;;; Or just see it here:
  ;;;     http://www.sover.net/~nichael/lisp99.html
  (labels ((foo (x)
    (and (<= 0 x) (cons x (foo (1- x))))))
    (format t (format nil 
          "~~{~~&~~@(~~%~~R ~A ~A!~~)~~:*~~&~~@(~~R ~0@*~A!~~)~~&~~@(~2@*~A!~~)~~&~~@(~~[~A~~:;~~:*~~R~~:*~~] ~0@*~A!~~)~~}"
              "bottles of beer"
              "on the wall"
              "take one down, pass it around"	
              "no more"
              )
  (foo 99)))


.. page::

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/s.html#Scheme
  
.. code:: scheme
	  
  ;;; Tim Goodwin (tim@pipex.net)

  (define bottles
    (lambda (n)
      (cond ((= n 0) (display "No more bottles"))
            ((= n 1) (display "One bottle"))
            (else (display n) (display " bottles")))
      (display " of beer")))

  (define beer
    (lambda (n)
      (if (> n 0)
          (begin
            (bottles n) (display " on the wall") (newline)
            (bottles n) (newline)
            (display "Take one down, pass it around") (newline)
            (bottles (- n 1)) (display " on the wall") (newline)
            (newline)
            (beer (- n 1))))))

  (beer 99)


.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer/Lisp

Common Lisp

.. code:: lisp

  (defun bottles (x)
    (loop for bottles from x downto 1
          do (format t "~a bottle~:p of beer on the wall~@
                        ~:*~a bottle~:p of beer~@
                        Take one down, pass it around~@
                        ~V[No more~:;~:*~a bottle~:p of~] beer on the wall~2%"
                    bottles (1- bottles))))

  (bottles 99)

.. page::

Scheme

https://rosettacode.org/wiki/99_Bottles_of_Beer#Scheme

.. code:: scheme

  (define (sing)
  (define (sing-to-x n)
    (if (> n -1)
      (begin 
          (display n)
          (display "bottles of beer on the wall")
          (newline)
          (display "Take one down, pass it around")
          (newline)
          (sing-to-x (- n 1)))
      (display "would you wanna me to sing it again?")))
  (sing-to-x 99))


My father's parentheses
-----------------------

Franz Lisp (?) and the ``]``

...the inevitable xkcd cartoon


COBOL
-----

Do I have anything to say?

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/c.html#Cobol

.. code:: cobol

	
  IDENTIFICATION DIVISION.
  PROGRAM-ID.BOTTLES_OF_BEER.
  AUTHOR.DONALD FRASER.
  *
  ENVIRONMENT DIVISION.
  CONFIGURATION SECTION.
  SOURCE-COMPUTER. VAX.
  OBJECT-COMPUTER. VAX.
  *
  INPUT-OUTPUT SECTION.
  FILE-CONTROL.
          SELECT OUTPUT-FILE
                  ASSIGN TO BEERS_ON_THE_WALL.
  *
  DATA DIVISION.
  FILE SECTION.
  FD OUTPUT-FILE
          LABEL RECORDS ARE OMITTED.
  01 BEERS-OUT                                   PIC X(133).
  *
  WORKING-STORAGE SECTION.
  01 FLAGS-COUNTERS-ACCUMULATORS.
          05 FLAGS.
                  10 E-O-F                                PIC 9.
                          88 END-OF-FILE                VALUE 1.
          05 COUNTERS.
                  10 BOTTLES                      PIC 999
                                                  VALUE 0.
  01 RECORD-OUT.
          05 LINE1.
                  10 NUMBER-OF-BEERS-1                    PIC ZZ9.
                  10                                      PIC X(28)
                                  VALUE "BOTTLES OF BEER IN THE WALL ".
                  10                                                        PIC
  X
                                  VALUE ",".
                          10 NUMBER-OF-BEERS-2            PIC ZZ9.
                  10                                                        PIC
  X.
                  10                                      PIC X(17)
                                  VALUE "BOTTLES OF BEER.".
          05 LINE2.
                  10                                              PIC X(34)
                                  VALUE "TAKE ONE DOWN AND PASS IT ARROUND ".
                  10 NUMBER-OF-BEERS-3            PIC ZZ9.
                  10                                      PIC X.
                  10                                      PIC X(28)
                                  VALUE "BOTTLES OF BEER IN THE WALL".
  *
  PROCEDURE DIVISION.
  DRIVER-MODULE.
        PERFORM INITIALIZATION.
        PERFORM PROCESS UNTIL END-OF-FILE.
        PERFORM TERMINATION.
        STOP RUN.
  *
  INITIALIZATION.
          OPEN OUTPUT OUTPUT-FILE.
          ADD 100 TO BOTTLES.
  *
  PROCESS.
          IF BOTTLES = 0 THEN
                  COMPUTE E-O-F = 1
          ELSE PERFORM WRITE-ROUTINE
          END-IF.
  *
  TERMINATION.
          CLOSE OUTPUT-FILE.
  *
  WRITE-ROUTINE.
            MOVE BOTTLES TO NUMBER-OF-BEERS-1, NUMBER-OF-BEERS-2.
          COMPUTE BOTTLES = BOTTLES - 1.
          WRITE BEERS-OUT FROM LINE1.
          MOVE BOTTLES TO NUMBER-OF-BEERS-3.
          WRITE BEERS-OUT FROM LINE2.

.. page::

https://rosettacode.org/wiki/Category:COBOL

A more concise version that adheres to the minimum guidelines. Leading zeros
are not suppressed. (OpenCOBOL - 1.1.0)

.. code:: cobol

  program-id. ninety-nine.
  data division.
  working-storage section.
  01  cnt       pic 99.

  procedure division.

    perform varying cnt from 99 by -1 until cnt < 1
      display cnt " bottles of beer on the wall"
      display cnt " bottles of beer"
      display "Take one down, pass it around"
      subtract 1 from cnt 
      display cnt " bottles of beer on the wall"
      add 1 to cnt
      display space
    end-perform.


Snobol
------

Double check

``<expression>, <jump if T>, <jump if F>``

Snobol versus Spitbol

.. page::

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/s.html#Snobol

.. code:: snobol

  * 99 BOTTLES OF BEER IN SNOBOL (UNTESTED)
          BEER = 99
  MOREBEER OUTPUT = BEER ' BOTTLES OF BEER ON THE WALL'
          OUTPUT = BEER ' BOTTLES OF BEER'
          OUTPUT = 'TAKE ONE DOWN, PASS IT AROUND'
          BEER = BEER - 1
          OUTPUT = BEER ' BOTTLES OF BEER ON THE WALL'
          GT(BEER,0)   : S(MOREBEER)
          OUTPUT = 'NO MORE BOTTLES OF BEER ON THE WALL'
          OUTPUT = 'NO MORE BOTTLES OF BEER'
          OUTPUT = 'GO TO THE STORE AND BUY SOME MORE'
          OUTPUT = '99 BOTTLES OF BEER'
  END

.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer#SNOBOL4

Works with: Macro Spitbol and CSnobol

Function version with string composition. Function returns one verse for x
bottles. Correctly handles bottle/bottles.

.. code:: snobol

          define('bottles(x)')
          nl = char(13) char(10) ;* Win/DOS, change as needed
          s2 = ' of beer'; s3 = ' on the wall'
          s4 = 'Take one down, pass it around'
          s5 = 'Go to the store, get some more' :(bottles_end)
  bottles s1 = (s1 = ' Bottle') ne(x,1) 's'
          output = nl x s1 s2 s3 nl x s1 s2
          x = gt(x,0) x - 1 :f(done)
          s1 = (s1 = ' Bottle') ne(x,1) 's'
          output = s4 nl x s1 s2 s3 :(return)
  done    output = s5 nl 99 s1 s2 s3 :(return)
  bottles_end

  *       # Test and display, only 2 bottles!
          n = 2
  loop    bottles(n); n = gt(n,0) n - 1 :s(loop)
  end


Spitbol
-------

Not sure if worth mentioning - probably either this or Snobol, unless the
difference is interesting?

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/s.html#Spitbol
  
.. code:: spitbol

	
  * MaxSPITBOL version (SPITBOL implementation on
  * the Macintosh from Catspaw, Inc. (Salida, CO).
  * NOTE:  I have no connection w/them other than being
  * a long-time satisfied user of their product
  * D.H.  <hedges@pilot.njin.net>

      p0 = "NO MORE" ;  p1 = " BOTTLE" ; p2 = "S" ; p3 = " OF BEER"
      p4 = " ON THE WALL" ; p5 = "TAKE ONE DOWN, PASS IT AROUND"

      b = 99
      p6 = ((NE(b,0) b, p0) p1 (NE(b,1) p2,) p3)
  A1   OUTPUT = p6 p4 ; OUTPUT = p6 ; OUTPUT = p5
      b = b - 1
      p6 = ((NE(b,0) b, p0) p1 (NE(b,1) p2,) p3)
      OUTPUT = p6 p4 ; OUTPUT = ; NE(b,0)                   :S(A1)
  END


BCPL
----

Also:

* ``$( .. )$``
* ``IF .. THEN`` and ``TEST .. THEN .. ELSE``
* a statement continues to the next line if it can't have ended (so, for
  instance, if the last character was the ``+`` of an arithmetic expression
* labels *are* values, and since everything is a word, you can do arithmetic
  on them.

.. page::

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/b.html#BCPL

.. code:: bcpl

	
  // BCPL version of 99 Bottles of Beer.
  // hacked by Akira KIDA <SDI00379@niftyserve.or.jp>

  GET "LIBHDR"

  MANIFEST $(
      BOTTLES = 99
  $)

  LET START() BE $(
      LET BEERS(N, S) BE $(
          TEST N = 0 THEN WRITEF("No more bottles")
                    ELSE WRITEF("%N bottle%S", N, (N = 1) -> "", "s")
          WRITEF(" of beer%S", S)
      $)

      FOR I = BOTTLES TO 1 BY -1 DO $(
              BEERS(I, " on the wall, ")
              BEERS(I, ".*NTake one down, pass it around.*N")
              BEERS(I - 1, " on the wall.*N")
      $)
      FINISH
  $)


Distraction - character sets
----------------------------

* 6 bit, 7, bit, 8 bit, 9 bit characters
* ICL 6 bit - how to represent lower case characters
* ASCII
* EBCDIC
* ISO 646
* ISO 10646 and Unicode (let's not go into details)

and others...


ASCII versus EBCDIC
-------------------

Characters that are in one but not the other

* https://www.daytodaygk.com/ascii-vs-ebcdic/
* http://www.dynamoo.com/technical/ascii-ebcdic.htm (opinitionated!)
* https://en.wikipedia.org/wiki/EBCDIC/


APL - ancestor of R
-------------------

IBM Selectric and golfball (picture would be nice) are mentioned on the APL
wikipedia page.

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

.. page::

APL -> J, using ASCII with digraphs instead of special symbols (basically, it
adds dot and colon to things to make new symbols)

APL -> S, a stastical programming language

R is an implementation of S with some extensions. Much S code should run
unaltered.

.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer#APL

Classic version:

.. I never could figure out how to display this with pandoc/XeLaTeX, so am
.. resorting to a screen shot - and I hope that square glyph in the screenshot
.. is meant to be a square!
..
.. And now I'm using rst2pdf, which also doesn't default to coping, I've
.. already *got* the screenshot...
..
..  bob  ←  { (⍕⍵), ' bottle', (1=⍵)↓'s of beer'}
..  bobw ←  {(bob ⍵) , ' on the wall'}
..  beer ←  { (bobw ⍵) , ', ', (bob ⍵) , '; take one down and pass it around, ', bobw ⍵-1}
..  ↑beer¨ ⌽(1-⎕IO)+⍳99

.. image:: images/apl-larger.png
   :scale: 150%
   :alt: APL code

and its equivalent in J

https://rosettacode.org/wiki/99_Bottles_of_Beer#J

.. code:: j

  bob =: ": , ' bottle' , (1 = ]) }. 's of beer'"_
  bobw=: bob , ' on the wall'"_
  beer=: bobw , ', ' , bob , '; take one down and pass it around, ' , bobw@<:
  beer"0 >:i.-99


.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer#R

Simple looping solution in R

.. code:: r

  #a naive function to sing for N bottles of beer...

  song = function(bottles){

    for(i in bottles:1){ #for every integer bottles, bottles-1 ... 1

      cat(bottles," bottles of beer on the wall \n",bottles," bottles of beer \nTake one down, pass it around \n",
          bottles-1, " bottles of beer on the wall \n"," \n" ,sep="")       #join and print the text (\n means new line)

          bottles = bottles - 1 #take one down...

    }

  }

  song(99)#play the song by calling the function
          

.. page::

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/s.html#S-Plus

S - is this the right S?

.. code:: s

  Using S-Plus code

  for(i in 100:1){
              if(i>1){
                          cat(i,"bottles of beer on the wall,",i,"bottles of beer\n")
                          cat("Take one down, pass it around\n")
                          cat(i-1,"bottles of beer on the wall\n",fill=TRUE)
              }
              else{
                          cat(i,"bottle of beer on the wall,",i,"bottle of beer\n")
                          cat("Take one down and pass it around\n")
                          cat("No bottles of beer on the wall!!\n",fill=TRUE)
              }
  }

.. page::

J

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/j.html#J

.. code:: j

  See http://www.cs.trinity.edu/About/The_Courses/cs2322/

  Date: Thu, 8 Mar 2001 09:23:02 -0500
  From: Roger Hui 
  Reply-To: forum@jsoftware.com
  To: APL Mailing List , J Forum , TimTroyR@ionet.net
  Subject: Jforum: Re: New Scientist Puzzle and Oddball Languages

  NB. a solution in J (http://www.jsoftware.com) to the 99 Bottles of Beer problem.

    bob =: ": , ' bottle'"_ , (1: = ]) }. 's of beer'"_
    bobw=: bob , ' on the wall'"_
    beer=: bobw , ', '"_ , bob , '; take one down and pass it around, '"_ , bobw@<:

  NB. For example:

      beer"0 >:i.-5
  5 bottles of beer on the wall, 5 bottles of beer; take one down and pass it around, 4 bottles of beer on the wall
  4 bottles of beer on the wall, 4 bottles of beer; take one down and pass it around, 3 bottles of beer on the wall
  3 bottles of beer on the wall, 3 bottles of beer; take one down and pass it around, 2 bottles of beer on the wall
  2 bottles of beer on the wall, 2 bottles of beer; take one down and pass it around, 1 bottle of beer on the wall
  1 bottle of beer on the wall, 1 bottle of beer; take one down and pass it around, 0 bottles of beer on the wall

.. page::
  
R

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/r.html#R

.. code:: r

  # R version of 99 Bottles of beer (Bottles.r)
  # See http://www.r-project.org/ for more informations
  # Philipp Winterberg, http://www.winterbergs.de

  for (b in 99:1){
    print(b)
    print(" bottle(s) of beer on the wall,")
    print(b)
    print(" bottle(s) of beer.")
    print("Take one down, pass it around,")
    print(b-1)
    print(" bottle(s) of beer on the wall.")
    print("")
  }
          

Algol 68 - case stropping
-------------------------

Why this was needed.

Other ways of doing it (Algol 68 keywords in CAPS, ??? keywords in single
quotes)

(also, bold stropping in print)


Algol 68 - whitespace in variable names
---------------------------------------

.. code:: pascal

   Strictly speaking we do not need this temporary variable but
   the code is clearer if we have it = 3


Algol 68 - REFs
---------------

Explain

.. page::

Algol 68

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/a.html#Algol-68

.. code:: algol68

  # 99 Bottles of Beer                         #
  # by Otto Stolz <Otto.Stolz@Uni-Konstanz.de> #
  ( PROC width = (INT x) INT: (x>9 | 2 | 1)
  ; FOR i FROM 99 BY -1 TO 1
    DO  printf ( ( $ 2l n(width(i))d
                  , x "bottle" b("","s") x "of beer on the wall,"
                  , x n(width(i))d
                  , x "bottle" b("","s") x "of beer."
                  , l "Take one down, pass it around,"
                  , x n(width(i-1))d
                  , x "bottle" b("","s") x "of beer."
                  $
                , i  , i=1
                , i  , i=1
                , i-1, i=2
              ) )
    OD
  )

.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer#ALGOL_68

Works with ALGOL 68 version Standard (no extensions to language used) and
with ALGOL 68G version Any (tested with release mk15-0.8b.fc9.i386)

.. code:: algol68

  main:(
    FOR bottles FROM 99 TO 1 BY -1 DO
      printf(($z-d" bottles of beer on the wall"l$, bottles));
      printf(($z-d" bottles of beer"l$, bottles));
      printf(($"Take one down, pass it around"l$));
      printf(($z-d" bottles of beer on the wall"ll$, bottles-1))
    OD
  )


RPG
---

Compare to Snobol ???

Can I actually make a sensible example for this?

.. page::

RPG/400

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/r.html#RPG/400

The following as presented appears to start with 5 spaces on each line.

.. code:: rpg

     H*
     H* RPG/400 VERSION OF THE BOTTLES PROGRAM *
     H*
     FSCREEN  O   F      80            WORKSTN
     C                     MOVE 100       X       30
     C           X         DOWGE0
     C                     EXCPT
     C                     SUB  1         X
     C                     END
     C                     SETON                     LR
     OSCREEN  E
     O                         X          3
     O                                   26 'BOTTLES OF BEER ON THE'
     O                                   31 'WALL,'
     O                         X         36
     O                                   53 'BOTTLES OF BEER'
     O        E
     O                                   22 'TAKE ONE DOWN AND PASS'
     O                                   32 'IT AROUND'


Smalltalk
---------

Almost no syntax

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/s.html#SmallTalk

.. code:: smalltalk

  "Programmer: patrick m. ryan - pryan@access.digex.net
  "http://www.access.digex.net/~pryan

  99 to: 1 by: -1 do: [ :i |
          i print. ' bottles of beer on the wall, ' print.
          i print. ' bottles of beer. ' print.
          'take one down, pass it around, ' print.
          (i-1) print. ' bottles of beer on the wall, ' print.

I think that's rather elegant.

.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer#Smalltalk

A straightforward approach

.. code:: smalltalk

  Smalltalk at: #sr put: 0 ; at: #s put: 0 !
  sr := Dictionary new.
  sr at: 0 put: ' bottle' ;
    at: 1 put: ' bottles' ;
    at: 2 put: ' of beer' ;
    at: 3 put: ' on the wall' ;
    at: 4 put: 'Take one down, pass it around' !
  99 to: 0 by: -1 do: [:v | v print.
          ( v == 1 ) ifTrue: [ s := 0. ] 
                      ifFalse: [ s := 1. ].
          Transcript show: (sr at:s) ; show: (sr at:2) ; show: (sr at:3) ; cr.
                      v print.
          Transcript show: (sr at:s) ; show: (sr at:2) ; cr.
                      (v ~~ 0) ifTrue: [ Transcript show: (sr at:4) ; cr. ].
    ].

.. page::

https://pharo.org/ - squeak variant


Occam
-----

Signficant indentation!

http://concurrency.cc/docs/ - documentation for occam-pi, a superset of occam2
that will run on an arduino. Last blogpost on the site was in 2015.

.. page::

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/o.html#Occam

.. code:: occam

  -- compiled with the University of Kent "kroc" compiler
  -- Tony Curtis <Tony.Curtis@vcpc.univie.ac.at> 1997
  --
  PROC beer (CHAN OF BYTE key, screen, error)

    VAL INT BEERS IS 99 :                       -- big fridge!

    #USE "tty_utils.tco"
    PROC sorp (VAL INT n, CHAN OF BYTE out)     -- singular or plural?
      IF
        n > 1
          out.string ("s", 1, out)
        TRUE
          SKIP
    :
    PROC sayit (VAL INT n, CHAN OF BYTE out)     -- text for each iteration
      SEQ
        out.number (n, 1, out)
        out.string (" bottle", 1, out)
        sorp (n, out)
        out.string (" of beer on the wall, ", 1, out)
        out.number (n, 1, out)
        out.string (" bottle", 1, out)
        sorp (n, out)
        out.string (" of beer.", 1, out)
        out.string ("*c*n", 1, out)
        out.string ("Take one down, pass it around, ", 1, out)
        VAL INT next IS  n - 1 :
        IF
          next > 0
            SEQ
              out.number (next, 1, out)
              out.string (" bottle", 1, out)
              sorp (next, out)
              out.string (" of beer on the wall.", 1, out)
          TRUE
            out.string ("no bottles of beer on the wall.", 1, out)
        out.string ("*c*n", 1, out)
    :
    PROC beers (VAL INT nbeers, CHAN OF BYTE out)
      INT b :
      SEQ
        b := nbeers
        WHILE b > 0
          SEQ
            sayit (b, out)
            b := b - 1
    :
    beers (BEERS, screen)
  :


Erlang and Prolog
-----------------

Full stop to end expressions/statements, not semicolon

(I've heard people say Erlang is inspired by Prolog in some sense?)

.. page::

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/e.html#Erlang

.. code:: erlang

	
  <a href=http://www.ericsson.se/cslab/erlang/>Erlang</a> is a language used for real-time control systems.

  % ---------------------------------------------------------------
  % Erlang version of the beer song
  % Kent Engström, kenen@ida.liu.se
  % ---------------------------------------------------------------
  % See http://www.ericsson.se/cslab/erlang/ for Erlang information
  % ---------------------------------------------------------------

  -module(beer).
  -export([song/0]).

  song() ->
      song(100).

  song(0) ->
      done;
  song(N) ->
      Bottles=bottles(N),
      Bottles1=bottles(N-1),
      io:format("~s of beer on the wall, ~s of beer.~n",
                [Bottles,Bottles]),
      io:format("Take one down and pass it around, ~s of beer on the wall.~n",
                [Bottles1]),
      song(N-1).

  bottles(0)->
      "no more bottles";
  bottles(1)->
      "1 bottle";
  bottles(N)->
      lists:append(integer_to_list(N)," bottles").

.. page::

Prolog

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/p.html#Prolog
      
.. code:: prolog

  % 99 bottles of beer.
  % Remko Troncon <spike@kotnet.org>

  bottles :-
      bottles(99).

  bottles(1) :- 
      write('1 bottle of beer on the wall, 1 bottle of beer,'), nl,
      write('Take one down, and pass it around,'), nl,
      write('Now they are alle gone.'), nl.
  bottles(X) :-
      X > 1,
      write(X), write(' bottles of beer on the wall,'), nl,
      write(X), write(' bottles of beer,'), nl,
      write('Take one down and pass it around,'), nl,
      NX is X - 1,
      write(NX), write(' bottles of beer on the wall.'), nl, nl,
      bottles(NX).

.. page::

Erlang

https://rosettacode.org/wiki/99_Bottles_of_Beer#Erlang

.. code:: erlang

  -module(beersong).
  -export([sing/0]).
  -define(TEMPLATE_0, "~s of beer on the wall, ~s of beer.~nGo to the store and buy some more, 99
  bottles of beer on the wall.~n").
  -define(TEMPLATE_N, "~s of beer on the wall, ~s of beer.~nTake one down and pass it around, ~s of
  beer on the wall.~n~n").

  create_verse(0)      -> {0, io_lib:format(?TEMPLATE_0, phrase(0))};
  create_verse(Bottle) -> {Bottle, io_lib:format(?TEMPLATE_N, phrase(Bottle))}.

  phrase(0)      -> ["No more bottles", "no more bottles"];
  phrase(1)      -> ["1 bottle", "1 bottle", "no more bottles"];
  phrase(2)      -> ["2 bottles", "2 bottles", "1 bottle"];
  phrase(Bottle) -> lists:duplicate(2, integer_to_list(Bottle) ++ " bottles") ++
  [integer_to_list(Bottle-1) ++ " bottles"].

  bottles() -> lists:reverse(lists:seq(0,99)).

  sing() ->
      lists:foreach(fun spawn_singer/1, bottles()),
      sing_verse(99).

  spawn_singer(Bottle) ->
      Pid = self(), 
      spawn(fun() -> Pid ! create_verse(Bottle) end).

  sing_verse(Bottle) ->
      receive
          {_, Verse} when Bottle == 0 ->
              io:format(Verse);
          {N, Verse} when Bottle == N ->
              io:format(Verse),
              sing_verse(Bottle-1)
      after 
          3000 ->
              io:format("Verse not received - re-starting singer~n"),
              spawn_singer(Bottle),
              sing_verse(Bottle)
      end.

.. page::

Prolog - works with SWI Prolog

https://rosettacode.org/wiki/99_Bottles_of_Beer/Prolog

.. code:: prolog

  bottles(0):-!.
  bottles(X):-
      writef('%t bottles of beer on the wall \n',[X]),
      writef('%t bottles of beer\n',[X]),
      write('Take one down, pass it around\n'),
      succ(XN,X),
      writef('%t bottles of beer on the wall \n\n',[XN]),
      bottles(XN).

  :- bottles(99).


.. page::

or, handling plurals:

.. code:: prolog

  line1(X):- line2(X),write(' on the wall'). 
  line2(0):- write('no more bottles of beer').
  line2(1):- write('1 bottle of beer').
  line2(X):- writef('%t bottles of beer',[X]).
  line3(1):- write('Take it down, pass it around').
  line3(X):- write('Take one down, pass it around').
  line4(X):- line1(X).

  bottles(0):-!.
  bottles(X):-	
      succ(XN,X),
      line1(X),nl,
      line2(X),nl,
      line3(X),nl,
      line4(XN),nl,nl,
      !,
      bottles(XN).

  :- bottles(99).


Forth and stack based languages
-------------------------------

(maybe mention PostScript and thus also PDF)

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/f.html#Forth

.. code:: forth

  \ Forth version of the 99 Bottles program.
  \ Dan Reish, dreish@izzy.net

  : .bottles ( n -- n-1 )
    dup 1 = IF  ." One bottle of beer on the wall," CR
                ." One bottle of beer," CR
                ." Take it down," 
    ELSE  dup . ." bottles of beer on the wall," CR
          dup . ." bottles of beer," CR
          ." Take one down," 
    THEN
    CR
    ." Pass it around," CR
    1-
    ?dup IF  dup 1 = IF  ." One bottle of beer on the wall;" 
              ELSE  dup . ." bottles of beer on the wall;" 
              THEN
          ELSE  ." No more bottles of beer on the wall." 
    THEN
    CR
  ;

  : nbottles ( n -- )
    BEGIN  .bottles  ?dup NOT UNTIL
  ;

  99 nbottles

.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer#Forth

.. code:: forth

  :noname   dup . ." bottles" ;
  :noname       ." 1 bottle"  ;
  :noname ." no more bottles" ;
  create bottles , , ,

  : .bottles  dup 2 min cells bottles + @ execute ;
  : .beer     .bottles ."  of beer" ;
  : .wall     .beer ."  on the wall" ;
  : .take     ." Take one down, pass it around" ;
  : .verse    .wall cr .beer cr
          1- .take cr .wall cr ;
  : verses    begin cr .verse ?dup 0= until ;

  99 verses

.. page::

or create a beer language and write the program:

.. code:: forth

  DECIMAL
  : BOTTLES ( n -- )
          DUP
          CASE
          1 OF    ." One more bottle " DROP ENDOF
          0 OF    ." NO MORE bottles " DROP ENDOF
                  . ." bottles "    \ DEFAULT CASE
          ENDCASE ;

  : ,   [CHAR] , EMIT  SPACE 100 MS CR ;
  : .   [CHAR] . EMIT  300 MS  CR CR CR ;

  : OF       ." of "   ;     : BEER     ." beer " ;
  : ON       ." on "   ;     : THE      ." the "  ;
  : WALL     ." wall" ;      : TAKE     ." take " ;
  : ONE      ." one "  ;     : DOWN     ." down, " ;
  : PASS     ." pass " ;     : IT       ." it "   ;
  : AROUND   ." around" ;

  : POPONE    1 SWAP CR ;
  : DRINK     POSTPONE DO ; IMMEDIATE
  : ANOTHER   S" -1 +LOOP" EVALUATE ; IMMEDIATE
  : HOWMANY   S" I " EVALUATE ; IMMEDIATE
  : ONELESS   S" I 1- " EVALUATE ; IMMEDIATE
  : HANGOVER    ." :-("  CR QUIT ;

  : BEERS ( n -- )   \ Usage:  99 BEERS
        POPONE
        DRINK
          HOWMANY BOTTLES OF BEER ON THE WALL ,
          HOWMANY BOTTLES OF BEER ,
          TAKE ONE DOWN PASS IT AROUND ,
          ONELESS BOTTLES OF BEER ON THE WALL .
        ANOTHER 
        HANGOVER ;


Maybe TCL?
----------

Not sure

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/t.html#TCL

.. code:: tcl

  # Tcl version of 99 bottles of beer on the wall
  # Author: Don Libes (libes@nist.gov)
  #

  proc bottles {i} {
          return "$i bottle[expr $i!=1?"s":""] of beer"
  }

  proc line123 {i} {
          puts "[bottles $i] on the wall,"
          puts "[bottles $i],"
          puts "take one down, pass it around,"
  }

  proc line4 {i} {
          puts "[bottles $i] on the wall.\n"
  }

  for {set i 99} {$i>0} {} {
          line123 $i
          incr i -1
          line4 $i
  }

.. page::

https://rosettacode.org/wiki/99_Bottles_of_Beer/Tcl

not sure it's worth including any here, but there are several examples,
showcasing the ways one might do it in tcl


ABC - Python's inspirational ancestor
-------------------------------------

Maybe, just for the sake of it

http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/a.html#ABC

.. code:: abc

  <a href=http://www.cwi.nl/cwi/projects/abc.html>ABC</a> was developed 
  at CWI in the Netherlands. 
  PUT "by Whitey (whitey@netcom.com) - 10/13/96" IN author

  HOW TO RETURN verse n:
    SELECT:
        n = 0:
          PUT "no more bottles of beer" IN s
        n = 1:
          PUT "1 bottle of beer" IN s
        ELSE:
          PUT "`n` bottles of beer" IN s
    RETURN s

  HOW TO DRINK:
    PUT 99 IN num
    WHILE num > 0:
        WRITE verse num, " on the wall, ", verse num, "," /
        WRITE "take one down, pass it around," /
        PUT num - 1 IN num
        WRITE verse num, " on the wall." /

  DRINK


We do not talk about INTERCAL
-----------------------------

Not *really* a language people use


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

.. page::

* https://en.wikipedia.org/wiki/History_of_programming_languages

* https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(syntax)
  (perhaps too much information)

* http://www.99-bottles-of-beer.net doesn't seem to be working at the moment

* https://web.mit.edu/kenta/www/two/beer.html has Fortran IV, but the pages
  for each language are on ``.net`` and don't seem to work at the moment

* http://www.info.univ-angers.fr/pub/gh/hilapr/beers/schade/ has Fortran IV
  and seems to work

.. page::

* https://www.hillelwayne.com/equals-as-assignment/ Why Does "=" Mean
  Assignment? also by Hillel Wayne, from 2018

.. page::
  
* FORTRAN IV

  - http://www.math-cs.gordon.edu/courses/cs323/FORTRAN/fortran.html
  - http://www.jaymoseley.com/hercules/fortran/fort_mini.htm
  - http://www.quadibloc.com/comp/fort03.htm some context with respect to
    FORTRAN II, and some talk on specifics of particular implenentations

  Still to look at:

  - https://hackaday.com/2015/10/26/this-is-not-your-fathers-fortran/1G

.. page::
  
Don't forget the excellent http://www.softwarepreservation.org/ and
particularly the http://www.softwarepreservation.org/projects page, which has
links to many pages of programming language history, with a huge number of
useful links.

.. page::

* https://www.whoishostingthis.com/resources/apl/


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

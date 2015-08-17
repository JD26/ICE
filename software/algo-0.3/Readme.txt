

Algorithm-Style Package Install and Use


1 Introduction

This package contains files that provide a new textclass and an article
layout example that allow to use the algorithm and algorithmic packages
directly through LyX without writing latex commands (ERT). The package
is to install under your local LyX configuration directory, and LyX
must be reconfigured to make the new layout available.


2 Package content

The package algo-<version>, where <version> is the current version
number, contains the following files:

algorithm.inc: algorithm textclass definition, that must be included
  in any .layout file that needs this text style.

algolyx.sty: the latex package providing the necessary commands.

article-algo.layout: document class layout example, taken from the
  standard article layout, that includes the algorithm.inc definition
  file to make the new algorithm textclass available.

algo.lyx: document that describes how to use the new algorithm textclass,
  and is at the same time an example of document using the article-algo
  layout. Thus, this document should be viewed once the package is
  installed.

algo.{tex, ps}: algo.lyx latex or postscript output, for those who
  are interested in reading the document before or without installing
  the package.

lang/: directory containing LyX/ps documents examples using the algorithm
  style in other languages than english. 

examples/: directory containing LyX/ps documents examples using some
  algorithm layout options.
  

3 Installing the package

To install the package, do as following:

1. Gunzip and untar the package under the directory you want:

  > gunzip -c algo<version>.tar.gz | tar xvf -

2. Copy the textclass definition (.inc) and the document class layout
  example (.layout) under your local LyX configuration layout directory,
  that usually is $HOME/.lyx/layouts. Check under lyx (Help->Version)
  your actual user directory.

  > cd algo-<version> 
  > cp algorithm.inc article-algo.layout $HOME/.lyx/layouts

3. Install algolyx.sty such that its path is seen by LaTeX. You can
  do as follow:

  > mkdir $HOME/.lyx/layouts/packages 
  > cp algolyx.sty $HOME/.lyx/layouts/packages 
  > export TEXINPUTS=:/$HOME/.lyx/layouts/packages//

4. Run LyX and reconfigure it using Options->Reconfigure.

5. Exit from LyX, and run it again. Once loaded, the new document class
  ``Article (algo)'' should be listed in the Layout->Document popup,
  and if so the install is successfully done.
  

4 Using the algorithm textclass

The algo.lyx (or algo.tex,ps) document describes how to use the layout.


5 Thanks

Thanks to the users, for their advice, comments, bug reports. And a
special thanks to Herbert Voss for his help!


6 Feedback

Please give your comments! It is the only way to efficiently improve
the textclass and fix the bugs. You can send e-mail to 
<nicolas.marsgui@libertysurf.fr>.

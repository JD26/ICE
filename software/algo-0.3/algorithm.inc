# Algorithm textclass definition file. Taken from initial LyX source code
# Author:  BG <nicolas.marsgui@libertysurf.fr>
# Version: 0.3

# This file contains a text style related to the algorithm and algorithmic
# packages, in order to write some algorithms through lyx.

Style Algorithm
#  Margin               First_Dynamic
  LatexType             Item_Environment
  LatexName             algor
  NextNoIndent          1
  LeftMargin            MM
  LabelSep              xxx
  ItemSep               0
  TopSep                0.5
  BottomSep             0.5
  ParSep                0
  Align                 Block
  AlignPossible         Block, Left
  LabelType             Manual
  LabelString           MM
  FreeSpacing           0

  # label font definition
  LabelFont
    Series              Bold
  EndFont

  Preamble
    \usepackage{algolyx}
  EndPreamble
End

Style Algorithm_(num)
  CopyStyle             Algorithm
  LatexParam            [1]

End


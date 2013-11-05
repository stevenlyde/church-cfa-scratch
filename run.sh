#!/usr/bin/env sh

racket analyze.rkt

if [ $? -eq 0 ]
then 
  echo SUCCESS
#  dot -Tpdf state-space.gv > state-space-$$.pdf
#  open state-space-$$.pdf
else
  echo FAILURE
fi

#!/bin/bash

PORT=$1
CPP_VERSION=src/lib/version.cc
PROLOG_VERSION=src/prolog/boxer/version.pl

(
echo    'namespace NLP {'
echo -n '  const char *VERSION = "'

if [[ -a .svn ]]; then
  svnversion -n . | tr -d '\n';
else
  tr -d '\n' < RELEASE.txt;
fi

echo '";'
echo

echo -n '  const char *BUILD = "('
echo -n $PORT 'build on' `date '+%e %B %Y, %T' | sed 's/^ //'`
echo ')";'

echo '}'

) > $CPP_VERSION

(
echo ':- module(version,[version/1]).'
echo -n "version('boxer "

if [[ -a .svn ]]; then
  svnversion -n . | tr -d '\n';
else
  tr -d '\n' < RELEASE.txt;
fi

echo -n ' ('
echo -n $PORT 'build on' `date '+%e %B %Y, %T' | sed 's/^ //'`
echo ")').";

) > $PROLOG_VERSION

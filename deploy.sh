#!/bin/bash

set -o errexit -o nounset
#PKG_TARBALL=$(ls -1t *.tar.gz | head -n 1)

addToDrat(){
  PKG_REPO=$PWD
  cd ..; mkdir drat; cd drat

  ## Set up Repo parameters
  git init
  git config user.name "Nima Hejazi"
  git config user.email "nh@nimahejazi.org"
  git config --global push.default simple

  ## Get drat repo
  git remote add upstream "https://$GH_TOKEN@github.com/jeremyrcoyle/sl3.git"
  git fetch upstream 2>err.txt
  git checkout gh-pages
  #git reset --hard "275c0dcce5977431a9df7e35e822bbd4fb7468fe"

  Rscript -e "drat::insertPackage('$PKG_REPO/$PKG_TARBALL', \
    repodir = '.', \
    commit='Travis publish update: build $TRAVIS_BUILD_NUMBER')"
  git push gh-pages 2> /tmp/err.txt
}
addToDrat


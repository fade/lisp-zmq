#!/bin/sh -e

files=$(git ls-files)
version=$(./version.sh)
dirname=lisp-zmq-$version
tarball=$dirname.tgz

git archive --format=tar --prefix=$dirname/ HEAD | gzip >| $tarball

tar tzf $tarball
echo
md5sum $tarball

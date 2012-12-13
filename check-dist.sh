#!/bin/bash

cabal sdist
version=$(gawk '/^Version:/ {print $2}' HTF.cabal)

echo "HTF version is $version"

tmpdir=$(mktemp -d /tmp/HTF-XXXXXX)
targz="HTF-$version.tar.gz"
cp "dist/$targz" "$tmpdir"
pushd "$tmpdir" > /dev/null
tar xfz "$targz"
pushd "HTF-$version" > /dev/null

echo "Now in directory $(pwd), running tests ..."

./run-all-tests-for-all-compilers.sh || exit 1

popd > /dev/null
popd > /dev/null
rm -rf "$tmpdir"

echo "Tests finished, everything ok!"
echo
echo "* Check git status:"
git status
echo
echo "* Add and commit"
echo
echo "* Create a tag:"
echo
echo "git tag -a $version -m \"version $version\""
echo
echo "* Push"
echo
echo "* Upload to hackage:"
echo
echo "cabal upload dist/$targz"
echo
echo "* Have a beer ;-)"
echo

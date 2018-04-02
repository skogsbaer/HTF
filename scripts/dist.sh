#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "USAGE: $0 [--no-check]"
    echo "Creates an dist tarball and checks that it builds."
    exit 1
fi

no_check=no
if [ "$1" == "--no-check" ]; then
    no_check=yes
fi

version=$(gawk -F: '/^Version:/ { print $2 }' HTF.cabal | sed 's/ //g')

echo "Building version $version"

# Somehow, --test-tarball always because of a strange permission error. That's why
# we do the checks here by hand.

stack sdist . --no-test-tarball || exit 1
tarball=$(find $(pwd)/.stack-work/dist -name "*${version}.tar.gz" | xargs ls -t | head -1)

echo "Distribution tarball is $tarball"

tmp=$(mktemp -d)
tar -C "$tmp" -x -z -f "$tarball" || exit 1

if [ "$no_check" == "yes" ]; then
    echo "Skipping tests as requested"
else
    pushd "${tmp}/HTF-${version}" > /dev/null
    echo "Running checks in directory $(pwd)"
    scripts/check.sh || exit 1
    popd > /dev/null
fi

git_tag="release/${version}"
if ! git tag --points-at HEAD | grep -E "^$git_tag$" > /dev/null; then
    git tag "$git_tag" || exit 1
    echo "Tagged HEAD with $git_tag"
fi

echo
echo
echo 'All checks succeeded, now upload to archive using the following command:'
echo '$ stack upload . --no-test-tarball'
echo
echo 'And do not forget to push all tags:'
echo '$ git push --tags'

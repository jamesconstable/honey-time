#! /usr/bin/env bash

# Delete site directory if already in existence
if [ -d site ]; then
  rm -rf site
fi

# Copy static files into site
mkdir site
cp static/* site/

# Compile PureScript
spago build
spago bundle
mv index.js site/

# Compile and run the SVG generator
pushd svg-generation
if [ -d output ]; then
  rm -rf output
fi
mkdir output
stack run > output/clock.svg
popd

# Assemble the SVG and HTML into a simple page
./assemble_page.py template.html > site/index.html

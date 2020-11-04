#! /usr/bin/env bash

# Delete site directory if already in existence
if [ -d site ]; then
  rm -rf site
fi

# Copy static files into site
mkdir site
cp static/* site/

# Compile PureScript
spago bundle --main Main --to site/index.js

# Compile and run the SVG generator
pushd svg-generation
if [ -d output ]; then
  rm -rf output
fi
mkdir output
stack run > output/clock.svg
popd

# Assemble the SVG and HTML into a single page
python3 assemble_page.py template.html > site/index.html

#! /usr/bin/env bash

set -e

# Delete site directory if already in existence
if [ -d site ]; then
  rm -rf site
fi

# Compile SCSS into site directory
mkdir site
sass style.scss site/style.css

# Compile PureScript
pushd purescript
spago build
spago bundle-app --main Main --to ../site/index.js
popd

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

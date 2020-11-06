#! /usr/bin/env python3

import re
import sys

matcher = re.compile(r'\s*<!-- INCLUDE "([^"]+)" -->')

for line in open(sys.argv[1]):
    match = matcher.match(line)
    if match:
        print(open(match.group(1)).read())
    else:
        print(line, end='')

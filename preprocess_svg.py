#! /bin/bash/env python3

"""
Preprocessor for the Vectornator SVG files containing the UI icons and glyphs.

Usage: python3 preprocess_svg.py

This script will look for its input files in svg-generation/assets/ and output
the results to svg-generation/output/ with the same file names. It is not
intended to be robust to variations in the input format, as it merely performs a
file-specific surface-level correction of some of the problems that cannot be
fixed within Vectornator itself, like multi-path masking and obligatory stroke
styles.
"""

import re

def process_myth_role_icons():
    vectornator_matcher = re.compile(r'<vectornator[^>]*>')
    open_group_matcher = re.compile(r'<g id="(?P<id>[^"]*)" [^>]*>')
    close_group_matcher = re.compile(r'</g>')
    stroke_width_matcher = re.compile(r' stroke-width="[^"]*"')
    stroke_matcher = re.compile(r' stroke-color="[^"]*"')

    output_file = open('svg-generation/output/myth-role-icons.svg', 'w')
    def write(text, end='\n'):
        print(text, end=end, file=output_file)

    current_id = None
    for line in open('svg-generation/assets/myth-role-icons.svg'):
        vectornator = vectornator_matcher.match(line)
        start = open_group_matcher.match(line)
        end = close_group_matcher.match(line)

        if start:
            current_id = start.group('id')
            write('<g id="' + current_id + '" stroke-width="5">')
            write('<defs><mask id="' + current_id + '-mask">')
        elif end:
            write('</mask></defs>')
            write('<rect x="-10" y="-10" width="120" height="120" mask="url(#' +
                current_id + '-mask)" fill="black"></g>')
        elif vectornator:
            pass
        else:
            line = (line
                .replace('#000000', 'white')
                .replace('#ffffff', 'black'))
            line = stroke_width_matcher.sub('', line)
            write(line, end='')

if __name__ == "__main__":
    process_myth_role_icons()

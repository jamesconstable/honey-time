#! /bin/bash/env python3

'''
Preprocessor for the Vectornator SVG files containing the UI icons and glyphs.

Usage: python3 preprocess_svg.py

This script will look for its input files in svg-generation/assets/ and output
the results to svg-generation/output/ with the same file names. It is not
intended to be robust to variations in the input format, as it merely performs a
file-specific surface-level correction of some of the problems that cannot be
fixed within Vectornator itself, like multi-path masking, obligatory stroke
styles, and missing parameters in radial gradients.
'''

import re

vectornator_matcher = re.compile(r'<vectornator[^>]*>')
open_group_matcher = re.compile(r'<g id="(?P<id>[^"]*)" [^>]*>')
close_group_matcher = re.compile(r'</g>')
stroke_width_matcher = re.compile(r' stroke-width="[^"]*"')
stroke_linecap_matcher = re.compile(r' stroke-linecap="[^"]*"')
stroke_linejoin_matcher = re.compile(r' stroke-linejoin="[^"]*"')
stroke_matcher = re.compile(r' stroke-color="[^"]*"')
gradient_matcher = re.compile(r'<radialGradient(?P<pre>.*) r="0"(?P<post>.*)>')
id_matcher = re.compile(r'.* id="([^"]*)".*')

def file_print(filename):
    f = open(filename, 'w')
    def print_fn(*args, **kwargs):
        print(*args, **kwargs, file=f)
    return print_fn

def process_numeral_icons():
    write = file_print('svg-generation/output/honey-numerals.svg')
    is_first_path = False
    for line in open('svg-generation/assets/honey-numerals.svg'):
        start = open_group_matcher.match(line)
        if start:
            is_first_path = True
            write(''.join([
                '<g id="', start.group('id') +
                '" stroke-linecap="round" stroke-linejoin="round" ',
                'stroke-width="5">']))
        elif vectornator_matcher.match(line):
            pass
        else:
            # The first path in each group is the background numeral shape,
            # which needs to inherit its parent's fill.
            if is_first_path:
                line = line.replace(' fill="none"', '')
            is_first_path = False

            line = stroke_width_matcher.sub('', line)
            line = stroke_linecap_matcher.sub('', line)
            line = stroke_linejoin_matcher.sub('', line)
            write(line, end='')

def process_letter_icons():
    write = file_print('svg-generation/output/honey-letters.svg')
    for line in open('svg-generation/assets/honey-letters.svg'):
        start = open_group_matcher.match(line)
        if start:
            write(''.join([
                '<g id="', start.group('id') +
                '" stroke-linecap="round" stroke-linejoin="round" ',
                'stroke-width="10">']))
        elif vectornator_matcher.match(line):
            pass
        else:
            line = stroke_width_matcher.sub('', line)
            line = stroke_linecap_matcher.sub('', line)
            line = stroke_linejoin_matcher.sub('', line)
            write(line, end='')

def process_myth_role_icons():
    write = file_print('svg-generation/output/myth-role-icons.svg')
    current_id = None
    for line in open('svg-generation/assets/myth-role-icons.svg'):
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
        elif vectornator_matcher.match(line):
            pass
        else:
            line = (line
                .replace('#000000', 'white')
                .replace('#ffffff', 'black'))
            line = stroke_width_matcher.sub('', line)
            write(line, end='')

def process_season_icons():
    write = file_print('svg-generation/output/season-icons.svg')
    current_id = None
    for line in open('svg-generation/assets/season-icons.svg'):
        start = open_group_matcher.match(line)
        end = close_group_matcher.match(line)
        if start:
            current_id = start.group('id')
            write('<g id="' + current_id + '" stroke-width="10">')
            write('<defs><mask id="' + current_id + '-mask">')
        elif end:
            write('</mask></defs>')
            write('<rect x="-10" y="-10" width="120" height="120" mask="url(#' +
                current_id + '-mask)" fill="black"></g>')
        elif vectornator_matcher.match(line):
            pass
        else:
            line = (line
                .replace('#000000', 'white')
                .replace('#ffffff', 'black'))
            line = stroke_width_matcher.sub('', line)
            write(line, end='')

def process_sun_moon():
    radius_lookup = {
        'RadialGradient':   18,
        'RadialGradient_2':  9,
        'RadialGradient_3': 15,
        'RadialGradient_4':  1,
        'RadialGradient_5':  1,
        'RadialGradient_6': 15,
        'RadialGradient_7':  1,
        'RadialGradient_8':  1,
    }
    write = file_print('svg-generation/output/sun-moon.svg')
    for line in open('svg-generation/assets/sun-moon.svg'):
        match = gradient_matcher.match(line)
        if match :
            # Vectornator 4.5.1 exports all radial gradients with a radius of 0
            # (seems to be bug), so we need to manually hack in the correct
            # values.
            write(''.join(['<radialGradient', match.group('pre'),
                ' r="', str(radius_lookup[id_matcher.match(line).group(1)]),
                '"', match.group('post'), '>']), end='')
        elif vectornator_matcher.match(line):
            continue
        else:
            write(line, end='')

if __name__ == "__main__":
    process_numeral_icons()
    process_letter_icons()
    process_myth_role_icons()
    process_season_icons()
    process_sun_moon()

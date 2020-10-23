#! /usr/bin/env python3

from math import sin, cos, pi, sqrt
import svgwrite

def get_hexagon(x, y, radius, rotation, defs=None):
  # If the requested shape has already been generated for this document,
  # return it directly.
  cache_key = ('hexagon', (x, y, radius, rotation))
  if defs != None and cache_key in defs:
    return defs[cache_key]

  points = []
  theta = pi / 2 - rotation
  for i in range(6):
    points.append((x + radius * cos(theta - i*pi/3),
                   y - radius * sin(theta - i*pi/3)))
  shape = svgwrite.shapes.Polygon(points=points)

  # Cache the shape if requested.
  if defs != None:
    defs[cache_key] = shape

  return shape

def get_hexagon_floret(hex_radius, defs=None):
  group = svgwrite.container.Group()
  hex_tile = get_hexagon(0, 0, hex_radius, pi/6, defs)
  usage = svgwrite.container.Use(hex_tile, class_='floret-center')
  group.add(usage)

  theta = -pi / 2
  short_radius = sqrt(3/4 * hex_radius * hex_radius)
  for i in range(6):
    usage = svgwrite.container.Use(hex_tile, class_='pos%d' % i)
    usage.translate(2 * short_radius * cos(theta + i*pi/3),
                    2 * short_radius * sin(theta + i*pi/3))
    group.add(usage)
  return group

def draw_minutes_ring(drawing, hex_radius, defs):
  group = svgwrite.container.Group(class_='minutes-ring')
  theta = -pi / 2
  short_radius = 6 * sqrt(3/4 * hex_radius * hex_radius)
  for i in range(6):
    floret = get_hexagon_floret(hex_radius, defs=defs)
    floret['class'] = 'pos%d' % i
    floret.translate(short_radius * cos(theta + i*pi/3),
                     short_radius * sin(theta + i*pi/3))
    group.add(floret)
  drawing.add(group)

def draw_subseconds_rings(drawing, hex_radius, defs):
  # Draw the 1/36th second ring.
  floret = get_hexagon_floret(hex_radius, defs=defs)
  floret['class'] = 'subsecond-units-ring'
  drawing.add(floret)

  # Draw the 1/6th second ring.
  group = svgwrite.container.Group(class_='subsecond-hexes-ring')
  hex_tile = get_hexagon(0, 0, hex_radius, pi/6, defs)
  radius = 3 * hex_radius
  theta = -pi / 3
  for i in range(6):
    usage = svgwrite.container.Use(hex_tile, class_='pos%d' % i)
    usage.translate(radius * cos(theta + i*pi/3),
                    radius * sin(theta + i*pi/3))
    group.add(usage)
  drawing.add(group)

def draw_seconds_ring(drawing, hex_radius, defs):
  group = svgwrite.container.Group(class_='seconds-ring')
  hex_tile = get_hexagon(0, 0, hex_radius, pi/6, defs)
  radius = 10 * sqrt(3/4 * hex_radius * hex_radius)
  theta = -pi / 2
  usage = svgwrite.container.Use(hex_tile, class_='pos%d' % 0)
  usage.translate(radius * cos(theta),
                  radius * sin(theta))
  group.add(usage)
  drawing.add(group)

def push_arc(path, target_angle, radius, rotation):
  path.push_arc(
      (radius * cos(target_angle), radius * sin(target_angle)),
      abs(rotation),
      (radius, radius),
      large_arc=False,
      angle_dir='+' if rotation > 0 else '-',
      absolute=True)

def draw_hours_ring(drawing, hex_radius, defs):
  inner_radius = hex_radius * 8
  outer_radius = hex_radius * 9
  theta = -pi / 2
  phi = pi / 5
  group = svgwrite.container.Group(class_='hours-ring')
  for i in range(10):
    start = theta + i*phi
    end = theta + (i+1) * phi
    segment = svgwrite.path.Path(class_='pos%d' % i)
    segment.push('M', outer_radius * cos(start), outer_radius * sin(start))
    push_arc(segment, end, outer_radius, phi)
    segment.push('L', inner_radius * cos(end), inner_radius * sin(end))
    push_arc(segment, start, inner_radius, -phi)
    segment.push('Z')
    group.add(segment)
  drawing.add(group)

def main():
  hex_radius = 15
  drawing = svgwrite.Drawing('clock-dial.svg', profile='tiny',
                             class_='clock-dial',
                             size=(320, 320))
  defs = {}
  drawing.defs.add(get_hexagon(0, 0, hex_radius, pi/6, defs=defs))
  img = svgwrite.container.Group()
  draw_hours_ring(img, hex_radius, defs)
  draw_minutes_ring(img, hex_radius, defs)
  draw_seconds_ring(img, hex_radius, defs)
  draw_subseconds_rings(img, hex_radius, defs)
  img.translate(hex_radius * 9, hex_radius * 9)
  drawing.add(img)
  drawing.save()

if __name__ == '__main__':
  main()

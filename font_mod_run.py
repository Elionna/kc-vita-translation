#! /usr/bin/env python
# -*- coding: utf-8 -*-

from font_manipulator import *
import itertools
import sys

start_unicode = "0xE000"
unicode_point_int = int(start_unicode, 0)
pairs = [line.rstrip('\n').split("|") for line in open('font_mod_character_pairs')]
pairs = filter(None, list(itertools.chain.from_iterable(pairs)))

fontname = sys.argv[1]
M20 = Schrift(fontname, "../fonts/"+fontname+".sfdir", "../fonts/"+fontname+"_mod.sfdir")

for pair in pairs:
    name = "uni%X" % unicode_point_int
    M20._ersetzen_durch_Kompositglyph(name, list(pair))
    unicode_point_int = unicode_point_int + 1

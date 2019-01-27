# adapted from https://svn.code.sf.net/p/unifraktur/code

# -*- coding: utf-8 -*-

import fileinput
import os
import shutil
import ast

def string_auswerten(string):
	try:
		string = ast.literal_eval(string)
	except ValueError:
		pass
	return string

def glyph_nach_datei(glyph):
	if glyph.startswith("uni"):
		return glyph + ".glyph"
	elif glyph == " ":
		return "space.glyph"
	elif glyph == "-":
		return "uni2010.glyph"
	elif glyph == "0":
		return "zero.glyph"
	elif glyph == "1":
		return "one.glyph"
	elif glyph == "2":
		return "two.glyph"
	elif glyph == "3":
		return "three.glyph"
	elif glyph == "4":
		return "four.glyph"
	elif glyph == "5":
		return "five.glyph"
	elif glyph == "6":
		return "six.glyph"
	elif glyph == "7":
		return "seven.glyph"
	elif glyph == "8":
		return "eight.glyph"
	elif glyph == "9":
		return "nine.glyph"
	elif glyph == "'":
		return "quotesingle.glyph"
	elif glyph == ".":
		return "period.glyph"
	elif glyph == "(":
		return "parenleft.glyph"
	elif glyph == ")":
		return "parenright.glyph"
	elif glyph == "!":
		return "exclam.glyph"
	elif glyph == "\"":
		return "quotedbl.glyph"
	elif glyph == "+":
		return "plus.glyph"
	else:
		chars = list(glyph)
		i = 0
		while i < len(chars):
			if chars[i].isupper():
				chars.insert(i, "_")
				i+=1
			i+=1
		return "".join(chars) + ".glyph"

def make_pairs(l):
    for i in range(0, len(l), 2): # For item i in a range that is a length of l,
        yield l[i:i+2]            # Create an index range for l of n items:

def stems_zerlegen(stemstring):
	ausgabe = []
	for stem in stemstring.split(">"):
		if stem.strip():
			stem_parts = stem.split("<")
			ebene = stem_parts[0].split()
			if len(ebene) != 2:
				ebene_pairs = list(make_pairs(ebene))
				while len(ebene_pairs) > 1:
					ausgabe += [[ebene_pairs.pop(0)]]
				ebene = ebene_pairs.pop(0)
			if len(stem_parts) == 2:
				positionen_roh = stem_parts[1].split()
				positionen = map(list,zip(*[iter(positionen_roh)]*2))
				ausgabe += [[ebene, positionen]]
			else:
				ausgabe += [[ebene]]
	return ausgabe

def stems_vereinen(stemslist):
	ausgabe = []
	for stem in [stem for stems in stemslist for stem in stems]:
		neu = True
		for bestehender_stem in ausgabe:
			if stem[0] == bestehender_stem[0]:
				if len(stem) == 2:
					bestehender_stem[1] += stem[1]
				neu = False
				break
		if neu:
			ausgabe += [stem]
	return ausgabe

def stems_nach_string(stems):
	ausgabe = ""
	for stem in stems:
		ausgabe += " ".join(stem[0])
		if len(stem) == 2:
			ausgabe += "<"
			ausgabe += " ".join([komp for position in stem[1] for komp in position])
			ausgabe += ">"
		ausgabe += " "
	return ausgabe

class Schrift:
	def __init__(self, name, quellordner, zielordner):
		self.name = name
		self.ordner = zielordner
		if not os.path.isdir(self.ordner):
			os.mkdir(self.ordner)
			for datei in os.listdir(quellordner):
				if os.path.isfile(quellordner + "/" + datei):
					shutil.copy(quellordner + "/" + datei, self.ordner)

		self.props = []
		self.props_oeffnen()
		for i,zeile in enumerate(self.props):
			for nameString in ["FontName:", "FullName:", "FamilyName:"]:
				if zeile.startswith(nameString):
					self.props[i] = nameString + " " + self.name + "\n"

	def __del__(self):
		self.props_schliessen()
	
	def props_oeffnen(self):
		if not self.props:
			with open(self.ordner + "/font.props", "rb") as propsdatei:
				self.props = propsdatei.readlines()
	
	def props_schliessen(self):
		if self.props:
			with open(self.ordner + "/font.props", "wb") as propsdatei:
				propsdatei.writelines(self.props)

	def ersetzen_durch_Kompositglyph(self, ziel, teil1, teil2):
		teile = (teil1,teil2)
		self._ersetzen_durch_Kompositglyph(ziel, teile)

	def _ersetzen_durch_Kompositglyph(self, ziel, teile):
		breiten = []
		encodings = []
		hstems = []
		vstems = []
		dstems = []
		verschiebungen = []
		verschiebungen.append(0)
		
		self.props_oeffnen()
		in_kerningtabelle = True
		anzahl_vor = None
		kerning_start = None
		indizes = [None, None]
		j = None
		kerning = 0
		for i,zeile in enumerate(self.props):
			if zeile.startswith("KernClass2"):
				in_kerningtabelle = True
				anzahl_vor = int(zeile.split()[1])
				j = anzahl_vor-1
				anzahl_nach = int(zeile.split()[2])
				kerning_start = i
			elif in_kerningtabelle:
				if zeile.startswith(" "):
					if ("{}" in zeile):
						if all(indizes):
							a = indizes[0] - kerning_start
							b = indizes[1] - kerning_start - anzahl_vor + 1
							kerning = int(zeile.split("{}")[a*anzahl_nach+b])
					else:
						inhalt = zeile.split()[1:]
						if teile[j>0] in inhalt:
							inhalt += [ziel]
						if teile[j<=0] in inhalt:
							indizes[j<=0] = i
						neuer_inhalt = " ".join(inhalt)
						self.props[i] = " " + str(len(neuer_inhalt)) + " " + neuer_inhalt + "\n"
						j -= 1
						continue
				
				in_kerningtabelle = False
				kerning_start = None
				anzahl_vor = None
				indizes = [None, None]
				j = None

		for i,teil in enumerate(teile):
			for zeile in open(self.ordner + "/" + glyph_nach_datei(teil), "rb"):
				if zeile.startswith("Width:"):
					breiten += [int(zeile.split()[1])]
				elif zeile.startswith("Encoding:"):
					encodings += [zeile.split()[2:]]
				elif zeile.startswith("HStem:"):
					hstems += [stems_zerlegen(zeile[7:-1])]
				elif zeile.startswith("VStem:"):
					vstems += [stems_zerlegen(zeile[7:-1])]
				elif zeile.startswith("DStem2:"):
					dstems += [stems_zerlegen(zeile[8:-1])]
		
		zieldatei = glyph_nach_datei(ziel)
		
		first = True
		for j,teil in enumerate(teile):
			if(first):
				first = False
				continue

			verschiebung = kerning
			r = range(0, j)
			for x in r:
				verschiebung = verschiebung + breiten[x]
			verschiebungen.append(verschiebung)

			if j < len(hstems):
				for hstem in hstems[j]:
					if len(hstem) == 2:
						for position in hstem[1]:
							for i in (0,1):
								position[i] = str(string_auswerten(position[i]) + verschiebung)

			if j < len(vstems):
				for vstem in vstems[j]:
					vstem[0][0] = str(string_auswerten(vstem[0][0]) + verschiebung)

			if j < len(dstems):
				for dstem in dstems[j]:
					for i in (0,2):
						dstem[0][i] = str(string_auswerten(dstem[0][i]) + verschiebung)
		
		for zeile in fileinput.input (self.ordner + "/" + zieldatei, inplace=1):
			if (
					   zeile.startswith("StartChar:")
					or zeile.startswith("Encoding:")
					or zeile.startswith("VWidth:")
					or zeile.startswith("GlyphClass:")
					or zeile.startswith("Flags:")
					or zeile.startswith("EndChar")
				):
				print zeile,
			elif zeile.startswith("Fore"):
				print zeile,
				for j,teil in enumerate(teile):
					print "Refer: %s %s N 1 0 0 1 %s 0 2" % (encodings[j][1], encodings[j][0], verschiebungen[j])
			elif zeile.startswith("Width:"):
				print "Width: %i" % (sum(map(int, breiten))+kerning)
			elif zeile.startswith("LayerCount:"):
				print zeile,
				
				stemstring = stems_nach_string(stems_vereinen(hstems))
				if stemstring:
					print "HStem: " + stems_nach_string(stems_vereinen(hstems))
					
				stemstring = stems_nach_string(stems_vereinen(vstems))
				if stemstring:
					print "VStem: " + stems_nach_string(stems_vereinen(vstems))
					
				stemstring = stems_nach_string(stems_vereinen(dstems))
				if stemstring:
					print "DStem2: " + stems_nach_string(stems_vereinen(dstems))

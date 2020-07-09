"""
iri2uri

Converts an IRI to a URI.

"""
# __author__ = "Joe Gregorio (joe@bitworking.org)"
# __copyright__ = "Copyright 2006, Joe Gregorio"
# __contributors__ = []
# __version__ = "1.0.0"
# __license__ = "MIT"
# __history__ = """
# """

import urllib.parse
from retic import List, Tuple, Int, String

# Convert an IRI to a URI following the rules in RFC 3987
#
# The characters we need to enocde and escape are defined in the spec:
#
# iprivate =  %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
# ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
#         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
#         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
#         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
#         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
#         / %xD0000-DFFFD / %xE1000-EFFFD

@fields({escape_range: List(Tuple(Int, Int))})
class Iri2Uri:

  escape_range = [
      (0xA0, 0xD7FF),
      (0xE000, 0xF8FF),
      (0xF900, 0xFDCF),
      (0xFDF0, 0xFFEF),
      (0x10000, 0x1FFFD),
      (0x20000, 0x2FFFD),
      (0x30000, 0x3FFFD),
      (0x40000, 0x4FFFD),
      (0x50000, 0x5FFFD),
      (0x60000, 0x6FFFD),
      (0x70000, 0x7FFFD),
      (0x80000, 0x8FFFD),
      (0x90000, 0x9FFFD),
      (0xA0000, 0xAFFFD),
      (0xB0000, 0xBFFFD),
      (0xC0000, 0xCFFFD),
      (0xD0000, 0xDFFFD),
      (0xE1000, 0xEFFFD),
      (0xF0000, 0xFFFFD),
      (0x100000, 0x10FFFD),
  ]

  #bg: really, Char->Char, but that's not a Python type
  def encode(self:Iri2Uri, c:String)->String:
      retval = c
      i = ord(c)
      for low, high in self.escape_range:
          if i < low:
              break
          if i >= low and i <= high:
              retval = "".join(["%%%2X" % o for o in c.encode('utf-8')])
              break
      return retval

  #bg: really, url:(bytes | string)
  def iri2uri(self:Iri2Uri, uri:String)->String:
      """Convert an IRI to a URI. Note that IRIs must be
      passed in a unicode strings. That is, do not utf-8 encode
      the IRI before passing it into the function."""
      if isinstance(uri ,str):
          (scheme, authority, path, query, fragment) = urllib.parse.urlsplit(uri)
          authority = authority.encode('idna').decode('utf-8')
          # For each character in 'ucschar' or 'iprivate'
          #  1. encode as utf-8
          #  2. then %-encode each octet of that utf-8
          uri = urllib.parse.urlunsplit((scheme, authority, path, query, fragment))
          uri = "".join([self.encode(c) for c in uri])
          return uri
      else:
          raise ValueError(uri)

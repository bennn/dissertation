#bg removed sha224 and sha384



# Copyright (C) 2011 by Stefano Palazzo
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

'''
    Pure Python Implementation of SHA1/SHA2

    Create a hash by calling one of the named constructor functions:
        sha1(), sha224(), sha256(), sha384(), and sha512().

    The resulting hash objects have these methods:

     - digest():       Return the digest of the message

     - hexdigest():    Like digest() except the digest is
                       returned as a string of double length,
                       containing only hexadecimal digits.


    For example, to obtain the digest of the string 'Hello World':

        >>> import slowsha
        >>> m = slowsha.sha1(b"Hello World")
        >>> m.digest()
        b'\\nMU\\xa8\\xd7x\\xe5\\x02/\\xabp\\x19w\\xc5\\xd8@\\xbb\\xc4\\x86\\xd0'

    More condensed:

        >>> slowsha.sha224(b"Hello World").hexdigest()
        'c4890faffdb0105d991a461e668e276685401b02eab1ef4372795047'

'''



def new(algorithm : String, message : Bytes) -> {'digest':Function([], Bytes)}:
    obj = {
        'sha1': SHA1,
        'sha512': SHA512,
    }[algorithm](message)
    return obj


def sha1(message : Bytes) -> SHA1:
    ''' Returns a new sha1 hash object '''
    x = new('sha1', message)
    return x


def sha512(message : Bytes) -> SHA512:
    ''' Returns a new sha512 hash object '''
    return new('sha512', message)

## -----------------------------------------------------------------------------

from Timer import Timer
from sha1 import SHA1
from sha512 import SHA512
import os

def main()->Void:
    #bg To unit test, compare against hashlib

    with open(os.path.join(os.path.dirname(__file__), "mysterious_words.txt"), "rb") as f:
      for line in f:
        for word in line.split():
          sha1(word).hexdigest()
          sha1(word).digest()
          sha512(word).hexdigest()
          sha512(word).digest()
    return None

t = Timer()
with t:
  main()

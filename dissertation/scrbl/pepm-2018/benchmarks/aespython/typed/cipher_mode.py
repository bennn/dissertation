"""
Cipher Mode of operation

Running this file as __main__ will result in a self-test of the algorithm.

Algorithm per NIST SP 800-38A http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf

Copyright (c) 2010, Adam Newman http://www.caller9.com/
Licensed under the MIT license http://www.opensource.org/licenses/mit-license.php
"""
__author__ = "Adam Newman"

@fields({"name":str
        ,"_block_cipher": Object("my_block_cipher", {"cipher_block":Function([List(Int)],List(int)),"decipher_block":Function([List(Int)],List(Int))})
        ,"_block_size": Int
        ,"_iv": List(Int)})
class CipherMode:
    """Perform Cipher operation on a block and retain IV information for next operation"""

    name = "ABSTRACT"

    def __init__(self:CipherMode, block_cipher:Object("my_block_cipher", {"cipher_block":Function([List(Int)],List(int)),"decipher_block":Function([List(Int)],List(Int))}), block_size : Int)->Void:
        self._block_cipher = block_cipher
        self._block_size = block_size
        self._iv = [0] * block_size

    def set_iv(self:CipherMode, iv : List(Int))->Void:
        if len(iv) == self._block_size:
            self._iv = iv

    def encrypt_block(self:CipherMode, plaintext : List(Int))->List(Int):
        raise(NotImplementedError, "Abstract function")

    def decrypt_block(self:CipherMode, ciphertext : List(Int))->List(Int):
        raise(NotImplementedError, "Abstract function")



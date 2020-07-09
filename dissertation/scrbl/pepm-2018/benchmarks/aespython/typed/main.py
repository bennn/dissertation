"""
Demonstration the pythonaes package. Requires Python 2.6 or 3.x

This program was written as a test. It should be reviewed before use on classified material.
You should also keep a copy of your original file after it is encrypted, all of my tests were
able to get the file back 100% in tact and identical to the original. 

___This is a demo program. Do not use in production*___

The method for creating the key and iv from a password is something that I made up, not an industry standard.
    There are 256 bits of salt pulled from OS's cryptographically strong random source.
    Any specific password will generate 2^128 different Keys.
    Any specific password will generate 2^128 different IVs independent of Key

On decryption, salt is read from first 32 bytes of encrypted file.

In the encrypted file, after salt(if present), are 4 bytes* representing file size. 4GB file size limit.
It would also take quite a while to process 4GB.

* For different sizes of 'L' (unsigned long) the size of the header will change causing incompatibility for
files encrypted/decrypted on a 64 bit system vs 32 bit. This could be resolved with a strict header format that 
is checksummed among other things, but this is a demo/test harness. It will work in that capacity and this does 
not affect the aespython module. This will often cause trailing blocks < 16 bytes to be truncated during
decryption.

Copyright (c) 2010, Adam Newman http://www.caller9.com/
Licensed under the MIT license http://www.opensource.org/licenses/mit-license.php
"""
__author__ = "Adam Newman"

import os
import struct

import key_expander
import aes_cipher
import cbc_mode

from Timer import Timer

@fields({"_salt": Void
        ,"_iv": List(int)
        ,"_key": List(int)
        ,"_python3": bool})
class AESdemo:
    def __init__(self:AESdemo)->Void:
        #bg# changed default values
        self._salt = None
        self._iv = []
        self._key = []
        self._python3 = True #bg# sys.version_info > (3, 0)

    def new_salt(self:AESdemo)->Void:
        self._salt = os.urandom(32)

    def set_iv(self:AESdemo, iv:List(int))->Void:
        self._iv = iv

    def set_key(self:AESdemo, key:List(int))->Void:
        self._key = key

    def decrypt_file(self:AESdemo, in_file_path:str, out_file_path:str)->bool:
        password = None #bg# was optional arg.
        with open(in_file_path, 'rb') as in_file:
            #If a password is provided, generate key and iv using salt from file.
            #bg#if password is not None:
            #bg#    self._salt = in_file.read (32)
            #bg#    self.create_key_from_password (password)
            #Key and iv have not been generated or provided, bail out
            if self._key is None or self._iv is None:
                return False
            #Initialize encryption using key and iv
            key_expander_256 = key_expander.KeyExpander(256)
            expanded_key = key_expander_256.expand(self._key)
            aes_cipher_256 = aes_cipher.AESCipher(expanded_key)
            aes_cbc_256 = cbc_mode.CBCMode(aes_cipher_256, 16)
            aes_cbc_256.set_iv(self._iv)
            #Read original file size
            filesize = struct.unpack('L',in_file.read(struct.calcsize('L')))[0]
            #Decrypt to eof
            with open(out_file_path, 'wb') as out_file:
                eof = False
                while not eof:
                    in_data = in_file.read(16)
                    if len(in_data) == 0:
                        eof = True
                    else:
                        out_data = aes_cbc_256.decrypt_block(list(bytearray(in_data)))
                        #At end of file, if end of original file is within < 16 bytes slice it out.
                        if filesize - out_file.tell() < 16:
                            out_file.write(bytes(out_data[:filesize - out_file.tell()]))
                        else:
                            out_file.write(bytes(out_data))
        self._salt = None
        return True

    def encrypt_file(self:AESdemo, in_file_path:str, out_file_path:str)->bool:
        password=None #bg# was optional arg
        #If a password is provided, generate new salt and create key and iv
        if password is not None:
            #bg#self.new_salt()
            #bg#self.create_key_from_password(password)
            pass #bg#
        else:
            self._salt = None
        #If key and iv are not provided are established above, bail out.
        if self._key is None or self._iv is None:
            return False
        #Initialize encryption using key and iv
        key_expander_256 = key_expander.KeyExpander(256)
        expanded_key = key_expander_256.expand(self._key)
        aes_cipher_256 = aes_cipher.AESCipher(expanded_key)
        aes_cbc_256 = cbc_mode.CBCMode(aes_cipher_256, 16)
        aes_cbc_256.set_iv(self._iv)
        #Get filesize of original file for storage in encrypted file
        #bg: removed exception handling
        filesize = os.stat(in_file_path)[6]
        with open(in_file_path, 'rb') as in_file:
            with open(out_file_path, 'wb') as out_file:
                #Write salt if present
                if self._salt is not None:
                    #bg#out_file.write(self._salt)
                    pass
                #Write filesize of original
                out_file.write(struct.pack('L',filesize))
                #Encrypt to eof
                eof = False
                while not eof:
                    in_data = in_file.read(16)
                    if len(in_data) == 0:
                        eof = True
                    else:
                        out_data = aes_cbc_256.encrypt_block([int(x) for x in bytearray(in_data)])
                        out_file.write(bytes(out_data))
        self._salt = None
        return True

t = Timer()
with t:
    to_encrypt = "aes_encrypt.in.txt"
    to_decrypt = "aes_decrypt.in.txt"
    encrypt_out_file = "aes_encrypt.out.txt"
    decrypt_out_file = "aes_decrypt.out.txt"
    key = [ord(c) for c in "This_key_for_demo_purposes_only!"]
    iv = [ord(c) for c in "InitializationVe"]

    demo = AESdemo()
    demo.set_key(key)
    demo.set_iv(iv)

    #print ('Encrypting', to_encrypt, 'to', encrypt_out_file)
    demo.encrypt_file( to_encrypt, encrypt_out_file)
    #print ('Decrypting', to_decrypt, 'to', decrypt_out_file)
    demo.decrypt_file( to_decrypt, decrypt_out_file)

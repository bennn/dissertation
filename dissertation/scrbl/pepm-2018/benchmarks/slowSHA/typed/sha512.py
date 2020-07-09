from sha2_64 import sha2_64

@fields({'h0': Int,'h7': Int, 'h6': Int, 'h5': Int,'h4': Int, 'h3': Int, 'h2': Int, 'h1': Int})  
class SHA512 (sha2_64):

    h0, h1, h2, h3, h4, h5, h6, h7 = (
        0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b,
        0xa54ff53a5f1d36f1, 0x510e527fade682d1, 0x9b05688c2b3e6c1f,
        0x1f83d9abfb41bd6b, 0x5be0cd19137e2179)

    def _digest(self:Self) -> Tuple(Int, Int, Int, Int, Int, Int, Int, Int):
        return (self.h0, self.h1, self.h2, self.h3,
            self.h4, self.h5, self.h6, self.h7)




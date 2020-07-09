@fields({'h0': Int,'h4': Int, 'h3': Int, 'h2': Int, 'h1': Int})  
class SHA1 (object):


    def __init__(self:Self, message:Bytes)->Void:
        self.h0, self.h1, self.h2, self.h3, self.h4, = (
            0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0)
        length = bin(len(message) * 8)[2:].rjust(64, "0")
        while len(message) > 64:
            self.handle(''.join(bin(i)[2:].rjust(8, "0")
                for i in message[:64]))
            message = message[64:]
        strmessage = ''.join(bin(i)[2:].rjust(8, "0") for i in message) + "1"
        strmessage += "0" * ((448 - len(strmessage) % 512) % 512) + length
        for i in range(len(strmessage) // 512):
            self.handle(bytes(strmessage[i * 512:i * 512 + 512], encoding="utf-8"))


    def handle(self: SHA1, chunk : Bytes)->Void:
        lrot = lambda x, n: (x << n) | (x >> (32 - n))
        w = []

        for j in range(len(chunk) // 32):
            w.append(int(chunk[j * 32:j * 32 + 32], 2))

        for i in range(16, 80):
            w.append(lrot(w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16], 1)
                & 0xffffffff)

        a = self.h0
        b = self.h1
        c = self.h2
        d = self.h3
        e = self.h4

        for i in range(80):

            if i <= i <= 19:
                f, k = d ^ (b & (c ^ d)), 0x5a827999
            elif 20 <= i <= 39:
                f, k = b ^ c ^ d, 0x6ed9eba1
            elif 40 <= i <= 59:
                f, k = (b & c) | (d & (b | c)), 0x8f1bbcdc
            elif 60 <= i <= 79:
                f, k = b ^ c ^ d, 0xca62c1d6

            temp = lrot(a, 5) + f + e + k + w[i] & 0xffffffff
            a, b, c, d, e = temp, a, lrot(b, 30), c, d

        self.h0 = (self.h0 + a) & 0xffffffff
        self.h1 = (self.h1 + b) & 0xffffffff
        self.h2 = (self.h2 + c) & 0xffffffff
        self.h3 = (self.h3 + d) & 0xffffffff
        self.h4 = (self.h4 + e) & 0xffffffff

    def _digest(self:Self) -> Tuple(Int, Int, Int, Int, Int):
        return (self.h0, self.h1, self.h2, self.h3, self.h4)

    def hexdigest(self:Self) -> String:
        return ''.join(hex(i)[2:].rjust(8, "0")
            for i in self._digest())

    def digest(self:Self) -> Bytes:
        hexdigest = self.hexdigest()
        return bytes(int(hexdigest[i * 2:i * 2 + 2], 16)
            for i in range(len(hexdigest) // 2))




@fields({'hd': Dyn, 'tl': Dyn})
class Stream:
    def __init__(self, n):
      self.hd = n
      self.tl = [n + 1]

def helper(f):
    return f(Stream(3))


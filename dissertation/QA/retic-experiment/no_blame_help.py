class Stream:
    def __init__(self, n):
      self.hd = n
      self.tl = [n + 1]

def helper(f:Function([], List(str)))->List(str):
    return f()


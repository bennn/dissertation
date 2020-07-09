from Timer import Timer
"""
bg:
- add `ITERATIONS` constant
- remove `main` function
- add missing type annotations
- replace `xrange` with `range`
- remove unused imports
"""

from math import sin, cos, sqrt

@fields({'x':float, 'y':float, 'z':float})
class Point(object):

    def __init__(self:Point, i:float)->Void:
        self.x = x = sin(i)
        self.y = cos(i) * 3
        self.z = (x * x) / 2

    def normalize(self:Point)->Void:
        x = self.x
        y = self.y
        z = self.z
        norm = sqrt(x * x + y * y + z * z)
        self.x /= norm
        self.y /= norm
        self.z /= norm

    def maximize(self:Point, other:Point)->Point:
        self.x = self.x if self.x > other.x else other.x
        self.y = self.y if self.y > other.y else other.y
        self.z = self.z if self.z > other.z else other.z
        return self


def maximize(points:List(Point))->Point:
    next = points[0]
    for p in points[1:]:
        next = next.maximize(p)
    return next

def benchmark(n:int)->Point:
    points = [Point(i) for i in range(n)]
    for p in points:
        p.normalize()
    return maximize(points)

POINTS = 200000

if __name__ == "__main__":
    t = Timer()
    with t:
        benchmark(POINTS)

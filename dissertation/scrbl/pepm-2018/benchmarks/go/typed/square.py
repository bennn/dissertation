import random
from constants import SIZE, GAMES, KOMI, EMPTY, WHITE, BLACK, SHOW, PASS, MAXMOVES, TIMESTAMP, MOVES

"""
bg: summary of changes from POPL'17 'go' to this 'go'
- add types to:
  - `Square.__init__`
  - `ZobristHash.__init__`
  - `EmptySet.__init__`
  - `Board.__init__`
  - `UCTNode.__init__`
- annotate `self` arguments
- made mandatory:
  - Square.remove(,,update)
  - Square.find(,update)
- add 3rd argument to `EmptySet.remove`
- replace `timer` with `Timer` class
- add `ITERATIONS` constant
- removed
  - `to_xy` never called

- BIG EDIT: remove types from everything except Square, refactor into 3 files

NOTE: the object fields in `Square` are Dyn
- subtyping failed with the right types (huge error message #recursion)
- cannot be 'object', retic doesn't know the object type
"""


def to_pos(x:int,y:int)->int:
    return y * SIZE + x

@fields({'pos':int
        ,'zobrist_strings':List(int)
        ,'color':int
        ,'board':Dyn #Board
        ,'timestamp':int
        ,'removestamp':int
        ,'reference':Dyn #Square
        ,'ledges':int
        ,'used':bool
        ,'neighbours':List(Dyn)}) #List(Square)
class Square:
    def __init__(self:Square, board:Board, pos:int)->Void:
        self.board = board
        self.pos = pos
        self.timestamp = TIMESTAMP
        self.removestamp = TIMESTAMP
        self.zobrist_strings = [random.randrange(9223372036854775807)
                                for i in range(3)]
        # Creating dummy initialization values
        self.color = 0
        self.reference = self
        self.ledges = 0
        self.used = False
        self.neighbours = []

    def set_neighbours(self:Square)->Void:
        x, y = self.pos % SIZE, self.pos // SIZE;
        self.neighbours = []
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            newx, newy = x + dx, y + dy
            if 0 <= newx < SIZE and 0 <= newy < SIZE:
                self.neighbours.append(self.board.squares[to_pos(newx, newy)])

    def move(self:Square, color:int)->Void:
        global TIMESTAMP, MOVES
        TIMESTAMP += 1
        MOVES += 1
        self.board.zobrist.update(self, color)
        self.color = color
        self.reference = self
        self.ledges = 0
        self.used = True
        for neighbour in self.neighbours:
            neighcolor = neighbour.color
            if neighcolor == EMPTY: 
                self.ledges += 1
            else:
                neighbour_ref = neighbour.find(True)
                if neighcolor == color:
                    if neighbour_ref.reference.pos != self.pos:
                        self.ledges += neighbour_ref.ledges 
                        neighbour_ref.reference = self
                    self.ledges -= 1
                else:
                    neighbour_ref.ledges -= 1
                    if neighbour_ref.ledges == 0:
                        neighbour.remove(neighbour_ref, True)
        self.board.zobrist.add()

    def remove(self:Square, reference:Square, update:bool)->Void:
        self.board.zobrist.update(self, EMPTY)
        self.removestamp = TIMESTAMP
        if update:
            self.color = EMPTY
            self.board.emptyset.add(self.pos)
#            if color == BLACK:
#                self.board.black_dead += 1
#            else:
#                self.board.white_dead += 1
        for neighbour in self.neighbours:
            if neighbour.color != EMPTY and neighbour.removestamp != TIMESTAMP:
                neighbour_ref = neighbour.find(update)
                if neighbour_ref.pos == reference.pos:
                    neighbour.remove(reference, update)
                else:
                    if update:
                        neighbour_ref.ledges += 1

    def find(self:Square, update:bool)->Square:
        reference = self.reference
        if reference.pos != self.pos:
            reference = reference.find(update)
            if update:
                self.reference = reference
        return reference


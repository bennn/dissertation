from Timer import Timer

import random
import math
from square import Square
from constants import SIZE, GAMES, KOMI, EMPTY, WHITE, BLACK, SHOW, PASS, MAXMOVES, TIMESTAMP, MOVES


#@fields({'empties':List(int)
#        ,'board':{'useful':Function(NamedParameters([('pos',int)])
#                 ,int)}
#        ,'empty_pos':List(int)})
class EmptySet:
    #def __init__(self:EmptySet, board:{'useful':Function(NamedParameters([('pos',int)]), int)})->Void:
    def __init__(self, board):
        self.board = board
        self.empties = list(range(SIZE*SIZE))
        self.empty_pos = list(range(SIZE*SIZE))

    #def random_choice(self:EmptySet)->int:
    def random_choice(self):
        choices = len(self.empties)
        while choices:  
            i = int(random.random()*choices)
            pos = self.empties[i]
            if self.board.useful(pos):
                return pos
            choices -= 1
            self.set(i, self.empties[choices])
            self.set(choices, pos)
        return PASS

    #def add(self:EmptySet, pos:int)->Void:
    def add(self, pos):
        self.empty_pos[pos] = len(self.empties)
        self.empties.append(pos)

    #def remove(self:EmptySet, pos:int, update:bool)->Void:
    def remove(self, pos, update):
        self.set(self.empty_pos[pos], self.empties[len(self.empties)-1])
        self.empties.pop()

    #def set(self:EmptySet, i:int, pos:int)->Void: 
    def set(self, i, pos):
        self.empties[i] = pos
        self.empty_pos[pos] = i

#@fields({'hash':int})
class ZobristHash:
    #def __init__(self:ZobristHash, board:{'squares':List(Square)})->Void:
    def __init__(self, board):
        self.hash_set = set()
        self.hash = 0
        for square in board.squares:
            self.hash ^= square.zobrist_strings[EMPTY]
        self.hash_set.clear()
        self.hash_set.add(self.hash)

    #def update(self:ZobristHash, square:Square, color:int)->Void:
    def update(self, square, color):
        self.hash ^= square.zobrist_strings[square.color]
        self.hash ^= square.zobrist_strings[color]

    #def add(self:ZobristHash)->Void:
    def add(self):
        self.hash_set.add(self.hash)

    def dupe(self):
        return self.hash in self.hash_set

#@fields({'squares':List(Square)
#        ,'emptyset':EmptySet
#        ,'zobrist':ZobristHash
#        ,'color':int
#        ,'finished':Bool
#        ,'history':List(int)
#        ,'white_dead':int
#        ,'black_dead':int
#        ,'lastmove':int})
class Board:
    #def __init__(self:Board)->Void:
    def __init__(self):
        self.squares = []
        self.emptyset = EmptySet(self)
        self.zobrist = ZobristHash(self)
        self.color = BLACK
        self.finished = False
        self.lastmove = -2
        self.history = []
        self.white_dead = 0
        self.black_dead = 0
        self.squares = [Square(self, pos) for pos in range(SIZE*SIZE)]
        for square in self.squares:
            square.set_neighbours()
            square.color = EMPTY
            square.used = False

    #def reset(self:Board)->Void:
    def reset(self):
        for square in self.squares:
            square.color = EMPTY
            square.used = False
        self.emptyset = EmptySet(self)
        self.zobrist = ZobristHash(self)
        self.color = BLACK
        self.finished = False
        self.lastmove = -2
        self.history = []
        self.white_dead = 0
        self.black_dead = 0

    #def move(self:Board, pos:int)->Void:
    def move(self, pos):
        square = self.squares[pos]
        if pos != PASS:
            square.move(self.color)
            self.emptyset.remove(square.pos, True)
        elif self.lastmove == PASS:
            self.finished = True
        if self.color == BLACK: self.color = WHITE
        else: self.color = BLACK
        self.lastmove = pos
        self.history.append(pos)

    #def random_move(self:Board)->int:
    def random_move(self):
        return self.emptyset.random_choice()

    #def useful_fast(self:Board, square:Square)->bool:
    def useful_fast(self, square):
        if not square.used:
            for neighbour in square.neighbours:
                if neighbour.color == EMPTY:
                    return True
        return False

    #def useful(self:Board, pos:int)->int: 
    def useful(self, pos):
        global TIMESTAMP
        TIMESTAMP += 1
        square = self.squares[pos]
        if self.useful_fast(square):
            return True
        old_hash = self.zobrist.hash
        self.zobrist.update(square, self.color)
        empties = opps = weak_opps = neighs = weak_neighs = 0
        for neighbour in square.neighbours:
            neighcolor = neighbour.color
            if neighcolor == EMPTY:
                empties += 1
                continue
            neighbour_ref = neighbour.find(False)
            if neighbour_ref.timestamp != TIMESTAMP:
                if neighcolor == self.color:  
                    neighs += 1
                else: 
                    opps += 1
                neighbour_ref.timestamp = TIMESTAMP
                neighbour_ref.temp_ledges = neighbour_ref.ledges
            neighbour_ref.temp_ledges -= 1
            if neighbour_ref.temp_ledges == 0:
                if neighcolor == self.color:  
                    weak_neighs += 1
                else:
                    weak_opps += 1
                    neighbour_ref.remove(neighbour_ref, False)
        dupe = self.zobrist.dupe()
        self.zobrist.hash = old_hash
        strong_neighs = neighs-weak_neighs
        strong_opps = opps-weak_opps
        return not dupe and \
               (empties or weak_opps or (strong_neighs and (strong_opps or weak_neighs)))

    #def useful_moves(self:Board)->List(int):
    def useful_moves(self):
        return [pos for pos in self.emptyset.empties if self.useful(pos)]

    #def replay(self:Board, history:List(int))->Void:
    def replay(self, history):
        for pos in history:
            self.move(pos)

    #def score(self:Board, color:int)->float:
    def score(self, color):
        if color == WHITE:
            count = KOMI + self.black_dead
        else:
            count = self.white_dead
        for square in self.squares:
            squarecolor = square.color
            if squarecolor == color:
                count += 1
            elif squarecolor == EMPTY:
                surround = 0
                for neighbour in square.neighbours:
                    if neighbour.color == color:
                        surround += 1
                if surround == len(square.neighbours): 
                    count += 1
        return count

    #def check(self:Board)->Void:
    def check(self):
        for square in self.squares:
            if square.color == EMPTY:
                continue

            members1 = set([square])
            changed = True
            while changed:
                changed = False
                for member in members1.copy():
                    for neighbour in member.neighbours:
                        if neighbour.color == square.color and neighbour not in members1:
                            changed = True
                            members1.add(neighbour)
            ledges1 = 0
            for member in members1:
                for neighbour in member.neighbours:
                    if neighbour.color == EMPTY:
                        ledges1 += 1

            root = square.find(False)

            members2 = set()
            for square2 in self.squares:
                if square2.color != EMPTY and square2.find(False) == root:
                    members2.add(square2)

            ledges2 = root.ledges

            assert members1 == members2
            assert ledges1 == ledges2, ('ledges differ at %r: %d %d' % (square, ledges1, ledges2))

            empties1 = set(self.emptyset.empties)

            empties2 = set()
            for square in self.squares:
                if square.color == EMPTY:
                    empties2.add(square.pos)


#@fields({'pos':int, 'wins':int, 'losses':int})
class UCTNode:
    #def __init__(self:UCTNode)->Void:
    def __init__(self):
        self.bestchild = None
        self.pos = -1
        self.wins = 0
        self.losses = 0
        self.pos_child = [None for x in range(SIZE*SIZE)]
        self.parent = None

    #def play(self:UCTNode, board:Board)->Void:
    def play(self, board):
        """ uct tree search """
        color = board.color
        node = self
        path = [node]
        while True:
            pos = node.select(board)
            if pos == PASS:
                break
            board.move(pos)
            child = node.pos_child[pos]
            if not child:
                child = node.pos_child[pos] = UCTNode()
                child.unexplored = board.useful_moves()
                child.pos = pos
                child.parent = node
                path.append(child)
                break
            path.append(child)
            node = child
        self.random_playout(board)
        self.update_path(board, color, path)

    #def select(self:UCTNode, board:Board)->int:
    def select(self, board):
        """ select move; unexplored children first, then according to uct value """
        if self.unexplored:
            i = random.randrange(len(self.unexplored))
            pos = self.unexplored[i]
            self.unexplored[i] = self.unexplored[len(self.unexplored)-1]
            self.unexplored.pop()
            return pos
        elif self.bestchild:
            return self.bestchild.pos
        else:
            return PASS

    #def random_playout(self:UCTNode, board:Board)->Void:
    def random_playout(self, board):
        """ random play until both players pass """
        for x in range(MAXMOVES): # XXX while not self.finished?
            if board.finished:
                break
            board.move(board.random_move())

    #def update_path(self:UCTNode, board:Board, color:int, path:List(UCTNode))->Void:
    def update_path(self, board, color, path):
        """ update win/loss count along path """
        wins = board.score(BLACK) >= board.score(WHITE)
        for node in path:
            if color == BLACK: color = WHITE
            else: color = BLACK
            if wins == (color == BLACK):
                node.wins += 1
            else:
                node.losses += 1
            if node.parent:
                node.parent.bestchild = node.parent.best_child()

    #def score(self:UCTNode)->float:
    def score(self):
        winrate = self.wins/float(self.wins+self.losses)
        parentvisits = self.parent.wins+self.parent.losses
        if not parentvisits:
            return winrate
        nodevisits = self.wins+self.losses
        return winrate + math.sqrt((math.log(parentvisits))/(5*nodevisits))

    #def best_child(self:UCTNode)->UCTNode:
    def best_child(self):
        maxscore = -1
        maxchild = None
        for child in self.pos_child:
            if child and child.score() > maxscore:
                maxchild = child
                maxscore = child.score()
        return maxchild

    #def best_visited(self:UCTNode)->UCTNode:
    def best_visited(self):
        maxvisits = -1
        maxchild = None
        for child in self.pos_child:
            if child and (child.wins+child.losses) > maxvisits:
                maxvisits, maxchild = (child.wins+child.losses), child
        return maxchild

#def computer_move(board:{'useful_moves':Function([], List(int)),
#                         'random_move':Function([], int),
#                         'history':List(int)})->int:
def computer_move(board):
    global MOVES
    pos = board.random_move()
    if pos == PASS:
        return PASS
    tree = UCTNode()
    tree.unexplored = board.useful_moves()
    nboard = Board()
    for game in range(GAMES):
        node = tree
        nboard.reset()
        nboard.replay(board.history)
        node.play(nboard)
    return tree.best_visited().pos

ITERATIONS = 2

# Histogram of method call counts:
#      328486 Board
#      235994 EmptySet
#     1001986 SQUARE
#       30810 UCTNode
#      242536 ZobristHash
# (obtained by adding a `print` at the top of each method call, counting the prints from 1 run)

if __name__ == "__main__":
    t = Timer()
    with t:
        for i in range(ITERATIONS):
            random.seed(1)
            board = Board()
            pos = computer_move(board)
            print(pos)


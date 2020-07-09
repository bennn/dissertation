from random import randrange, shuffle, random, seed
from copy import deepcopy
from retic import List, Void, Tuple, Bool, Int, Float, fields
from player import Player

min_val = 2
max_val = 7
turns = 10
stack_size = 5

@fields({"players": List(Player), "bull_points": List(Int), "cards_per_game": Int})
class Dealer:
    """
    To represent the Dealer for the whole game
    """

    def __init__(self:Dealer, players:List(Player), bull_points:List(Int), cards_per_game:Int)->Void:
        """
        :param deck: [Card ...]
        :param players: [Player ...]
        :param bull_points: [Int ...]
        """
        self.deck = self.create_deck(cards_per_game)
        self.players = players
        self.bull_points = bull_points
        self.cards_per_game = cards_per_game

    def simulate_game(self:Dealer)->List(Tuple(Int, Int)):
        """
        Similulates a game and returns the players' scores
        :return: [Tuple ...]
        """
        while not max(self.bull_points) >= 66:

            #hand cards
            for i, player in enumerate(self.players):
                hand = []
                for i in range(i + 1 * 10):
                    hand.append(self.deck[i])
                player.cards = hand


            stacks = self.create_stacks()
            for i in range(turns):
                for j in range(len(self.players)):
                    player = self.players[j]
                    chosen_stack_index = player.choose_correct_stack(stacks)
                    (p, s) = self.update_game(player, chosen_stack_index, stacks)
                    self.bull_points[j]+=p
                    stacks = s
        return self.output_scores()



    #Problem: if you change return type to Tuple(int), it will pass guarded check and not pass transient.
    def create_deck(self:Dealer, deck_size, bull_points:Float = .5, order:Float = .5)->List(Tuple(Int, Int)):
        """
        :param deck_size: Int, number of cards in deck
        :param min: Int, minimum number of bull points
        :param max: Int, maximum number of bull points
        :param bull_points: float, bull points parametrization
        :param order: float, order of cards parametrization
        :return: [Card ...]
        """
        seed(bull_points)
        cards = []
        for i in range(deck_size):
            cards.append((i+1, randrange(min_val, max_val)))
        s = (order or random())
        shuffle(cards, lambda: s)
        return cards


    def create_stacks(self:Dealer)->(List(List(Tuple(Int, Int)))):
        """
        create 4 new stacks each having 1 card from the deck
        at the start of every round
        Initialize all players with that stack
        :return: [[Tuple] ...]
        """
        stacks = []
        for i in range(4):
            stacks.append([self.deck.pop()])
        return stacks

    def output_scores(self:Dealer)->List(Tuple(Int, Int)):
        """
        Outputs the names of the winning and losing players
        :param players: [Player ...]
        :return: (Player, Player)
        """
        res = []
        for i in range(len(self.players)):
            player_points = self.bull_points[i]
            player_name = self.players[i].name
            res.append((player_name, player_points))
        return res

    def update_game(self:Dealer, player:Player, stack_index:Int, stacks:List(List(Tuple(Int, Int))))->\
            Tuple(Int, List(List(Tuple(Int, Int)))):
        """
        update playe's bull points based on chosen stack
        :param stack_index: Int
        :param stacks: [[Tuple...]...] where len(stacks)=4
        :return: Tuple
        """
        top_cards = list(map(lambda stack: stack[-1], stacks))
        discarded_index = player.discard()
        discarded = player.cards.pop(discarded_index)

        if discarded[0] < min(list(map(lambda card: card[0], top_cards))):
            bull_points = sum(list(map(lambda card: card[1], stacks[stack_index])))
            new_stacks = self.replace_card(discarded, stack_index, stacks)
            return bull_points, new_stacks

        else:
            my_stack = stacks[stack_index]
            if len(my_stack) == stack_size:
                bull_points = sum(list(map(lambda card: card[1], my_stack)))
                new_stacks = self.replace_card(discarded, stack_index, stacks)
                return (bull_points, new_stacks)
            else:
                new_stacks = deepcopy(stacks)
                new_stacks[stack_index].append(discarded)

                return 0, new_stacks


    def replace_card(self:Dealer, card:Tuple(Int, Int), index:Int, stacks:List(List(Tuple(Int, Int))))->List(List(Tuple(Int, Int))):
        """
        Replaces stack with card and returns new stack
        :param card: Tuple
        :param index: Int
        :param stacks: [[Tuples ...] ...]
        :return [[Tuple...]...]
        """
        new_stacks = deepcopy(stacks)
        new_stacks[index] = [card]
        return new_stacks


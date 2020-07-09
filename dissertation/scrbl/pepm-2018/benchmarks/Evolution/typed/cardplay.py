from retic import Void, List, Int, Bool, Dyn, fields
from evolution.player.player_state import PlayerState


@fields({'played_card_index': Int})
class CardPlay:
    """
    CardPlay is one of:
    - SupplementSpecies
    - ReplaceCard
    - ExchangeCard
    """

    def __init__(self:CardPlay, played_card_index:Int)->Void:
        """
        :param played_card_index: the card that was played
        :type played_card_index: TraitCard
        :return: None
        """
        self.played_card_index = played_card_index

    def apply(self:CardPlay, player_state:PlayerState)->Void:
        """
        Applies this card play to the given PlayerState
        :param player_state: PlayerState to be applied to
        :type player_state: PlayerState
        :return: None
        """
        raise NotImplementedError("Method not yet implemented.")

    def get_card_indices(self:CardPlay)->List(Int):
        """
        Get the indices of the TraitCards used by this CardPlay
        :return: list of the indices used
        :rtype: [Nat, ...]
        """
        return [self.played_card_index]

    #TODO: Fix types for this method
    def verify_self(self:CardPlay, player_state:PlayerState, food_card_index:Int, card_plays_before_this:List(CardPlay))->Bool:
        """
        Verifies that this card play is valid
        :param player_state: the player state playing this card
        :type player_state: PlayerState
        :type food_card_index: Nat
        :type card_plays_before_this: [CardPlay, ...]
        :return: True if this is valid, False otherwise
        """
        self_indicies = self.get_card_indices()
        previous_indicies = {food_card_index}
        for card_play in card_plays_before_this:
            previous_indicies.union(set(card_play.get_card_indices()))

        no_dups = len(self_indicies) == len(set(self_indicies))
        no_overlap =  (previous_indicies.intersection(set(self_indicies)) == set())
        in_range =  set(self_indicies).issubset(set(range(0, len(player_state.hand))))

        return no_dups and no_overlap and in_range

    def update_trait_counts(self:CardPlay, species_trait_count:List(Int))->List(Int):
        """
        Append the species trait count to the given list
        if this is an ExchangeForSpecies
        :param species_trait_count: All trait counts up to this card play
        :type species_trait_count:[Nat, ...]
        :return: updated list of trait counts
        :rtype: [Nat, ...]
        """
        raise NotImplementedError("Method not yet implemented")

    def num_species_created(self:CardPlay)->Int:
        """
        returns the number of species created
        as a result of this card play
        :return:
        """
        return 0


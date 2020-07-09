from cardplay import CardPlay
from evolution.species import Species
from retic import Int, Bool, List, Void, Dyn, String, fields
from evolution.player.player_state import PlayerState



@fields({'played_card_index': Int, 'loi': List(Int)})
class ExchangeForSpecies(CardPlay):
    """
    Represents exchanging cards for more species
    """

    def __init__(self:ExchangeForSpecies, played_card_index:Int, loi:List(Int))->Void:
        """
        :param played_card_index: index of the card
        :type played_card_index: Nat
        :param loi: the indices of the trait cards to put on the
        species
        :type loi: [Nat, ...]

        behavioral contract:
        len(loc) <= 3

        """
        super().__init__(played_card_index)
        self.loi = loi

    def apply(self:ExchangeForSpecies, player_state:PlayerState)->Void:
        traits = [player_state.hand[i] for i in self.loi]
        new_species = Species(played_cards=traits)
        player_state.species_list.append(new_species)

    def get_card_indices(self:ExchangeForSpecies)->List(Int):
        indices = super(ExchangeForSpecies, self).get_card_indices() + self.loi
        return indices

    def verify_self(self:ExchangeForSpecies, player_state:PlayerState, food_card_index:Int, card_plays_before_this:List(CardPlay))->Bool:
        verify_rest = super(ExchangeForSpecies, self).verify_self(player_state, food_card_index, card_plays_before_this)
        verify_loi = player_state.validate_trait_cards_indicies(self.loi)

        return verify_loi and verify_rest

    def num_species_created(self:ExchangeForSpecies)->Int:
        return 1

    def update_trait_counts(self:ExchangeForSpecies, species_trait_count:List(Int))->List(Int):
        species_trait_count.append(len(self.loi))
        return species_trait_count

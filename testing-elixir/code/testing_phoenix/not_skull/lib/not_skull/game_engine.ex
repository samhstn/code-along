#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.GameEngine do
  alias NotSkull.Errors.GameError

  @callback update_game(Game.t(), Move.t()) ::
              {:ok, Game.t()} | {:error, GameError.t()}

  @default_starting_hand [:skull, :rose, :rose, :rose]

  def phases_and_moves do
    %{
      first_card: [:add],
      add_or_bet: [:add, :bet],
      bet_or_pass: [:bet, :pass],
      turning_cards: [:turn_card],
      choosing_next_player: [:choose_starter]
    }
  end

  defmodule Bet do
    @type t :: %__MODULE__{}
    defstruct player_id: nil,
              value: 0
  end

  defmodule Game do
    @type t :: %__MODULE__{}
    defstruct active?: false,
              current_bet: %Bet{},
              current_phase: nil,
              current_player_id: nil,
              id: nil,
              players: [],
              total_cards_on_table: 0
  end

  defmodule Player do
    @type t :: %__MODULE__{}
    defstruct name: nil,
              cards_in_hand: [],
              cards_on_table: [],
              id: nil,
              score: nil
  end

  defmodule Move do
    @type t :: %__MODULE__{}
    defstruct player_id: nil,
              phase: nil,
              type: nil,
              value: nil
  end

  def new_game(overrides \\ []) do
    overridable = [:players]

    overrides_as_map =
      for {key, value} <- overrides,
          key in overridable,
          into: %{},
          do: {key, value}

    default_game = %NotSkull.GameEngine.Game{
      current_phase: :joining,
      id: Ecto.UUID.generate()
    }

    {:ok, Map.merge(default_game, overrides_as_map)}
  end

  def join(%Game{active?: true}, _) do
    {:error, GameError.exception(message: "Player can't join: game is already in progress.")}
  end

  def join(%Game{} = game, %Player{} = player) do
    {:ok, %{game | players: game.players ++ [player]}}
  end

  def start(%Game{active?: true}, _) do
    {:error, GameError.exception(message: "Game has already been started.")}
  end

  def start(%Game{active?: false} = game, %Player{} = player) do
    joined_player_ids = for player <- game.players, do: player.id

    result =
      cond do
        Enum.count(game.players) < 2 ->
          {:error,
           GameError.exception(message: "At least two players must have joined to start a game.")}

        player.id in joined_player_ids ->
          players_with_cards_and_in_new_order =
            game.players
            |> Enum.shuffle()
            |> Enum.map(fn player ->
              %{player | cards_in_hand: @default_starting_hand}
            end)

          first_player = hd(players_with_cards_and_in_new_order)

          started_game = %{
            game
            | active?: true,
              current_phase: :first_card,
              current_player_id: first_player.id,
              players: players_with_cards_and_in_new_order
          }

          {:ok, started_game}

        true ->
          {:error,
           GameError.exception(message: "Only players who have joined can start the game.")}
      end

    result
  end

  def allowed?(%Game{active?: false}, _) do
    false
  end

  def allowed?(game, move) do
    game.current_player_id == move.player_id &&
      game.current_phase == move.phase &&
      move.type in Map.get(phases_and_moves(), game.current_phase)
  end

  def update_game(%{current_phase: :first_card} = game, %{type: :add} = move) do
    current_player = Enum.find(game.players, fn player -> player.id == move.player_id end)

    result =
      if Enum.empty?(current_player.cards_on_table) do
        new_cards_in_hand = List.delete(current_player.cards_in_hand, move.value)

        new_cards_on_table = [{move.value, :down}]

        updated_current_player = %{
          current_player
          | cards_in_hand: new_cards_in_hand,
            cards_on_table: new_cards_on_table
        }

        updated_players = update_player_in_group(game.players, updated_current_player)

        current_phase =
          if all_players_have_placed_a_card?(updated_players),
            do: :add_or_bet,
            else: :first_card

        updated_game = %{
          game
          | current_phase: current_phase,
            players: updated_players,
            total_cards_on_table: game.total_cards_on_table + 1
        }

        {:ok, updated_game}
      else
        {:error, GameError.exception(message: "You have already played a card.")}
      end

    result
  end

  def update_game(%{current_phase: :add_or_bet} = game, %{type: :bet} = move) do
    [current_player | _other_players] = game.players

    updated_bet = %Bet{player_id: move.player_id, value: move.value}

    updated_game = %{
      game
      | current_bet: updated_bet,
        current_phase: :bet_or_pass,
        current_player_id: next_player_id(game, current_player.id)
    }

    {:ok, updated_game}
  end

  def update_game(%{current_phase: :add_or_bet} = game, %{type: :add} = move) do
    current_player = Enum.find(game.players, fn player -> player.id == move.player_id end)

    result =
      if Enum.count(current_player.cards_in_hand) > 0 do
        updated_current_player = move_card_to_table(current_player, move.value)

        total_cards = game.total_cards_on_table + 1
        new_current_player_id = next_player_id(game, current_player.id)

        updated_players = update_player_in_group(game.players, updated_current_player)

        updated_game = %{
          game
          | players: updated_players,
            total_cards_on_table: total_cards,
            current_player_id: new_current_player_id
        }

        {:ok, updated_game}
      else
        {:error, GameError.exception(message: "You have no cards in your hand. You must bet.")}
      end

    result
  end

  defp all_players_have_placed_a_card?(players) do
    Enum.reduce_while(players, true, fn
      %{cards_on_table: []}, _acc -> {:halt, false}
      _, acc -> {:cont, acc}
    end)
  end

  # replaces player with matching id, keeping origal order
  defp update_player_in_group(players, updated_player) do
    for player <- players do
      if player.id == updated_player.id do
        updated_player
      else
        player
      end
    end
  end

  defp move_card_to_table(
         %{cards_in_hand: in_hand, cards_on_table: on_table} = player,
         card_value
       ) do
    in_hand = List.delete(in_hand, card_value)
    on_table = [{card_value, :down} | on_table]
    %{player | cards_in_hand: in_hand, cards_on_table: on_table}
  end

  defp next_player_id(game, current_player_id) do
    current_player_index =
      Enum.find_index(game.players, fn player ->
        player.id == current_player_id
      end)

    if next_player = Enum.at(game.players, current_player_index + 1) do
      next_player.id
    else
      List.first(game.players).id
    end
  end
end

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.GameEngineTest do
  use ExUnit.Case
  import Support.AssertionHelpers
  alias NotSkull.{Factory, GameEngine}
  alias GameEngine.{Game, Move}
  alias NotSkull.Errors.{GameError}

  @expected_phases_and_move_types %{
    first_card: [:add],
    add_or_bet: [:add, :bet],
    bet_or_pass: [:bet, :pass],
    turning_cards: [:turn_card],
    choosing_next_player: [:choose_starter]
  }

  @phases Map.keys(@expected_phases_and_move_types)
  @move_types Map.values(@expected_phases_and_move_types)
              |> List.flatten()
              |> Enum.uniq()

  @starting_hand [:skull, :rose, :rose, :rose]

  describe "new_game/0" do
    test "it returns a new game" do
      assert {:ok, %GameEngine.Game{id: new_game_id} = new_game} = GameEngine.new_game()

      assert is_uuid(new_game_id)

      expected_game = %NotSkull.GameEngine.Game{
        current_bet: %NotSkull.GameEngine.Bet{},
        current_phase: :joining,
        current_player_id: nil,
        id: new_game_id,
        players: [],
        total_cards_on_table: 0
      }

      assert new_game == expected_game
    end

    test "it accepts overrides for players" do
      overrides = [players: [Factory.struct_for(:player)]]
      assert {:ok, %GameEngine.Game{id: new_game_id} = new_game} = GameEngine.new_game(overrides)

      expected_game = %NotSkull.GameEngine.Game{
        current_bet: %NotSkull.GameEngine.Bet{},
        current_phase: :joining,
        current_player_id: nil,
        id: new_game_id,
        players: Keyword.get(overrides, :players),
        total_cards_on_table: 0
      }

      assert new_game == expected_game
    end
  end

  describe "join/2" do
    test "success: it adds a player when game is unstarted" do
      new_game = Factory.struct_for(:new_game)

      player = Factory.struct_for(:player)

      assert {:ok, updated_game} = GameEngine.join(new_game, player)

      expected_game = %{new_game | players: new_game.players ++ [player]}

      assert expected_game == updated_game
    end

    test "error: returns error tuple when game is active" do
      active_game = Factory.struct_for(:new_game, %{active?: true})

      player = Factory.struct_for(:player)

      assert {:error, %GameError{message: message}} = GameEngine.join(active_game, player)

      assert message == "Player can't join: game is already in progress."
    end
  end

  describe "start/2" do
    test "success: it starts the game when at least 2 players have joined" do
      players = for _x <- 1..4, do: Factory.struct_for(:player, %{cards_in_hand: []})
      new_game = Factory.struct_for(:new_game, %{players: players})
      assert {:ok, %Game{} = returned_game} = GameEngine.start(new_game, Enum.random(players))

      expected_game = %{new_game | active?: true, current_phase: :first_card}

      assert_values_for(
        expected: expected_game,
        actual: returned_game,
        fields: fields_for(expected_game) -- [:current_player_id, :players]
      )

      # Starting the game shuffles the order. We can't assert on the order but
      # should make sure all the players are there. Also, they should have cards
      # now.
      expected_players =
        for player <- expected_game.players,
            do: %{player | cards_in_hand: @starting_hand}

      assert_unordered_lists_are_equal(
        expected: expected_players,
        actual: returned_game.players
      )

      [player_in_first_slot | _tl] = returned_game.players
      assert returned_game.current_player_id == player_in_first_slot.id
    end

    test "error: it returns error tuple when 'starting player' isn't in the game" do
      new_game =
        Factory.struct_for(:new_game, %{
          players: [Factory.struct_for(:player), Factory.struct_for(:player)]
        })

      assert {:error, %GameError{message: message}} =
               GameEngine.start(new_game, Factory.struct_for(:player))

      assert message == "Only players who have joined can start the game."
    end

    test "error: it returns and error tuple if there are fewer than 2 players" do
      player = Factory.struct_for(:player)
      new_game = Factory.struct_for(:new_game, %{players: [player]})

      assert {:error, %GameError{message: message}} = GameEngine.start(new_game, player)

      assert message ==
               "At least two players must have joined to start a game."
    end

    test "error: it returns and error tuple for an active game" do
      player = Factory.struct_for(:player)
      active_game = Factory.struct_for(:new_game, %{players: [player], active?: true})

      assert {:error, %GameError{message: message}} = GameEngine.start(active_game, player)

      assert message == "Game has already been started."
    end
  end

  describe "phases_and_moves/0" do
    game_engine_reponse = GameEngine.phases_and_moves()

    for {phase, moves} <- game_engine_reponse do
      assert MapSet.new(moves) ==
               MapSet.new(@expected_phases_and_move_types[phase])
    end

    assert Enum.count(game_engine_reponse) ==
             Enum.count(@expected_phases_and_move_types)
  end

  describe "allowed?/2" do
    test "false: when game is not active" do
      %{game: game, move: move} = Factory.game_and_move(:add_or_bet, :add)
      inactive_game = %{game | active?: false}

      assert GameEngine.allowed?(inactive_game, move) == false
    end

    test "false: when move is made by non-current player" do
      game_state = %Game{current_player_id: Factory.uuid()}
      move = %Move{player_id: Factory.uuid()}
      assert GameEngine.allowed?(game_state, move) == false
    end

    test "false: when move phase is not current phase" do
      player_id = Factory.uuid()

      for move_phase <- @phases do
        for current_phase <- @phases, current_phase != move_phase do
          game_state = %Game{
            current_player_id: player_id,
            current_phase: current_phase
          }

          move = %Move{player_id: player_id}

          assert GameEngine.allowed?(game_state, move) == false,
                 "expected allowed?: false when:\ncurrent_phase: #{current_phase}\nmove phase:#{
                   move_phase
                 }"
        end
      end
    end

    test "false: when move is invalid for phase" do
      player_id = Factory.uuid()

      for {phase, valid_move_types} <- @expected_phases_and_move_types do
        for bad_move_type <- @move_types,
            bad_move_type not in valid_move_types do
          game_state = %Game{
            current_player_id: player_id,
            current_phase: phase
          }

          move = %Move{
            player_id: player_id,
            phase: phase,
            type: bad_move_type
          }

          assert GameEngine.allowed?(game_state, move) == false,
                 "expected allowed?: false when:\nphase: #{inspect(phase)}\nmove: #{
                   inspect(bad_move_type)
                 }"
        end
      end
    end
  end

  describe "update_game/2 :first_card" do
    test "success: it moves card from hand to face-down on table, doesn't advance current_player" do
      %{game: game, move: move} = Factory.game_and_move(:first_card)

      # exercise
      assert {:ok, updated_game} = GameEngine.update_game(game, move)

      assert updated_game.current_player_id == game.current_player_id

      [starting_first_player | expected_players_that_havent_moved] = game.players

      [actual_first_player | actual_players_that_havent_moved] = updated_game.players

      assert updated_game.total_cards_on_table ==
               game.total_cards_on_table + 1

      # this is checking that opponent cards are all still the same as before the game was updated
      assert expected_players_that_havent_moved ==
               actual_players_that_havent_moved

      assert actual_first_player.cards_on_table == [{move.value, :down}]

      assert actual_first_player.cards_in_hand ==
               starting_first_player.cards_in_hand -- [move.value]
    end

    test "success: any player can add a card first, does not advance phase" do
      %{game: game, move: move} = Factory.game_and_move(:first_card)

      for player <- game.players do
        move = %{move | player_id: player.id}
        # exercise
        assert({:ok, updated_game} = GameEngine.update_game(game, move))

        returned_player = Enum.find(updated_game.players, fn plyr -> plyr.id == player.id end)

        assert returned_player.cards_on_table == [{move.value, :down}]

        assert returned_player.cards_in_hand ==
                 player.cards_in_hand -- [move.value]

        assert updated_game.current_phase == :first_card
      end
    end

    test "success: it updates phase to :add_or_bet when all players have played their first card" do
      player1 =
        Factory.struct_for(
          :player,
          %{cards_in_hand: [:rose, :rose, :rose], cards_on_table: [{:skull, :down}]}
        )

      player2 =
        Factory.struct_for(
          :player,
          %{cards_in_hand: [:skull, :skull, :rose, :rose], cards_on_table: []}
        )

      game =
        Factory.struct_for(:game, %{
          active?: true,
          current_phase: :first_card,
          players: [player1, player2],
          total_cards_on_table: 1
        })

      move = %Move{
        player_id: player2.id,
        phase: :first_card,
        type: :add,
        value: :skull
      }

      assert({:ok, updated_game} = GameEngine.update_game(game, move))

      assert updated_game.current_phase == :add_or_bet
      assert updated_game.total_cards_on_table == 2
      assert is_uuid(updated_game.current_player_id)
    end

    test "error: returns error tuple if player already has a card on the table" do
      %{game: game, move: move} = Factory.game_and_move(:first_card)
      [player | rest] = game.players

      # having a card on the table means that the player has already made their move
      player = %{
        player
        | cards_in_hand: [:rose, :rose, :rose],
          cards_on_table: [{:skull, :down}]
      }

      players = [player | rest]
      game = %{game | players: players}

      assert {:error, %GameError{message: message}} = GameEngine.update_game(game, move)
    end
  end

  describe "update_game/2: add_or_bet: :bet" do
    setup do
      %{game: game, move: move} = Factory.game_and_move(:add_or_bet, :bet)

      # this provides safety against debugging if the move was never valid
      assert GameEngine.allowed?(game, move)
      %{game: game, move: move}
    end

    test "success: updates current_bet for value and player", %{
      game: game,
      move: move
    } do
      {:ok, updated_game} = GameEngine.update_game(game, move)

      expected_current_bet = %GameEngine.Bet{
        player_id: move.player_id,
        value: move.value
      }

      assert updated_game.current_bet == expected_current_bet
    end

    test "success: it sets current player to next player", %{
      game: game,
      move: move
    } do
      {:ok, updated_game} = GameEngine.update_game(game, move)

      expected_next_player_id =
        game.players
        |> next_players()
        |> Map.get(move.player_id)

      assert updated_game.current_player_id == expected_next_player_id
    end

    test "success: it sets game phase to :bet_or_pass", %{
      game: game,
      move: move
    } do
      {:ok, updated_game} = GameEngine.update_game(game, move)

      assert updated_game.current_phase == :bet_or_pass
    end
  end

  describe "update_game/2: add_or_bet: :add" do
    setup do
      %{game: game, move: move} = Factory.game_and_move(:add_or_bet, :add)

      # this provides safety against debugging if the move was never valid
      assert GameEngine.allowed?(game, move)

      %{game: game, move: move}
    end

    test "success: it increases total cards on table count", %{
      game: game,
      move: move
    } do
      assert {:ok, updated_game} = GameEngine.update_game(game, move)

      assert updated_game.total_cards_on_table ==
               game.total_cards_on_table + 1
    end

    test "success: leaves current_bet at defaults", %{game: game, move: move} do
      {:ok, updated_game} = GameEngine.update_game(game, move)

      assert %GameEngine.Bet{} == updated_game.current_bet
    end

    test "success: it updates the :current_player_id", %{
      game: game,
      move: move
    } do
      {:ok, updated_game} = GameEngine.update_game(game, move)

      expected_next_player_id =
        game.players
        |> next_players()
        |> Map.get(move.player_id)

      assert updated_game.current_player_id == expected_next_player_id
    end

    test "success: it updates the player taking their turn", %{
      game: game,
      move: move
    } do
      player_before_move = List.first(game.players)

      {:ok, updated_game} = GameEngine.update_game(game, move)

      player_after_move =
        Enum.find(updated_game.players, fn player ->
          player.id == player_before_move.id
        end)

      expected_cards_in_hand = List.delete(player_before_move.cards_in_hand, move.value)

      expected_cards_on_table = [
        {move.value, :down} | player_before_move.cards_on_table
      ]

      expected_player = %{
        player_before_move
        | cards_in_hand: expected_cards_in_hand,
          cards_on_table: expected_cards_on_table
      }

      assert_values_for(
        expected: expected_player,
        actual: player_after_move,
        fields: fields_for(expected_player)
      )
    end

    test "error: it returns and error tuple if player doesn't have any cards to play",
         %{
           game: game,
           move: move
         } do
      [moving_player | remaining_players] = game.players

      moving_player = %{moving_player | cards_in_hand: []}
      players = [moving_player | remaining_players]

      game = %{game | players: players}

      assert {:error, %GameError{message: message}} = GameEngine.update_game(game, move)

      assert message = "You have no cards in your hand. You must bet."
    end
  end

  defp next_players(players) do
    for current_player <- players, into: %{} do
      current_player_index =
        Enum.find_index(players, fn player ->
          player.id == current_player.id
        end)

      next_player_id =
        if next_player = Enum.at(players, current_player_index + 1) do
          next_player.id
        else
          List.first(players).id
        end

      {current_player.id, next_player_id}
    end
  end
end

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Factory do
  alias NotSkull.Accounts.User
  alias NotSkull.GameEngine
  alias NotSkull.Repo

  def params(:user) do
    %{
      email: email(),
      name: first_name(),
      password: password()
    }
  end

  def set_password(user, password) do
    hashed_password = Argon2.hash_pwd_salt(password)
    %{user | password: hashed_password}
  end

  # DO NOT SORT THESE!
  @phases_in_order [
    :joining,
    :first_card,
    :add_or_bet,
    :bet_or_pass,
    :turning_cards,
    :choose_starter,
    :game_summary
  ]

  def struct_for(:game) do
    player_count = Enum.random(3..6)
    players = for _x <- 1..player_count, do: struct_for(:player)

    %GameEngine.Game{
      current_bet: %GameEngine.Bet{},
      current_phase: :first_card,
      current_player_id: List.first(players).id,
      id: uuid(),
      players: players
    }
  end

  def struct_for(:player) do
    %GameEngine.Player{
      cards_in_hand: starting_cards(:cards_in_hand),
      cards_on_table: starting_cards(:cards_on_table),
      id: uuid(),
      name: first_name(),
      score: 0
    }
  end


  def struct_for(:new_game) do
    struct_for(:game, %{
      id: uuid(),
      active?: false,
      current_bet: %GameEngine.Bet{},
      current_phase: :joining,
      current_player_id: nil,
      players: [],
      total_cards_on_table: 0
    })
  end

  def struct_for(:move) do
    %GameEngine.Move{
      player_id: uuid(),
      phase: Enum.random(@phases_in_order),
      type: :add
    }
  end

  def game_and_move(:first_card) do
    player_count = Enum.random(3..6)

    players =
      for _x <- 1..player_count do
        struct_for(:player, %{cards_in_hand: [:skull, :rose, :rose, :rose], cards_on_table: []})
      end

    first_player = List.first(players)

    game =
      struct_for(:game, %{
        active?: true,
        current_bet: %GameEngine.Bet{},
        current_phase: :first_card,
        current_player_id: first_player.id,
        players: players,
        total_cards_on_table: player_count
      })

    value = Enum.random([:skull, :rose])

    move =
      struct_for(:move,
        %{player_id: first_player.id,
        phase: :first_card,
        type: :add,
        value: value}
      )

    %{game: game, move: move}
  end

  def game_and_move(:add_or_bet, type) do
    player_count = Enum.random(3..6)

    players =
      for _x <- 1..player_count do
        {cards_in_hand, cards_on_table} = player_cards_add_or_bet()

        struct_for(:player, %{cards_in_hand: cards_in_hand, cards_on_table: cards_on_table})
      end

    first_player = List.first(players)

    game =
      struct_for(:game, %{
        active?: true,
        current_bet: %GameEngine.Bet{},
        current_phase: :add_or_bet,
        current_player_id: first_player.id,
        players: players,
        total_cards_on_table: player_count
      })

    value =
      if type == :add do
        Enum.random(first_player.cards_in_hand)
      else
        1
      end

    move =
      struct_for(:move,
        %{player_id: first_player.id,
        phase: :add_or_bet,
        type: type,
        value: value}
      )

    %{game: game, move: move}
  end

  def player_cards_add_or_bet do
    all_cards = [:skull, :rose, :rose, :rose]
    card_on_table = Enum.random(all_cards)
    cards_in_hand = List.delete(all_cards, card_on_table)
    {cards_in_hand, [{card_on_table, :down}]}
  end

  def no_cards, do: %{cards_in_hand: [], cards_on_table: []}

  def starting_cards do
    %{cards_in_hand: [:skull, :rose, :rose, :rose], cards_on_table: []}
  end

  def starting_cards(type) when type in [:cards_in_hand, :cards_on_table] do
    starting_cards()[type]
  end

  ### Inserts

  def insert(:user, overrides \\ %{}) do
    atom_params(:user, overrides)
    |> User.create_changeset()
    |> Repo.insert()
  end

  ### Utility Functions

  def struct_for(factory_name, overrides \\ %{})
      when is_atom(factory_name) and is_map(overrides) do
    Map.merge(struct_for(factory_name), overrides)
  end

  def string_params(factory_name, overrides \\ %{})
      when is_atom(factory_name) and is_map(overrides) do
    atom_params(factory_name, overrides)
    |> convert_atom_keys_to_strings()
  end

  def atom_params(factory_name, overrides \\ %{})
      when is_atom(factory_name) and is_map(overrides) do
    defaults = params(factory_name)
    Map.merge(defaults, overrides)
  end

  def build_one(%Ecto.Changeset{} = changeset) do
    if changeset.valid? do
      {:ok, Ecto.Changeset.apply_changes(changeset)}
    else
      {:error, changeset}
    end
  end

  def convert_atom_keys_to_strings(values) when is_list(values) do
    Enum.map(values, &convert_atom_keys_to_strings/1)
  end

  def convert_atom_keys_to_strings(%{__struct__: _} = record)
      when is_map(record) do
    Map.from_struct(record) |> convert_atom_keys_to_strings()
  end

  def convert_atom_keys_to_strings(record) when is_map(record) do
    Enum.reduce(record, Map.new(), fn {key, value}, acc ->
      Map.put(acc, to_string(key), convert_atom_keys_to_strings(value))
    end)
  end

  def convert_atom_keys_to_strings(value) do
    value
  end

  ### Data Generation
  def email, do: Faker.Internet.email()
  def first_name, do: Faker.Name.first_name()
  def uuid, do: Ecto.UUID.generate()
  def password, do: Faker.Lorem.characters(32) |> to_string
end

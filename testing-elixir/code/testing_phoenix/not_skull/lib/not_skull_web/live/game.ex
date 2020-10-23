#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.Game do
  use Phoenix.LiveView
  alias NotSkull.ActiveGames
  alias NotSkull.Accounts
  alias NotSkullWeb.Router.Helpers, as: Routes

  defp topic(game_id) do
    "game-#{game_id}"
  end

  def mount(params, session, socket) do
    game_id = params["game_id"]
    user_id = session["user_id"]

    case ActiveGames.get_game_by_id(game_id) do
      {:error, %{message: _message}} ->
        {:ok, redirect(socket, to: Routes.user_path(socket, :show, user_id))}

      {:ok, game} ->
        player =
          Enum.find(game.players, fn player -> player.id == user_id end)

        NotSkullWeb.Endpoint.subscribe(topic(game_id))

        {:ok,
         assign(socket, %{
           user_id: user_id,
           game: game,
           basic_player: player,
           page_title: "NotSkull - Game"
         })}
    end
  end

  def handle_event("join", _params, socket) do
    IO.inspect("join called")
    game = socket.assigns.game
    user_id = socket.assigns.user_id

    {updated_game, joined?, player, flash} =
      with false <- game.active?,
           {:ok, user} <- Accounts.get_user_by_id(user_id),
           player = %NotSkull.GameEngine.Player{name: user.name, id: user.id},
           {:ok, updated_game} = ActiveGames.join(game.id, player) do
        {updated_game, true, player, {:info, "You successfully joined."}}
      else
        _ ->
          {game, false, nil,
           {:error, "There was an issue and you couldn't join."}}
      end

    updated_socket =
      socket
      |> update_game(updated_game)
      |> assign_player(player)
      |> update_flash(flash)

    if joined? do
      NotSkullWeb.Endpoint.broadcast_from(
        self(),
        topic(game.id),
        "joined",
        %{}
      )
    end

    {:noreply, updated_socket}
  end

  def handle_event("start-game", _params, socket) do
    {game, player} = game_and_player(socket)

    updated_socket =
      case ActiveGames.start(game.id, player) do
        {:ok, updated_game} ->
          NotSkullWeb.Endpoint.broadcast_from(
            self(),
            topic(updated_game.id),
            "started",
            %{}
          )

          update_game(socket, updated_game)

        {:error, %{message: message}} ->
          update_flash(socket, {:error, message})
      end

    {:noreply, updated_socket}
  end

  def handle_event("add-" <> value, _params, socket) do
    value_as_atom = String.to_existing_atom(value)
    {game, player} = game_and_player(socket)

    move = %NotSkull.GameEngine.Move{
      player_id: player.id,
      phase: game.current_phase,
      type: :add,
      value: value_as_atom
    }

    updated_socket =
      case ActiveGames.move(game.id, move) do
        {:ok, game} ->
          NotSkullWeb.Endpoint.broadcast_from(
            self(),
            topic(game.id),
            "updated",
            %{
              flash: {:info, "#{player.name} played a card."}
            }
          )

          update_game(socket, game)

        {:error, %{message: message}} ->
          update_flash(socket, {:error, message})
      end

    {:noreply, updated_socket}
  end

  def handle_info(%{event: "joined"}, socket) do
    updated_socket =
      socket
      |> refresh_game()
      |> update_flash({:info, "A new player has joined."})

    {:noreply, updated_socket}
  end

  def handle_info(%{event: "started"}, socket) do
    updated_socket =
      socket
      |> refresh_game()
      |> update_flash({:info, "The game has started!"})

    {:noreply, updated_socket}
  end

  def handle_info(%{event: "updated", payload: payload}, socket) do
    flash = Map.get(payload, :flash, {:info, "something just changed"})

    updated_socket =
      socket
      |> refresh_game()
      |> update_flash(flash)

    {:noreply, updated_socket}
  end

  # def render(%{user_id: user_id} = assigns) when is_nil(user_id) do
  #   # redirect(NotSkullWeb.SessionView, "new.html", assigns)
  # end

  def render(%{game: %{active?: false, user_id: nil} = game} = assigns) do
    ~L"""
    <button phx-click="join">join game</button>

    <div>
    Players:
      <%= for player <- game.players do %>
        <%= player.name %>
      <%end%>
    </div>
    """
  end

  def render(%{game: %{active?: false} = game} = assigns) do
    ~L"""
    <div>
      <%= cond do %>
      <% is_nil(assigns.basic_player) -> %>
        <button phx-click="join">join</button>
      <% Enum.count(game.players) > 1 -> %>
        <button phx-click="start-game">start game</button>
      <% true -> %>
        Waiting for other players to join...
      <% end %>

      <div id="players">
      Players:
        <ul>
          <%= for player <- game.players do %>
            <li id=<%= player.id %>><%= player.name %><%= if player.id == assigns.user_id, do: "*", else: "" %></li>
          <%end%>
        </ul>
      </div>
    </div>
    """
  end

  def render(assigns) do
    {game, basic_player} = game_and_player(assigns)

    player_view(
      assigns,
      game.current_phase,
      game.current_player_id,
      basic_player,
      game
    )
  end

  def player_view(assigns, :first_card, current_player_id, basic_player, game) do
    current_player =
      Enum.find(game.players, fn player -> player.id == current_player_id end)

    user_player =
      Enum.find(game.players, fn player -> player.id == basic_player.id end)

    other_players =
      opponents_from_user_player_perspective(game.players, user_player.id)

    ~L"""
    <div class="container">
      <div class="alert">
        <%= if Enum.empty?(current_player.cards_on_table) do %>
        Please play your first card.
        <% end %>
      </div>
      <div class="opponents-section row center">
        <%= for opponent <- other_players do %>
          <%= opponent_html(assigns, opponent) %>
        <%end%>
      </div>

      <div class="row">
        <%= user_player_html(assigns, user_player) %>
      </div>
    </div>
    """
  end

  def player_view(assigns, :add_or_bet, current_player_id, basic_player, game) do
    current_player =
      Enum.find(game.players, fn player -> player.id == current_player_id end)

    user_player =
      Enum.find(game.players, fn player -> player.id == basic_player.id end)

    other_players =
      opponents_from_user_player_perspective(game.players, user_player.id)

    current_bet_value = game.current_bet.value || 0
    min_bet = current_bet_value + 1
    max_bet = game.total_cards_on_table
    possible_bet_values = min_bet..max_bet

    player_opts =
      if current_player.id == user_player.id do
        [possible_bet_values: possible_bet_values]
      else
        []
      end

    ~L"""
    <div class="container">
      <div class="alert">
      <%= if current_player.id == user_player.id do %>
        It's your turn. Please play a card or bet.
      <% else %>
        Waiting for <%= current_player.name %> to play or bet.
      <% end %>
      </div>
      <div class="opponents-section row center">
        <%= for opponent <- other_players do %>
          <%= opponent_html(assigns, opponent) %>
        <%end%>
      </div>

      <div class="row">
        <%= user_player_html(assigns, user_player, player_opts) %>
      </div>
    </div>
    """
  end

  defp opponents_from_user_player_perspective(players, user_player_id) do
    [_user_player | opponents] =
      rotate_player_to_front(players, user_player_id)

    opponents
  end

  defp rotate_player_to_front(
         [%{id: user_player_id} | _tail] = players,
         user_player_id
       ) do
    players
  end

  defp rotate_player_to_front([head | tail], user_player_id) do
    rotate_player_to_front(tail ++ [head], user_player_id)
  end

  defp opponent_html(assigns, opponent, _opts \\ []) do
    column_size = trunc(12 / (Enum.count(assigns.game.players) - 1))

    background_style =
      if opponent.id == assigns.game.current_player_id,
        do: "current-player",
        else: "waiting-player"

    ~L"""
    <div class="opponent col-<%=column_size%> <%= background_style %>">
      <%= opponent.name %>
      <div class="playingCards">
        <ul class="hand center">
          <%= for _card <- opponent.cards_in_hand do %>
            <li><div class="card back">*</div></li>
          <% end %>
        </ul>
      </div>
      In Hand: <%= Enum.count(opponent.cards_in_hand) %> cards
      On Table:
      <div class="playingCards">
        <%= for {value, position} <- opponent.cards_on_table do %>
          <button class="card back"><%= value %>:<%= position %></button>
        <% end %>
      </div>
    </div>
    """
  end

  defp user_player_html(assigns, user_player, opts \\ []) do
    possible_bet_values = Keyword.get(opts, :possible_bet_values, false)

    ~L"""
      <div class="user-player center">
        YOU: (<%= user_player.name %>)
        On Table:
        <div class="playingCards">
          <ul class="hand">
            <%= for {_value, _position} <- user_player.cards_on_table do %>
              <li>
                <div class="card back"></div>
              </li>
            <% end %>
          </ul>
        </div>
        In Hand:
        <div class="playingCards">
          <ul class="hand">
            <%= for value <- user_player.cards_in_hand do %>
              <div class="card" phx-click="add-<%= value %>">
                <li><div value="<%= value %>"><%= value %></div></li>
              </div>
            <% end %>
          </ul>
        </div>
        <%= if possible_bet_values do %>
        Possible Bets:
          <%= for value <- possible_bet_values do %>
            <button phx-click="bet-<%= value %>">Bet <%= value %></button>

          <% end %>
        <% end %>
      </div>
    """
  end

  defp assign_player(socket, player) do
    assign(socket, %{basic_player: player})
  end

  defp update_game(socket, game) do
    assign(socket, %{game: game})
  end

  defp update_flash(socket, {type, message}) do
    put_flash(socket, type, message)
  end

  defp game_and_player(%Phoenix.LiveView.Socket{} = socket) do
    {socket.assigns.game, socket.assigns.basic_player}
  end

  defp game_and_player(assigns) do
    {assigns.game, assigns.basic_player}
  end

  defp refresh_game(socket) do
    {:ok, updated_game} =
      NotSkull.ActiveGames.get_game_by_id(socket.assigns.game.id)

    assign(socket, game: updated_game)
  end
end

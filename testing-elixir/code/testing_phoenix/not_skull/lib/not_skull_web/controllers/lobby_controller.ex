#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.LobbyController do
  use NotSkullWeb, :controller
  alias NotSkull.ActiveGames

  def new(conn, _params) do
    with {:ok, user_id} <- get_user_from_session(conn),
         {:ok, %NotSkull.Accounts.User{} = user} <-
           NotSkull.Accounts.get_user_by_id(user_id) do
      player = %NotSkull.GameEngine.Player{
        name: user.name,
        id: user.id
      }

      {:ok, game} = ActiveGames.new_game(players: [player])

      redirect(conn, to: "/game?game_id=#{game.id}")
    end
  end

  defp get_user_from_session(conn) do
    if user = get_session(conn, :user_id) do
      {:ok, user}
    else
      {:error, :not_found}
    end
  end
end

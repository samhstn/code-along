#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.GameLiveTest do
  use NotSkullWeb.ConnCase

  import Phoenix.LiveViewTest
  alias NotSkull.ActiveGames

  describe "mount" do
    test "success: it renders the", %{conn: conn} do
      {:ok, user} = Factory.insert(:user)
      conn = Plug.Test.init_test_session(conn, user_id: user.id)

      player = %NotSkull.GameEngine.Player{
        name: user.name,
        id: user.id
      }

      {:ok, game} = NotSkull.ActiveGames.new_game(players: [player])

      conn = get(conn, "/game?game_id=#{game.id}")

      {:ok, view, _html} = live(conn)

      {:ok, user2} = Factory.insert(:user)
      player2 = %NotSkull.GameEngine.Player{name: user2.name, id: user2.id}
      {:ok, _updated_game} = ActiveGames.join(game.id, player2)

      send(view.pid, %{event: "joined"})

      result = render(view)

      assert [{_, _, [found_player_name]}] =
               Floki.parse_fragment!(result) |> Floki.find(id(player.id))

      assert found_player_name == user.name <> "*"
    end
  end

  def id(string) do
    "##{string}"
  end
end

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.PageLiveTest do
  use NotSkullWeb.ConnCase

  import Phoenix.LiveViewTest

  # TODO: this entire file has been ignored.
  test "disconnected and connected render", %{conn: conn} do
    {:ok, game} = NotSkull.ActiveGames.new_game()
    conn_w_session = Plug.Test.init_test_session(conn, %{user_id: Factory.uuid()})

    {:ok, page_live, disconnected_html} = live(conn_w_session, "/game?game_id=#{game.id}")

    assert disconnected_html =~ "join"
    assert render(page_live) =~ "join"
  end
end

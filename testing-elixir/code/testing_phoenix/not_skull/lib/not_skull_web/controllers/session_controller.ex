#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.SessionController do
  use NotSkullWeb, :controller

  alias NotSkull.Accounts.User
  alias NotSkull.Accounts

  action_fallback NotSkullWeb.FallbackController

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, %{"user" => %{"email" => email, "password" => password}}) do
    with {:ok, %User{} = user} <-
           Accounts.get_user_by_credentials(email, password) do
      conn
      |> put_session(:user_id, user.id)
      |> put_flash(:info, "Welcome back! It's good to see you again.")
      |> redirect(to: Routes.user_path(conn, :show, user.id))
    end
  end

  def delete(conn, _params) do
    conn
    |> clear_session()
    |> configure_session(drop: true)
    |> redirect(to: Routes.session_path(conn, :new))
  end
end

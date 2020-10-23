#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.Plugs.MatchJWTUserIdTest do
  use NotSkullWeb.ConnCase

  describe "call/2" do
    test "success: returns a conn if the client id from jwt and path match" do
      user = insert_user()

      input_conn =
        %{
          build_conn()
          | path_params: %{"id" => user.id},
            params: %{"context_id" => user.id}
        }
        |> Plug.Conn.fetch_query_params()

      returned_conn = NotSkullWeb.Plugs.MatchJWTUserId.call(input_conn, %{})

      assert returned_conn == input_conn
    end

    test "error: returns a halted Conn with status 403 if the user id from jwt and path don't match" do
      user = insert_user()

      conn =
        %{
          build_conn()
          | path_params: %{"id" => user.id}
        }
        |> Plug.Conn.fetch_query_params()

      user_id_for_jwt = Factory.uuid()

      input_conn =
        put_req_header(
          conn,
          "authorization",
          "Bearer " <> sign_jwt(user_id_for_jwt)
        )

      returned_conn = NotSkullWeb.Plugs.MatchJWTUserId.call(input_conn, %{})

      assert %Plug.Conn{halted: true, status: 403} = returned_conn

      assert returned_conn.resp_body |> Jason.decode!() == %{
               "errors" => [
                 %{
                   "field" => "token",
                   "message" => "You are not authorized for that action."
                 }
               ]
             }
    end
  end

  defp insert_user do
    {:ok, user} =
      Factory.atom_params(:user)
      |> NotSkull.Accounts.User.create_changeset()
      |> Repo.insert()

    user
  end
end

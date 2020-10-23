#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Plugs.ValidatJWTTest do
  use NotSkullWeb.ConnCase

  alias NotSkullWeb.Plugs.ValidateJWT

  describe "call/2" do
    test "success: returns a conn with user id in params if given valid jwt",
         %{conn: conn} do
      {:ok, user} = Factory.insert(:user)

      conn_with_token =
        put_req_header(conn, "authorization", "Bearer " <> sign_jwt(user.id))

      returned_conn = ValidateJWT.call(conn_with_token, _empty_params = %{})
      assert returned_conn.params["context_id"] == user.id
    end

    test "success: returns a halted conn with status of 401 if jwt is missing",
         %{conn: conn} do
      assert %Plug.Conn{halted: true, status: 401} =
               returned_conn = ValidateJWT.call(conn, _empty_params = %{})

      assert returned_conn.resp_body |> Jason.decode!() == %{
               "errors" => [
                 %{"field" => "token", "message" => "Invalid token."}
               ]
             }
    end
  end
end

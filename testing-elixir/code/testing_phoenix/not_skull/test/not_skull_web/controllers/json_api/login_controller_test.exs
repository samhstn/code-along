#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.JsonApi.LoginControllerTest do
  use NotSkullWeb.ConnCase

  setup context do
    password = Factory.password()

    {:ok, existing_user} =
      Factory.atom_params(:user, %{password: password})
      |> NotSkull.Accounts.User.create_changeset()
      |> Repo.insert()

    Map.merge(context, %{user: existing_user, password: password})
  end

  describe "login/2" do
    test "success: returns 200 for a valid credentials", %{
      conn: conn,
      user: user,
      password: password
    } do
      login_payload = %{
        "auth" => %{"email" => user.email, "password" => password}
      }

      response =
        conn
        |> post("/api/login", login_payload)
        |> json_response(200)

      token = response["data"]["token"]

      assert {:ok, claims} = claims_from_jwt(token)

      assert claims["cid"] == user.id
    end

    for field <- ["email", "password"] do
      test "error: returns 401 and error if #{field} is missing", %{
        conn: conn,
        user: user,
        password: password
      } do
        field = unquote(field)

        login_payload = %{
          "auth" =>
            Map.delete(
              %{"email" => user.email, "password" => password},
              field
            )
        }

        response =
          conn
          |> post("/api/login", login_payload)
          |> json_response(401)

        assert response["errors"] == [
                 %{"detail" => "#{field} is required to authenticate"}
               ]
      end
    end

    test "error: returns 401 and error if credentials are invalid", %{
      conn: conn,
      user: user
    } do
      login_payload = %{
        "auth" => %{"email" => user.email, "password" => Factory.password()}
      }

      response =
        conn
        |> post("/api/login", login_payload)
        |> json_response(401)

      assert response["errors"] == [
               %{"detail" => "invalid email or password"}
             ]
    end
  end
end

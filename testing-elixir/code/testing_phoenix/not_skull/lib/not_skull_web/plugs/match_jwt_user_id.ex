#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.Plugs.MatchJWTUserId do
  @moduledoc false

  def init(options) do
    options
  end

  def call(conn, _opts) do
    conn = Plug.Conn.fetch_query_params(conn)
    user_id_from_jwt = conn.params["context_id"]
    user_id_from_path = conn.path_params["id"]

    if user_id_from_jwt == user_id_from_path do
      conn
    else
      error =
        Jason.encode!(%{
          "errors" => [
            %{
              field: "token",
              message: "You are not authorized for that action."
            }
          ]
        })

      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.resp(403, error)
      |> Plug.Conn.halt()
    end
  end
end

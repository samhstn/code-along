#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.JsonApi.UserController do
  @moduledoc false

  use NotSkullWeb, :controller

  alias NotSkull.Accounts

  alias NotSkull.ExternalServices.Email

  def create(conn, params) do
    case Accounts.create_user(params) do
      {:ok, user} ->
        Email.send_welcome(user)

        conn
        |> put_status(201)
        |> json(user_map_from_struct(user))

      {:error, error_changeset} ->
        conn
        |> put_status(422)
        |> json(errors_from_changset(error_changeset))
    end
  end

  def update(conn, params) do
    with {:ok, user} <- Accounts.get_user_by_id(params["id"]), 
         {:ok, updated_user} <- Accounts.update_user(user, params) do
      conn 
      |> put_status(200)
      |> json(user_map_from_struct(updated_user))
    else
      {:error, error_changeset} ->
        conn 
        |> put_status(422)
        |> json(errors_from_changset(error_changeset))
    end
  end

  defp user_map_from_struct(user) do
    user
    |> Map.from_struct()
    |> Map.drop([:__struct__, :__meta__])
  end

  defp errors_from_changset(changeset) do
    serializable_errors =
      for {field, {message, _}} <- changeset.errors,
          do: %{"field" => to_string(field), "message" => message}

    %{errors: serializable_errors}
  end

end

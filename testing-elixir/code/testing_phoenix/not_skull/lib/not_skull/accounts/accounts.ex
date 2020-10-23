#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Accounts do
  @moduledoc false
  alias NotSkull.Repo
  alias NotSkull.Accounts.User
  alias NotSkull.Errors.{ResourceNotFoundError, InvalidEmailOrPasswordError}

  def get_user_by_id(user_id) do
    if user = Repo.get(User, user_id) do
      {:ok, user}
    else
      {:error, ResourceNotFoundError.exception(resource: "user", id: user_id)}
    end
  end

  def get_user_by_credentials(email, password) do
    with %User{} = user <- Repo.get_by(User, %{email: email}),
         true <- Argon2.verify_pass(password, user.password) do
      {:ok, user}
    else
      _ -> {:error, InvalidEmailOrPasswordError.exception()}
    end
  end

  def create_user(params) do
    params
    |> User.create_changeset()
    |> Repo.insert()
  end

  def update_user(user, update_params) do
    user
    |> User.update_changeset(update_params)
    |> Repo.update()
  end

  def delete_user(user) do
    Repo.delete(user)
  end
end

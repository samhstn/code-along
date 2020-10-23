#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Accounts.User do
  use NotSkull.Schema
  import Ecto.Changeset

  @optional_create_fields [:id, :inserted_at, :updated_at]
  @forbidden_update_fields [:id, :inserted_at, :updated_at]

  schema "users" do
    field(:email, :string)
    field(:name, :string)
    field(:password, :string)

    timestamps()
  end

  defp all_fields do
    __MODULE__.__schema__(:fields)
  end

  def create_changeset(params) do
    %__MODULE__{}
    |> cast(params, all_fields())
    |> validate_required(all_fields() -- @optional_create_fields)
    |> cleanup_email()
    |> hash_password()
    |> unique_constraint(:email)
  end

  def update_changeset(%__MODULE__{} = user, params) do
    user
    |> cast(params, all_fields() -- @forbidden_update_fields)
    |> validate_required(all_fields() -- @forbidden_update_fields)
    |> cleanup_email()
    |> hash_password()
    |> unique_constraint(:email)
  end

  defp cleanup_email(changeset) do
    changeset
    |> update_change(:email, &String.downcase/1)
    |> update_change(:email, &String.trim/1)
  end

  def hash_password(changeset) do
    changeset
    |> update_change(:password, &Argon2.hash_pwd_salt/1)
  end
end

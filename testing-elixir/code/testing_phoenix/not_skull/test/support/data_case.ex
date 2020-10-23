#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.DataCase do
  @moduledoc """
  This module defines the setup for tests requiring
  access to the application's data layer.

  You may define functions here to be used as helpers in
  your tests.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use NotSkull.DataCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import NotSkull.DataCase
      import Support.AssertionHelpers

      alias Ecto.Changeset
      alias NotSkull.Factory
      alias NotSkull.Repo
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(NotSkull.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(NotSkull.Repo, {:shared, self()})
    end

    :ok
  end

  def valid_params(fields_with_types) do
    valid_value_by_type = %{
      date: fn -> to_string(Faker.Date.date_of_birth()) end,
      float: fn -> :rand.uniform() * 10 end,
      string: fn -> Faker.Lorem.word() end,
      utc_datetime_usec: fn -> DateTime.utc_now() end,
      binary_id: fn -> Ecto.UUID.generate() end
    }

    for {field, type} <- fields_with_types, into: %{} do
      {Atom.to_string(field), valid_value_by_type[type].()}
    end
  end

  def invalid_params(fields_with_types) do
    invalid_value_by_type = %{
      date: fn -> Faker.Lorem.word() end,
      float: fn -> Faker.Lorem.word() end,
      string: fn -> DateTime.utc_now() end,
      utc_datetime_usec: fn -> Faker.Lorem.word() end,
      binary_id: fn -> 1 end
    }

    for {field, type} <- fields_with_types, into: %{} do
      {Atom.to_string(field), invalid_value_by_type[type].()}
    end
  end

  @doc """
  A helper that transforms changeset errors into a map of messages.

      assert {:error, changeset} = Accounts.create_user(%{password: "short"})
      assert "password is too short" in errors_on(changeset).password
      assert %{password: ["password is too short"]} = errors_on(changeset)

  """
  def errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Regex.replace(~r"%{(\w+)}", message, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end

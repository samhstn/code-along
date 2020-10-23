#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule Support.AssertionHelpers do
  @moduledoc false

  import ExUnit.Assertions

  def fields_for(%schema_name{__meta__: _}),
    do: schema_name.__schema__(:fields)

  def fields_for(%{} = map), do: Map.keys(map)
  def fields_for(schema_name), do: schema_name.__schema__(:fields)

  def db_assigned_fields(additions \\ []) do
    [:id, :inserted_at, :updated_at] ++ Keyword.fetch!(additions, :plus)
  end

  def assert_values_for(all_the_things) do
    expected = Keyword.fetch!(all_the_things, :expected)
    actual = Keyword.fetch!(all_the_things, :actual)
    fields = Keyword.fetch!(all_the_things, :fields)

    opts = all_the_things[:opts] || []

    expected = update_keys(expected)
    actual = update_keys(actual)
    fields = maybe_convert_fields_to_atoms(fields)

    for field <- fields do
      with {{:ok, expected}, _} <- {Map.fetch(expected, field), :expected},
           {{:ok, actual}, _} <- {Map.fetch(actual, field), :actual} do
        expected =
          maybe_convert_datetime_to_string(expected, opts[:convert_dates])

        actual =
          maybe_convert_datetime_to_string(actual, opts[:convert_dates])

        assert(
          expected == actual,
          "Values did not match for field: #{field}\nexpected: #{
            inspect(expected)
          }\nactual: #{inspect(actual)}"
        )
      else
        {:error, type} ->
          flunk("Key for field: #{field} didn't exist in #{type}")
      end
    end
  end

  defp maybe_convert_fields_to_atoms(fields) do
    Enum.map(
      fields,
      fn
        field when is_binary(field) -> String.to_atom(field)
        field when is_atom(field) -> field
      end
    )
  end

  defp update_keys({map, :string_keys}) when is_map(map) do
    for {key, value} <- map, into: %{}, do: {String.to_atom(key), value}
  end

  defp update_keys({map, :atom_keys}) do
    map
  end

  defp update_keys(map) do
    map
  end

  def ecto_struct_as_map(%_schema_name{} = schema) do
    schema
    |> Map.from_struct()
    |> Map.delete(:__schema__)
  end

  def ecto_struct_as_map_with_string_keys(%_schema_name{} = schema) do
    for {key, value} <- ecto_struct_as_map(schema),
        into: %{},
        do: {Atom.to_string(key), value}
  end

  defp maybe_convert_datetime_to_string(
         %DateTime{} = datetime,
         true = _convert_dates
       ) do
    DateTime.to_iso8601(datetime)
  end

  defp maybe_convert_datetime_to_string(datetime, _) do
    datetime
  end

  def assert_unordered_lists_are_equal(lists) do
    expected = Keyword.fetch!(lists, :expected)
    actual = Keyword.fetch!(lists, :actual)
    assert Enum.sort(expected) == Enum.sort(actual)
  end

  def is_uuid(id) do
    case Ecto.UUID.dump(id) do
      {:ok, _} -> true
      _ -> false
    end
  end
end

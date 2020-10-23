#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule TestingEcto.Schemas.UserBasicSchema1Test do
  use ExUnit.Case
  alias Ecto.Changeset
  alias TestingEcto.Schemas.UserBasicSchema

  @schema_fields [
    :date_of_birth,
    :favorite_number,
    :first_name,
    :last_name,
    :phone_number
  ]

  describe "changeset/1" do
    test "success: returns a valid changeset when given valid arguments" do
      params = %{
        "date_of_birth" => "1948-02-28",
        "email" => "example@example.com",
        "favorite_number" => 3.14,
        "first_name" => "Bob",
        "last_name" => "Matthews",
        "phone_number" => "555-555-5555"
      }

      changeset = UserBasicSchema.changeset(params) 
      assert %Changeset{valid?: true, changes: changes} = changeset

      mutated = [:date_of_birth]

      for field <- @schema_fields, field not in mutated do
        actual = Map.get(changes, field)
        expected = params[Atom.to_string(field)]
        assert actual == expected,
               "Values did not match for field: #{field}\nexpected: #{
                 inspect(expected)
               }\nactual: #{inspect(actual)}"
      end

      expected_dob = Date.from_iso8601!(params["date_of_birth"])
      assert changes.date_of_birth == expected_dob
    end


    test "error: returns an error changeset when given un-castable values" do
      not_a_string = DateTime.utc_now()

      params = %{
        "date_of_birth" => "not a date",
        "favorite_number" => "not a number",
        "first_name" => not_a_string,
        "last_name" => not_a_string,
        "phone_number" => not_a_string
      }

      changeset = UserBasicSchema.changeset(params)

      assert %Changeset{valid?: false, errors: errors} = changeset

      for field <- @schema_fields do
        assert errors[field], "expected an error for #{field}" 
        {_, meta} = errors[field]

        assert meta[:validation] == :cast,
               "The validation type, #{meta[:validation]}, is incorrect." 
      end
    end

    test "error: returns error changeset when required fields are missing" do
      params = %{}

      assert %Changeset{valid?: false, errors: errors} =
               UserBasicSchema.changeset(params)

      optional_params = [:favorite_number]
      expected_fields = @schema_fields -- optional_params

      for field <- expected_fields do
        assert errors[field], "The field #{inspect(field)} is missing from errors."
        {_, meta} = errors[field]

        assert meta[:validation] == :required,
               "The validation type, #{meta[:validation]}, is incorrect."
      end
    end
  end
end

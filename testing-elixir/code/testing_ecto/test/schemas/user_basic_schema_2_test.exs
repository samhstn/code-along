#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule TestingEcto.Schemas.UserBasicSchema2Test do
  use ExUnit.Case
  alias Ecto.Changeset
  alias TestingEcto.Schemas.UserBasicSchema

  @expected_fields_with_types [
    {:date_of_birth, :date},
    {:email, :string},
    {:favorite_number, :float},
    {:first_name, :string},
    {:last_name, :string},
    {:phone_number, :string}
  ]
  @optional [:favorite_number]
  describe "fields and types" do   
    @tag :schema_definition  
    test "it has the correct fields and types" do
      actual_fields_with_types =
        for field <- UserBasicSchema.__schema__(:fields) do 
          type = UserBasicSchema.__schema__(:type, field) 
          {field, type}
        end

      assert MapSet.new(actual_fields_with_types) ==
               MapSet.new(@expected_fields_with_types)
    end
  end


  describe "changeset/1" do
    test "success: returns a valid changeset when given valid arguments" do
      valid_params = valid_params(@expected_fields_with_types)

      changeset = UserBasicSchema.changeset(valid_params)
      assert %Changeset{valid?: true, changes: changes} = changeset

      mutated = [:date_of_birth]

      for {field, _} <- @expected_fields_with_types, field not in mutated do
        actual = Map.get(changes, field)
        expected = valid_params[Atom.to_string(field)]
        assert actual == expected,
               "Values did not match for field: #{field}\nexpected: #{
                 inspect(expected)
               }\nactual: #{inspect(actual)}"
      end

      expected_dob = Date.from_iso8601!(valid_params["date_of_birth"])
      assert changes.date_of_birth == expected_dob
    end


    test "error: returns in error changeset when given un-castable values" do
      invalid_params = invalid_params(@expected_fields_with_types)  

      assert %Changeset{valid?: false, errors: errors} =
               UserBasicSchema.changeset(invalid_params)

      for {field, _} <- @expected_fields_with_types do
        assert errors[field], "The field :#{field} is missing from errors."
        {_, meta} = errors[field]

        assert meta[:validation] == :cast,
               "The validation type, #{meta[:validation]}, is incorrect."
      end
    end


    test "error: returns error changeset when required fields are missing" do
      params = %{}

      assert %Changeset{valid?: false, errors: errors} =
               UserBasicSchema.changeset(params)

      for {field, _} <- @expected_fields_with_types, field not in @optional do 
        assert errors[field], "The field :#{field} is missing from errors."
        {_, meta} = errors[field]

        assert meta[:validation] == :required,
               "The validation type, #{meta[:validation]}, is incorrect."
      end
    end

  end

  defp valid_params(fields_with_types) do
    valid_value_by_type = %{
      date: fn -> to_string(Faker.Date.date_of_birth()) end,
      float: fn -> :rand.uniform() * 10 end,
      string: fn -> Faker.Lorem.word() end
    }

    for {field, type} <- fields_with_types, into: %{} do
      {Atom.to_string(field), valid_value_by_type[type].()}
    end
  end


  defp invalid_params(fields_with_types) do
    invalid_value_by_type = %{
      date: fn -> Faker.Lorem.word() end,
      float: fn -> Faker.Lorem.word() end,
      string: fn -> DateTime.utc_now() end
    }

    for {field, type} <- fields_with_types, into: %{} do
      {Atom.to_string(field), invalid_value_by_type[type].()}
    end
  end


end

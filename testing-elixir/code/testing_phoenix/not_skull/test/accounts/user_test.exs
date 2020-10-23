#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Accounts.UserTest do
  use NotSkull.DataCase
  alias NotSkull.Accounts.User

  @expected_fields_with_types [
    {:id, :binary_id},
    {:email, :string},
    {:name, :string},
    {:password, :string},
    {:inserted_at, :utc_datetime_usec},
    {:updated_at, :utc_datetime_usec}
  ]
  @optional_for_create [:id, :inserted_at, :updated_at]
  @forbidden_update_fields [:id, :inserted_at, :updated_at]
  @update_fields_with_types for {field, type} <-
                                  @expected_fields_with_types,
                                field not in @forbidden_update_fields,
                                do: {field, type}

  describe "fields and types" do
    @tag :schema_definition
    test "it has the correct fields and types" do
      actual_fields_with_types =
        for field <- User.__schema__(:fields) do
          type = User.__schema__(:type, field)
          {field, type}
        end

      assert Enum.sort(actual_fields_with_types) ==
               Enum.sort(@expected_fields_with_types)
    end
  end

  describe "create_changeset/1" do
    test "success: returns a valid changeset when given valid arguments" do
      valid_params = valid_params(@expected_fields_with_types)

      changeset = User.create_changeset(valid_params)
      assert %Changeset{valid?: true, changes: changes} = changeset

      mutated = [:password]

      for {field, _} <- @expected_fields_with_types, field not in mutated do
        actual = Map.get(changes, field)
        expected = valid_params[Atom.to_string(field)]

        assert actual == expected,
               "Values did not match for field: #{field}\nexpected: #{
                 inspect(expected)
               }\nactual: #{inspect(actual)}"
      end

      assert "$argon2id$v=19$m=131072,t=8,p=4" <> _hashed_pw =
               changes.password
    end

    test "success: it downcases and strips email address" do
      bad_email = "Jeffrey@matthias.org "
      params = Factory.string_params(:user, %{email: bad_email})

      assert %Changeset{valid?: true, changes: changes} =
               User.create_changeset(params)

      expected_email =
        bad_email
        |> String.downcase()
        |> String.trim()

      assert changes.email == expected_email
    end

    test "error: returns in error changeset when given un-castable values" do
      invalid_params = invalid_params(@expected_fields_with_types)

      assert %Changeset{valid?: false, errors: errors} =
               User.create_changeset(invalid_params)

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
               User.create_changeset(params)

      for {field, _} <- @expected_fields_with_types,
          field not in @optional_for_create do
        assert errors[field], "The field :#{field} is missing from errors."
        {_, meta} = errors[field]

        assert meta[:validation] == :required,
               "The validation type, #{meta[:validation]}, is incorrect."
      end
    end

    test "error: returns error changeset when an email address is reused" do
      Ecto.Adapters.SQL.Sandbox.checkout(NotSkull.Repo)

      {:ok, existing_user} =
        valid_params(@expected_fields_with_types)
        |> User.create_changeset()
        |> NotSkull.Repo.insert()

      changeset_with_repeated_email =
        valid_params(@expected_fields_with_types)
        |> Map.put("email", existing_user.email)
        |> User.create_changeset()

      assert {:error, %Changeset{valid?: false, errors: errors}} =
               NotSkull.Repo.insert(changeset_with_repeated_email)

      assert errors[:email], "The field :email is missing from errors."
      {_, meta} = errors[:email]

      assert meta[:constraint] == :unique,
             "The validation type, #{meta[:validation]}, is incorrect."
    end
  end

  describe "update_changeset/1" do
    setup do
      {:ok, user} = Factory.insert(:user)

      %{user: user}
    end

    test "success: returns a valid changeset when given valid arguments", %{
      user: user
    } do
      valid_params = valid_params(@update_fields_with_types)

      changeset = User.update_changeset(user, valid_params)
      assert %Changeset{valid?: true, changes: changes} = changeset

      mutated = [:password]

      for {field, _} <- @update_fields_with_types, field not in mutated do
        assert Map.get(changes, field) == valid_params[Atom.to_string(field)]
      end

      assert "$argon2id$v=19$m=131072,t=8,p=4" <> _hashed_pw =
               changes.password
    end

    test "success: it downcases and strips email address", %{
      user: user
    } do
      bad_email = "Jeffrey@matthias.org "
      params = Factory.string_params(:user, %{email: bad_email})

      assert %Changeset{valid?: true, changes: changes} =
               User.update_changeset(user, params)

      expected_email =
        bad_email
        |> String.downcase()
        |> String.trim()

      assert changes.email == expected_email
    end

    test "error: returns in error changeset when given un-castable values", %{
      user: user
    } do
      invalid_params = invalid_params(@update_fields_with_types)

      assert %Changeset{valid?: false, errors: errors} =
               User.update_changeset(user, invalid_params)

      for {field, _} <- @update_fields_with_types do
        assert errors[field], "The field :#{field} is missing from errors."
        {_, meta} = errors[field]

        assert meta[:validation] == :cast,
               "The validation type, #{meta[:validation]}, is incorrect."
      end
    end

    test "error: returns error changeset when required fields are missing", %{
      user: user
    } do
      params =
        for {field, _type} <- @update_fields_with_types,
            into: %{},
            do: {field, ""}

      assert %Changeset{valid?: false, errors: errors} =
               User.update_changeset(user, params)

      for {field, _} <- @update_fields_with_types do
        assert errors[field], "The field :#{field} is missing from errors."
        {_, meta} = errors[field]

        assert meta[:validation] == :required,
               "The validation type, #{meta[:validation]}, is incorrect."
      end
    end

    test "error: returns error changeset when an email address is reused", %{
      user: user
    } do
      {:ok, existing_user} =
        valid_params(@expected_fields_with_types)
        |> User.create_changeset()
        |> NotSkull.Repo.insert()

      params_with_repeated_email =
        valid_params(@update_fields_with_types)
        |> Map.put("email", existing_user.email)

      changeset_with_repeated_email =
        User.update_changeset(user, params_with_repeated_email)

      assert {:error, %Changeset{valid?: false, errors: errors}} =
               NotSkull.Repo.update(changeset_with_repeated_email)

      assert errors[:email], "The field :email is missing from errors."
      {_, meta} = errors[:email]

      assert meta[:constraint] == :unique,
             "The validation type, #{meta[:validation]}, is incorrect."
    end
  end
end

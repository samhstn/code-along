#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.AccountsTest do
  use NotSkull.DataCase
  alias Ecto.Changeset
  alias NotSkull.Accounts
  alias NotSkull.Accounts.User
  alias NotSkull.Errors

  describe "get_user_by_id/1" do
    test "success: it returns a user" do
      {:ok, existing_user} = Factory.insert(:user)

      assert {:ok, %User{} = returned_user} =
               Accounts.get_user_by_id(existing_user.id)

      assert_values_for(
        expected: existing_user,
        actual: returned_user,
        fields: fields_for(User)
      )
    end

    test "error: it returns an error tuple when user id doesn't exist" do
      assert {:error, %Errors.ResourceNotFoundError{}} =
               Accounts.get_user_by_id(Factory.uuid())
    end
  end

  describe "get_user_by_credentials/2" do
    setup [:with_existing_user]

    test "success: it returns user when credentials are valid", %{existing_user: existing_user, password: password} do
      assert {:ok, %User{} = returned_user} =
               Accounts.get_user_by_credentials(existing_user.email, password)

      assert_values_for(
        expected: existing_user,
        actual: returned_user,
        fields: fields_for(User)
      )
    end

    test "error: it returns an error tuple when email isn't valid" do
      password = Factory.password()
      non_existing_email = Factory.email()

      assert {:error, %Errors.InvalidEmailOrPasswordError{}} =
               Accounts.get_user_by_credentials(non_existing_email, password)
    end


    test "error: it returns an error tuple when email is good but password isn't valid", %{existing_user: existing_user} do
      bad_password = Factory.password()

      assert {:error, %Errors.InvalidEmailOrPasswordError{}} =
               Accounts.get_user_by_credentials(
                 existing_user.email,
                 bad_password
               )
    end
  end

  describe "create_user/1" do
    test "success: it creates and returns a user when given valid params" do
      params = Factory.string_params(:user)

      assert {:ok, %User{} = returned_user} = Accounts.create_user(params)

      user_from_db = Repo.get(User, returned_user.id)

      assert user_from_db == returned_user

      assert_values_for(
        expected: {params, :string_keys},
        actual: user_from_db,
        fields: fields_for(User) -- db_assigned_fields(plus: [:password])
      )
    end

    test "error: it returns an error/changeset tuple when user can't be created" do
      bad_params = %{}

      assert {:error, %Changeset{}} = Accounts.create_user(bad_params)
    end
  end

  describe "update_user/2" do
    setup [:with_existing_user]

    test "success: it updates and returns a user when given good params", %{
      existing_user: existing_user
    } do
      updatable_params = [:email]

      update_params =
        :user
        |> Factory.atom_params()
        |> Map.take(updatable_params)

      assert {:ok, %User{} = returned_user} =
               Accounts.update_user(existing_user, update_params)

      user_from_db = Repo.get(User, existing_user.id)

      assert returned_user == user_from_db

      # make sure only the updatable params changed
      assert_values_for(
        expected: existing_user,
        actual: returned_user,
        fields: fields_for(User) -- (updatable_params ++ [:updated_at])
      )

      # make sure any updateable params were updated
      assert_values_for(
        expected: update_params,
        actual: returned_user,
        fields: updatable_params
      )

      # make sure the updated_at is newer
      assert DateTime.compare(
               existing_user.updated_at,
               returned_user.updated_at
             ) == :lt
    end

    test "error: it returns an error/changeset tuple when user can't be updated",
         %{existing_user: existing_user} do
      {:ok, %{email: already_used_email}} = Factory.insert(:user)

      bad_params = %{"email" => already_used_email}

      assert {error, %Changeset{}} =
               Accounts.update_user(existing_user, bad_params)
    end
  end

  describe "delete/1" do
    setup [:with_existing_user]

    test "success: it deletes the user", %{existing_user: existing_user} do
      assert {:ok, _deleted_user} = Accounts.delete_user(existing_user)

      refute Repo.get(User, existing_user.id)
    end
  end

  defp with_existing_user(context) do
    password = Factory.password()

    {:ok, existing_user} = Factory.insert(:user, %{password: password})

    Map.merge(context, %{existing_user: existing_user, password: password})
  end
end

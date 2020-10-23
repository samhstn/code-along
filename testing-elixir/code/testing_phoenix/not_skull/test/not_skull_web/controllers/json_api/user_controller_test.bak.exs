#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.JsonApi.UserControllerTest do
  use NotSkullWeb.ConnCase, async: false  

  alias NotSkull.Accounts.User


  describe "POST /users" do
    test "success: creates_user, redirects to show page when user is created",
         %{conn: conn} do
      params = Factory.string_params(:user)

      # welcome email should get sent
      expect_email_to(params["email"])

      conn = post(conn, "/api/users", params)

      assert body = json_response(conn, 201)

      user_from_db = Repo.get(User, body["id"])

      assert_values_for(
        expected: {params, :string_keys},
        actual: user_from_db,
        fields: fields_for(User) -- db_assigned_fields(plus: [:password])
      )
    end

    test "error: does not insert, returns erros when given invalid attributes",
         %{
           conn: conn
         } do
      flunk_if_email_is_sent()

      expected_user_count = Repo.all(User) |> Enum.count()
      conn = post(conn, "/api/users", user: %{})

      assert body = json_response(conn, 422)

      actual_errors = body["errors"]
      refute Enum.empty?(actual_errors)

      expected_error_keys = ["field", "message"]

      for error <- actual_errors do
        assert_unordered_lists_are_equal(
          actual: Map.keys(error),
          expected: expected_error_keys
        )
      end

      assert Repo.all(User) |> Enum.count() == expected_user_count,
             "There should have been no records inserted during this test."
    end
  end

  describe "PUT /api/users/:id" do
    setup context do
      {:ok, user} = Factory.insert(:user)

      conn_with_token = 
        put_req_header(  
          context.conn,
          "authorization",
          "Bearer " <> sign_jwt(user.id)  
        )

      Map.merge(context, %{user: user, conn_with_token: conn_with_token})
    end

    test "success: updates db returns record with good params", %{
      conn_with_token: conn_with_token,
      user: existing_user
    } do
      new_name = "#{existing_user.name}-updated"

      conn =
        put(conn_with_token, "/api/users/#{existing_user.id}", %{
          name: new_name
        })

      assert parsed_return = json_response(conn, 200)

      user_from_db = Repo.get(User, existing_user.id)

      assert_values_for( 
        expected: %{existing_user | name: new_name},
        actual: user_from_db,
        fields: fields_for(User) -- [:updated_at] 
      )

      assert DateTime.to_unix(user_from_db.updated_at, :microsecond) > 
               DateTime.to_unix(existing_user.updated_at, :microsecond)

      # checking that the updated record is what is returned from endpoint
      assert_values_for(  
        expected: user_from_db,
        actual: {parsed_return, :string_keys},
        fields: fields_for(User),
        opts: [convert_dates: true]
      )
    end


    test "error: does not update, returns errors when given invalid attributes",
         %{
           conn_with_token: conn_with_token,  
           user: existing_user
         } do
      conn =
        put(conn_with_token, "/api/users/#{existing_user.id}", %{name: ""})  

      assert body = json_response(conn, 422) 

      user_from_db = Repo.get(User, existing_user.id)
      assert user_from_db == existing_user  

      actual_errors = body["errors"]
      refute Enum.empty?(actual_errors)

      expected_error_keys = ["field", "message"]

      for error <- actual_errors do  
        assert_unordered_lists_are_equal(
          actual: Map.keys(error),
          expected: expected_error_keys
        )
      end
    end


    test "auth error: returns 401 when valid jwt isn't in headers", %{
      conn: conn,  
      user: existing_user
    } do
      conn =
        put(conn, "/api/users/#{existing_user.id}", %{ 
          name: "#{existing_user.name}-updated"
        })

      assert body = json_response(conn, 401) 

      assert %{ 
               "errors" => [
                 %{"message" => "Invalid token.", "field" => "token"}
               ]
             } == body

      user_from_db = Repo.get(User, existing_user.id)  

      assert_values_for(
        expected: existing_user,
        actual: user_from_db,
        fields: fields_for(User)
        )
    end

    test "auth error: returns 403 when path and jwt user ids don't match",
         %{
           conn_with_token: conn_with_token,
           user: existing_user
         } do
          conn =
            put(conn_with_token, "/api/users/#{Factory.uuid()}", %{
              name: "#{existing_user.name}-updated"
            })

      assert body = json_response(conn, 403)

      assert %{
               "errors" => [
                 %{
                   "message" => "You are not authorized for that action.",
                   "field" => "token"
                 }
               ]
             } == body

      user_from_db = Repo.get(User, existing_user.id)

      assert_values_for(
        expected: existing_user,
        actual: user_from_db,
        fields: fields_for(User)
        )
    end
  end
end

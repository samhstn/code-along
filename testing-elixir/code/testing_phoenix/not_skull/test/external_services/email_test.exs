#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.ExternalServices.EmailTest do
  use ExUnit.Case
  import Mox
  alias NotSkull.Factory
  alias NotSkull.ExternalServices.Email
  alias NotSkull.Accounts.User

  @sendgrid_api_key Application.get_env(:not_skull, :email_api_key)
  @email_from_address Application.get_env(:not_skull, :email_from_address)

  describe "send_welcome" do
    setup [:verify_on_exit!]

    test "success: it sends a welcome email" do
      {:ok, user} =
        :user
        |> Factory.atom_params()
        |> User.create_changeset()
        |> Factory.build_one()

      expect(HttpClientMock, :request, fn method,
                                          url,
                                          headers,
                                          body,
                                          options ->
        assert options == []
        assert method == :post
        assert url == "https://api.sendgrid.com/v3/mail/send"

        assert Enum.count(headers) == 2
        assert {"Authorization", "Bearer #{@sendgrid_api_key}"} in headers
        assert {"Content-Type", "application/json"} in headers

        expected_email_subject = "Welcome to NotSkull"
        expected_email_body = "You have been added to NotSkull. Welcome."

        assert body == %{
                 "content" => [
                   %{"type" => "text/plain", "value" => expected_email_body}
                 ],
                 "from" => %{"email" => @email_from_address},
                 "personalizations" => [
                   %{
                     "to" => [
                       %{"email" => user.email}
                     ]
                   }
                 ],
                 "subject" => expected_email_subject
               }
      end)

      Email.send_welcome(user)
    end
  end
end

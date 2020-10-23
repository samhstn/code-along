#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use NotSkullWeb.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      import Ecto
      import Mox
      import NotSkullWeb.ConnCase
      import Phoenix.ConnTest
      import Plug.Conn
      import Support.AssertionHelpers
      import Support.JWTHelpers

      alias NotSkull.Factory
      alias NotSkull.Repo
      alias NotSkullWeb.Router.Helpers, as: Routes

      # The default endpoint for testing
      @endpoint NotSkullWeb.Endpoint
    end
  end

  setup tags do
    Mox.verify_on_exit!() 

    :ok = Ecto.Adapters.SQL.Sandbox.checkout(NotSkull.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(NotSkull.Repo, {:shared, self()}) 
    end

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end

  def expect_email_to(expected_email_address) do
    Mox.expect(HttpClientMock, :request, fn method,  
                                            url,
                                            _headers,
                                            body,
                                            _opts ->
      assert method == :post
      assert url == "https://api.sendgrid.com/v3/mail/send"

      assert %{
               "personalizations" => [
                 %{
                   "to" => [
                     %{"email" => ^expected_email_address} 
                   ]
                 }
               ]
             } = body
    end)
  end

  def flunk_if_email_is_sent do
    Mox.stub(HttpClientMock, :request, fn _, _, _, _, _ ->
      flunk("An email should not have been sent.")
    end)
  end
end

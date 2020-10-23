#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule YourApp.SetupWithHelperFunctionsTest do
  use ExUnit.Case

  setup [:create_organization, :with_admin, :with_authenticated_user]

  def create_organization(context) do
    organization = Organization.create()
    Map.put(context, :organization, organization)
  end
  
  def with_admin(context) do
    user = User.create(%{name: "Meg Ross"})
    admin = TestHelper.make_admin(user)
    Map.put(context, :admin, admin)
  end

  def with_authenticated_user(context) do
    user = User.create(%{name: "Bob Robertson"})
    authenticated_user = TestHelper.authenticate(user)

    Map.put(context, :authenticated_user, user)
  end
end

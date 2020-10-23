#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule MyApp.RebooterBehaviour do
  @callback reboot() :: :ok
end

defmodule MyApp.Rebooter do
  @behaviour MyApp.RebooterBehaviour

  @impl true
  defdelegate reboot(), to: :init
end

Mox.defmock(RebooterFake, for: MyApp.Rebooter)

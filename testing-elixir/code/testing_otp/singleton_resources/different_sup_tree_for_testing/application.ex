#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule MyApp.Application do
  use Application

  @impl true
  @mix_env Mix.env()

  def start(_type, _args) do
    Supervisor.start_link(children(@mix_env), strategy: :one_for_one)
  end
  
  defp children(:test) do
    [
      MyApp.OtherChild
    ]
  end
  
  defp children(_env) do
    [
      MyApp.VMRestarter,
      MyApp.OtherChild
    ]
  end
end

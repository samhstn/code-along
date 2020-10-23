#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.MixProject do
  use Mix.Project

  def project do
    [
      app: :soggy_waffle,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {SoggyWaffle.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      # other dependencies
      {:bypass, ">= 0.0.0", only: :test}
    ]
  end
end
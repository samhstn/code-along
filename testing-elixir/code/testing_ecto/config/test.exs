#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
use Mix.Config

config :testing_ecto, TestingEcto.Repo,
  database: "testing_ecto_test",
  pool: Ecto.Adapters.SQL.Sandbox

config :logger, :console, level: :warn

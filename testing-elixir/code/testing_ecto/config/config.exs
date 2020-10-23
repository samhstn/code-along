#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
use Mix.Config

config :testing_ecto,
  ecto_repos: [TestingEcto.Repo]

config :testing_ecto, TestingEcto.Repo,
  username: "testing_ecto",
  password: "testing_ecto_password",
  database: "testing_ecto_dev",
  hostname: "localhost",
  pool_size: 10,
  migration_timestamps: [type: :utc_datetime_usec]

  import_config("#{Mix.env()}.exs")

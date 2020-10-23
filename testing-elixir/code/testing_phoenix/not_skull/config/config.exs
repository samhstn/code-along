#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :not_skull,
  ecto_repos: [NotSkull.Repo]

config :not_skull, NotSkull.Repo,
  username: "postgres",
  password: "postgres",
  database: "not_skull_dev",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  migration_timestamps: [type: :utc_datetime_usec],
  pool_size: 10

# Configures the endpoint
config :not_skull, NotSkullWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base:
    "Fgc+mP/ZqduwF6eEjQ0S0h7CMXN5X79PiwJxUWn8ZIqSEZBHXHRPGpasAh/uWV6g",
  render_errors: [
    view: NotSkullWeb.ErrorView,
    accepts: ~w(html json),
    layout: false
  ],
  pubsub_server: NotSkull.PubSub,
  live_view: [signing_salt: "oGvcL5BXI+YC6KuWsaPH4VopY89BflIA"]

config :not_skull,
  secret_passphrase:
    "Q9cCDOa+5Td1MpZxnig8mCk1xfYhJzSBpDbp/vVdl73Z2EKMpyYubtpFh4rx3B66",
  user_auth_ttl_seconds: 1 * 24 * 60 * 60,
  jwt_algorithm: "HS256"

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :not_skull,
  email_api_key: System.get_env("SENDGRID_API_KEY"),
  email_from_address: System.get_env("FROM_ADDRESS")

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"

import Config

if Mix.env() == :test do
  config :logger, level: :warn
else
  config :logger, level: :info
end

config :todo, http_port: 5454

import_config "#{Mix.env()}.exs"

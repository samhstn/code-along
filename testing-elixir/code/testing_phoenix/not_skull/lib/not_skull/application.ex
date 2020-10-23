#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      NotSkull.Repo,
      # Start the Telemetry supervisor
      NotSkullWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: NotSkull.PubSub},
      # Start the Endpoint (http/https)
      NotSkullWeb.Endpoint,
      # Start a worker by calling: NotSkull.Worker.start_link(arg)
      # {NotSkull.Worker, arg}
      NotSkull.ActiveGames
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: NotSkull.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    NotSkullWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

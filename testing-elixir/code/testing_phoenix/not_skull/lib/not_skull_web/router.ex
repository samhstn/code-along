#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.Router do
  use NotSkullWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {NotSkullWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated_api do
    plug :accepts, ["json"]
    plug NotSkullWeb.Plugs.ValidateJWT
    plug NotSkullWeb.Plugs.MatchJWTUserId
  end

  scope "/", NotSkullWeb do
    pipe_through :browser
    get "/", WelcomeController, :index

    get "/login", SessionController, :new
    post "/login", SessionController, :create
    delete "/logout", SessionController, :delete

    post "/lobby/new", LobbyController, :new

    resources "/users", UserController

    live "/game", Game
  end

  scope "/api", NotSkullWeb.JsonApi do
    pipe_through :api

    post "/login", LoginController, :login
  end

  scope "/api", NotSkullWeb.JsonApi do
    pipe_through :api

    resources "/users", UserController, only: [:create]
  end

  scope "/api", NotSkullWeb.JsonApi do
    pipe_through :authenticated_api

    resources "/users", UserController, only: [:update]
  end

  # Enables LiveDashboard only for development
  #
  # If you want to use the LiveDashboard in production, you should put
  # it behind authentication and allow only admins to access it.
  # If your application does not have an admins-only section yet,
  # you can use Plug.BasicAuth to set up some basic authentication
  # as long as you are also using SSL (which you should anyway).
  if Mix.env() in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: NotSkullWeb.Telemetry
    end
  end
end

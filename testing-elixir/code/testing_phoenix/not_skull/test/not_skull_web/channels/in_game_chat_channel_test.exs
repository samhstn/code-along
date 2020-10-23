#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkullWeb.InGameChatChannelTest do
  use NotSkullWeb.ChannelCase

  alias NotSkullWeb.{
    InGameChatChannel,
    UserSocket
  }

  describe "connecting to socket" do
    test "success: connects when passed a valid token" do
      {:ok, user} = Factory.insert(:user)
      jwt = sign_jwt(user.id)

      assert {:ok, socket} = connect(UserSocket, %{token: jwt})

      assert socket.id == "user_socket:#{user.id}"
      assert socket.assigns.user_id == user.id
    end

    @tag capture_log: true
    test "refuses connection when passed a bad token" do
      assert :error = connect(UserSocket, %{token: "bad token"})
    end

    test "refuses connection when there is no user_id" do
      assert :error = connect(UserSocket, _params_without_user_id = %{})
    end
  end

  describe "joining a specific chat channel" do
    test "success: returns :success when user is player in game" do
      player = %NotSkull.GameEngine.Player{
        name: Factory.first_name(),
        id: Factory.uuid()
      }

      {:ok, game} = NotSkull.ActiveGames.new_game(players: [player])

      socket = socket(UserSocket, "user_id", %{user_id: player.id})

      assert {:ok, :joined, _socket} =
               subscribe_and_join(
                 socket,
                 InGameChatChannel,
                 "in_game_chat:#{game.id}",
                 %{}
               )
    end

    test "error: returns 'unauthorized' when user is not player in game" do
      player = %NotSkull.GameEngine.Player{
        name: Factory.first_name(),
        id: Factory.uuid()
      }

      {:ok, game} = NotSkull.ActiveGames.new_game(players: _no_players = [])

      socket = socket(UserSocket, "user_id", %{user_id: player.id})

      assert {:error, %{reason: :unauthorized}} =
               subscribe_and_join(
                 socket,
                 InGameChatChannel,
                 "in_game_chat:#{game.id}",
                 %{}
               )
    end
  end

  describe "sending and receiving messages" do
    setup do
      player = %NotSkull.GameEngine.Player{
        name: Factory.first_name(),
        id: Factory.uuid()
      }

      {:ok, game} = NotSkull.ActiveGames.new_game(players: [player])

      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{user_id: player.id})
        |> subscribe_and_join(
          InGameChatChannel,
          "in_game_chat:#{game.id}",
          %{}
        )

      %{socket: socket, player: player}
    end

    test "success: returns :ok, broadcasts message to chat members",
         context do
      %{socket: socket, player: player} = context

      player_name = player.name

      ref =
        push(socket, "msg", %{"message" => "hello", player_name: player.name})

      assert_reply(ref, :ok, reply)
      assert reply == %{status: :ok}

      assert_broadcast("msg", payload)
      assert %{"message" => "hello", "player_name" => ^player_name} = payload
    end
  end
end

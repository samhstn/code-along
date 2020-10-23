#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule Shipping.TwilioTest do
  use ExUnit.Case

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  test "send_message/2 hits POST /send_message", %{bypass: bypass} do
    phone_number = "+1 111 111 1111"
    body = "Hello!"
    test_server_url = "http://localhost:#{bypass.port}"

    Bypass.expect_once(bypass, "POST", "/send_message", fn conn ->
      {:ok, req_body, conn} = Plug.Conn.read_body(conn)
      params = Shipping.JSON.decode(req_body)

      assert params["phone_number"] == phone_number
      assert params["body"] == body

      Plug.Conn.resp(conn, 204, "")
    end)

    assert :ok =
             Shipping.Twilio.send_message(
               phone_number,
               body,
               test_server_url
             )
  end
end

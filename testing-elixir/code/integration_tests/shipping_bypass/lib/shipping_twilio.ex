#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule Shipping.Twilio do
  @default_base_url "https://example.twilio.com"

  def send_message(phone_number, body, base_url \\ @default_base_url) do
    params = %{phone_number: phone_number, body: body}
    {:ok, 204, ""} = Shipping.HTTP.post("#{base_url}/send_message", params)
    :ok
  end
end

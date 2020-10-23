#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.Twilio.Behaviour do
  @callback send_sms(phone_number :: String.t(), text :: String.t()) ::
              :ok | {:error, reason :: term()}
end

defmodule SoggyWaffle.Twilio do
  @behaviour SoggyWaffle.Twilio.Behaviour

  @impl true
  def send_sms(phone_number, text)
      when is_binary(phone_number) and is_binary(text) do
    # Make calls to the Twilio API here
  end
end

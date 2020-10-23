#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.WeatherCheckerTest do
  use ExUnit.Case, async: true

  # same setup as before,

  test "when the process \"ticks\", the Twilio interface is called" do
    interval_in_ms = 5
    phone_number = "+1 111 11 1111"
    test_pid = self()
    ref = make_ref()

    stub(SoggyWaffle.TwilioMock, :send_sms, fn to, text ->
      assert to == phone_number
      # TODO: assert on text
      send(test_pid, {:send_sms_called, ref})
      :ok
    end)

    start_options = [
      interval: interval_in_ms,
      city: "Los Angeles",
      phone_number: phone_number
    ]

    start_supervised!({SoggyWaffle.WeatherChecker, start_options})

    assert_receive {:send_sms_called, ^ref}
  end
end

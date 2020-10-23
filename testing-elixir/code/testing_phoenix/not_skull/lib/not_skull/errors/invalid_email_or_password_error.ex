#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Errors.InvalidEmailOrPasswordError do
  @moduledoc false

  defexception message: "", plug_status: 401

  def exception(opts \\ []) do
    message = Keyword.get(opts, :message, "invalid email or password")
    %__MODULE__{message: message}
  end
end

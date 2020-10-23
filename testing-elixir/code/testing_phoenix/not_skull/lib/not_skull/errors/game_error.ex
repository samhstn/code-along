#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Errors.GameError do
  @moduledoc false

  @type t :: %__MODULE__{}

  defexception message: ""

  def exception(opts \\ []) do
    message =
      Keyword.get(opts, :message, "Something went wrong with the game.")

    %__MODULE__{message: message}
  end
end

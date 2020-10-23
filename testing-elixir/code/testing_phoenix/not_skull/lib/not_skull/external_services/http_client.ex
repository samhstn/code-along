#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.HTTPClient do
  @moduledoc false

  @callback request(term(), binary(), [], term(), []) ::
              {:ok, integer, list, binary}
              | {:ok, integer, list}
              | {:error, term()}

  @spec request(term(), binary(), [], term(), []) ::
          {:ok, integer, list, binary}
          | {:ok, integer, list}
          | {:error, term()}
  def request(method, url, headers, body, options) do
    :hackney.request(method, url, headers, body, options)
  end
end

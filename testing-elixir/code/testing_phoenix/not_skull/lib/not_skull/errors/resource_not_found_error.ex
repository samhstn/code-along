#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Errors.ResourceNotFoundError do
  defexception message: "Resource not found", plug_status: 404

  def exception(opts) do
    resource = Keyword.fetch!(opts, :resource)
    id = Keyword.fetch!(opts, :id)

    %__MODULE__{message: "#{resource} not found for #{id}"}
  end
end

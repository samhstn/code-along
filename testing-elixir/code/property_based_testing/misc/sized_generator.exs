#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
StreamData.sized(fn size ->
  if size < 20 do
    StreamData.integer()
  else
    StreamData.one_of([StreamData.integer(), StreamData.float()])
  end
end)

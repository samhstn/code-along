#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule StatefulTesting do
  def command do
    one_of([
      command(:set, [random_key, term()]),
      command(:delete, [random_key])
    ])
  end

  def set({key, _old_value}, key, new_value) do
    {key, new_value}
  end

  def delete({key, _value}, key) do
    {key, nil}
  end

  def get({key, value}, key) do
    value
  end
end

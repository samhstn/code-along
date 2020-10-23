#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule RollingAverageMeasurements do
  def new(max_measurements) do
    {[], max_measurements}
  end

  def add_element({measurements, max_measurements}, new_element)
      when length(measurements) < max_measurements do
    {[new_element | measurements], max_measurements}
  end

  def add_element({measurements, max_measurements}, new_element) do
    without_oldest = Enum.drop(measurements, -1)
    {[new_element | without_oldest], max_measurements}
  end

  def average({measurements, _max_measurements}) do
    Enum.sum(measurements) / length(measurements)
  end
end

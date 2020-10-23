#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule RollingAverageMeasurementsTest do
  use ExUnit.Case

  describe "add_element/2" do
    test "adds an element when there are fewer elements than the size" do
      measurements = RollingAverageMeasurements.new(_max_measurements = 3)
      
      RollingAverageMeasurements.add_element(1)
      RollingAverageMeasurements.add_element(2)

      assert RollingAverageMeasurements.add_element(
               measurements,
               3
             ) == {[3, 2, 1], 3}
    end

    test "adding an element to a full list rolls a value" do
      max_measurements = Enum.random(1..10)
      measurements = RollingAverageMeasurements.new(max_measurements)
      
      measurements =
        Enum.reduce(1..max_measurements, measurements, fn _, acc ->
          RollingAverageMeasurements.add_element(acc, 1)
        end)
      
      expected_list = [10 | List.duplicate(1, max_measurements - 1)]
        
      assert RollingAverageMeasurements.add_element(measurements, 3) == 
               {expected_list, max_measurements}
    end
  end

  describe "average/1" do
    test "it returns the average of the list" do
      max_measurements = Enum.random(1..100)
      measurements = RollingAverageMeasurements.new(max_measurements)

      input_list = Enum.take_random(1..1_000, max_measurements)
      
      measurements =
        Enum.reduce(input_list, measurements, fn _, acc ->
          RollingAverageMeasurements.add_element(acc, 1)
        end)
      
      expected_average = Enum.sum(input_list) / length(input_list)
      assert RollingAverageMeasurements.average(input_list) ==
               expected_average
    end
  end
end


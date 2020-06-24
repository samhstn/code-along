defmodule CalculatorTest do
  use ExUnit.Case, async: false

  @iterations 10

  describe "Calculator" do
    test "value" do
      pid = Calculator.start()

      Enum.each(1..@iterations, fn _ ->
        Calculator.value(pid) == 0
      end)
      assert Calculator.value(pid) == 0
    end

    test "add" do
      StreamData.integer()
      |> Stream.chunk_every(2)
      |> Enum.take(@iterations)
      |> Enum.each(fn [int1, int2] ->
        pid = Calculator.start()

        assert Calculator.add(pid, int1) == int1
        assert Calculator.add(pid, int2) == int1 + int2
      end)
    end

    test "sub" do
      StreamData.integer
      |> Stream.chunk_every(2)
      |> Enum.take(@iterations)
      |> Enum.each(fn [int1, int2] ->
        pid = Calculator.start()

        assert Calculator.add(pid, int1) == int1
        assert Calculator.sub(pid, int2) == int1 - int2
      end)
    end

    test "mul" do
      StreamData.integer
      |> Stream.chunk_every(2)
      |> Enum.take(@iterations)
      |> Enum.each(fn [int1, int2] ->
        pid = Calculator.start()

        assert Calculator.add(pid, int1) == int1
        assert Calculator.mul(pid, int2) == int1 * int2
      end)
    end

    test "div" do
      StreamData.integer
      |> Stream.chunk_every(2)
      |> Stream.filter(fn [_int1, int2] -> int2 != 0 end)
      |> Enum.take(@iterations)
      |> Enum.each(fn [int1, int2] ->
        pid = Calculator.start()

        assert Calculator.add(pid, int1) == int1
        assert Calculator.div(pid, int2) == int1 / int2
      end)

      StreamData.integer()
      |> Enum.take(@iterations)
      |> Enum.each(fn int ->
        pid = Calculator.start()

        Calculator.add(pid, int)

        assert_raise ArithmeticError, "can't divide by 0", fn ->
          Calculator.div(pid, 0)
        end
      end)
    end
  end
end

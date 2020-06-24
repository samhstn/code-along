defmodule Todo.CsvImporterTest do
  use ExUnit.Case

  test "Todo.CsvImporter.import/1" do
    todo_imported = Todo.CsvImporter.import("./test/todos.csv")

    todo_expected =
      Todo.List.new([
        %Todo.Entry{date: ~D[2018-12-19], title: "Dentist"},
        %Todo.Entry{date: ~D[2018-12-20], title: "Shopping"},
        %Todo.Entry{date: ~D[2018-12-19], title: "Movies"}
      ])

    assert Todo.List.entries(todo_imported, ~D[2018-12-19]) ==
             Todo.List.entries(todo_expected, ~D[2018-12-19])

    assert Todo.List.entries(todo_imported, ~D[2018-12-20]) ==
             Todo.List.entries(todo_expected, ~D[2018-12-20])
  end
end

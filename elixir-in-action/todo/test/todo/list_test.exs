defmodule Todo.ListTest do
  use ExUnit.Case, async: true

  test "crud operations for entries" do
    todo_list =
      Todo.List.new()
      |> Todo.List.add_entry(%Todo.Entry{date: ~D[2018-12-19], title: "Dentist"})
      |> Todo.List.add_entry(%Todo.Entry{date: ~D[2018-12-20], title: "Shopping"})
      |> Todo.List.add_entry(%Todo.Entry{date: ~D[2018-12-19], title: "Movies"})

    assert Todo.List.entries(todo_list, ~D[2018-12-19]) ==
             [
               %Todo.Entry{date: ~D[2018-12-19], id: 1, title: "Dentist"},
               %Todo.Entry{date: ~D[2018-12-19], id: 3, title: "Movies"}
             ]

    assert Todo.List.entries(todo_list, ~D[2018-12-18]) == []

    todo_list =
      Todo.List.update_entry(
        todo_list,
        1,
        &Map.put(&1, :date, ~D[2018-12-20])
      )

    assert [_entry_one] = Todo.List.entries(todo_list, ~D[2018-12-19])
    assert [_entry_one, _entry_two] = Todo.List.entries(todo_list, ~D[2018-12-20])

    todo_list = Todo.List.delete_entry(todo_list, 1)

    assert [%Todo.Entry{id: 2}] = Todo.List.entries(todo_list, ~D[2018-12-20])
  end

  test "new/1" do
    todos = [
      %Todo.Entry{date: ~D[2018-12-19], title: "Dentist"},
      %Todo.Entry{date: ~D[2018-12-20], title: "Shopping"},
      %Todo.Entry{date: ~D[2018-12-19], title: "Movies"}
    ]

    todo_list = Todo.List.new(todos)

    assert [_one, _two] = Todo.List.entries(todo_list, ~D[2018-12-19])
    assert [_one] = Todo.List.entries(todo_list, ~D[2018-12-20])
  end
end

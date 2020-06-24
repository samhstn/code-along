defmodule Todo.CacheTest do
  use ExUnit.Case, async: false

  setup do
    File.rm_rf!("./persist")
    File.mkdir_p!("./persist")
  end

  test "cache creates server processes" do
    bobs_pid = Todo.Cache.server_process("Bob's list")

    assert bobs_pid == Todo.Cache.server_process("Bob's list")
    assert bobs_pid != Todo.Cache.server_process("Alice's list")
  end

  test "cache stores to correct server processes" do
    bobs_list = Todo.Cache.server_process("Bob's list")
    alices_list = Todo.Cache.server_process("Alice's list")

    Todo.Server.add_entry(bobs_list, %Todo.Entry{date: ~D[2018-12-19], title: "Dentist"})

    assert [_one] = Todo.Server.entries(bobs_list, ~D[2018-12-19])
    assert [] = Todo.Server.entries(alices_list, ~D[2018-12-19])

    bobs_list = Todo.Cache.server_process("Bob's list")

    assert [_one] = Todo.Server.entries(bobs_list, ~D[2018-12-19])
  end

  test "persistence" do
    john = Todo.Cache.server_process("john")
    Todo.Server.add_entry(john, %Todo.Entry{date: ~D[2018-12-20], title: "Shopping"})
    assert [_one] = Todo.Server.entries(john, ~D[2018-12-20])

    Process.exit(john, :kill)

    entries =
      "john"
      |> Todo.Cache.server_process()
      |> Todo.Server.entries(~D[2018-12-20])

    assert [%Todo.Entry{date: ~D[2018-12-20], title: "Shopping"}] = entries
  end
end

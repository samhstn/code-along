defmodule Todo.WebTest do
  use ExUnit.Case, async: false
  use Plug.Test

  @opts Todo.Web.init([])

  setup do
    File.rm_rf!("./persist")
    File.mkdir_p!("./persist")
  end

  test "adds an entry" do
    opts = %{list: "bob", date: "2018-12-19", title: "Dentist"}
    conn = conn(:post, "/add_entry", opts)

    conn = Todo.Web.call(conn, @opts)

    assert conn.state == :sent
    assert conn.status == 200
    assert conn.resp_body == "OK"

    bobs_entry =
      "bob"
      |> Todo.Cache.server_process()
      |> Todo.Server.entries(~D[2018-12-19])

    assert [%Todo.Entry{title: "Dentist"}] = bobs_entry
  end

  test "gets all entries" do
    bobs_list = Todo.Cache.server_process("Bob's list")
    Todo.Server.add_entry(bobs_list, %Todo.Entry{date: ~D[2018-12-19], title: "Dentist"})

    opts = %{list: "bob", date: "2018-12-19"}
    conn = conn(:get, "/entries", opts)

    conn = Todo.Web.call(conn, @opts)

    assert conn.state == :sent
    assert conn.status == 200
    assert conn.resp_body in ["", "2018-12-19 Dentist"]
  end
end

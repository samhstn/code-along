defmodule Todo.Entry do
  @enforce_keys [:date, :title]
  defstruct [:date, :title, :id]
end

defmodule Todo.List do
  defstruct auto_id: 1, entries: %{}

  def new(entries \\ []) do
    Enum.reduce(
      entries,
      %Todo.List{},
      fn entry, todo_list_acc ->
        add_entry(todo_list_acc, entry)
      end
    )
  end

  def add_entry(todo_list, entry = %Todo.Entry{}) do
    entry = Map.put(entry, :id, todo_list.auto_id)

    new_entries =
      Map.put(
        todo_list.entries,
        todo_list.auto_id,
        entry
      )

    %Todo.List{todo_list | entries: new_entries, auto_id: todo_list.auto_id + 1}
  end

  def entries(todo_list, date) do
    todo_list.entries
    |> Stream.filter(fn {_, entry} -> entry.date == date end)
    |> Enum.map(fn {_, entry} -> entry end)
  end

  def update_entry(todo_list, id, f) do
    case Map.fetch(todo_list.entries, id) do
      :error ->
        todo_list

      {:ok, old_entry = %{id: old_entry_id}} ->
        new_entry = %Todo.Entry{id: ^old_entry_id} = f.(old_entry)
        new_entries = Map.put(todo_list.entries, id, new_entry)
        %Todo.List{todo_list | entries: new_entries}
    end
  end

  def delete_entry(todo_list, id) do
    new_entries = Map.delete(todo_list.entries, id)
    %Todo.List{todo_list | entries: new_entries}
  end
end

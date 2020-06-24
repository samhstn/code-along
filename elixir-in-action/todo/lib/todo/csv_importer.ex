defmodule Todo.CsvImporter do
  def import(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.split(&1, ","))
    |> Stream.map(&parse_date/1)
    |> Stream.map(&tuple_to_entry/1)
    |> Enum.to_list()
    |> Todo.List.new()
  end

  defp parse_date([date_string, title]) do
    date =
      date_string
      |> String.replace("/", "-")
      |> Date.from_iso8601!()

    {date, title}
  end

  defp tuple_to_entry({date, title}) do
    %Todo.Entry{date: date, title: title}
  end
end

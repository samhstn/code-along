#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule TestingEcto.Schemas.UserValidator do  
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields [:favorite_number]

  @primary_key false
  embedded_schema do
    field(:date_of_birth, :date)
    field(:email, :string)
    field(:favorite_number, :float)
    field(:first_name, :string)
    field(:last_name, :string)
    field(:phone_number, :string)
  end

  defp all_fields do
    __MODULE__.__schema__(:fields)
  end

  def cast_and_validate(params) do  
    %__MODULE__{}
    |> cast(params, all_fields())
    |> validate_required(all_fields() -- @optional_fields)
    |> apply_changes_if_valid()  
  end

  defp apply_changes_if_valid(%Ecto.Changeset{valid?: true} = changeset) do  
    {:ok, Ecto.Changeset.apply_changes(changeset)} 
  end

  defp apply_changes_if_valid(%Ecto.Changeset{} = changeset) do
    {:error, changeset}
  end   
end

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule TestingEcto.Schemas.UserDatabaseSchema do  
  use Ecto.Schema
  import Ecto.Changeset

  @timestamps_opts type: :utc_datetime_usec  
  @primary_key {:id, :binary_id, autogenerate: true} 
  @optional_fields [:id, :favorite_number]  

  schema "users" do  
    field(:date_of_birth, :date)
    field(:email, :string)
    field(:favorite_number, :float)
    field(:first_name, :string)
    field(:last_name, :string)
    field(:phone_number, :string)

    timestamps()  
  end

  defp all_fields do
    __MODULE__.__schema__(:fields)
  end

  def changeset(params) do
    %__MODULE__{}
    |> cast(params, all_fields())
    |> validate_required(all_fields() -- @optional_fields)
    |> unique_constraint(:email)  
  end
end

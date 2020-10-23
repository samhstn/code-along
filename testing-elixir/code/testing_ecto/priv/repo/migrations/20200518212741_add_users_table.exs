#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule TestingEcto.Repo.Migrations.AddUsersTable do
  use Ecto.Migration

  def change do
    create table(:users, primary_key: false) do  
      add(:id, :uuid, primary_key: true)   
      add(:date_of_birth, :date, null: false)
      add(:email, :string, null: false)
      add(:favorite_number, :float)
      add(:first_name, :string, null: false)
      add(:last_name, :string, null: false)
      add(:phone_number, :string, null: false)

      timestamps() 
    end

    create(unique_index(:users, [:email]))  
  end
end

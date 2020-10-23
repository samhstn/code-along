#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule Support.JWTHelpers do
  def sign_jwt(user_id) do
    # TODO: add ttl/exp mechanism
    claims = %{"cid" => user_id}
    headers = %{"alg" => "HS256"}

    jwk()
    |> JOSE.JWT.sign(headers, claims)
    |> JOSE.JWS.compact()
    |> elem(1)
  end

  def claims_from_jwt(jwt) do
    {true, %{fields: fields}, _} =
      JOSE.JWT.verify_strict(jwk(), ["HS256"], jwt)

    {:ok, fields}
  end

  defp jwk do
    passphrase = Application.fetch_env!(:not_skull, :secret_passphrase)

    passphrase
    |> Base.decode64!()
    |> JOSE.JWK.from_oct()
  end
end

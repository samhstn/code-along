#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.WeatherAPI do
  @default_base_url "https://api.openweathermap.org"

  @spec get_forecast(String.t(), String.t()) ::
          {:ok, map()} | {:error, reason :: term()}
  def get_forecast(city, base_url \\ @default_base_url)
      when is_binary(city) do
    app_id = SoggyWaffle.api_key()
    query_params = URI.encode_query(%{"q" => city, "APPID" => app_id})
    url = base_url <> "/data/2.5/forecast?" <> query_params

    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200} = response} ->
        {:ok, Jason.decode!(response.body)}

      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, {:status, status_code}}

      {:error, reason} ->
        {:error, reason}
    end
  end
end

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule APISmokeTest do
  use ExUnit.Case
  use ExUnitProperties

  property "API only returns a response with status of 200, 400, or 404" do
    host = "myapi.example.com"

    check all method <- http_method_generator(),
              path <- path_generator(),
              headers <- headers_generator(),
              body <- binary() do
      response = send_http_request(host, method, path, headers, body)
      assert response.status in [200, 400, 404]
    end
  end

  defp http_method_generator do
    frequency([
      {4, member_of(["HEAD", "GET", "POST", "PUT", "DELETE"])},
      {1, string([?a..?z, ?A..?Z], min_length: 1)}
    ])
  end

  defp path_generator do
    # ...
  end

  defp headers_generator do
    # ...
  end
end

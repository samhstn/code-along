#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule HTTPAPITest do
  use ExUnit.Case

  test "posts are created correctly" do
    params = %{
      "title" => "My post",
      "body" => "The body of the post",
      "author_email" => "me@example.com"
    }

    response = simulate_http_call("POST", "/create_post", params)

    assert response.status == 200
    assert %{"post_id" => post_id} = response.body

    assert {:ok, post} = Database.fetch_by_id(Post, post_id)
    assert post.title == "My post"
    assert post.body == "The body of the post"
    assert post.author_email == "me@example.com"
  end
end

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule MyApp.Router do
  route("POST", "/create_post", {MyApp.PostController, :create_post})
end

defmodule MyApp.Controller do
  def create_post(connection, params) do
    post = %Post{
      title: params["title"],
      body: params["body"],
      author_email: params["author_ermail"]
    }

    post_id = Database.create(post)
    send_response(connection, _status_code = 200, %{post_id: post_id})
  end
end

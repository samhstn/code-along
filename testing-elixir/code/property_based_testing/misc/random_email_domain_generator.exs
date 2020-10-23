#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
common_domains = ["gmail.com", "yahoo.com", "icloud.com"]

random_email_domain_generator =
  StreamData.sized(fn
    size when size <= 5 ->
      StreamData.member_of(common_domains)

    _size ->
      StreamData.string([?a..?z], min_length: 1)
      |> StreamData.scale(fn size -> trunc(:math.log(size)) end)
      |> StreamData.map(fn string -> string <> ".com" end) 
  end)

#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
domains = ["gmail.com", "yahoo.com", "icloud.com"]
random_domain_generator = StreamData.member_of(domains)
username_generator = StreamData.string(:alphanumeric, min_length: 1) 

random_email_generator =
  StreamData.bind(random_domain_generator, fn domain ->
    StreamData.map(username_generator, fn username ->
      "#{username}@#{domain}"
    end) 
  end)
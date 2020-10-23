#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
domains = ["gmail.com", "yahoo.com", "icloud.com", "hotmail.com"]

email_generator =
  gen all username <- string(:alphanumeric),
          domain <- member_of(domains) do
    username <> "@" <> domain
  end
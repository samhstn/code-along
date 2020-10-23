# Telemetry ...and metrics for all

Taken from: https://www.youtube.com/watch?v=cOuyOmcDV1U

```elixir
def handle_request(host, method, body) do
  update_counter(
    "my_app.http.requests",
    host: host,
    method: method
  )
  ...

  case MyApp.business_logic(params) do
    {:ok, data} ->
      update_counter(
        "my_app.http.responses",
        host: host,
        method: method
        status: 200
      )
    :error ->
      update_counter(
        "my_app.http.responses",
        host: host,
        method: method
        status: 400
      )
  end
end
```

```elixir
def business_logic(params) do
  update_counter("my_app.business_logic", ...)
  with :ok <- authorize(params),
       {:ok, data} <- query_database(params),
       :ok <- push_event(data) do
    {:ok, data}
  end
end

def authorize(params) do
  update_counter("my_app.auth.requests", ...)
  ...
end

def query_database(params) do
  update_counter("my_app.db.queries", ...)
  ...
end

def push_event(params) do
  update_counter("my_app.broker.events_pushed", ...)
  ...
end
```

We also should record memory use:

```elixir
record_value("my_app.memory.total")
```

"The monitoring and reporting systems should be like an exoskeleton built around your system, not woven into it."
- Michael Nygard, Release It!

```elixir
def handle_http_start(
  event,
  measurements,
  metadata,
  config
) do
  udpate_counter("my_app.http.requests", ...)
  update_sum(
    "my_app.http.payload_size",
    measurements.payload_size,
    ...
  )
end
```

We want this function to update the above metrics when every request is started.
The only thing our request handling code needs to do is to invoke this function so that metrics are updated.

We can make use of this above function with telemetry by attaching telemetry:

```elixir
:telemetry.attach(
  "my-handler", # unique id
  [:my-app, :http, :request, :start], # event name
  &handle_http_start/4, # function to handle event when emitted
  nil
)
```

In telemetry the events are not messages, we're not sending messages to some process.
We're retrieving all event handlers attached to an event and we're invoking the event handlers, one by one, synchronously
in the process that is emitting the event.

```elixir
:telemetry.execute(
  [:my_app, :http, :request, :start], # event name
  %{payload_size: payload_size}, # map of measurements
  %{host: host, method: method} # metadata
)
```

This can be run in our previous code as the following:

```elixir
def handle_request(host, method, body) do
  params = decode(body)
  :telemetry.execute([:my_app, :http, :request, :start])

  case MyApp.business_logic(params) do
    {:ok, data} ->
      body = encode(data)
      :telemetry.execute([:my_app, :http, :request, :stop])
      send_resp(200, body)

    :error ->
      :telemetry.execute([:my_app, :http, :request, :stop])
      body = %{error: "Oops"}
      send_resp(400, body)
  end
end
```

Telemetry comes with the following out of the box:

```elixir
last_value("vm.memory.total")
sum("http.request.payload_size")
counter("db.query.count") # counting the number of times this has happened
distribution("http.request.duration") # general metrics and statistics
```

All of these get sent to "reporters" which sends them at runtime to where we are aggregating.

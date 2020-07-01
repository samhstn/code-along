# Todo

## Installation

```bash
mix do deps.get, compile
```

## Start the development server

```bash
iex -S mix
```

## Run the tests

```bash
mix test
```

## Production release and start the server

Build our release

```bash
mix release
```

Start the system in foreground

```bash
_build/prod/rel/todo/bin/todo start
```

Start the system in background

```bash
_build/prod/rel/todo/bin/todo daemon
```

Stop the background system

```bash
_build/prod/rel/todo/bin/todo stop
```

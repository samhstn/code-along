up:
	docker-compose up -d

down:
	docker-compose down

cli:
	docker-compose exec postgres psql -U testing_ecto -d testing_ecto_test

create.test:
	MIX_test=test mix ecto.create

migrate.test:
	MIX_test=test mix ecto.migrate

reset.test:
	MIX_test=test mix ecto.drop
	MIX_test=test mix ecto.create
	MIX_test=test mix ecto.migrate

create.dev:
	MIX_dev=dev mix ecto.create

migrate.dev:
	MIX_dev=dev mix ecto.migrate

reset.dev:
	MIX_dev=dev mix ecto.drop
	MIX_dev=dev mix ecto.create
	MIX_dev=dev mix ecto.migrate
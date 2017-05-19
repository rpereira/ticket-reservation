# Railway Ticket Reservation System

<p align="center">
  <img src="./railway.png" alt="Railway"/>
</p>

## Setup database

```
❯ psql postgres
psql (9.6.2, server 9.5.3)
Type "help" for help.

postgres=# CREATE ROLE railway WITH LOGIN PASSWORD 'railway';
postgres=# CREATE DATABASE railway;
```

## Seed database

```
❯ psql railway < scripts/seeds.sql
```

## API

### Station

```
# GET /stations
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations

# GET /stations/:name
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations/Richmond
```

### Train

```
# GET /trains
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/trains

# GET /trains/:names
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/trains/Alpha%20Train
```

### User

```
# POST /users
curl --verbose --request POST --header "Content-Type: application/json" \
  --data '{"name": "Foo Bar", "email": "foo@example.com"}' \
  http://localhost:8080/users

# GET /users
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/users

# GET /users/:name
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/users/Foo%20Bar
```

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

### List Schedules

`GET /schedules`

**Query parameters:**

- `?page=2`: Get schedules by page (default is 1)

Example:
```
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/schedules?page=3
```

### Station

```
# GET /stations
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations

# GET /stations/:crs_code/timetable
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations/EPH/timetable

# GET /stations/:crs_code/timetable/:time
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations/EPH/timetable/2017-05-15T05:01:00Z
```

### Train

```
# GET /trains
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/trains

# GET /trains/:name
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/trains/G00515
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

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

## API

### Station

```
# GET users
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations

# GET users/:name
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/stations/Richmond
```

### Train

```
# GET users
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/trains

# GET users/:name
curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8080/trains/Alpha%20Train
```

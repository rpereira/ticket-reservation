# TicketReservation

## Setup database

```
❯ psql postgres
psql (9.6.2, server 9.5.3)
Type "help" for help.

postgres=# CREATE ROLE railway WITH LOGIN PASSWORD 'railway';
postgres=# CREATE DATABASE railway;
```

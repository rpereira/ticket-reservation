TRUNCATE train, station, schedule;

COPY train (id, name)
  FROM '/Users/rap/code/thesis/TicketReservation/metadata/trains.csv'
  DELIMITER ',' CSV HEADER;

COPY station (id, name, crs_code, tiplic_code)
  FROM '/Users/rap/code/thesis/TicketReservation/metadata/stations.csv'
  DELIMITER ',' CSV HEADER;

COPY schedule (service, train_id, departure_time, arrival_time, pass_time,
  src_station_id, dst_station_id, station_id)
  FROM '/Users/rap/code/thesis/TicketReservation/metadata/schedules.csv'
  DELIMITER ',' CSV HEADER;

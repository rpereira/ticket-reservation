TRUNCATE train, station;

INSERT INTO train VALUES (1, 'Alpha Train');
INSERT INTO train VALUES (2, 'Beta Train');
INSERT INTO train VALUES (3, 'Zeta Train');

COPY station (name, code)
  FROM '/Users/rap/code/thesis/TicketReservation/metadata/station_codes.csv'
  DELIMITER ',' CSV;

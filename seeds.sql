TRUNCATE train, station;

INSERT INTO train VALUES (1, 'Alpha Train');
INSERT INTO train VALUES (2, 'Beta Train');
INSERT INTO train VALUES (3, 'Zeta Train');

COPY station (name) FROM '/Users/rap/code/thesis/TicketReservation/metadata/stations.txt';

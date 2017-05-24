require 'pg'

conn = PG.connect(dbname: 'railway')

conn.exec('SELECT * FROM train') do |result|
  result.each do |row|
    train_id = row['id']
    (1..150).each do |i|
      conn.exec_params("INSERT INTO train_seats (train_id, number) VALUES
      ($1::int, $2::int)", [train_id, i])
    end
  end
end

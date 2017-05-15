require 'csv'
require 'date'
require 'json'
require 'net/http'

BASE_URL       = 'https://transportapi.com/v3/uk/train'
STATIONS_URL   = "#{BASE_URL}/stations/bbox.json?app_id=221cce2f&app_key=d209929236fc97196775650c2bdb639e&maxlat=51.2868&maxlon=0.3340&minlat=51.6923&minlon=-0.5103"
TIME_TABLE_URL = "#{BASE_URL}/station/$0/2017-05-15/06:00/timetable.json?app_id=221cce2f&app_key=d209929236fc97196775650c2bdb639e&to_offset=PT10:00:00&train_status=passenger&type=arrival,departure,pass"

STATIONS_FILE_PATH  = 'metadata/stations.csv'
SCHEDULES_FILE_PATH = 'metadata/schedules.csv'
TRAINS_FILE_PATH    = 'metadata/trains.csv'

class Hash
  # Removes the given keys from hash and returns it.
  #   hash = { a: true, b: false, c: nil }
  #   hash.except!(:c) # => { a: true, b: false }
  #   hash             # => { a: true, b: false }
  def except!(*keys)
    keys.each { |key| delete(key) }
    self
  end
end

def json_to_csv(data)
  column_names = data.first.keys
  csv_string = CSV.generate do |csv|
    csv << column_names
    data.each do |hash|
      csv << hash.values
    end
  end
  csv_string
end

# Get all values of the specified column from the specified CSV file.
def csv_to_json(fpath, column)
  rows = []
  CSV.foreach(fpath, headers: true) do |row|
    rows.push row[column]
  end
  rows
end

def csv_to_array(fpath)
  csv = CSV::parse(File.open(fpath, 'r') { |f| f.read })
  fields = csv.shift
  csv.collect { |record| Hash[*fields.zip(record).flatten ] }
end

def write_to_file(data, fpath)
  File.open(fpath, 'w') do |f|
    f.write(data)
  end
end

def get_as_json(url)
  response = Net::HTTP.get(URI(url))
  JSON.parse(response)
end

def fetch_stations
  puts "\n==> Fetching list of stations"
  response = get_as_json(STATIONS_URL)
  rpp = response['rpp']
  total = response['total']
  total_pages = total / rpp

  stations = parse_stations(response, 0)
  puts "page = #{response['page']}; total = #{stations.length}"
  (2..total_pages).each do |page|
    response = get_as_json("#{STATIONS_URL}&page=#{page}")
    stations.concat parse_stations(response, stations.size)
    puts "page = #{page}; total = #{stations.length}"
  end

  stations
end

def parse_stations(data, index_start)
  stations = []
  data['stations'].each_with_index do |station, index|
    stations.push({
      id: index_start + index,
      name: station['name'],
      crs_code: station['station_code'],
      tiploc_code: station['tiploc_code']
    })
  end
  stations
end

def fetch_schedules
  puts "\n==> Fetching time tables"
  schedules = []
  crs_codes = csv_to_json(STATIONS_FILE_PATH, 'crs_code')
  crs_codes.each do |code|
    schedules.concat fetch_schedule_for_station(code)
  end
  schedules
end

def fetch_schedule_for_station(crs_code)
  print "#{crs_code} "
  schedules = []
  url = TIME_TABLE_URL.clone.sub! '$0', crs_code
  response = get_as_json(url)
  schedules.concat parse_schedules(response)
end

def parse_schedules(data)
  station_id = get_station_id(data['station_code'])
  schedules = []
  data['updates']['all'].each do |x|
    next unless x['mode'] == 'train'
    x.except!('mode', 'platform', 'operator', 'operator_name', 'source', 'category')
    x.merge!("station_id" => station_id,
             "aimed_departure_time" => time_to_utc(x['aimed_departure_time']),
             "aimed_arrival_time" => time_to_utc(x['aimed_arrival_time']),
             "aimed_pass_time" => time_to_utc(x['aimed_pass_time']))
    schedules.push map_schedule(x)
  end
  schedules
end

def get_station_id(crs)
  stations = csv_to_array(STATIONS_FILE_PATH)
  station = stations.find { |s| s['crs_code'] == crs }
  station['id']
end

# TODO: better date support
def time_to_utc(str)
  return DateTime.parse("2017-05-15 #{str}") unless str.nil?
end

def map_schedule(hash)
  mappings = {
    "train_uid" => "train_id", "aimed_departure_time" => "departure_time",
    "aimed_arrival_time" => "arrival_time", "aimed_pass_time" => "pass_time",
    "origin_name" => "src_station", "destination_name" => "dst_station"
  }
  hash.map {|k, v| [mappings[k] || k, v] }.to_h
end

def extract_trains
  puts "\n==> Extracting trains"

  trains = []
  train_names = csv_to_json(SCHEDULES_FILE_PATH, 'train_id').uniq
  train_names.each_with_index do |value, index|
    trains.push({ :id => index, :name => value })
  end
  trains
end

def update_train_id
  trains = csv_to_array(TRAINS_FILE_PATH)
  schedules = csv_to_array(SCHEDULES_FILE_PATH)
  updated = []
  schedules.each do |entry|
    train = trains.find { |t| t['name'] == entry['train_id'] }
    updated.push entry.merge!("train_id" => train['id'])
  end
  updated
end

def replace_station_name_with_id
  stations = csv_to_array(STATIONS_FILE_PATH)
  schedules = csv_to_array(SCHEDULES_FILE_PATH)
  updated = []
  schedules.each do |entry|
    src = stations.find { |s| s['name'] == entry['src_station'] }
    dst = stations.find { |s| s['name'] == entry['dst_station'] }
    next if src.nil? || dst.nil?  # some destinations are not on the stations dataset
    updated.push entry.merge!("src_station" => src['id'], "dst_station" => dst['id'])
  end
  updated
end

def main
  stations = fetch_stations
  write_to_file(json_to_csv(stations), STATIONS_FILE_PATH)

  schedules = fetch_schedules
  write_to_file(json_to_csv(schedules), SCHEDULES_FILE_PATH)

  trains = extract_trains
  write_to_file(json_to_csv(trains), TRAINS_FILE_PATH)

  updated_schedules = update_train_id
  write_to_file(json_to_csv(updated_schedules), SCHEDULES_FILE_PATH)

  updated_schedules = replace_station_name_with_id
  write_to_file(json_to_csv(updated_schedules), SCHEDULES_FILE_PATH)
end

main

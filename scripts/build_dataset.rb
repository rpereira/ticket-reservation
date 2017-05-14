require 'net/http'
require 'csv'
require 'json'

BASE_URL       = 'https://transportapi.com/v3/uk/train'
STATIONS_URL   = "#{BASE_URL}/stations/bbox.json?app_id=221cce2f&app_key=d209929236fc97196775650c2bdb639e&maxlat=51.2868&maxlon=0.3340&minlat=51.6923&minlon=-0.5103"
TIME_TABLE_URL = "#{BASE_URL}/station/$0/2017-05-15/06:00/timetable.json?app_id=221cce2f&app_key=d209929236fc97196775650c2bdb639e&to_offset=PT10:00:00&train_status=passenger&type=arrival,departure,pass"

STATIONS_FILE_PATH  = 'metadata/stations.csv'
SCHEDULES_FILE_PATH = 'metadata/schedules.csv'

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

def csv_to_json(fpath)
  rows = []
  CSV.foreach(fpath, headers: true) do |row|
    rows.push row['crs_code']
  end
  rows
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

  stations = parse_stations(response)
  puts "page = #{response['page']}; total = #{stations.length}"
  (2..total_pages).each do |page|
    response = get_as_json("#{STATIONS_URL}&page=#{page}")
    stations.concat parse_stations(response)
    puts "page = #{page}; total = #{stations.length}"
  end

  stations
end

def parse_stations(data)
  stations = []
  data['stations'].each do |station|
    stations.push({
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
  crs_codes = csv_to_json(STATIONS_FILE_PATH)  # better API would be pass crs_code key as arg
  crs_codes.each do |code|
    schedules.concat fetch_schedule_for_station(code)
  end
  schedules
end

def fetch_schedule_for_station(crs_code)
  print "#{crs_code} "
  departures = []
  url = TIME_TABLE_URL.clone.sub! '$0', crs_code
  response = get_as_json(url)
  departures.concat parse_departures(response)
end

def parse_departures(data)
  departures = []
  data['updates']['all'].each do |x|
    next unless x['mode'] == 'train'
    departures.push x
  end
end

def main
  stations = fetch_stations
  write_to_file(json_to_csv(stations), STATIONS_FILE_PATH)

  schedules = fetch_schedules
  write_to_file(json_to_csv(schedules), SCHEDULES_FILE_PATH)
end

main

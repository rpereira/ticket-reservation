require 'net/http'
require 'csv'
require 'json'

STATIONS_URL = 'https://transportapi.com/v3/uk/train/stations/bbox.json?app_id=221cce2f&app_key=d209929236fc97196775650c2bdb639e&maxlat=51.2868&maxlon=0.3340&minlat=51.6923&minlon=-0.5103'

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

def write_to_file(data, path)
  File.open(path, 'w') do |f|
    f.write(data)
  end
end

def get_as_json(url)
  response = Net::HTTP.get(URI(url))
  JSON.parse(response)
end

def fetch_stations
  response = get_as_json(STATIONS_URL)
  rpp = response['rpp']
  total = response['total']
  total_pages = total / rpp

  stations = parse_stations(response)
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

def main
  stations = fetch_stations
  write_to_file(json_to_csv(stations), 'metadata/stations.csv')
end

main

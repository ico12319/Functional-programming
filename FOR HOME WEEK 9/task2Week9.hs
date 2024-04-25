main :: IO()
main = do
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 (-16)), (City "Plovdiv" 120 34), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 (-15)), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Plovdiv" 2 (-1)), (City "Sofia" 65 12), (City "Gabrovo" 32 21)]), (Country "Germany" "Berlin" [(City "Dortmund" 287 (-54)), (City "Bremen" 87 8), (City "Frankfurt" 86 61)]), (Country "France" "Paris" [(City "Nice" 180 54), (City "Lyon" 7 12), (City "Paris" 213 3)])] == "Germany" --myTest





type Name = String
type Capital = Name 
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]


coldestCapital :: [Country] -> Name
coldestCapital countries = fst $ foldl findColdest (head zipped) (tail zipped)
 where
    getAvgTemp :: [City] -> Double
    getAvgTemp cities = sum (map (\(City _ _ temp) -> temp) cities) / fromIntegral (length cities)

    avgTemps = map (\(Country _ _ cities) -> getAvgTemp cities) countries

    zipped = zip (map (\(Country name _ _) -> name) countries) avgTemps

    findColdest :: (Name, AvgYearlyTemperature) -> (Name, AvgYearlyTemperature) -> (Name, AvgYearlyTemperature)
    findColdest (coldestCountry, coldestTemp) (country, temp)
     | temp < coldestTemp = (country, temp)
     | otherwise = (coldestCountry, coldestTemp)

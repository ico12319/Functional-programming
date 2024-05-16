main :: IO()
main = do

    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"
    print $ highestCapital [(Country "Chile" "Santiago" [(City "Valparaiso" 10 15), (City "Concepcion" 20 14), (City "Santiago" 520 13)]),(Country "Peru" "Lima" [(City "Arequipa" 200 15), (City "Lima" 150 12), (City "Cusco" 3400 15)]),(Country "Nepal" "Kathmandu" [(City "Kathmandu" 1400 15), (City "Pokhara" 827 14), (City "Lalitpur" 1300 15)])] == "Nepal" --myTest




type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature 
 deriving (Show)

data Country = Country Name Capital [City] 
 deriving (Show)


capitalElevation :: Country -> Elevation
capitalElevation (Country _ capital cities) = head [elevation | City name elevation _ <- cities, name == capital]

higherCountry :: Country -> Country -> Country
higherCountry country1 country2
 | capitalElevation country1 > capitalElevation country2 = country1
 | otherwise = country2

highestCapital :: [Country] -> Name -- using fold only here?
highestCapital (x:xs) = countryName $ foldl higherCountry x xs
 where
    countryName (Country name _ _) = name

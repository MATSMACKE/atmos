import Control.Monad
import Text.Read

main :: IO ()
main = do
    putStrLn "What value is given? (alt, pres, dens)"
    input <- getLine
    unless (input == "quit" || input == "q" || input == "exit") $ do
        get_input input

get_input :: String -> IO()
get_input "alt" = do
    putStrLn "What is the altitude?"
    input <- getLine
    putStrLn $ calculate_from_alt (readMaybe input :: Maybe Float) ++ "\n"
    do
        main
get_input x = do
    putStrLn $ "I don't know what to do with a '" ++ x ++ "'.\n"
    do 
        main

sea_level_state :: (Float, Float) -- (pressure, density, temperature)
sea_level_state = (101325, 288.15)

layers :: [(Float, Float)] -- [(Max altitude of layer, temperature gradient)]
layers = [(11000, -0.0065), (20000, 0), (32000, 0.001), (47000, 0.0028), (51000, 0), (71000, -0.0028), (80000, -0.002)]

g :: Float
g = 9.80665

calculate_from_alt :: Maybe Float -> String
calculate_from_alt (Just alt) = 
    let 
        (pres, temp) = calculate_state_from_alt alt 
        dens = pres / (287 * temp)
        speed_of_sound = sqrt (1.4 * 287 * temp)
    in
        (if alt > 80000 then
            "\n=== IMPORTANT: The International Standard Atmosphere only extends to 80 km, calculations above this may not be accurate ===\n"
        else "\n")
        ++
        "Pressure: " ++ show pres ++
        "\nDensity: " ++ show dens ++
        "\nTemperature: " ++ show temp ++
        "\nSpeed of sound: " ++ show speed_of_sound
calculate_from_alt (Nothing)  = "I don't recognize this as a number"

calculate_state_from_alt :: Float -> (Float, Float) -- (pressure, density, temperature)
calculate_state_from_alt alt = aux 0 alt sea_level_state layers
    where 
        aux prev_alt alt current_state ((max_alt, gradient):layers_above) = 
            if alt < max_alt then
                new_state current_state gradient (alt - prev_alt)
            else 
                if null layers_above then
                    new_state current_state 0 (alt - prev_alt)
                else
                    aux max_alt alt (new_state current_state gradient (max_alt - prev_alt)) layers_above

new_state :: (Float, Float) -> Float -> Float -> (Float, Float) -- (pressure, density, temperature)
new_state (pressure, temperature) gradient alt_increase = 
    if gradient == 0 then
        (
            pressure * (exp (-g * alt_increase/(287 * temperature))), 
            temperature
        )
    else
        let 
            old_temp = temperature
            new_temp = old_temp + gradient * alt_increase
        in
            (
                pressure * ((new_temp / old_temp) ** (-g / (287 * gradient))), 
                new_temp
            )

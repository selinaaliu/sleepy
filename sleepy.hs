import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String
type Status = String

type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("bedroom", "n"), "den"),
    (("den", "s"), "bedroom"),
    (("bedroom", "d"), "bed"),
    (("bed", "u"), "bedroom")
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations = [
    ("flyswatter", "den"), 
    ("fly", "bedroom"),
    ("light switch", "den"),
    ("light switch", "bedroom"),
    ("myself", "bedroom")
    ]
-- first argument can be a Location or Thing
type Statuses = [(String, Status)]
statuses :: Statuses
statuses = [
    ("fly", "alive"),
    ("light switch", "visible"),
    ("den", "lit"),
    ("bedroom", "lit")
    ]

type World = (PathMap, LocationMap, Statuses, Response)

world :: IO (PathMap, LocationMap, Statuses, Response)
world = return (paths, locations, statuses, "")

main :: IO (String)
main = do
    putStrLn "\nWelcome to the Sleepy game!\n"
    putStrLn instructions
    play_game $ return (do_command "look" paths locations statuses)
    play_game $ return (paths, locations, statuses, "")
    return "Goodbye!"

play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, statuses, response) <- world
    putStrLn response
    putStrLn ""
    if game_over statuses 
        then return ([], [], [], "")
        else do
            putStr "command> "
            command <- getLine
            if command == "quit"
            then return (paths, locations, statuses,  "Quitting.")
            else play_game $ 
                 return (do_command command paths locations statuses)

game_over :: Statuses -> Bool
game_over statuses =
    (get "myself" statuses) == "asleep" && (elem ("fly", "dead") statuses) 


instructions :: String
instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main                    -- to start the game.\n" ++
    "n  s  e  w  u  d        -- to go in that direction.\n" ++
    "take object             -- to pick up an object.\n" ++
    "drop object             -- to put down an object.\n" ++
    "use object              -- to manipulate an object.\n" ++
    "look                    -- to look around you again.\n" ++
    "on.  off.               -- to control the room lights.\n" ++
    "sleep                   -- to try to go to sleep.\n" ++
    "instructions            -- to see this message again.\n" ++
    "stop                    -- to end the game and quit.\n" ++
    "\n"

use :: Thing -> PathMap -> LocationMap -> Statuses -> World
use thing paths locations statuses =
    let here = get "myself" locations
    in case thing of 
       "flyswatter" -> swat paths locations statuses
       "bed" -> if here == "bedroom" 
                then go "d" paths locations statuses 
                else (paths, locations, statuses, "It's in the bedroom!\n")
       "light switch" -> if (get here statuses) == "lit"
                         then off_switch paths locations statuses
                         else on_switch paths locations statuses
       thing -> (paths, locations, statuses, "You can't use that.")

on_switch :: PathMap -> LocationMap -> Statuses -> World
on_switch paths locations statuses =
    let here = get "myself" locations
    in if (here == "bed")
       then (paths, locations, statuses, 
            "You can't reach the light switch from here.\n")
       else if (get here statuses) == "lit" 
            then (paths, locations, statuses, "The lights are already on.\n")
            else let new_statuses = put here "lit" statuses 
                     response = "The room lights come on.\n" ++ 
                                look_string here paths locations statuses
                 in (paths, locations, new_statuses, response)

off_switch :: PathMap -> LocationMap -> Statuses -> World
off_switch paths locations statuses =
    let here = get "myself" locations
    in if (here == "bed")
       then (paths, locations, statuses, 
            "You can't reach the light switch from here.\n")
       else if (get here statuses) == "dark" 
            then (paths, locations, statuses, "The lights are already off.\n")
            else let new_statuses = put here "dark" statuses 
                 in (paths, locations, new_statuses, "It is now dark in here.\n")

sleep :: PathMap -> LocationMap -> Statuses -> World
sleep paths locations statuses  
    | (get "myself" locations) /= "bed" = 
        (paths, locations, statuses, 
        "You find it hard to sleep standing up.\n")
    | (get "bedroom" statuses) == "lit" = 
        (paths, locations, statuses, 
        "You can't get to sleep with the light on.\n")
    | (get "den" statuses) == "lit" =
        (paths, locations, statuses, 
        "The light from the den is keeping you awake.\n")
    | (elem ("flyswatter", "holding") locations) || 
      (elem ("flyswatter", "bed") locations) =
        (paths, locations, statuses, 
        "What? Sleep with a dirty old flyswatter?\n")
    | (get "fly" statuses) == "alive" =
        let new_statuses1 = make_visible "flyswatter" statuses
            new_statuses2 = make_visible "fly" new_statuses1
            response = "As soon as you start to doze off, a fly lands on \n" 
                        ++ "your face and wakes you up again.\n"
        in (paths, locations, new_statuses2, response)
    | otherwise = (paths, locations, statuses, 
                   "Ahhh...you (yawn) made...it...zzzzzzzz.\n" ++
                   "The game is over.\n")

swat :: PathMap -> LocationMap -> Statuses -> World
swat paths locations statuses 
    | get here statuses /= "lit" = 
        (paths, locations, statuses, "You flail aimlessly in the dark!\n")
    | get "flyswatter" locations /= "holding" =
        (paths, locations, statuses, "You aren't holding the flatswatter.\n")
    | (elem ("fly", "dead") statuses) = 
        (paths, locations, statuses, "He's dead, Jim.\n")
    | get "fly" locations /= here =
        (paths, locations, statuses, 
         "You swish the flyswatter through the air.\n")
    where here = get "myself" locations
swat paths locations statuses 
    -- fly buzzes off
    | (fly_location == "bedroom" && den_status == "lit") = 
        (paths, (put "fly" "den" locations), statuses, 
         "The fly escapes into the other room!")
    -- fly buzzes off
    | (fly_location == "den" && bedroom_status == "lit") =
        (paths, (put "fly" "bedroom" locations), statuses, 
         "The fly escapes into the other room!")
    | otherwise =  
        let new_statuses = kill "fly" statuses
        in (paths, locations, new_statuses,
            "Success! You killed that pesty fly!\n")
    where fly_location = get "fly" locations
          den_status = get "den" statuses
          bedroom_status = get "bedroom" statuses

kill :: Thing -> Statuses -> Statuses 
kill thing statuses =  
    if (elem (thing, "dead") statuses) 
    then statuses
    else if (elem (thing, "alive") statuses) 
         then let pred_func (x, y) = (x, y) /= (thing, "alive")
                  new_statuses = filter pred_func statuses 
              in (thing, "dead") : new_statuses
         else (thing, "dead") : statuses

make_visible :: Thing -> Statuses -> Statuses 
make_visible thing statuses =
    if (elem (thing, "invisible") statuses) 
    then let new_thing_status = (thing, "visible")
             pred_func (x, y) = (x, y) /= (thing, "invisible")
             new_statuses = filter pred_func statuses
         in new_thing_status : new_statuses
    else if (elem (thing, "visible") statuses)
         then statuses 
         else (thing, "visible") : statuses 

buzz_off :: PathMap -> LocationMap -> Statuses -> World
buzz_off paths locations statuses 
        | (fly_location == "bedroom" && den_status == "lit") = 
            (paths, (put "fly" "den" locations), statuses, 
             "The fly escapes into the other room!")
        | (fly_location == "den" && bedroom_status == "lit") =
            (paths, (put "fly" "bedroom" locations), statuses, 
             "The fly escapes into the other room!")
        where fly_location = get "fly" locations
              den_status = get "den" statuses
              bedroom_status = get "bedroom" statuses

game_take :: Thing -> PathMap -> LocationMap -> Statuses -> World 
game_take "fly" paths locations statuses = 
    (paths, locations, statuses, "It's too fast for you!\n")
game_take "light switch" paths locations statuses = 
    (paths, locations, statuses, "It's firmly embedded on the wall!\n")
game_take thing paths locations statuses =
    let here = get "myself" locations
        there = get thing locations
    in if here == there
       then (paths, (put thing "holding" locations), statuses, "OK, taken.")
       else if there == "holding"
            then (paths, locations, statuses, "You are already holding it.")
            else (paths, locations, statuses, "I don't see it here.")

game_drop :: Thing -> PathMap -> LocationMap -> Statuses -> World          
game_drop thing paths locations statuses = 
    let here = get "myself" locations
        there = get thing locations
    in if there == "holding"
        then (paths, (put thing here locations), statuses, "OK, dropped.")
        else (paths, locations, statuses, "You aren't holding it.")

go :: String -> PathMap -> LocationMap -> Statuses -> World
go direction paths locations statuses = do
    let my_location = get "myself" locations
    if can_move my_location direction paths statuses 
        then do
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = look_string new_location paths new_locations statuses
            (paths, new_locations, statuses, response)
        else (paths, locations, statuses, 
                cannot_move_because my_location direction)


put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

can_move :: Location -> Direction -> PathMap -> Statuses -> Bool
can_move "bedroom" "n" _ statuses = get "bedroom" statuses == "lit"
can_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
cannot_move_because "bedroom" "n" = "You trip over something in the dark.\n"
cannot_move_because _ _ = "You can't go that way."

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> Statuses -> World
do_command "n" paths locations statuses = go "n" paths locations statuses
do_command "e" paths locations statuses = go "e" paths locations statuses
do_command "s" paths locations statuses = go "s" paths locations statuses
do_command "w" paths locations statuses = go "w" paths locations statuses
do_command "u" paths locations statuses = go "u" paths locations statuses
do_command "d" paths locations statuses = go "d" paths locations statuses
do_command "on" paths locations statuses  = on_switch paths locations statuses
do_command "off" paths locations statuses = off_switch paths locations statuses
do_command "look" paths locations statuses = look paths locations statuses
do_command "sleep" paths locations statuses = sleep paths locations statuses
do_command "instructions" paths locations statuses = 
    (paths, locations, statuses, instructions)
do_command "quit" paths locations statuses = 
    (paths, locations, statuses, "quit")
do_command "dump" paths locations statuses =
    (paths, locations, statuses, "paths = " ++ show paths ++ 
     "\nlocations = " ++ show locations ++ "\nstatuses = " ++ show statuses)

do_command cmd paths locations statuses = 
    do_command_2 cmd paths locations statuses

do_command_2 :: String -> PathMap -> LocationMap -> Statuses -> World
do_command_2 cmd paths locations statuses
    | isPrefixOf "take " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations statuses
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations statuses
    | isPrefixOf "use " cmd =
          use (tail $ snd $ span isLetter cmd) paths locations statuses
    | otherwise = (paths, locations, statuses, "I don't understand: " ++ cmd)

items_here :: LocationMap -> Statuses -> Response
items_here locations statuses =
    let here = get "myself" locations
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, 
                                    place == here, 
                                    get place statuses == "lit",
                                    get thing statuses == "visible", 
                                    thing /= "myself"]
    in intercalate "\n" things

look :: PathMap -> LocationMap -> Statuses -> World
look paths locations statuses =
    if things == []
    then (paths, locations, statuses, 
          (describe my_location locations statuses))
    else (paths, locations, statuses, 
          (describe my_location locations statuses ++ "\n\n" ++ things))
    where my_location = get "myself" locations
          things = items_here locations statuses

look_string :: Location -> PathMap -> LocationMap -> Statuses -> String
look_string new_location paths locations statuses = 
    if things == []
    then describe new_location locations statuses
    else describe new_location locations statuses ++ "\n\n" ++ things
    where things = items_here locations statuses

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

describe :: Location -> LocationMap -> Statuses -> Response 
describe new_location locations statuses =
    let here = get "myself" locations
        here_status = get here statuses 
    in describe_helper here here_status locations 

describe_helper :: Location -> String -> LocationMap -> String
describe_helper "bedroom" "lit" locations = description "bedroom1"
describe_helper "bedroom" "dark" locations = description "bedroom2"
describe_helper "den" "lit" locations = description "den1"
describe_helper "den" "dark" locations = description "den2"
describe_helper here  _ locations = description here

description :: String -> String
-- when bedroom is lit
description "bedroom1" =
    "You are in a bedroom with a large, comfortable bed.\n" ++
    "It has been a long, tiresome day, and you would like\n" ++
    "nothing better than to go to sleep.\n"

-- when bedroom is dark 
description "bedroom2" = 
    "You are in your bedroom. It is nice and dark.\n"

description "bed" =
    "You are in bed, and it feels great!\n"

-- when den is lit
description "den1" =
    "You are in your den. There is a lot of stuff here,\n" ++
    "but you are too sleepy to care about most of it.\n"

-- when den is dark
description "den2" =
    "You are in your den. It is dark.\n"

description someplace = someplace ++ ", and you can't see anything."

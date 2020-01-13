# Production version of Pinot Pairs/Which Wine pairing model

# About:
# Objective: Create a wine pairing Alexa Skill with several thousand pairings (3K+)
# which is significantly more than anything I found online. To accomplish this we need to take
# exisiting qualiy pairings and model those on other foods. I use Google Keyword Tool to find most commonly
# search foods

# This code does several things:
# 1. Laods in Exisiting wine pairing from top sources
# 2. Create different food groups and assigns them to each food that are most commonly Google Searched
# 3. Uses a Random Forest ML model to predict each food pairing
# 4. Pushes the food pairing recommendations into a format that can be uploaded to our Alexa Skill


# Wine pairing model
# load libraries
library(dplyr)
library(caret)
library(randomForest)
library(Hmisc)
library(doParallel)
library(foreach)
library(e1071)
library(forecast)

##### Load in my food sample
# Food Wine pairing file comes from:
#   https://www.intowine.com/food-wine-pairing-tool
# Pulled on 6/11/2019
# This comes from a wine pairing website, but only has a few pairings, not enough for me.
wine_parings <- read.csv("C:/Users/peter.klibowitz/Desktop/pairwise/wine_parings.csv")
wine_parings <- unique(wine_parings)
wine_parings2 <- wine_parings
ww <- wine_parings


#### Janice Robinson, wine writer from FT has provided some wine pairings on her website
# Janice pairings:
#   https://www.jancisrobinson.com/learn/food-matching/food-first
# Date: 7/15/2019
janice <- read.csv("C:/Users/peter.klibowitz/Desktop/pairwise/janice_pairings.csv")
janice$Dish <- tolower(janice$Dish)
ww$Dish <- tolower(ww$Dish)
janice$tmp <- janice$Dish %in% ww$Dish # janice wine pairing overlaps with original wine pairing list
janice$tmp2 <- ifelse(janice$tmp == TRUE, 1, 0)
www <- janice[which(janice$tmp2 == 1), ] # remove dupelicates
sum(janice$tmp2)
www <- left_join(www, ww, by = "Dish")

# check for how many pairings each wine has
metrics <- wine_parings
metrics$count <- 1
wine_parings_counts <- metrics %>%
  group_by(Wine.Type) %>%
  summarise(count_wines = sum(count))
rm(metrics)

## Alternate wines -------------------------------
######
# I'm changing a few wine pairings to similar alternatives because
# many wine types only have one food pairing which messes up our model
# instead its better to move it to a similar wine with multiple food pairings.
wine_parings$Wine.Type <- as.character(wine_parings$Wine.Type)
wine_parings$Wine.Type2 <- wine_parings$Wine.Type
# Barbaresco = Nebbiolo or Barbera
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Barbaresco', 'Nebbiolo', wine_parings$Wine.Type2)
# Barbera = Barbaresco = Nebbiolo
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Barbera', 'Nebbiolo', wine_parings$Wine.Type2)
# Barolo = Nebbiolo
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Barolo', 'Nebbiolo', wine_parings$Wine.Type2)
# Central Coast Pinot Noir = Pinot Noir
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Central Coast Pinot Noir', 'Pinot Noir', wine_parings$Wine.Type2)
# Chianti = Sangiovese
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Chianti', 'Sangiovese', wine_parings$Wine.Type2)
# Dolcetto = Syrah
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Dolcetto', 'Syrah', wine_parings$Wine.Type2)
# Sparkling wine = Prosecco
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Sparkling wine', 'Prosecco', wine_parings$Wine.Type2)
# Reisling = Riesling
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Reisling', 'Riesling', wine_parings$Wine.Type2)
# Vouvray = Riesling
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Vouvray', 'Riesling', wine_parings$Wine.Type2)
# Grenache based dessert wine = Port
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Grenache based dessert wine', 'Port', wine_parings$Wine.Type2)
# Pinot Gris = Pinot Grigio
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Pinot Gris', 'Pinot Grigio', wine_parings$Wine.Type2)
# Gamay = Pinot Noir
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Gamay', 'Pinot Noir', wine_parings$Wine.Type2)
# Montepulciano = Sangiovese
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Montepulciano', 'Sangiovese', wine_parings$Wine.Type2)
# Rhone blends = Cabernet Sauvignon
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Rhone blends', 'Cabernet Sauvignon', wine_parings$Wine.Type2)
# Rhone blends White = Chardonnay
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Rhone blends White', 'Chardonnay', wine_parings$Wine.Type2)
# Super Tuscan = Merlot
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Super Tuscan', 'Merlot', wine_parings$Wine.Type2)
# Tempranillo = Rioja
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Tempranillo', 'Rioja', wine_parings$Wine.Type2)
# Ribolla Gialla = Muscadet
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Ribolla Gialla', 'Muscadet', wine_parings$Wine.Type2)
# Rosso Piceno = Sangiovese
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Rosso Piceno', 'Sangiovese', wine_parings$Wine.Type2)
# Valpoicella = Zinfandel
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Valpoicella', 'Zinfandel', wine_parings$Wine.Type2)
# White Bordeaux Blend = White Bordeaux Blend / Sav blanc
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Semillon', 'White Bordeaux Blend', wine_parings$Wine.Type2)
#Salice Salentino = Sauvignon Blanc
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Salice Salentino', 'Sauvignon Blanc', wine_parings$Wine.Type2)
# Sagrantino = Cabernet Sauvignon
wine_parings$Wine.Type2 <- ifelse(wine_parings$Wine.Type == 'Sagrantino', 'Cabernet Sauvignon', wine_parings$Wine.Type2)

# A check to see how many wines have pairings
metrics <- wine_parings
metrics$count <- 1
wine_parings_counts <- metrics %>%
  group_by(Wine.Type2) %>%
  summarise(count_wines = sum(count))
rm(metrics)

# one off remove rose
wine_parings <- wine_parings[which(wine_parings$Wine.Type != "Rosé"), ]

# Rename using the alternatives
wine_parings$Wine.Type <- wine_parings$Wine.Type2
wine_parings$Wine.Type2 <- NULL

# Peter Categories for model
##############################
# Main things to think about:
# Sodium (Salty)
# Salty foods (olives, cured meats, feta, oysters, parmesan, and miso)
# Acidic or tart items (citrus, vinegar, capers, pickles, and green apple)
# Sweet foods (berries, caramelized onions, beets, carrots, brown sugar, and roasted vegetables)

# EL: As odd as this may sound, consider using your eyesight. Just like red meat-red wine, pair red/brown/earthy colored foods like red cabbage and onion, beets, shiitake mushrooms, sun-dried tomatoes, cherries, plums, dark berries, nutty whole grains, lentils, and walnuts with a lighter-bodied red wine like Pinot Noir. 
# Light/yellow/green colored foods like cauliflower, asparagus, potatoes, chard, lime, yellow tomato, kiwi, split peas, white rice, and couscous go better with a "greenish" white wine like Sauvignon Blanc.

# Acid - Acidic foods require wine with the same or higher level of acidity. Acid and tannin are not friends. Acidic wines can cut through cream and fat.
# Salt - Salty foods require either high-acid or sweet wines to balance the palate.
# Sweet - Sweetness in food requires wines with the same or higher level of residual sugar. Sweet wines can tone down spicy elements in food and diminish the perception of high salt content.
# Fat - Fat attracts tannin. Fatty meats and cheeses require wine higher in tannin. However, fat from deep fried foods is cleansed from the palate by wines high in acid, not tannin.
# Tannin - Tannic wines cut through fatty, rich foods, can aggravate salty flavors, and overwhelm delicate flavors.
# Alcohol - High alcohol content can overwhelm delicate flavors and do not complement salty foods, as well as accentuating spicy impressions on the palate.
# Oak - Oak tannins can clash with salty, bitter components in food. They can overwhelm delicate and acidic foods but go well with smoked dishes.

# Food groups --------------------------------------
# To pair each food, we are going to break down what food categories 
# each food belongs to to make it easier to group and pair it
salty <- c('Feta', 'Feta Cheese', 'Parmesan Cheese', 'Parmesan', 'Salami', 'Prosciutto', 'anchovy', 'olive', 'popcorn', 'chips', 'ceasar salad', 'salt', 'salted', 'Bolillos')
sweet <- c('beet', 'walnut', 'molasses', 'carrot', 'cranberry.sauce', 'cranberry', 'blueberry', 'strawberry', 'pineapple', 'maple.syrup', 'honey', 'maple syrup' )
acidic <- c('capers', 'pickles', 'vinegar', 'lemon', 'grapefruit', 'lime', 'lemon.juice', 'lime.juice', 'orange', 'orange.juice', 'citrus', 'ceviche', 'sauerkraut', 'kimchi')
red_brown <- c('salmon','chili','chilli','onion', 'beet', 'plum', 'lentil', 'cabbage', 'mushroom','tomato', 'vodka sauce', 'red', 'marinara', 'lasagna', 'bolognese', 'manhatten', 'pizza', 'carbonara')

# greenish
tmp <- read.csv("greenish.csv")
greenish  <- tmp$foods

# breakfast
breakfast <- c('smoothie', 'jam', 'muffin','banana bread','acai','brunch', 'breakfast', 'waffle', 'omelet', 'Omelets', 'pancake', 'pancakes', 'Scrambled Eggs', 'eggs', 'quiche', 'Hollandaise Sauce', 'hollandaise',
               'french toast', 'doughnuts', 'acai bowl', 'toast', 'bacon', 'pastry', 'crepe', 'Huevos', 'eggs') # also use brunch

# seafood
seafood <- c('pescado','Salmon', 'Tuna', 'Crab Salad', 'crab', 'Bass', 'Bass', 'Bouillabaisse', 'Calamari', 'Ceviche', 'Cod', 'Crab', 'Crab Cakes', 'Grouper', 'Light Fish',
             'Lobster', 'Mackerel', 'Mahi Mahi', 'Monkfish', 'Mussels', 'Steamed Oysters', 'Raw Oysters', 'Oysters', 'squid',
             'Swordfish', 'Trout', 'Turbot', 'Shrimp', 'Sole', 'Sardines', 'Scallops', 'Sea Bass', 'seafood', 'mussel', 'oyster', 'sardine', 'sardines', 'raw oyster', 'steamed oyster',
             'fish', 'anchovy', 'halibut', 'tilapia', 'trout', 'raw oysters', 'steamed oysters', 'sushi', 'white clam sauce', 'crawfish',
             'new england clam chowder', 'clam chowder', 'red clam sauce', 'poke', 'sturgeon')

# red wine fishes
salmon <- c('salmon', 'sturgeon', 'snapper', 'manhatten')

# Meat
meat <- c('burger','reuben', 'jambalaya','sloppy joes','Bistec', 'beef', 'Carnitas', 'pollo', 'bacon', 'beef.rib', 'beef.shank', 'beef.tenderloin', 'chicken', 'duck', 'ham', 'lamb', 'ground.lamb', 'hamburger', 'ground.beef', 
          'lamb.chop', 'lamb.shank', 'meat', 'meatball', 'meatloaf', 'pork', 'pork.chop', 'pork.tenderloin', 'pork.rib', 'pork chops', 'pork ribs', 'beef ribs',
          'poultry.sausage', 'poultry.sausages', 'chicken sausages', 'chicken sausage', 'poultry', 'prosciutto', 'rabbit', 'rabbits', 'rack.of.lamb', 'sausages', 'sausage',
          'veal', 'venison', 'turkey', 'corned beef', 'Filet Mignon', 'Steak', 'beef wellington', 'Brisket', 'Beef bourguignonne', 'Chateaubriand', 'Liver', 'New York Strip', 'Kansas City Strip', 'Pot Roast',
          'prime rib', 'rare steak', 'ribeye', 'Tenderloin', 'tri-tip', 'Veal Chops', 'Veal Marsala', 'Veal Parmigiana', 'Veal shanks', 'Veal Piccata', 'duck breast', 'chorizo',
          'kabobs','kebab', 'hen', 'ham', 'meatball', 'salami', 'burger', 'cheese burger', 'meatloaf', 'shepherd', 'shawarma', 'hot dog', 'carne', 'wing', 'bbq',
          'brat', 'wurst', 'bratwurst', 'gyro', 'sausage'
)

# desserts
desert <- c('oreos','icing','pudding','cupcakes', 'fairy','brittle', 'fudge', 'cookie','custard','dessert','mochi','desert', 'Dark Chocolate', 'Chocolate Chip Cookies', 'Apple Tart', 'Apple Pie', 'Chocolate Mousse', 'Chocolate Cake', 'cookies', 'brownie', 'ice cream', 'chocolate', 'pie', 'cake', 'cheesecake',
            'mochi', 'peanut butter cookies', 'sugar cookie', 'unicorn cake', 'churros', 'oatmeal cookies', 'rice krispie treats', 'pumpkin pie', 'cinnamon rolls', 'desert', 'dessert', 'mousse', 'tart', 'strudel', 'apple crisp')

# Fried fat
fried_fat <- c('deep.fry', 'fried', 'deep fry', 'fried chicken', 'fried turkey', 'butter', 'twinkies', 'bacon', 'french fries', 'fries') 

# Spicy food
spicy <- c('zhoug','tikka masala', 'jambalaya','curry', 'indian food', 'pepper', 'chili', 'jalapeno', 'ghost peper', 'spicy', 'thai food', 'chili','Jalapeno','Habanero','Tabasco','Cayenne')

# cheeses that pair with red wines
red_cheese <- c('gorgonzola', 'gouda', 'gruyere', 'mozzarella', 'macaroni', 'pecorino', 'romano', 'mac and', 'parmesan cheese', 'parmesan', 'quesadilla', 'fajitas')

# Cheeses that pair with white wines
white_cheese <- c('camembert', 'cheddar', 'chevre', 'fontina', 'goat cheese', 'munster', 'swiss', 'mild', 'feta', 'white wine', 'citrus')

# cheeses that pair with port
port_cheese <- c('rochefort','rocquefort', 'stilton', 'bleu')

# Ethnic cuisine
ethnic_cuisine <- c('chutney','mexican', 'tikka masala', 'asian', 'mein', 'chinese', 'baba ganoush', "thai", 'thai food', 'curry', 'oriental', 'guacamole', 'guac', 'salsa', 'indian', 'moroccan', 'ethiopian', 'sushi', 'suey')

# White meats
white_meat <- c('sausage','gyro','bratwurst','chicken', 'ham', 'pollo', 'lamb', 'ground.lamb', 'Chorizo', 'Carnitas', 
                'lamb.chop', 'lamb.shank', 'pork', 'pork.chop', 'pork.tenderloin', 'pork.rib', 'pork chops', 'pork ribs',
                'poultry.sausage', 'poultry.sausages', 'chicken sausage', 'poultry', 'rack.of.lamb','turkey','hen', 'hot dog')

# Birds
birds <- c('chicken', 'duck','poultry.sausage', 'poultry.sausages', 'chicken sausages', 'chicken sausage', 'poultry',
           'turkey','duck breast','hen', 'Pheasant', 'quail')

# Fish
fish <- c('pescado', 'Salmon', 'Tuna', 'Bass', 'Bass', 'Cod','Grouper', 'Light Fish','sturgeon',
          'Mackerel', 'Mahi Mahi', 'Monkfish','Swordfish', 'Trout', 'Turbot', 'Sole', 'Sardines', 'Sea Bass', 'sardine', 'sardines',
          'fish', 'anchovy', 'halibut', 'tilapia', 'trout', 'nicoise salad', 'poke', 'sashimi')

# french
french <- c('beef bourguignonne', 'chateaubriand', 'duck confit', 'bouillabaisse', 'hollandaise sauce')

# Fruit
tmp <- read.csv("fruit .csv")
fruit  <- tmp$foods

# Italian foods
italian_food <- c('marsala', 'parmigiana', 'piccata', 'pecorino', 'alfredo sauce', 'bolognese', 'carbonara sauce', 'pizza', 'lasagna', 'macaroni', 'marinara', 'spaghetti', 'mac',
                  'pasta', 'risotto', 'alfredo', 'bruschetta', 'ravioli', 'italian'
)


# veggies 
tmp <- read.csv("veggies .csv")
veggies  <- tmp$foods

# Bread & Noodles
tmp <- read.csv("bread_noodle.csv")
bread_noodle <- tmp$foods

# Goat cheese
tmp <- read.csv("goat_cheese.csv")
goat_cheese <- tmp$foods

# Mexican foods
tmp <- read.csv("mexican_foods.csv")
mexican_foods <- tmp$foods


# Make everything to lower ---------------
wine_parings$Dish <- tolower(wine_parings$Dish)
fried_fat <- tolower(fried_fat)
desert <- tolower(desert)
meat <- tolower(meat)
salmon <- tolower(salmon)
seafood <- tolower(seafood)
breakfast <- tolower(breakfast)
birds <- tolower(birds)
greenish <- tolower(greenish)
red_brown <- tolower(red_brown)
acidic <- tolower(acidic)
sweet <- tolower(sweet)
salty <- tolower(salty)
spicy <- tolower(spicy)
red_cheese <- tolower(red_cheese)
white_cheese <- tolower(white_cheese)
port_cheese <- tolower(port_cheese)
ethnic_cuisine <- tolower(ethnic_cuisine)
white_meat <- tolower(white_meat)
fish <- tolower(fish)
french <- tolower(french)
italian_food <- tolower(italian_food)
fruit <- tolower(fruit)
veggies <- tolower(veggies)
mexican_foods <- tolower(mexican_foods)
goat_cheese <- tolower(goat_cheese)
bread_noodle <- tolower(bread_noodle)


# # remove periods with spaces
# names(epi_r) <- gsub("\\.", " ", names(epi_r)) 

# Built these fileds into our wine pairings to make ML model ---------------
wine_parings$fried_fat <- ifelse(grepl(paste(fried_fat, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$fried_fat <- ifelse(grepl(paste(fried_fat, collapse = " | "), wine_parings$Dish), 1, wine_parings$fried_fat)
wine_parings$desert <- ifelse(grepl(paste(desert, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$desert <- ifelse(grepl(paste(desert, collapse = " | "), wine_parings$Dish), 1, wine_parings$desert)
wine_parings$meat <- ifelse(grepl(paste(meat, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$meat <- ifelse(grepl(paste(meat, collapse = " | "), wine_parings$Dish), 1, wine_parings$meat)
wine_parings$salmon <- ifelse(grepl(paste(salmon, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$salmon <- ifelse(grepl(paste(salmon, collapse = " | "), wine_parings$Dish), 1, wine_parings$salmon)
wine_parings$seafood <- ifelse(grepl(paste(seafood, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$seafood <- ifelse(grepl(paste(seafood, collapse = " | "), wine_parings$Dish), 1, wine_parings$seafood)
wine_parings$breakfast <- ifelse(grepl(paste(breakfast, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$breakfast <- ifelse(grepl(paste(breakfast, collapse = " | "), wine_parings$Dish), 1, wine_parings$breakfast)
wine_parings$greenish <- ifelse(grepl(paste(greenish, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$greenish <- ifelse(grepl(paste(greenish, collapse = " | "), wine_parings$Dish), 1, wine_parings$greenish)
wine_parings$red_brown <- ifelse(grepl(paste(red_brown, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$red_brown <- ifelse(grepl(paste(red_brown, collapse = " | "), wine_parings$Dish), 1, wine_parings$red_brown)
wine_parings$acidic <- ifelse(grepl(paste(acidic, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$acidic <- ifelse(grepl(paste(acidic, collapse = " | "), wine_parings$Dish), 1, wine_parings$acidic)
wine_parings$sweet <- ifelse(grepl(paste(sweet, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$sweet <- ifelse(grepl(paste(sweet, collapse = " | "), wine_parings$Dish), 1, wine_parings$sweet)
wine_parings$salty <- ifelse(grepl(paste(salty, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$salty <- ifelse(grepl(paste(salty, collapse = " | "), wine_parings$Dish), 1, wine_parings$salty)
wine_parings$spicy <- ifelse(grepl(paste(spicy, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$spicy <- ifelse(grepl(paste(spicy, collapse = " | "), wine_parings$Dish), 1, wine_parings$spicy)
wine_parings$red_cheese <- ifelse(grepl(paste(red_cheese, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$red_cheese <- ifelse(grepl(paste(red_cheese, collapse = " | "), wine_parings$Dish), 1, wine_parings$red_cheese)
wine_parings$white_cheese <- ifelse(grepl(paste(white_cheese, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$white_cheese <- ifelse(grepl(paste(white_cheese, collapse = " | "), wine_parings$Dish), 1, wine_parings$white_cheese)
wine_parings$port_cheese <- ifelse(grepl(paste(port_cheese, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$port_cheese <- ifelse(grepl(paste(port_cheese, collapse = " | "), wine_parings$Dish), 1, wine_parings$port_cheese)
wine_parings$ethic_cuisine <- ifelse(grepl(paste(ethnic_cuisine, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$ethic_cuisine <- ifelse(grepl(paste(ethnic_cuisine, collapse = " | "), wine_parings$Dish), 1, wine_parings$ethic_cuisine)
wine_parings$birds <- ifelse(grepl(paste(birds, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$birds <- ifelse(grepl(paste(birds, collapse = " | "), wine_parings$Dish), 1, wine_parings$birds)
wine_parings$white_meat <- ifelse(grepl(paste(white_meat, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$white_meat <- ifelse(grepl(paste(white_meat, collapse = " | "), wine_parings$Dish), 1, wine_parings$white_meat)
wine_parings$fish <- ifelse(grepl(paste(fish, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$fish <- ifelse(grepl(paste(fish, collapse = " | "), wine_parings$Dish), 1, wine_parings$fish)
wine_parings$french <- ifelse(grepl(paste(french, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$french <- ifelse(grepl(paste(french, collapse = " | "), wine_parings$Dish), 1, wine_parings$french)
wine_parings$italian_food <- ifelse(grepl(paste(italian_food, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$italian_food <- ifelse(grepl(paste(italian_food, collapse = " | "), wine_parings$Dish), 1, wine_parings$italian_food)
wine_parings$fruit <- ifelse(grepl(paste(fruit, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$fruit <- ifelse(grepl(paste(fruit, collapse = " | "), wine_parings$Dish), 1, wine_parings$fruit)
wine_parings$veggies <- ifelse(grepl(paste(veggies, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$veggies <- ifelse(grepl(paste(veggies, collapse = " | "), wine_parings$Dish), 1, wine_parings$veggies)
wine_parings$mexican_foods <- ifelse(grepl(paste(mexican_foods, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$mexican_foods <- ifelse(grepl(paste(mexican_foods, collapse = " | "), wine_parings$Dish), 1, wine_parings$mexican_foods)
wine_parings$goat_cheese <- ifelse(grepl(paste(goat_cheese, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$goat_cheese <- ifelse(grepl(paste(goat_cheese, collapse = " | "), wine_parings$Dish), 1, wine_parings$goat_cheese)
wine_parings$bread_noodle <- ifelse(grepl(paste(bread_noodle, collapse = "|"), wine_parings$Dish), 1, 0)
wine_parings$bread_noodle <- ifelse(grepl(paste(bread_noodle, collapse = " | "), wine_parings$Dish), 1, wine_parings$bread_noodle)

# One off fixes --------------
wine_parings$desert <- ifelse(wine_parings$Dish == 'chicken pot pie', 0, wine_parings$desert)
wine_parings$desert <- ifelse(wine_parings$Dish == 'turkey pot pie', 0, wine_parings$desert)
wine_parings$desert <- ifelse(wine_parings$Dish == 'pancake', 0, wine_parings$desert)
wine_parings$desert <- ifelse(wine_parings$Dish == 'crab cakes', 0, wine_parings$desert)
wine_parings$desert <- ifelse(wine_parings$breakfast == 1, 0, wine_parings$desert)
wine_parings$white_meat <- ifelse(wine_parings$Dish == 'hamburgers', 0, wine_parings$white_meat)
wine_parings$italian_food <- ifelse(wine_parings$Dish == 'spaghetti squash', 0, wine_parings$italian_food)
wine_parings$desert <- ifelse(wine_parings$Dish == 'pierogi', 0, wine_parings$desert)
wine_parings$desert <- ifelse(wine_parings$meat == 1, 0, wine_parings$desert)

# Make a 'white' in the title flag
wine_parings$red_brown <-  ifelse(grepl("red", wine_parings$Dish), 1, wine_parings$red_brown)
wine_parings$white_flag <-  ifelse(grepl("white", wine_parings$Dish), 1, 0)

# check sums
wine_parings$tmp <- rowSums(wine_parings[c(6:ncol(wine_parings))])
wine_parings$tmp <- NULL

# Build model -------------------
#############

# create training and testing data sets
wine_parings$Wine.2 <- NULL
wine_parings$Wine.3 <- NULL
wine_parings$Bottle.Recommendation <- NULL
set.seed(123456)
training_model <- createDataPartition(y = wine_parings$Wine.Type, p=1.0, list=FALSE)

# Make training and testing sets
training <- wine_parings[training_model,]
testing <- wine_parings[-training_model,]

# output dimension counts to make sure they match
dim(training); dim(testing)

# set.sed and paraellel
set.seed(123456)
registerDoParallel()

# random forest model
col_to_remove <- "Wine.Type"

x <- training[, !(colnames(training) %in% col_to_remove)]
y <- as.factor(training$Wine.Type)
x$Dish <- NULL
rf <- foreach(ntree = rep(40, 6), .combine = randomForest::combine, .packages = 'randomForest') %dopar% {
  randomForest(x, y, ntree = ntree)
}

# training RMSE
training$predictions <- predict(rf, newdata = training)
metrics <- training
metrics$count <- 1
wine_parings_counts <- metrics %>%
  group_by(Wine.Type) %>%
  summarise(count_wines = sum(count))
wine_parings_counts2 <- metrics %>%
  group_by(predictions) %>%
  summarise(count_wines_predictions = sum(count))
rm(metrics)

# Review training
wine_parings_counts <- left_join(wine_parings_counts, wine_parings_counts2, by = c("Wine.Type" = "predictions"))

# testing RMSE
testing$predictions <- predict(rf, newdata = testing)

### Let's load in the Google Data --------------------
# These are the most commonly search foods and food recipes on Google and come
# from Google Keyword Tool
google_keyword_2 <- read.csv("C:/Users/peter.klibowitz/Desktop/pairwise/google_keyword_2.csv", header = TRUE, skip = 2)

# remove the word recipe from the data
google_keyword_2$Keyword <- gsub("recipes", "", google_keyword_2$Keyword)
google_keyword_2$Keyword <- gsub("recipe", "", google_keyword_2$Keyword)
google_keyword_2$Keyword <- gsub("near me", "", google_keyword_2$Keyword)
google_keyword_2$Keyword <- gsub("menudo", "", google_keyword_2$Keyword)
google_keyword_2$Keyword <- gsub("menu", "", google_keyword_2$Keyword)
google_keyword_2$Keyword <- trimws(google_keyword_2$Keyword)
google_keyword_2$Keyword <- gsub("'", '', google_keyword_2$Keyword)
google_keyword_2$Keyword <- tolower(google_keyword_2$Keyword)

# Remove the extra columns
google_keyword_2 <- google_keyword_2[, c(1,4:5)]
google_keyword_2$Min.search.volume <- 1000
google_keyword_2$Max.search.volume <- 1000
google_keyword_2 <- unique(google_keyword_2)
google_keyword_2$rank <- 1:nrow(google_keyword_2)

# change name
names(google_keyword_2)[1] <- "Dish"

# Built these fileds into our wine pairings to make ML model ---------------
google_keyword_2$fried_fat <- ifelse(grepl(paste(fried_fat, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$fried_fat <- ifelse(grepl(paste(fried_fat, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$fried_fat)
google_keyword_2$desert <- ifelse(grepl(paste(desert, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$desert <- ifelse(grepl(paste(desert, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$desert)
google_keyword_2$meat <- ifelse(grepl(paste(meat, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$meat <- ifelse(grepl(paste(meat, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$meat)
google_keyword_2$salmon <- ifelse(grepl(paste(salmon, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$salmon <- ifelse(grepl(paste(salmon, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$salmon)
google_keyword_2$seafood <- ifelse(grepl(paste(seafood, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$seafood <- ifelse(grepl(paste(seafood, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$seafood)
google_keyword_2$breakfast <- ifelse(grepl(paste(breakfast, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$breakfast <- ifelse(grepl(paste(breakfast, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$breakfast)
google_keyword_2$greenish <- ifelse(grepl(paste(greenish, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$greenish <- ifelse(grepl(paste(greenish, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$greenish)
google_keyword_2$red_brown <- ifelse(grepl(paste(red_brown, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$red_brown <- ifelse(grepl(paste(red_brown, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$red_brown)
google_keyword_2$acidic <- ifelse(grepl(paste(acidic, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$acidic <- ifelse(grepl(paste(acidic, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$acidic)
google_keyword_2$sweet <- ifelse(grepl(paste(sweet, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$sweet <- ifelse(grepl(paste(sweet, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$sweet)
google_keyword_2$salty <- ifelse(grepl(paste(salty, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$salty <- ifelse(grepl(paste(salty, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$salty)
google_keyword_2$spicy <- ifelse(grepl(paste(spicy, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$spicy <- ifelse(grepl(paste(spicy, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$spicy)
google_keyword_2$red_cheese <- ifelse(grepl(paste(red_cheese, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$red_cheese <- ifelse(grepl(paste(red_cheese, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$red_cheese)
google_keyword_2$white_cheese <- ifelse(grepl(paste(white_cheese, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$white_cheese <- ifelse(grepl(paste(white_cheese, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$white_cheese)
google_keyword_2$port_cheese <- ifelse(grepl(paste(port_cheese, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$port_cheese <- ifelse(grepl(paste(port_cheese, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$port_cheese)
google_keyword_2$ethic_cuisine <- ifelse(grepl(paste(ethnic_cuisine, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$ethic_cuisine <- ifelse(grepl(paste(ethnic_cuisine, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$ethic_cuisine)
google_keyword_2$birds <- ifelse(grepl(paste(birds, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$birds <- ifelse(grepl(paste(birds, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$birds)
google_keyword_2$white_meat <- ifelse(grepl(paste(white_meat, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$white_meat <- ifelse(grepl(paste(white_meat, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$white_meat)
google_keyword_2$fish <- ifelse(grepl(paste(fish, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$fish <- ifelse(grepl(paste(fish, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$fish)
google_keyword_2$french <- ifelse(grepl(paste(french, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$french <- ifelse(grepl(paste(french, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$french)
google_keyword_2$italian_food <- ifelse(grepl(paste(italian_food, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$italian_food <- ifelse(grepl(paste(italian_food, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$italian_food)
google_keyword_2$fruit <- ifelse(grepl(paste(fruit, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$fruit <- ifelse(grepl(paste(fruit, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$fruit)
google_keyword_2$veggies <- ifelse(grepl(paste(veggies, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$veggies <- ifelse(grepl(paste(veggies, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$veggies)
google_keyword_2$mexican_foods <- ifelse(grepl(paste(mexican_foods, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$mexican_foods <- ifelse(grepl(paste(mexican_foods, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$mexican_foods)
google_keyword_2$goat_cheese <- ifelse(grepl(paste(goat_cheese, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$goat_cheese <- ifelse(grepl(paste(goat_cheese, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$goat_cheese)
google_keyword_2$bread_noodle <- ifelse(grepl(paste(bread_noodle, collapse = "|"), google_keyword_2$Dish), 1, 0)
google_keyword_2$bread_noodle <- ifelse(grepl(paste(bread_noodle, collapse = " | "), google_keyword_2$Dish), 1, google_keyword_2$bread_noodle)

# One off fixes --------------
google_keyword_2$desert <- ifelse(google_keyword_2$Dish == 'chicken pot pie', 0, google_keyword_2$desert)
google_keyword_2$desert <- ifelse(google_keyword_2$Dish == 'turkey pot pie', 0, google_keyword_2$desert)
google_keyword_2$desert <- ifelse(google_keyword_2$Dish == 'pancake', 0, google_keyword_2$desert)
google_keyword_2$desert <- ifelse(google_keyword_2$Dish == 'crab cakes', 0, google_keyword_2$desert)
google_keyword_2$desert <- ifelse(google_keyword_2$breakfast == 1, 0, google_keyword_2$desert)
google_keyword_2$white_meat <- ifelse(google_keyword_2$Dish == 'hamburgers', 0, google_keyword_2$white_meat)
google_keyword_2$italian_food <- ifelse(google_keyword_2$Dish == 'spaghetti squash', 0, google_keyword_2$italian_food)
google_keyword_2$bread_noodle <- ifelse(google_keyword_2$Dish == 'spaghetti squash', 0, google_keyword_2$bread_noodle)
google_keyword_2$italian_food <- ifelse(google_keyword_2$Dish == 'chicken pesto pasta', 0, google_keyword_2$italian_food)
google_keyword_2$italian_food <- ifelse(google_keyword_2$Dish == 'pasta salad', 0, google_keyword_2$italian_food)
google_keyword_2$desert <- ifelse(google_keyword_2$Dish == 'pierogi', 0, google_keyword_2$desert)
google_keyword_2$desert <- ifelse(google_keyword_2$meat == 1, 0, google_keyword_2$desert)
google_keyword_2$desert <- ifelse(google_keyword_2$fish == 1, 0, google_keyword_2$desert)

# Make a 'white' in the title flag
google_keyword_2$red_brown <-  ifelse(grepl("red", google_keyword_2$Dish), 1, google_keyword_2$red_brown)
google_keyword_2$white_flag <-  ifelse(grepl("white", google_keyword_2$Dish), 1, 0)

# row sum check
google_keyword_2$tmp <- rowSums(google_keyword_2[c(5:ncol(google_keyword_2))])
google_keyword_2 <- google_keyword_2[which(google_keyword_2$tmp > 0), ]
google_keyword_2$tmp <- NULL

# Test out wine selections
google_keyword_2$predictions <- predict(rf, newdata = google_keyword_2)


######## Make unique list -------------
wine_parings2$Dish <- tolower(wine_parings2$Dish)
wine_parings2$tmp <- substr(wine_parings2$Dish, nchar(wine_parings2$Dish), nchar(wine_parings2$Dish))
wine_parings2$tmp2 <- wine_parings2$tmp == "s"
wine_parings3 <- wine_parings2[which(wine_parings2$tmp2 == TRUE), ]
wine_parings3$Dish <- substr(wine_parings3$Dish, 1, nchar(wine_parings3$Dish)-1)
wine_parings2$tmp <- NULL; wine_parings2$tmp2 <- NULL
wine_parings3$tmp <- NULL; wine_parings3$tmp2 <- NULL
wine_parings2 <- rbind(wine_parings2, wine_parings3)
wine_parings2 <- unique(wine_parings2)

google_keyword_2$check <- google_keyword_2$Dish %in% wine_parings2$Dish
google_keyword_2 <- google_keyword_2[which(google_keyword_2$check == FALSE), ]
google_keyword_2$check <- google_keyword_2$Dish %in% wine_parings$Dish
google_keyword_2 <- google_keyword_2[which(google_keyword_2$check == FALSE), ]
google_keyword_2$Min.search.volume <- NULL
google_keyword_2$Max.search.volume <- NULL
google_keyword_2$rank <- NULL
google_keyword_2$check <- NULL

# combine the data sets
wine_parings2$Bottle.Recommendation <- NULL
wine_parings2$Wine.2 <- NULL
wine_parings2$Wine.3 <- NULL
google_keyword_2 <- google_keyword_2[,c(1,29)]
names(google_keyword_2)[2] <- "Wine.Type"
wine_parings2 <- rbind(wine_parings2, google_keyword_2)
wine_parings2 <- unique(wine_parings2)

# Layer in the Janice OG Selections
janice <- janice[, c("Dish", "Edited.Wine.OG")]
janice$Edited.Wine.OG <- as.character(janice$Edited.Wine.OG)
janice2 <- janice
janice2$Dish <- paste0(janice2$Dish, "s")
janice <- rbind(janice, janice2); rm(janice2)
janice <- unique(janice)
wine_parings2 <- left_join(wine_parings2, janice, by = "Dish")
wine_parings2$tmp <- ifelse(is.na(wine_parings2$Edited.Wine.OG) == TRUE, 0, 1)
wine_parings2$Wine.Type <- as.character(wine_parings2$Wine.Type)
wine_parings2$wine_parings2$Edited.Wine.OG
wine_parings2$Wine.Type <- ifelse(wine_parings2$tmp == 1, wine_parings2$Edited.Wine.OG, wine_parings2$Wine.Type)
wine_parings2$Edited.Wine.OG <- NULL; wine_parings2$tmp <- NULL
names(janice)[2] <- "Wine.Type"
wine_parings2 <- rbind(wine_parings2, janice)
wine_parings2 <- unique(wine_parings2)

# check everything that ends in s
wine_parings2$tmp <- substr(wine_parings2$Dish, nchar(wine_parings2$Dish), nchar(wine_parings2$Dish))
wine_parings2$tmp2 <- wine_parings2$tmp == "s"
wine_parings3 <- wine_parings2[which(wine_parings2$tmp2 == TRUE), ]
wine_parings3$Dish <- substr(wine_parings3$Dish, 1, nchar(wine_parings3$Dish)-1)
wine_parings2$tmp <- NULL; wine_parings2$tmp2 <- NULL
wine_parings3$tmp <- NULL; wine_parings3$tmp2 <- NULL
wine_parings2 <- rbind(wine_parings2, wine_parings3)
wine_parings2 <- unique(wine_parings2)
rm(wine_parings3, wine_parings_counts, wine_parings_counts2, training, testing)


### Make into a JSON file ------
wine_parings2$json <- paste0("'", wine_parings2$Dish, "': ", "'You should pair ", wine_parings2$Dish, 
                             " with a glass of ", wine_parings2$Wine.Type, "',")

# Add things without s's and include S's
wine_parings2$tmp <- substr(wine_parings2$Dish, nchar(wine_parings2$Dish), nchar(wine_parings2$Dish))
wine_parings2$tmp2 <- wine_parings2$tmp == "s"
wine_parings3 <- wine_parings2[which(wine_parings2$tmp2 == FALSE), ]
wine_parings3$Dish <- paste0(wine_parings3$Dish,"s")
wine_parings2$tmp <- NULL; wine_parings2$tmp2 <- NULL
wine_parings3$tmp <- NULL; wine_parings3$tmp2 <- NULL
wine_parings3$tmp <- wine_parings3$Dish %in% wine_parings2$Dish
wine_parings3 <- wine_parings3[which(wine_parings3$tmp == FALSE), ]
wine_parings3$tmp <- NULL

# add stuff
wine_parings2 <- rbind(wine_parings2, wine_parings3)
wine_parings2 <- unique(wine_parings2)


# remove some one-offs
wine_parings2 <- wine_parings2[which(wine_parings2$Dish != "bas"), ]
wine_parings2 <- wine_parings2[which(wine_parings2$Dish != "sea bas"), ]
wine_parings2 <- wine_parings2[which(wine_parings2$Dish != "sea ba"), ]
wine_parings2 <- wine_parings2[which(wine_parings2$Dish != "chicken expres"), ]

# ice cream override
wine_parings2$json <- ifelse(wine_parings2$Dish == "ice cream", "'ice cream': 'You should pair ice cream with a very sweet sherry which you pour over your ice cream',",
                             wine_parings2$json)
wine_parings2$json <- ifelse(wine_parings2$Dish == "ice creams", "'ice creams': 'You should pair ice cream with a very sweet sherry which you pour over your ice cream',",
                             wine_parings2$json)



# add in json editor text
wine_parings2$json_editor <- paste0('{"name" : { "value": "', wine_parings2$Dish, '" } },')
wine_parings2$tmp <- 1:nrow(wine_parings2)
wine_parings2$json_editor <- ifelse(wine_parings2$tmp == nrow(wine_parings2), paste0('{"name" : { "value": "', wine_parings2$Dish, '" } }'),
                                    wine_parings2$json_editor)
wine_parings2$tmp <- NULL

# for the website
wine_parings2$website <- paste0("<p>You should pair ", wine_parings2$Dish, 
                                " with a glass of ", wine_parings2$Wine.Type, "</p>")

# save the file
write.csv(wine_parings2, paste0("wine_parings2_", Sys.Date(),".csv"), row.names = FALSE)

### Make into a JSON file ------
# ideally paste into a config file
fff <- wine_parings2$json
cat(fff,file = "outfile.txt", sep = "\n")
file <- "outfile.txt"
fff <- readChar(file, file.info(file)$size)  #readLines("outfile.txt")
#lapply(fff, write, "recipes.txt", append = TRUE, ncolumns = 1)
zzz <- paste0("/* eslint-disable  func-names */
              /* eslint-disable max-len */
              /* eslint quote-props: ['error', 'consistent'] */
              module.exports = {
              'RECIPE_EN_GB': {
              ",
              fff, "
              },
              'RECIPE_EN_US': { ",
              fff, "
              
              },
              
              'RECIPE_EN_USA': {
              ", fff, " 
              },
              };")
writeLines(zzz, paste0("recipe_config_", Sys.Date(), ".txt"))

zzz <- paste0("/* eslint-disable  func-names */
              /* eslint-disable max-len */
              /* eslint quote-props: ['error', 'consistent'] */
              module.exports = {
              'RECIPE_EN_GB': {
              
              },
              'RECIPE_EN_US': { 
              
              },
              
              'RECIPE_EN_USA': {
              
              },
              };")
writeLines(zzz, paste0("recipe_config_balnk_", Sys.Date(), ".txt"))

# JSON Editor
fff <- wine_parings2$json_editor
cat(fff,file = "outfile.txt", sep = "\n")
file <- "outfile.txt"
fff <- readChar(file, file.info(file)$size)  #readLines("outfile.txt")
zzz <- paste0("{
              'interactionModel': {
              'languageModel': {
              'invocationName': 'pinot pairs',
              'intents': [
              {
              'name': 'RecipeIntent',
              'slots': [
              {
              'name': 'Item',
              'type': 'LIST_OF_ITEMS'
              }
              ],
              'samples': [
              'what are the ingredients for a {Item}',
              'what are the ingredients for an {Item}',
              'what are the ingredients for {Item}',
              'what do I need for a {Item}',
              'what's the recipe for a {Item}',
              'how do I pair an {Item}',
              'how do I pair a {Item}',
              'how do I pair {Item}',
              'how do I wine pair an {Item}',
              'how do I wine pair a {Item}',
              'how do I wine pair {Item}',
              'how should I pair an {Item}',
              'how should I pair a {Item}',
              'how should I pair {Item}',
              'what's the wine pairing for an {Item}',
              'what's the wine pairing for a {Item}',
              'what's the wine pairing for {Item}',
              'what should I pair with an {Item}',
              'what should I pair with a {Item}',
              'what should I pair with {Item}',
              'what's the pairing for an {Item}',
              'what's the pairing for a {Item}',
              'what's the pairing for {Item}',
              'what's pairs with an {Item}',
              'what's pairs with a {Item}',
              'what's pairs with {Item}',
              'wine pairing for an {Item}',
              'wine pairing for a {Item}',
              'wine pairing for {Item}',
              'pairing for an {Item}',
              'pairing for a {Item}',
              'pairing for {Item}',
              'pair an {Item}',
              'pair a {Item}',
              'pair {Item}',
              'how to get {Item}'
              ]
              },
              {
              'name': 'AMAZON.RepeatIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.HelpIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.StopIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.CancelIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.MoreIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.NavigateHomeIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.NavigateSettingsIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.NextIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.PageUpIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.PageDownIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.PreviousIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.ScrollRightIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.ScrollDownIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.ScrollLeftIntent',
              'samples': []
              },
              {
              'name': 'AMAZON.ScrollUpIntent',
              'samples': []
              }
              ],
              'types': [
              {
              'name': 'LIST_OF_ITEMS',
              'values': [",
              fff, 
              "
              
              ]
              }
              ]
              }
              }
              }")
zzz <- gsub("'", '"', zzz)
zzz <- gsub('what"s', "what's", zzz)
writeLines(zzz, paste0("json_config_", Sys.Date(), ".txt"))



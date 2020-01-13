# ml-winepairing-model-alexa-skill
Production version of Pinot Pairs/Which Wine Alexa Skills wine pairing model and output.  
Objective: Create a wine pairing Alexa Skill with several thousand pairings (3K+).

## This code does several things:
1. Laoads in Exisiting wine pairing from top sources
2. Create different food groups and assigns them to each food that are most commonly Google Searched
3. Uses a Random Forest ML model to predict each food pairing
4. Pushes the food pairing recommendations into a format that can be uploaded to our Alexa Skill



This is code for the Auckland bus twitter bot `@tuureiti`, modified to hide the secret keys.  It's not pretty code because it was written
as a quick and dirty experimental script, but it seems to work and people are asking for it. 

You'll need Twitter authorisation keys and an Auckland Transport authorisation key to use the code as is.

I followed the instructions here https://venturebeat.com/2017/02/02/how-to-build-your-own-twitter-bot-in-less-than-30-minutes/ to get  
“Consumer Key (API Key)” 
“Consumer Secret (API Secret)” 
“Access Token”
“Access Token Secret.”
I put these in a list called `secrets` with elements `$consumer_key`, `$consumer_secret`, `$access_token`, and `$access_secret`

I got an Auckland Transport API key from https://api.at.govt.nz/ and put it in the `secrets` list with name `$atapikey`
The line `load("~/.ssh/twitter-bot-secrets.rda")` loads this `secrets` list into R.

When the buses are doing well, the bot creates a beeswarm dotplot of all the bus delays, colour-coded by timeliness. 
When the buses are doing badly, it creates a map of Auckland with all the buses shown.  
The necessary shape information is in the object `auck2` in the file
`auckland-thinned.rda`

The variable `mihi` contains a greeting in te reo Maori that's added to the start of the next message. There's a startup greeting (Kia ora)
and a random choice of two morning greetings. 

After its first tweet after 10pm the bot goes to sleep for eight hours. 

The bot and its documentation are CC BY-SA 4.0.  
The Auckland map data are based on shapefiles for Auckland Wards published by Auckland Regional Council and are CC BY 3.0 NZ

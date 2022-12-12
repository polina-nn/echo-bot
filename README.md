# echo-bot
# Introduction
This is an educational project. Task is here - https://coda.io/@metalamp/education/3-14
I used template  - https://github.com/fullstack-development/haskell-internship/tree/master/echo-bot-template
This bot has settings specified in the config file.
You must choose some  parameters for the bot and create the file **/echo-bot/config.conf**
 1. You must choose  console or  telegram version.
 2. You must choose logging  to file or to console.
 3. You must choose minimum logging level.
 
Attention please!
You must have **config.conf** file at the root of the project. 
The file **/echo-bot/templateConfig.conf** contains configs examples for the bot.
If the config file was not created, or contains invalid values, an error message will be displayed. In this case the bot will start (console version) with default parameters.
If you want to use the telegram version of the bot, you must write your token to a config file. 
Of course, you should already have your own telegram bot. For this, you need to contact to @BotFather in telegram.


# Build and start the application

1. Clone this repository.
2. Create the file **/echo-bot/config.conf**  like **/echo-bot/templateConfig.conf** 
3. To run echo-bot to use Stack
   ```
   stack build
   stack exec echo-bot-exe
   ```

# Basic libraries
- сonfigurator  # to work with config
- time          # to work with time in logging
- req           # to work with http 
- aeson         # to work with json


# Project structure 
I explain only the contents of the **src/** folder, in other folders the contents are standard.

```
src/
│
├── FrontEnd/        
│   ├── Console.hs  # The console front-end is responsible for console I/O 
│   ├── Telegram.hs  # The telegram front-end is responsible for telegram I/O
│   ├── TelegramAPI.hs # Function for http requests to API Telegram  
│   ├── TelegramException.hs # Telegram Exception errors leading to a bot stop, requiring the involvement of an engineer     
│   └── TelegramTypes.hs # Types for http request to  API Telegram
│ 
├── Logger/      
│   └── Impl.hs   # The default implementation of the Logger interface
│ 
├── Congig.hs  # Configuration reader and default config value
├── ConfigurationTypes.hs # Type for config
├── EchoBot.hs # The pure echo-bot logic module.
└── Logger.hs # The logger interface module. Not define an implementation

```
# ToDoList or Weaknesses of the project
Thanks to my mentor [Pavel Zarubin](https://github.com/pavelzarubin), I could to close all ToDoList.
1. ~~Bot crashes with exception if in getMe if token is not valid. I want it to fall politely with a simple message to the user.~~ 
2. ~~The sendMessage function returns IO () and at the same time sends  information to the terminal. This should be logged.~~
3. ~~Why does the telegram bot duplicate messages about creating a config?~~
4. ~~When I worked with EchoBot handle, I used only work with config and logging. Other functions and the State monad are not used. When I writed a console bot with State, I used another's code and chart's help. It would be good to attach State and other functions to the telegram version by myself.~~
5. ~~Now the project is works with the repeat file (for recording repetitions) and it needs to make sure that it  empty or it has only what the bot wrote down. Otherwise - trouble.~~
6. ~~A lot of repetitive code in TelegramAPI, think about more universal functions.~~

# What I learned during the project review process
1. It was possible to apply pure logic from the template to the telegram project and console bot
2. Started to understand the handle pattern.
3. It was possible to work with "map" storing states in "IORef".
4. Little progress in understanding exception handling.
5. I began to better understand the work of http requests and responses. And what to do or not to do with them. And why?
6. In the end, I understood that data structures and functions should be clear. Need to work on this.
7. There was practice in working with the git.
## NetRunner

NetRunner is a project for allowing various AI agents use a web browser to complete tasks.
It is split into two seperate parts, one is a web application to use LLMs to complete 
tasks with the help of web search tools and the second is a Reinforcement Learning
environment to train RL agents to use the web browser in a human like way.

## Usage
Make sure to have [cabal](https://www.haskell.org/cabal/) installed to run the project.
You also need [geckodriver](https://github.com/mozilla/geckodriver/releases) and [Firefox](https://www.mozilla.org/en-US/firefox/new/) 
installed.

```
git clone git@github.com:JussiKarkkainen/NetRunner.git
cd NetRunner
./path/to/geckodriver

# For LLM application
python3 ai_server/server.py 
cabal run NetRunner

# For RL environment
python3 ai_server/rl_server.py
cabal run NetRunner -- rl
```


## LLM details
![llmvideo](/docs/llmtools.gif)










## RL Details
![rlvideo](/docs/rlvideo.gif)



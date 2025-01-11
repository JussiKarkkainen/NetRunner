## NetRunner
NetRunner is a project designed to enable AI agents to interact with web browsers to accomplish tasks. 
It consists of two main components: a web application that allows large language models (LLMs) to 
search the internet, and a reinforcement learning (RL) environment for training RL agents 
to operate within a web browser.

## Usage
Make sure to have [cabal](https://www.haskell.org/cabal/) installed to run the project.
You also need [geckodriver](https://github.com/mozilla/geckodriver/releases) and [Firefox](https://www.mozilla.org/en-US/firefox/new/).

```
git clone git@github.com:JussiKarkkainen/NetRunner.git
cd NetRunner
./path/to/geckodriver

# For LLM application
python3 ai_server/server.py 
cabal run NetRunner

# For RL environment
cabal run NetRunner -- rl
```

## LLM details
<img src="/docs/llmtools_new.gif" alt="LLM example" width="400">

The actual LLM calls are implemented in a small python file that can easily be modified to support 
different models. Currently OpenAI GPT-4o is used, but the core Haskell code is LLM independent.
The tasks are stored in a SQLite database and the browser tool use is implemented by communicating
with [geckodriver](https://github.com/mozilla/geckodriver/releases) using the [WebDriver](https://www.w3.org/TR/webdriver2/)
api.


## RL Details
<img src="/docs/rlvideo_new.gif" alt="RL Env" width="300">
(Run ```python3 ai_server/rl_human.py``` to play the environment after starting the servers)

The actual RL environment is implemented in [ai_server/rl_server.py](/ai_server/rl_server.py). It closely follows
the regular [Gym](https://gymnasium.farama.org/index.html) api, but has a few small differences. Fistly, the observations
(340x170 RGB images) are continuously streamed from the Haskell server to the environment, which means that even if the agent code never
calls ```env.step()```, new observations can be received. You also need to call ```env.start()``` to begin streaming
observations. 

The (current) action space looks like this:
```python
@dataclass
class NetworkOutput:
  action_type: Tensor   # shape: (3,) softmax probabilities
  keyboard_key: Tensor  # shape (78,) softmax probabilities 
  mouse: Tensor         # shape (2,) sigmoid coordinates in [0, 1]
```

The action type refers to either a no-op, keyboard action or a mouse action. There are 78 keyboard actions and a mouse click, 
which is represented by normalised x and y coordinates between 0 and 1.

from rl_server import StreamingEnv, NetworkOutput
from tinygrad import Tensor, nn
from dataclasses import dataclass


from tinygrad.tensor import Tensor
import tinygrad.nn as nn

class BrowserAgent:
  def __init__(self, input_shape, hidden_units=256):
    self.conv1 = nn.Conv2d(3, 16, 3, stride=2, padding=1)  # Input: (3, 170, 340) -> (16, 85, 170)
    self.conv2 = nn.Conv2d(16, 32, 3, stride=2, padding=1) # (16, 85, 170) -> (32, 43, 85)
    self.conv3 = nn.Conv2d(32, 64, 3, stride=2, padding=1) # (32, 43, 85) -> (64, 22, 43)
    self.flatten = nn.Linear(64 * 22 * 43, hidden_units) 
    
    self.action_type_fc = nn.Linear(hidden_units, 3)    
    self.keyboard_key_fc = nn.Linear(hidden_units, 78)   
    self.mouse_fc = nn.Linear(hidden_units, 2)       
    
    self.value_fc = nn.Linear(hidden_units, 1)        

  def __call__(self, obs: Tensor) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    x = self.conv1(obs).relu()
    x = self.conv2(x).relu()
    x = self.conv3(x).relu()
    x = x.reshape(x.shape[0], -1) 
    
    x = self.flatten(x).relu()
    
    action_type = self.action_type_fc(x).log_softmax() 
    keyboard_key = self.keyboard_key_fc(x).log_softmax()
    mouse = self.mouse_fc(x).sigmoid()  
    
    value = self.value_fc(x)
    
    return action_type, keyboard_key, mouse, value

def calculate_returns_and_advantages(rewards, values, gamma=0.99):
  pass

def update_actor_critic(actor_critic_fn, params, optimizer):
  pass

def collect_experience(env, agent):
  env.reset()
  observations, actions, log_probs, rewards, values, dones = [], [], [], [], [], []
  for step in range(config["steps_per_batch"]):
    observation = Tensor(env.get_observation()).div(255).mul(2).sub(1).permute(0, 3, 1, 2) # Normalize the image
    action, log_prob, value = agent(obs)

    reward, done = env.step()

		observations.append(obs)
    actions.append(action)
    log_probs.append(log_prob)
    rewards.append(reward)
    values.append(value)
    dones.append(done)

    if done:
      env.reset()

	observations = Tensor(observations)
  actions = Tensor(actions)
  log_probs = Tensor(log_probs)
  rewards = Tensor(rewards)
  values = Tensor(values)
  dones = Tensor(dones)

  return observations, actions, log_probs, rewards, values, dones

    
def rl_loop(env):
  env.start()
  print("Environment started. Press Ctrl+C to exit.")

  agent = BrowserAgent()
  try:
    for iteration in range(config["num_iterations"]):
      obs, actions, log_probs, rewards, values = collect_experience(env, agent)
      returns advantages = calculate_returns_and_advantages(rewards, values, config["gamma"])

      for _ in range(ppo_epochs):
        indices = sample_minibatches(obs, actions, returns, advantages)
        for batch in indices:
          optimizer.zero_grad()

          new_log_probs, value_preds = agent(batch["obs"])
          ratios = (new_log_probs - batch["log_probs"]).exp()
          clipped_ratios = ratios.clamp(1 - config["epsilon"], 1 + config["epsilon"])

          policy_loss = -Tensor.min(ratios * batch["advantages"], clipped_ratios * batch["advantages"]).mean()
          value_loss = (value_preds - batch["returns"]).pow(2).mean()

          loss = policy_loss + config["critic_loss_weight"] * value_loss - config["entropy_weight"] * entropy(new_log_probs)
          optimizer.step(loss)

  except KeyboardInterrupt:
    print("\nExiting loop. Stopping environment.")
  finally:
    env.stop()

config = {
    "num_iterations": 1000,
    "ppo_epochs": 5,
    "gamma": 0.99
    "epsilon": 0.2,
    "critic_loss_weight": 0.5,
    "entropy_weight": 0.01}


if __name__ == "__main__":
  env = StreamingEnv(task="wikipedia")
  rl_loop(env)

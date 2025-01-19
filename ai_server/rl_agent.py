from rl_server import StreamingEnv, NetworkOutput
from tinygrad import Tensor, nn
from dataclasses import dataclass
import numpy as np
import time

@dataclass 
class BrowserAgentOut:
  action_type: Tensor
  action_type_log_prob: Tensor
  keyboard_key: Tensor
  keyboard_key_log_prob: Tensor
  mouse_action: Tensor
  mouse_log_prob: Tensor
  value: Tensor

class BrowserAgent:
  def __init__(self, hidden_units=256):
    self.conv1 = nn.Conv2d(3, 16, 3, stride=2, padding=1)  # Input: (3, 170, 340) -> (16, 85, 170)
    self.conv2 = nn.Conv2d(16, 32, 3, stride=2, padding=1) # (16, 85, 170) -> (32, 43, 85)
    self.conv3 = nn.Conv2d(32, 64, 3, stride=2, padding=1) # (32, 43, 85) -> (64, 22, 43)
    self.flatten = nn.Linear(64 * 22 * 43, hidden_units) 
    
    self.action_type_fc = nn.Linear(hidden_units, 3)    
    self.keyboard_key_fc = nn.Linear(hidden_units, 78)   
    self.mouse_fc = nn.Linear(hidden_units, 2)       

    self.mouse_log_std = Tensor(np.zeros((1, 2)))
    
    self.value_fc = nn.Linear(hidden_units, 1)        

  def __call__(self, obs: Tensor) -> BrowserAgentOut:
    x = self.conv1(obs).relu()
    x = self.conv2(x).relu()
    x = self.conv3(x).relu()
    x = x.reshape(x.shape[0], -1) 
    
    x = self.flatten(x).relu()
    
    action_type_log_prob = self.action_type_fc(x).log_softmax() 
    action_type = action_type_log_prob.exp().multinomial()
    keyboard_key_log_prob = self.keyboard_key_fc(x).log_softmax()
    keyboard_key = keyboard_key_log_prob.exp().multinomial()

    mouse_mean = self.mouse_fc(x).sigmoid()  
    mouse_std = self.mouse_log_std.exp()
    
    noise = Tensor(np.random.randn(*mouse_mean.shape))
    mouse_action = mouse_mean + noise * mouse_std

    mouse_log_prob = -0.5 * (Tensor.log(2 * Tensor(np.pi)) + 2 * self.mouse_log_std + \
                     (mouse_action - mouse_mean)**2 / mouse_std**2).sum(axis=-1)

    value = self.value_fc(x)
    
    return BrowserAgentOut(
        action_type=action_type,
        action_type_log_prob=action_type_log_prob,
        keyboard_key=keyboard_key, 
        keyboard_key_log_prob=keyboard_key_log_prob,
        mouse_action=mouse_action, 
        mouse_log_prob=mouse_log_prob, 
        value=value)

def calculate_returns_and_advantages(rewards, values, gamma=0.99, normalize=True):
  returns = Tensor.zeros_like(rewards)
  returns[-1] = rewards[-1] * gamma * values[-1]

  for t in reversed(range(rewards.shape[1] - 1)):
    returns[:, t] = rewards[:, t] + gamma * returns[t + 1]

  if normalize:
    returns = (returns - returns.mean()) / (returns.std() + 1e-8)

  advantages = returns - values
  if normalize:
    advantages = (advantages - advantages.mean()) / (advantages.std() + 1e-8)

  return returns, advantages

def collect_experience(env, agent):
  env.reset()
  observations, actions = [], []
  action_type_log_probs, keyboard_log_probs, mouse_log_probs = [], [], []
  rewards, values, dones = [], [], []
  for step in range(config["steps_per_batch"]):
    print(f"Step: {step}")
    obs = Tensor(env.get_observation()).unsqueeze(dim=0).div(255).mul(2).sub(1).permute(0, 3, 1, 2) # Normalize the image
    m_st = time.perf_counter()
    model_output = agent(obs)
    m_et = time.perf_counter()
    print(f"Time for model: {(m_et - m_st):.4f}")
    
    action = NetworkOutput(
        action_type=model_output.action_type.item(),
        keyboard_key=model_output.keyboard_key.item(),
        mouse=model_output.mouse_action.squeeze().tolist()
    )

    e_st = time.perf_counter()
    reward, done = env.step(action)
    e_et = time.perf_counter()
    print(f"Time for env step: {(e_et - e_st):.4f}\n")

    observations.append(obs)
    actions.append(action)
    action_type_log_probs.append(model_output.action_type_log_prob)
    keyboard_log_probs.append(model_output.keyboard_key_log_prob)
    mouse_log_probs.append(model_output.mouse_log_prob)
    rewards.append(reward)
    values.append(model_output.value)
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
      returns, advantages = calculate_returns_and_advantages(rewards, values, config["gamma"])
      raise Exception("returns and advantages")

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

  except Exception as e:
    print(f"Error: {e}")
    print("\nExiting loop. Stopping environment.")
  finally:
    env.stop()

config = {
    "num_iterations": 1000,
    "ppo_epochs": 5,
    "steps_per_batch": 2048,
    "gamma": 0.99,
    "epsilon": 0.2,
    "critic_loss_weight": 0.5,
    "entropy_weight": 0.01}


if __name__ == "__main__":
  env = StreamingEnv(task="wikipedia")
  rl_loop(env)

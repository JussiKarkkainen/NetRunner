from rl_server import StreamingEnv, NetworkOutput
from tinygrad import Tensor, nn
from dataclasses import dataclass
import numpy as np

class BrowserAgent:
  def __init__(self, input_shape, hidden_units=256):
    self.conv1 = nn.Conv2d(3, 16, 3, stride=2, padding=1)  # Input: (3, 170, 340) -> (16, 85, 170)
    self.conv2 = nn.Conv2d(16, 32, 3, stride=2, padding=1) # (16, 85, 170) -> (32, 43, 85)
    self.conv3 = nn.Conv2d(32, 64, 3, stride=2, padding=1) # (32, 43, 85) -> (64, 22, 43)
    self.flatten = nn.Linear(64 * 22 * 43, hidden_units) 
    
    self.action_type_fc = nn.Linear(hidden_units, 3)    
    self.keyboard_key_fc = nn.Linear(hidden_units, 78)   
    self.mouse_fc = nn.Linear(hidden_units, 2)       

    self.mouse_log_std = Tensor(np.zeros((1, 2)))
    
    self.value_fc = nn.Linear(hidden_units, 1)        

  def __call__(self, obs: Tensor) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
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

    mouse_log_prob = -0.5 * (Tensor.log(2 * Tensor(np.pi)) + 2 * self.mouse_log_std + (mouse_action - mouse_mean)**2 / mouse_std**2).sum(dim=-1)

    value = self.value_fc(x)
    
    return BrowserActionout(
        action_type=action_type,
        action_type_log_prob=action_type_log_prob,
        keyboard_key=keyboard_key, 
        keyboard_key_log_prob=keyboard_key_log_prob,
        mouse_action=mouse_action, 
        mouse_log_prob=mouse_log_prob, 
        value=value)

@dataclass 
class BrowserAgentOut:
  action_type: Tensor
  action_type_log_prob: Tensor
  keyboard_key: Tensor
  keyboard_key_log_prob: Tensor
  mouse_action: Tensor
  mouse_log_prob: Tensor
  value: Tensor

def calculate_returns_and_advantages(rewards, values, gamma=0.99):
  pass

def update_actor_critic(actor_critic_fn, params, optimizer):
  pass

def collect_experience(env, agent):
  env.reset()
  observations, actions, log_probs, rewards, values, dones = [], [], [], [], [], []
  for step in range(config["steps_per_batch"]):
    observation = Tensor(env.get_observation()).div(255).mul(2).sub(1).permute(0, 3, 1, 2) # Normalize the image
    model_output = agent(obs)
    
    output = NetworkOutput(
        action_type=model_output.action_type,
        keyboard=model_output.keyboard
        mouse=model_output.mouse_action
    )

    reward, done = env.step(output)

    total_log_prob = model_output.action_type_log_prob + model_output.keyboard_key_log_prob + \
                     model_output.mouse_log_prob

		observations.append(obs)
    actions.append(action)
    log_probs.append(total_log_prob)
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
    "steps_per_batch": 2048,
    "gamma": 0.99
    "epsilon": 0.2,
    "critic_loss_weight": 0.5,
    "entropy_weight": 0.01}


if __name__ == "__main__":
  env = StreamingEnv(task="wikipedia")
  rl_loop(env)

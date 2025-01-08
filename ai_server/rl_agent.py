from rl_server import StreamingEnv, NetworkOutput
from tinygrad import Tensor, nn
from dataclasses import dataclass


def conv3x3(x, weight, bias):
	return x.conv2d(weight, bias, stride=1, padding=1)

def group_norm(x, weight, bias, num_groups=32):
	x = x.reshape(x.shape[0], num_groups, -1).layernorm(eps=1e-5).reshape(x.shape)
	return x * weight.reshape(1, -1, *[1] * (len(x.shape)-2)) + bias.reshape(1, -1, *[1] * (len(x.shape)-2))

def res_block(x, in_channels, out_channels, norm_weight, norm_bias, conv_weight, 
              conv_bias, skip_weight=None, skip_bias=None):
	num_groups = max(1, in_channels // 32) # TODO: Remove magic values
	x_norm = group_norm(x, norm_weight, norm_bias, num_groups).silu()
	x_conv = conv3x3(x_norm, conv_weight, conv_bias)
	x_skip = x if in_channels == out_channels else x.conv2d(skip_weight, skip_bias)
	return x_skip + x_conv

def lstm(x, hc, weight_ih, weight_hh, bias_ih, bias_hh):
	gates = (x.matmul(weight_ih.T) + bias_ih) + (hc[0].matmul(weight_hh.T) + bias_hh)
	i, f, g, o = gates.chunk(4, dim=1)
	i, f, g, o = i.sigmoid(), f.sigmoid(), g.tanh(), o.sigmoid()
	new_c = f * hc[1] + i * g
	new_h = o * new_c.tanh()
	return (new_h.contiguous(), new_c.contiguous())

def init_lstm():
	return [Tensor.zeros(1, 512), Tensor.zeros(1, 512)]

def actor_critic_head(x, critic_w, actor_w, critic_b, actor_b):
	return x.matmul(actor_w.T) + actor_b, x.matmul(critic_w.T) + critic_b

def actor_critic_encoder(x, conv_weights, conv_biases, norm_weights, norm_biases, 
                         skip_weight, skip_bias):
	x = conv3x3(x, conv_weights[0], conv_biases[0])
	channels = [32, 32, 64, 64]
	for i in range(len(channels)):
		in_channels = channels[max(0, i - 1)]
		out_channels = channels[i]
		x = res_block(x, in_channels, out_channels, norm_weights[i], norm_biases[i],
									conv_weights[i+1], conv_biases[i+1], skip_weight, skip_bias)
		x = x.max_pool2d()
	return x

def actor_critic(x, hc, params: ActorCriticParameters):
  x = actor_critic_encoder(x, params.conv_weights, params.conv_biases, 
                           params.norm_weights, params.norm_biases,
                           params.skip_weight, params.skip_bias)
  x = x.flatten(start_dim=1)
  h, c = lstm(x, hc, *params.lstm_weights, *params.lstm_biases)
  action_logits, value = actor_critic_head(h, *params.head_weights, 
                                           *params.head_biases)
  return action_logits.softmax(axis=1).multinomial(), value, (h, c)

@dataclass 
class ActorCriticParameters:
	conv_weights = [v for k, v in state_dict.items() if k.startswith("actor_critic.encoder") \
									and not "norm" in k and not "skip_projection" in k and "weight" in k]
	conv_biases = [v for k, v in state_dict.items() if k.startswith("actor_critic.encoder") \
									and not "norm" in k and not "skip_projection" in k and "bias" in k]
	norm_weights = [v for k, v in state_dict.items() if k.startswith("actor_critic.encoder") \
									and "norm" in k and "weight" in k]
	norm_biases = [v for k, v in state_dict.items() if k.startswith("actor_critic.encoder") \
									and "norm" in k and "bias" in k]
	skip_weight = state_dict["actor_critic.encoder.encoder.5.skip_projection.weight"]
	skip_bias = state_dict["actor_critic.encoder.encoder.5.skip_projection.bias"]
	lstm_weights = [state_dict["actor_critic.lstm.weight_ih"], state_dict["actor_critic.lstm.weight_hh"]]
	lstm_biases = [state_dict["actor_critic.lstm.bias_ih"], state_dict["actor_critic.lstm.bias_hh"]]
	head_weights = [state_dict["actor_critic.critic_linear.weight"], state_dict["actor_critic.actor_linear.weight"]]
	head_biases = [state_dict["actor_critic.critic_linear.bias"], state_dict["actor_critic.actor_linear.bias"]]

def update_actor_critic(actor_critic_fn, params, optimizer):
  pass

def rl_loop(env):
	env.start()
	print("Environment started. Press Ctrl+C to exit.")

	try:
    # .unsqueeze(dim=0).div(255).mul(2).sub(1).permute(0, 3, 1, 2)
		observation = Tensor(env.get_observation())

		for epoch in range(config["num_epochs"]):
			network_output = simulate_network_output()
			
			print("Network output:")
			print(f"Action type probs: {network_output.action_type.tolist()}")
			print(f"Selected action: {network_output.action_type.numpy().argmax()}")
			
			env.step(network_output)
			print("Action taken")
			raise Exception
			
			observation = env.get_observation()
			time.sleep(10)

	except KeyboardInterrupt:
		print("\nExiting loop. Stopping environment.")
	finally:
		env.stop()

config = {"num_epochs": 1000}

if __name__ == "__main__":
  env = StreamingEnv()
  rl_loop(env)

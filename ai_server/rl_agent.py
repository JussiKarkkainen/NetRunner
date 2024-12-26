from rl_server import StreamingEnv, NetworkOutput
from tinygrad import Tensor, nn







def rl_loop(env):
	env.start()
	print("Environment started. Press Ctrl+C to exit.")

	try:
		observation = Tensor(env.get_observation())

		while True:
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

if __name__ == "__main__":
  env = StreamingEnv()
  rl_loop(env)

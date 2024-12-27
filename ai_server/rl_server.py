import asyncio
import threading
import time
import queue
import numpy as np
from PIL import Image
import io
import websockets
import json
from dataclasses import dataclass
import dataclasses
from enum import Enum
from typing import List, Set, Optional, Tuple
from tinygrad import Tensor

class ActionType(Enum):
  NO_OP = 0
  KEYBOARD = 1
  MOUSE = 2

@dataclass
class NetworkOutput:
  action_type: Tensor   # shape: (3,) softmax probabilities
  keyboard_key: Tensor  # shape (78,) softmax probabilities 
  mouse: Tensor         # shape (2,) sigmoid coordinates in [0, 1]

@dataclass
class BrowserAction:
  action_type: int
  keyboard_key: str = ""
  mouse_x: int = 0
  mouse_y: int = 0

class BrowserActionSpace:
  def __init__(self, screen_width: int, screen_height: int):
    # Basic keyboard mapping (77 actions: 76 keys + 1 no-op)
    self.keyboard_map = {
      0: "",  # no-op
      # Letters (lowercase)
      1: "a", 2: "b", 3: "c", 4: "d", 5: "e", 6: "f", 7: "g", 8: "h",
      9: "i", 10: "j", 11: "k", 12: "l", 13: "m", 14: "n", 15: "o", 16: "p",
      17: "q", 18: "r", 19: "s", 20: "t", 21: "u", 22: "v", 23: "w", 24: "x",
      25: "y", 26: "z",
      # Numbers
      27: "0", 28: "1", 29: "2", 30: "3", 31: "4", 32: "5", 33: "6", 34: "7",
      35: "8", 36: "9",
      # Punctuation
      37: ".", 38: ",", 39: ":", 40: ";", 41: "'", 42: '"', 43: "!", 44: "?",
      45: "`", 46: "´",
      # Operators
      47: "+", 48: "-", 49: "*", 50: "/", 51: "^", 52: "=", 53: "%",
      # Brackets
      54: "(", 55: ")", 56: "[", 57: "]", 58: "{", 59: "}", 60: "<", 61: ">",
      # Special chars
      62: "_", 63: "-", 64: "\\", 65: "~", 66: "@", 67: "#", 68: "$",
      69: "SPACE",  # Space
      70: "ENTER",  # Enter
      71: "BACKSPACE",  # Backspace
      72: "TAB",  # Tab
      73: "ESCAPE",  # Escape
      # Arrow keys
      74: "LEFT",  # Left
      75: "RIGHT",  # Right
      76: "UP",  # Up
      77: "DOWN",  # Up
    }
    
    self.screen_width = screen_width
    self.screen_height = screen_height

		
  def network_output_to_action(self, network_output: NetworkOutput) -> BrowserAction:
    action_type = network_output.action_type.numpy().argmax()
    
    if action_type == ActionType.NO_OP.value:
      return BrowserAction(action_type=ActionType.NO_OP.value)
    
    elif action_type == ActionType.KEYBOARD.value:
      key_idx = network_output.keyboard_key.numpy().argmax()
      return BrowserAction(
          action_type=ActionType.KEYBOARD.value,
          keyboard_key=self.keyboard_map[key_idx]
      )
    
    elif action_type == ActionType.MOUSE.value:
      mouse_coords = network_output.mouse.numpy()
      return BrowserAction(
          action_type=ActionType.MOUSE.value,
          mouse_x=int(mouse_coords[0] * self.screen_width),
          mouse_y=int(mouse_coords[1] * self.screen_height)
      )
    
    else:
      raise ValueError(f"Invalid action type: {action_type}")

class StreamingEnv:
  def __init__(self, task, obs_uri="ws://localhost:8765", action_uri="ws://localhost:8766", control_uri="ws://localhost:8764", 
               obs_shape=(340, 170)):
    self.obs_uri = obs_uri
    self.obs_shape = obs_shape
    self.action_uri = action_uri
    self.control_uri = control_uri
    self.observation_queue = queue.Queue(maxsize=1) 
    self.running = False
    self.action_space = BrowserActionSpace(screen_width=1920, screen_height=995)

    self.task = task
    self.task_list = ["wikipedia"]
    assert self.task in self.task_list, f"No such task as: {self.task}. Available tasks: {self.task_list}"
    asyncio.run(self._send_task())

  async def _send_task(self):
    try:
      async with websockets.connect(self.control_uri) as websocket:
        await websocket.send(json.dumps({"envName": self.task}))
        response = await websocket.recv()
        print(f"Response from control server: {response}")
    except Exception as e:
      print(f"Error during WebSocket communication with control server: {e}")
 
  def _background_listener(self):
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    loop.run_until_complete(self.receive_image(self.obs_uri))
    loop.close()

  async def receive_image(self, uri):
    while self.running:
      try:
        async with websockets.connect(uri, ping_interval=30, ping_timeout=10) as websocket:
          while self.running:
            image_data = await websocket.recv()
            image = Image.open(io.BytesIO(image_data)).convert("RGB")
            print(np.array(image).shape)
            image = image.resize(self.obs_shape, Image.LANCZOS)
            img_array = np.array(image)

            if not self.observation_queue.full():
              self.observation_queue.put(img_array)
      except websockets.exceptions.ConnectionClosedError as e:
        print(f"Connection closed: {e}. Reconnecting...")
        await asyncio.sleep(1)
      except Exception as e:
        print(f"Error in WebSocket listener: {e}")
        time.sleep(5)

    asyncio.run(receive_image(self.obs_uri))

  async def _action_sender(self, action):
    try:
      async with websockets.connect(self.action_uri) as websocket:
        await websocket.send(json.dumps(dataclasses.asdict(action)))
        response = await websocket.recv()
        response_data = json.loads(response)
        return response_data.get("reward", 0), response_data.get("done", False)
    except Exception as e:
      print(f"Error during WebSocket communication with action server: {e}")

  async def send_action(self, action):
    return await self._action_sender(action)

  def start(self):
    self.running = True
    self.listener_thread = threading.Thread(target=self._background_listener, daemon=True)
    self.listener_thread.start()

  def stop(self):
    self.running = False
    if hasattr(self, 'action_websocket') and self.action_websocket:
      asyncio.run(self.action_websocket.close())
    if self.listener_thread.is_alive():
        self.listener_thread.join()

  def get_observation(self):
    return self.observation_queue.get()

  def step(self, network_output: NetworkOutput):
    action = self.action_space.network_output_to_action(network_output)
    reward, done = asyncio.run(self.send_action(action))
    return reward, done

  def reset(self):
    with self.observation_queue.mutex:
      self.observation_queue.queue.clear()
    return self.get_observation()

def simulate_network_output() -> NetworkOutput:
  # softmax output for action type (NO_OP, KEYBOARD, MOUSE)
  action_logits = Tensor([0.1, 0.2, 0.7]) 
  
  # softmax output for keyboard (77 possibilities)
  keyboard_logits = Tensor.randn(77)
  
  # sigmoid output for mouse coordinates
  mouse_coords = Tensor([0.5, 0.4]) 
  
  return NetworkOutput(
      action_type=action_logits,
      keyboard=keyboard_logits,
      mouse=mouse_coords
  )

def rl_loop_with_human(env):
	env.start()
	print("Environment started. Press Ctrl+C to exit.")

	try:
		observation = Tensor(env.get_observation())

		while True:
			network_output = simulate_network_output()
			
			print("Network output:")
			print(f"Action type probs: {network_output.action_type.tolist()}")
			print(f"Selected action: {network_output.action_type.numpy().argmax()}")
			
			reward, done = env.step(network_output)
			print("Action taken")
			raise Exception
			
			observation = env.get_observation()
			time.sleep(10)

	except KeyboardInterrupt:
		print("\nExiting loop. Stopping environment.")
	finally:
		env.stop()

if __name__ == "__main__":
  env = StreamingEnv("wikipedia")
  rl_loop_with_human(env)


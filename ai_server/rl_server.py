import asyncio
import threading
import queue
import numpy as np
from PIL import Image
import io
import websockets
from dataclasses import dataclass
from typing import List
from tinygrad import Tensor

class StreamingEnv:
  def __init__(self, obs_uri="ws://localhost:8765", action_uri="ws://localhost:8766", obs_shape=(256, 256, 3)):
    self.obs_uri = obs_uri
    self.obs_shape = obs_shape
    self.action_uri = action_uri
    self.observation_queue = queue.Queue(maxsize=1) 
    self.running = False

  def _background_listener(self):
    async def receive_image(uri):
      async with websockets.connect(uri) as websocket:
        while self.running:
          try:
            image_data = await websocket.recv()
            image = Image.open(io.BytesIO(image_data))
            img_array = np.array(image)

            if not self.observation_queue.full():
              self.observation_queue.put(img_array)
          except Exception as e:
            print(f"Error in WebSocket listener: {e}")

    asyncio.run(receive_image(self.obs_uri))

	async def _action_sender(self, action):
    async with websockets.connect(self.action_uri) as websocket:
      await websocket.send(json.dumps(action))
      response = await websocket.recv()
      print(f"Action server response: {response}")

  def send_action(self, action):
    asyncio.run(self._action_sender(action))

  def start(self):
    self.running = True
    self.listener_thread = threading.Thread(target=self._background_listener, daemon=True)
    self.listener_thread.start()

  def stop(self):
    self.running = False
    if self.listener_thread.is_alive():
        self.listener_thread.join()

  def get_observation(self):
    return self.observation_queue.get()

  def step(self, action):
    if self.verify_action(action):
      self.send_action(action)
    else:
      raise Exception("Invalid action given: {action}")

  def reset(self):
    with self.observation_queue.mutex:
      self.observation_queue.queue.clear()
    return self.get_observation()

@dataclass 
class MouseAction:
  action_type: int
  x: float
  y: float
  modifiers: List[int]

@dataclass
class KeyboardAction:
  action_type: int
  key: int
  modifiers: List[int]

@dataclass 
class BrowerEnvActionSpace: 
  mouseAction: MouseAction
  keyboardAction: KeyboardAction

def human_action_input():
  print("\nMouseAction:")
  mouse_action_type = int(input("Mouse Action Type (0: None, 1: Click): "))
  mouse_x = float(input("Mouse X (normalized 0.0 to 1.0): "))
  mouse_y = float(input("Mouse Y (normalized 0.0 to 1.0): "))
  mouse_modifiers = [int(x) for x in input("Mouse Modifiers (0/1 for Shift, Ctrl, Alt, separated by space): ").split()]

  mouse_action = MouseAction(
    action_type=mouse_action_type,
    x=mouse_x,
    y=mouse_y,
    modifiers=mouse_modifiers
  )

  print("\nKeyboardAction:")
  keyboard_action_type = int(input("Keyboard Action Type (0: None, 1: Press): "))
  keyboard_key = int(input("Keyboard Key Index (use predefined mapping): "))
  keyboard_modifiers = [int(x) for x in input("Keyboard Modifiers (0/1 for Shift, Ctrl, Alt, separated by space): ").split()]

  keyboard_action = KeyboardAction(
    action_type=keyboard_action_type,
    key=keyboard_key,
    modifiers=keyboard_modifiers
  )

  return BrowerEnvActionSpace(
    mouseAction=mouse_action,
    keyboardAction=keyboard_action
  )

def display_observation(observation):
  img = Image.fromarray(observation)
  img.show()

def rl_loop_with_human(env):
  env.start()  
  print("Environment started. Press Ctrl+C to exit.")

  try:
    observation = Tensor(env.get_observation())

    while True:
      print("\nTake an action:")
      action = human_action_input()

      print(observation.shape)
      raise Exception
      env.step(action)

      observation = env.get_observation()
      display_observation(observation)

      time.sleep(10)

  except KeyboardInterrupt:
    print("\nExiting loop. Stopping environment.")
  finally:
    env.stop()

if __name__ == "__main__":
  env = StreamingEnv()
  rl_loop_with_human(env)


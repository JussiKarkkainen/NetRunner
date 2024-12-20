import asyncio
import threading
import queue
import numpy as np
from PIL import Image
import io
import websockets
import json
from dataclasses import dataclass
import dataclasses
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
      await websocket.send(json.dumps(dataclasses.asdict(action)))
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

  def verify_action(self, action):
    return True

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
  mActionType: int
  x: float
  y: float

@dataclass
class KeyboardAction:
  kActionType: int
  key: int
  kModifiers: List[int]

@dataclass 
class BrowerEnvActionSpace: 
  mouseAction: MouseAction
  keyboardAction: KeyboardAction

def human_action_input():
  mouse_action_type = 1
  mouse_x = 0.5
  mouse_y = 0.4

  mouse_action = MouseAction(
    mActionType=mouse_action_type,
    x=mouse_x,
    y=mouse_y,
  )

  keyboard_action_type = 1
  keyboard_key = 10
  keyboard_modifiers = [0, 0, 0]

  keyboard_action = KeyboardAction(
    kActionType=keyboard_action_type,
    key=keyboard_key,
    kModifiers=keyboard_modifiers
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
      action = human_action_input()
      print(observation.shape)
      env.step(action)
      print("Action taken")
      raise Exception
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


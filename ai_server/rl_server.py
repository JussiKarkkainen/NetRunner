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
from typing import List, Set
from tinygrad import Tensor

class ActionType(Enum):
  NO_OP = 0
  KEYBOARD = 1
  MOUSE = 2

class SimpleActionSpace:
  def __init__(self, screen_width: int, screen_height: int):
    # Basic keyboard mapping (77 actions: 76 keys + 1 no-op)
    self.keyboard_map = {
      0: None,  # no-op
      1: 'a', 2: 'b', 3: 'c', 4: 'd', 5: 'e', 6: 'f', 7: 'g', 8: 'h',
      9: 'i', 10: 'j', 11: 'k', 12: 'l', 13: 'm', 14: 'n', 15: 'o', 16: 'p',
      17: 'q', 18: 'r', 19: 's', 20: 't', 21: 'u', 22: 'v', 23: 'w', 24: 'x',
      25: 'y', 26: 'z',
      # Numbers
      27: '0', 28: '1', 29: '2', 30: '3', 31: '4', 32: '5', 33: '6', 34: '7',
      35: '8', 36: '9',
      # Punctuation
      37: '.', 38: ',', 39: ':', 40: ';', 41: "'", 42: '"', 43: '!', 44: '?',
      45: '`', 46: '´',
      # Operators
      47: '+', 48: '-', 49: '*', 50: '/', 51: '^', 52: '=', 53: '%',
      # Brackets
      54: '(', 55: ')', 56: '[', 57: ']', 58: '{', 59: '}', 60: '<', 61: '>',
      # Special chars
      62: '_', 63: '-', 64: '\\', 65: '~', 66: '@', 67: '#', 68: '$',
      # Control keys
      69: 'SPACE', 70: 'ENTER', 71: 'BACKSPACE', 72: 'TAB', 73: 'ESCAPE',
      # Arrow keys
      74: 'LEFT', 75: 'RIGHT', 76: 'UP'
    }
    
    self.screen_width = screen_width
    self.screen_height = screen_height

  def decode_action(self, 
                   action_type: int, 
                   action_value: int,
                   mouse_x: Optional[int] = None,
                   mouse_y: Optional[int] = None) -> Tuple[ActionType, Optional[str], Optional[Tuple[int, int]]]:
    """
    Neural network will output:
    - action_type: 0 (no-op), 1 (keyboard), 2 (mouse)
    - For keyboard: action_value is the key index (0-76)
    - For mouse: mouse_x and mouse_y are the coordinates
    """
    if action_type == ActionType.NO_OP.value:
      return ActionType.NO_OP, None, None
    
    elif action_type == ActionType.KEYBOARD.value:
      key = self.keyboard_map.get(action_value)
      return ActionType.KEYBOARD, key, None
        
    elif action_type == ActionType.MOUSE.value:
      if mouse_x is None or mouse_y is None:
        raise ValueError("Mouse coordinates required for mouse action")
      # Ensure coordinates are within screen bounds
      x = max(0, min(mouse_x, self.screen_width))
      y = max(0, min(mouse_y, self.screen_height))
      return ActionType.MOUSE, None, (x, y)
        
    else:
      raise ValueError(f"Invalid action type: {action_type}")

class StreamingEnv:
  def __init__(self, obs_uri="ws://localhost:8765", action_uri="ws://localhost:8766", obs_shape=(170, 340, 3)):
    self.obs_uri = obs_uri
    self.obs_shape = obs_shape
    self.action_uri = action_uri
    self.observation_queue = queue.Queue(maxsize=1) 
    self.running = False
		self.action_space = KeyboardActionSpace()

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
  img = img.resize((340, 170), Image.LANCZOS)
  img.show()

def rl_loop_with_human(env):
  env.start()  
  print("Environment started. Press Ctrl+C to exit.")

  try:
    observation = Tensor(env.get_observation())

    while True:
      action = human_action_input()
      print(observation.shape)
      display_observation(observation.numpy())
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


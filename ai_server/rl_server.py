import asyncio
import threading
import queue
import numpy as np
from PIL import Image
import io
import websockets

class StreamingEnv:
    def __init__(self, uri, obs_shape=(256, 256, 3)):
      self.uri = uri
      self.obs_shape = obs_shape
      self.observation_queue = queue.Queue(maxsize=100) 
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

      asyncio.run(receive_image(self.uri))

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

    def reset(self):
      with self.observation_queue.mutex:
        self.observation_queue.queue.clear()
      return self.get_observation()


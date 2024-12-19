import asyncio
import websockets
import numpy as np
from PIL import Image
import io

async def handle_client(websocket):
  print("Client connected")
  try:
    while True:
      image_data = await websocket.recv()

      image = Image.open(io.BytesIO(image_data))
      image.show()

      img_array = np.array(image)
      print(f"Received image with shape: {img_array.shape}")

  except websockets.exceptions.ConnectionClosed as e:
    print(f"Connection closed: {e}")
  except Exception as e:
    print(f"Error: {e}")
  finally:
    print("Closing connection")
    await websocket.close()

async def start_server():
  server = await websockets.serve(handle_client, "localhost", 8765)
  print("WebSocket server running on ws://localhost:8765")
  await server.wait_closed()

asyncio.get_event_loop().run_until_complete(start_server())


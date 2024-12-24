import tkinter as tk
from tkinter import ttk
import numpy as np
from PIL import Image, ImageTk
import threading
import queue
from rl_server import StreamingEnv, NetworkOutput
from dataclasses import dataclass
from typing import Optional
from tinygrad import Tensor
import ast

class HumanInterface:
  def __init__(self, env):
    self.env = env
    self.root = tk.Tk()
    self.root.title("RL Environment Human Interface")
    
    self.main_frame = ttk.Frame(self.root, padding="10")
    self.main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
    
    self.canvas = tk.Canvas(self.main_frame, width=340, height=170)
    self.canvas.grid(row=0, column=0, columnspan=2, pady=5)
    
    self.action_type_var = tk.StringVar(value="NO_OP")
    action_frame = ttk.LabelFrame(self.main_frame, text="Action Type", padding="5")
    action_frame.grid(row=1, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=5)
    
    ttk.Radiobutton(action_frame, text="No Operation", variable=self.action_type_var, 
                   value="NO_OP").grid(row=0, column=0, padx=5)
    ttk.Radiobutton(action_frame, text="Keyboard", variable=self.action_type_var, 
                   value="KEYBOARD").grid(row=0, column=1, padx=5)
    ttk.Radiobutton(action_frame, text="Mouse", variable=self.action_type_var, 
                   value="MOUSE").grid(row=0, column=2, padx=5)
    
    keyboard_frame = ttk.LabelFrame(self.main_frame, text="Keyboard Input", padding="5")
    keyboard_frame.grid(row=2, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=5)
    
    self.key_entry = ttk.Entry(keyboard_frame, width=10)
    self.key_entry.grid(row=0, column=0, padx=5)
    
    mouse_frame = ttk.LabelFrame(self.main_frame, text="Mouse Coordinates", padding="5")
    mouse_frame.grid(row=3, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=5)
    
    ttk.Label(mouse_frame, text="X:").grid(row=0, column=0, padx=5)
    self.mouse_x = ttk.Entry(mouse_frame, width=10)
    self.mouse_x.grid(row=0, column=1, padx=5)
    self.mouse_x.insert(0, "0")
    
    ttk.Label(mouse_frame, text="Y:").grid(row=0, column=2, padx=5)
    self.mouse_y = ttk.Entry(mouse_frame, width=10)
    self.mouse_y.grid(row=0, column=3, padx=5)
    self.mouse_y.insert(0, "0")
    
    self.send_button = ttk.Button(self.main_frame, text="Send Action", command=self.send_action)
    self.send_button.grid(row=4, column=0, columnspan=2, pady=10)
    
    self.image_queue = queue.Queue(maxsize=1)
    self.running = True
    
    self.obs_thread = threading.Thread(target=self._update_observation, daemon=True)
    self.obs_thread.start()
    
    self.root.after(50, self._update_display)

  def _update_observation(self):
    while self.running:
      try:
        obs = self.env.get_observation()
        if not self.image_queue.full():
          self.image_queue.put(obs)
      except Exception as e:
        print(f"Error getting observation: {e}")

  def _update_display(self):
    try:
      if not self.image_queue.empty():
        obs = self.image_queue.get_nowait()
        image = Image.fromarray(obs)
        photo = ImageTk.PhotoImage(image)
        self.canvas.create_image(0, 0, anchor=tk.NW, image=photo)
        self.canvas.image = photo  # Keep a reference to prevent garbage collection
    except Exception as e:
      print(f"Error updating display: {e}")
        
    self.root.after(50, self._update_display) 

  def send_action(self):
    action_type = self.action_type_var.get()
    
    action_probs = np.zeros(3)
    if action_type == "NO_OP":
      action_probs[0] = 1.0
    elif action_type == "KEYBOARD":
      action_probs[1] = 1.0
    else:  # MOUSE
      action_probs[2] = 1.0
        
    keyboard_logits = np.zeros(78)
    if action_type == "KEYBOARD":
      key = self.key_entry.get()
      key = key.encode().decode("unicode_escape")
      for idx, mapped_key in self.env.action_space.keyboard_map.items():
        if mapped_key == key:
          keyboard_logits[idx] = 1.0
          break
    
    try:
      mouse_x = float(self.mouse_x.get())
      mouse_y = float(self.mouse_y.get())
    except ValueError:
      mouse_x, mouse_y = 0.0, 0.0
        
    network_output = NetworkOutput(
      action_type=Tensor(action_probs),
      keyboard_key=Tensor(keyboard_logits),
      mouse=Tensor([mouse_x, mouse_y])
    )
    
    self.env.step(network_output)

  def run(self):
    self.root.mainloop()
    self.running = False

def main():
  env = StreamingEnv()
  env.start()
  
  try:
    interface = HumanInterface(env)
    interface.run()
  finally:
    env.stop()

if __name__ == "__main__":
  main()

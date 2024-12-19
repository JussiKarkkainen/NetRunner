from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import threading
from openai import OpenAI

DEFAULT_SYSTEM_PROMPT = """
You are an LLM integrated into an Agentic Application designed for conducting long-form research reports on various topics. 
The application operates iteratively, providing the following information at each inference step:

1. **Original Task Prompt**  
  - A detailed description of the task you need to accomplish.

2. **Task Creation Time**  
  - The timestamp when the task was created.

3. **Current Time**  
  - The current timestamp.

4. **Iteration Number**  
  - The number of iterations (inference passes) completed so far for this task.

5. **Previous Outputs**  
  - Your prior responses and tool use commands from all the previous iterations.

6. **Tool Output**
  - The output of the tool use command from the previous iteration.

### **Your Role**
Your role is to:
- Advance the task by building on the context and results from previous iterations.
- Manage your context efficiently by summarizing or distilling prior information when necessary.
- Provide instructions for yourself or refine the task strategy to maintain focus and clarity over multiple iterations.

### **Available Tools**
You can access external information through a JSON-based interface using the following tools. You don't need to
use tools on every iteration. Instead you can focus on summarising and planning for following iterations.

1. **Search**  
To perform a Google search, use this format:  
{"action": "search", "arg": "<INSERT QUERY HERE>"}

2. **Browser**
To retrieve a specific webpage, use this format:
{"action": "browse", "arg": "<INSERT URL HERE>"}

Once you think you have completed the task use the command:
{"action": "finish", "arg": "completed"}
This will end the task.

### Key Guidelines
1. Context Management:
  - As your context length is limited, prioritize concise, relevant responses. Summarize prior iterations as needed to maintain critical information within the available context window.
2. Tool Use:
  - Ensure tool commands are unambiguous and align with the task's requirements.
  - Make sure the commands are formatted exactly like the examples. Do not include
    any other JSON in your response. Use Markdown to format your outputs.
  - Validate outputs from tools before integrating them into your responses.
3. Self-Instructions:
  - Use each iteration to refine your approach or provide future guidance if necessary to complete the task efficiently.
4. Iterative Process:
  - Each inference pass should meaningfully advance the task, considering the inputs, outputs, and constraints provided.
  - Be precise, focused, and goal-oriented in all responses.
"""

class LLMServer(BaseHTTPRequestHandler):
  def do_POST(self):
    if self.path == "/model":
      content_length = int(self.headers['Content-Length'])
      post_data = self.rfile.read(content_length)
      try:
        data = json.loads(post_data.decode('utf-8'))
      except json.JSONDecodeError:
        self.send_error(400, "Invalid JSON")
        return
      
      try:
        user_prompt = self.format_user_prompt(data)
        messages = [
            {"role": "system", "content": DEFAULT_SYSTEM_PROMPT},
            {"role": "user", "content": user_prompt},
        ]
      except KeyError as e:
        self.send_error(400, f"Missing required field: {e}")
        return
      
      output_string = self.generate(messages)
      response = json.dumps({"output": output_string})
      
      self.send_response(200)
      self.send_header('Content-type', 'application/json')
      self.end_headers()
      self.wfile.write(response.encode('utf-8'))

  def format_user_prompt(self, data):
    og_user_prompt = data['ogUserPrompt']
    task_creation_time = data['taskCreationTime']
    cur_time = data['curTime']
    iteration = data['iteration']
    prev_outputs = data.get('prevOutput', [])

    formatted_prev_outputs = "\n".join(
      (f"Iteration {i}:\nYour Previous Response: {output['serverOutput']['formatOutText']}\nPreviously Used Commands: {output['serverOutput']['formatCommands']}\n"
       f"Previous tool use output: {output['toolResults']}\n")
      for i, output in enumerate(reversed(prev_outputs))
    )

    user_prompt = (
      f"Original Task Prompt: {og_user_prompt}\n"
      f"Task Creation Time: {task_creation_time}\n"
      f"Current Time: {cur_time}\n"
      f"Iteration Number: {iteration}\n"
      f"Previous Outputs:\n{formatted_prev_outputs}"
    )
    print(user_prompt)
    print("\n"*20)
    return user_prompt

  def generate(self, messages):
    import openai  
    client = OpenAI()
    completion = client.chat.completions.create(
        model="gpt-4o",
        messages=messages
    )
    return completion.choices[0].message.content

def run(server_class=HTTPServer, handler_class=LLMServer, port=5050):
  server_address = ('', port)
  httpd = server_class(server_address, handler_class)
  print(f"Starting server on port {port}")
  httpd.serve_forever()

if __name__ == "__main__":
  run()

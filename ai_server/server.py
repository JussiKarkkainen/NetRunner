from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import threading

'''
Example of input
  {
    'curTime': '2024-12-11T17:02:27.877975139Z', 
    'iteration': 0, 
    'ogUserPrompt': 'Gather todays most important news', 
    'prevInput': None, 
    'taskCreationTime': '2024-12-10T15:18:57.137769742Z'
  }
'''

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

5. **Previous Inputs and Outputs**  
  - Your prior responses and the results of any tool commands used.

### **Your Role**
Your role is to:
- Advance the task by building on the context and results from previous iterations.
- Manage your context efficiently by summarizing or distilling prior information when necessary.
- Provide instructions for yourself or refine the task strategy to maintain focus and clarity over multiple iterations.

### **Available Tools**
You can access external information through a JSON-based interface using the following tools:

1. **Search**  
To perform a Google search, use this format:  
{"action": "search", "query": "<INSERT QUERY HERE>"}

2. **Browser**
To retrieve a specific webpage, use this format:
{"action": "browse", "url": "<INSERT URL HERE>"}

### Key Guidelines
1. Context Management:
  - As your context length is limited, prioritize concise, relevant responses. Summarize prior iterations as needed to maintain critical information within the available context window.
2. Tool Use:
  - Ensure tool commands are unambiguous and align with the task's requirements.
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
      data = json.loads(post_data.decode('latin-1'))
      parsed_input = self.parse_input_data(data)
      output_string = self.generate(DEFAULT_SYSTEM_PROMPT, parsed_input)
      response = json.dumps({"input": data, "output": output_string})
      self.send_response(200)
      self.send_header('Content-type', 'application/json')
      self.end_headers()
      self.wfile.write(response.encode('latin-1'))

  def generate(self, system_prompt: str, prompt: str) -> str:
    response = f"##############################\n\n\n\n\n\nTHIS IS THE DEFAULT TEST RESPONSE FOR THE PROMPT:\n {prompt}\n\n\n\n\n\n##############################\n"
    return response

  def parse_input_data(self, data):
    prompt = data.get("ogUserprompt")


def run(server_class=HTTPServer, handler_class=LLMServer, port=5050):
  server_address = ('', port)
  httpd = server_class(server_address, handler_class)
  print(f"Starting server on port {port}")
  httpd.serve_forever()

if __name__ == "__main__":
  run()


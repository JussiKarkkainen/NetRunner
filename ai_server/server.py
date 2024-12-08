from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import threading

class LLMServer(BaseHTTPRequestHandler):
  def do_POST(self):
    if self.path == "/model":
      content_length = int(self.headers['Content-Length'])
      post_data = self.rfile.read(content_length)
      data = json.loads(post_data.decode('latin-1'))
      input_string = data.get('input_string', '')
      output_string = self.generate(input_string)
      response = json.dumps({"output_string": output_string})
      self.send_response(200)
      self.send_header('Content-type', 'application/json')
      self.end_headers()
      self.wfile.write(response.encode('latin-1'))

  def generate(self, prompt: str) -> str:
    response = f"##############################\n\n\n\n\n\nTHIS IS THE DEFAULT TEST RESPONSE FOR THE PROMPT:\n {prompt}\n\n\n\n\n\n##############################\n"
    return response

def run(server_class=HTTPServer, handler_class=LLMServer, port=5050):
  server_address = ('', port)
  httpd = server_class(server_address, handler_class)
  print(f"Starting server on port {port}")
  httpd.serve_forever()

if __name__ == "__main__":
  run()


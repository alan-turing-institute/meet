import os
import sys
import random
import requests
import urllib.parse
import http.server
import socketserver

### CONFIG

# The rum app is installed on the main Turing directory. However, it doesn't
# have the appropriate set up for auth flow. It probably also does not have
# enough permissions to do all of what we would like to do. However, it might
# be enough to make a start with.

# CLIENT_ID = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"  # rum
# TENANT_ID = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"  # turing Azure directory

# The hut23meet app is installed on the hut23meet directory, which I set up
# myself. It has been set up for auth-flow, and I can grant the app any
# permissions I want, since I have admin privileges on that directory.
# Unfortunately, that directory does not have Outlook, so there are no
# calendars...

CLIENT_ID = "60ae4dcf-bec0-4078-9e35-8141e609cae9"  # hut23meet/meet
TENANT_ID = "2fd2cbb5-743e-4d35-b783-92f98c93380d"  # hut23meet

# Ideally read this in from a config file. For now I just source ~/.secrets
CLIENT_SECRET = os.getenv("HUT23MEET_CLIENT_SECRET")
if not CLIENT_SECRET:
    raise ValueError("HUT23MEET_CLIENT_SECRET env variable not set")

PORT = 8912
STATE = ''.join(random.choice("abcdefghijklmnopqrstuvwxyz1234567890") for _ in range(25))
REDIRECT_URI = f"http://localhost:{PORT}"

# https://learn.microsoft.com/en-us/graph/auth-v2-user?tabs=http
auth_url = f"https://login.microsoftonline.com/{TENANT_ID}/oauth2/v2.0/authorize/?"
auth_url += urllib.parse.urlencode({
    "client_id": CLIENT_ID,
    "response_type": "code",
    "redirect_uri": REDIRECT_URI,
    "response_mode": "query",
    "scope": "user.read",    # calendars.read.shared calendars.readwrite
    "state": STATE,
})


### CUSTOM HTTP SERVER CLASSES

class AuthorisationSuccess(Exception):
    def __init__(self, code):
        self.code = code

class MyRequestHandler(http.server.BaseHTTPRequestHandler):
    def return_text(self, code, body):
        self.send_response(code)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(body)

    # Silence logging
    def log_request(self, code='-', size='-'):
        pass

    def do_GET(self):
        try:
            query = urllib.parse.parse_qs(urllib.parse.urlparse(self.path).query)
            code = query['code'][0]
            state = query['state'][0]
        except Exception as e:
            self.return_text(400, b"Bad request")
            raise e

        if state != STATE:
            self.return_text(400, b"Bad request, state mismatch")
            raise Exception("State mismatch")

        self.return_text(200, b"Code received; you can close this window now.")
        raise AuthorisationSuccess(code)

class MyTCPServer(socketserver.TCPServer):
    # Convenience function to allow us to run the script multiple times in
    # quick succession
    allow_reuse_address = True

    # Must subclass to re-raise AuthorisationSuccess otherwise it gets
    # swallowed and doesn't bubble up
    def _handle_request_noblock(self):
        try:
            request, client_address = self.get_request()
        except OSError:
            return
        if self.verify_request(request, client_address):
            try:
                self.process_request(request, client_address)
            except AuthorisationSuccess as e:
                self.shutdown_request(request)
                raise AuthorisationSuccess(e.code)
            except:
                self.handle_error(request, client_address)
                self.shutdown_request(request)
        else:
            self.shutdown_request(request)

    def __exit__(self, *args):
        self.server_close()


### MAIN

# User logs in to get auth code

try:
    with MyTCPServer(("", PORT), MyRequestHandler) as httpd:
        # Open browser and listen on port for auth code
        os.system(f"open '{auth_url}'")
        httpd.handle_request()
except AuthorisationSuccess as e:
    auth_code = e.code
except Exception as e:
    print("Error:", e, file=sys.stderr)
    sys.exit(1)


token_url = f"https://login.microsoftonline.com/{TENANT_ID}/oauth2/v2.0/token/?"
token_post_body = {
    "client_id": CLIENT_ID,
    "code": auth_code,
    "redirect_uri": REDIRECT_URI,
    "scope": "calendars.read.shared",
    "grant_type": "authorization_code",
    "client_secret": CLIENT_SECRET,
}

# Exchange auth code for token

response = requests.post(token_url, data=token_post_body)
json = response.json()
try:
    token = json['access_token']
except Exception as e:
    print("Error getting token:", json)
    sys.exit(1)

# At this point we can use the token for whatever
me_url = "https://graph.microsoft.com/v1.0/me"
resp = requests.get(me_url, headers={"Authorization": f"Bearer {token}"})
print(resp.json())

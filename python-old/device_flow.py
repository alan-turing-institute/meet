# https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-device-code
import os
import sys
import requests
import time
import itertools

### CONFIG

CLIENT_ID = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"  # rum
TENANT_ID = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"  # turing Azure directory

url = f"https://login.microsoftonline.com/{TENANT_ID}/oauth2/v2.0/devicecode"
data = {
    "client_id": CLIENT_ID,
    "scope": "user.read calendars.read.shared",
}
response_json = requests.post(url, data=data).json()
os.system(f"open {response_json['verification_uri']}")
os.system(f"pbcopy <<< {response_json['user_code']}")
print(response_json['message'], "The code has already been copied to your clipboard.")

def spinner():
    yield from itertools.cycle(["|", "/", "-", "\\"])
spinner = spinner()

# TODO implement a timeout here
polling_message_shown = False
token = None
while True:
    poll_url = f"https://login.microsoftonline.com/{TENANT_ID}/oauth2/v2.0/token"
    poll_data = {
        "grant_type": "urn:ietf:params:oauth:grant-type:device_code",
        "client_id": CLIENT_ID,
        "device_code": response_json['device_code'],
    }
    poll_response = requests.post(poll_url, data=poll_data).json()

    # Aren't match statements just AMAZING?
    match poll_response.get("error"):
        case None:
            if "access_token" in poll_response:
                token = poll_response['access_token']
                print("Authorised!")
                break
            else:
                raise Exception(f"No error raised but also no token provided: {poll_response}")

        case "authorization_pending":
            if polling_message_shown:
                sys.stdout.write("\033[F")  # Clear input line

            print(f"{next(spinner)} Waiting for authorisation...")
            polling_message_shown = True
            time.sleep(0.2)

        case "authorization_declined":
            print("Authorisation declined.")
            sys.exit(1)

        case "expired_token":
            print("Device code expired.")
            sys.exit(1)

        case "bad_verification_code":
            raise Exception("Bad verification code: this is a bug and should not happen.")
        
        case _:
            raise Exception(f"Unknown error: {poll_response}")

# At this point we can use the token for whatever
me_url = "https://graph.microsoft.com/v1.0/me"
resp = requests.get(me_url, headers={"Authorization": f"Bearer {token}"})
print(resp.json())

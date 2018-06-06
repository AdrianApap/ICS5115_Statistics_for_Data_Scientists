# Flight-Data-Collector
A Python3 Script used to collect data on direct flights originating from Malta using the [https://api.skypicker.com/](https://api.skypicker.com/) API.
The raw json files are then uploaded to a Google Drive account.

## Requirements

* requests 2.18.4
* pydrive 1.3.1

```
pip3 install -r requirements.txt
```
## Running
To run the program a Google Developer account is required to generate a client_secrets.json file.

client_secrets.json is required to run the script and must be located in the same directory as the script.

[https://pythonhosted.org/PyDrive/quickstart.html](https://pythonhosted.org/PyDrive/quickstart.html) contains instructions on how to generate a client_secrets.json for a Google account.

The first time the program is executed a web browser opens and asks for permission to access the Google Drive account. As soon as you allow the script access to Google Drive a mycreds.txt file is generated containing an access token.

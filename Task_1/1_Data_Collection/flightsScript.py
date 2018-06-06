#import required packages
import os
import json
import requests
import datetime

from datetime import date
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

#Constants
BASE_URL = 'https://api.skypicker.com/'
OUTPUT_FILE_SUFFIX = '_raw_data.json'

LOCAL_OUTPUT_DIR = 'data/'

#Helper Functions
def sendRequest(params):
    response = requests.get(BASE_URL+params)
    return json.loads(response.text)

#Main Function
def main():
    #Get current script path
    os.chdir(os.path.dirname(os.path.realpath(__file__)))

    #Init google auth
    gauth = GoogleAuth()
    #Load google credentials
    gauth.LoadCredentialsFile("mycreds.txt")
    #If they dont exists launch a webbrowser to authenticate user
    if gauth.credentials is None:
        gauth.LocalWebserverAuth()
    #If they expired refresh them
    elif gauth.access_token_expired:
        gauth.Refresh()
    #Else use the loaded credentials
    else:
        gauth.Authorize()

    #Save the credentials file for future use
    gauth.SaveCredentialsFile("mycreds.txt")

    #Init GoogleDrive with the generated authentication
    drive = GoogleDrive(gauth)

    #Get the current date and time
    now = datetime.datetime.now()
    timestamp = now.strftime('%Y-%m-%d-%H-%M')

    #Append date to file name
    file_name = timestamp + OUTPUT_FILE_SUFFIX

    #Check if the local output directory exists else create it
    if(not os.path.exists(LOCAL_OUTPUT_DIR)):
        os.makedirs(LOCAL_OUTPUT_DIR)

    #Generate the base request for direct-one-way flights from Malta
    request =  'flights?flyFrom=MLA&typeFlight=oneway&directFlights=1'

    #Generate a date range starting from the 1st of May til the end of June
    start_date = date(2018, 5, 1)
    end_date = date(2018, 6, 30)

    #Generate a list of dates using the date range
    date_list = [date.fromordinal(i) for i in range(start_date.toordinal(), end_date.toordinal()+1)]


    final_response = {}
    final_response['data'] = []

    #For each date in the list
    for i, tmp_date in enumerate(date_list):
        #Append the date to the request
        date_range = '&dateFrom='+ tmp_date.strftime('%d/%m/%Y') +'&dateTo=' + tmp_date.strftime('%d/%m/%Y')
        final_request = request+date_range

        try:
            #Send request
            response = sendRequest(final_request)

            #If it is the first request being sent
            if i == 0:
                #Add header info to final response
                for attribute in response:
                    if(attribute != 'data'):
                        final_response[attribute] = response[attribute]
            #Add data to final response
            final_response['data'].extend(response['data'])

        except Exception as e:
            pass

    #Convert response dictionary to json
    json_data_string = json.dumps(final_response)

    #Write to local file system
    with open(LOCAL_OUTPUT_DIR+file_name, 'w') as out_file:
        out_file.write(json_data_string)

    #Save to Google Drive
    g_file = drive.CreateFile({"parents": [{"kind": "drive#childList","id": "1A6gahcUQxgyjobtEFdLH_sMod5_x_2Hy"}],'title': file_name})
    g_file.SetContentString(json_data_string)
    g_file.Upload()

if __name__ == '__main__':
  main()

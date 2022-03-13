#!/usr/bin/env python3
#
# Mutt OAuth2 token management script, version 2020-08-07
# Written against python 3.7.3, not tried with earlier python versions.
#
#   Copyright (C) 2020 Alexander Perlis
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#   02110-1301, USA.
'''Mutt OAuth2 token management'''

import sys
import json
import urllib.parse
import urllib.request
import base64
import secrets
import hashlib
from datetime import timedelta, datetime
import socket
import http.server
import subprocess
import webbrowser


registrations = {
    'google': {
        'authorize_endpoint': 'https://accounts.google.com/o/oauth2/auth',
        'devicecode_endpoint': 'https://oauth2.googleapis.com/device/code',
        'token_endpoint': 'https://accounts.google.com/o/oauth2/token',
        'redirect_uri': 'urn:ietf:wg:oauth:2.0:oob',
        'imap_endpoint': 'imap.gmail.com',
        'pop_endpoint': 'pop.gmail.com',
        'smtp_endpoint': 'smtp.gmail.com',
        'sasl_method': 'OAUTHBEARER',
        'scope': 'https://mail.google.com/',
        'client_id': '',
        'client_secret': '',
    },
    'microsoft': {
        'authorize_endpoint': 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize',
        'devicecode_endpoint': 'https://login.microsoftonline.com/common/oauth2/v2.0/devicecode',
        'token_endpoint': 'https://login.microsoftonline.com/common/oauth2/v2.0/token',
        'redirect_uri': 'https://login.microsoftonline.com/common/oauth2/nativeclient',
        'tenant': 'common',
        'imap_endpoint': 'outlook.office365.com',
        'pop_endpoint': 'outlook.office365.com',
        'smtp_endpoint': 'smtp.office365.com',
        'sasl_method': 'XOAUTH2',
        'scope': ('offline_access https://outlook.office.com/IMAP.AccessAsUser.All '
                  'https://outlook.office.com/POP.AccessAsUser.All '
                  'https://outlook.office.com/SMTP.Send'),
        'client_id': '',
        'client_secret': '',
    },
}


token = {}
sub = subprocess.run(["/usr/bin/pass", "show", "mutt/token"], check=True,
                     capture_output=True)
token = json.loads(sub.stdout)

def writetokenfile():
    '''Writes global token dictionary into token file.'''
    sub2 = subprocess.run(["/usr/bin/pass", "insert", "-e", "-f", "mutt/token"], check=True, input=json.dumps(token).encode())


if (not token) or (not "access_token" in token):
    print('Available app and endpoint registrations:', *registrations)
    token['access_token'] = ''
    token['access_token_expiration'] = ''
    token['refresh_token'] = ''
    writetokenfile()

if token['registration'] not in registrations:
    sys.exit(f'ERROR: Unknown registration "{token["registration"]}". Delete token file '
             f'and start over.')
registration = registrations[token['registration']]

baseparams = {'client_id': token['client_id']}
# Microsoft uses 'tenant' but Google does not
if 'tenant' in registration:
    baseparams['tenant'] = registration['tenant']


def access_token_valid():
    '''Returns True when stored access token exists and is still valid at this time.'''
    token_exp = token['access_token_expiration']
    return token_exp and datetime.now() < datetime.fromisoformat(token_exp)


def update_tokens(r):
    '''Takes a response dictionary, extracts tokens out of it, and updates token file.'''
    token['access_token'] = r['access_token']
    token['access_token_expiration'] = (datetime.now() +
                                        timedelta(seconds=int(r['expires_in']))).isoformat()
    if 'refresh_token' in r:
        token['refresh_token'] = r['refresh_token']
    writetokenfile()


if not token['access_token']:
    p = baseparams.copy()
    p['scope'] = registration['scope']

    verifier = secrets.token_urlsafe(90)
    challenge = base64.urlsafe_b64encode(hashlib.sha256(verifier.encode()).digest())[:-1]
    redirect_uri = registration['redirect_uri']
    listen_port = 0
    # Find an available port to listen on
    s = socket.socket()
    s.bind(('127.0.0.1', 0))
    listen_port = s.getsockname()[1]
    s.close()
    redirect_uri = 'http://localhost:'+str(listen_port)+'/'
    # Probably should edit the port number into the actual redirect URL.

    p.update({'login_hint': token['email'],
              'response_type': 'code',
              'redirect_uri': redirect_uri,
              'code_challenge': challenge,
              'code_challenge_method': 'S256'})
    url = registration["authorize_endpoint"] + '?' + \
          urllib.parse.urlencode(p, quote_via=urllib.parse.quote)
    print(url)
    webbrowser.open(url, new=0, autoraise=True)
    print('Visit displayed URL to authorize this application. Waiting...',
          end='', flush=True)

    class MyHandler(http.server.BaseHTTPRequestHandler):
        '''Handles the browser query resulting from redirect to redirect_uri.'''

        # pylint: disable=C0103
        def do_HEAD(self):
            '''Response to a HEAD requests.'''
            self.send_response(200)
            self.send_header('Content-type', 'text/html')
            self.end_headers()

        def do_GET(self):
            '''For GET request, extract code parameter from URL.'''
            # pylint: disable=W0603
            global authcode
            querystring = urllib.parse.urlparse(self.path).query
            querydict = urllib.parse.parse_qs(querystring)
            if 'code' in querydict:
                authcode = querydict['code'][0]
            self.do_HEAD()
            self.wfile.write(b'<html><head><title>Authorizaton result</title></head>')
            self.wfile.write(b'<body><p>Authorization redirect completed. You may '
                             b'close this window.</p></body></html>')
    with http.server.HTTPServer(('127.0.0.1', listen_port), MyHandler) as httpd:
        try:
            httpd.handle_request()
        except KeyboardInterrupt:
            pass

    if not authcode:
        sys.exit('Did not obtain an authcode.')

    for k in 'response_type', 'login_hint', 'code_challenge', 'code_challenge_method':
        del p[k]
    p.update({'grant_type': 'authorization_code',
              'code': authcode,
              'client_secret': token['client_secret'],
              'code_verifier': verifier})
    try:
        response = urllib.request.urlopen(registration['token_endpoint'],
                                          urllib.parse.urlencode(p).encode())
    except urllib.error.HTTPError as err:
        print(err.code, err.reason)
        response = err
    response = response.read()
    response = json.loads(response)
    if 'error' in response:
        print(response['error'])
        if 'error_description' in response:
            print(response['error_description'])
        sys.exit(1)


    update_tokens(response)


if not access_token_valid():
    if not token['refresh_token']:
        sys.exit('ERROR: No refresh token. Run script with "--authorize".')
    p = baseparams.copy()
    p.update({'client_secret': token['client_secret'],
              'refresh_token': token['refresh_token'],
              'grant_type': 'refresh_token'})
    try:
        response = urllib.request.urlopen(registration['token_endpoint'],
                                          urllib.parse.urlencode(p).encode())
    except urllib.error.HTTPError as err:
        print(err.code, err.reason)
        response = err
    response = response.read()
    response = json.loads(response)
    if 'error' in response:
        print(response['error'])
        if 'error_description' in response:
            print(response['error_description'])
        print('Perhaps refresh token invalid. Try running once with "--authorize"')
        sys.exit(1)
    update_tokens(response)


if not access_token_valid():
    sys.exit('ERROR: No valid access token. This should not be able to happen.')


print(token['access_token'])

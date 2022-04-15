#!/usr/bin/python
import subprocess
import os

def get_account_path(account):
    password_store = f"{os.path.expanduser('~')}/.password-store"
    args = ["find", password_store, "-wholename", f"*/mutt/{account}/client_id.gpg"]
    sub = subprocess.run(args, check=True, capture_output=True)
    filepath = "/".join(sub.stdout.decode("utf-8").split("/")[4:-1])
    return filepath

def get_app_password(account):
    path = get_account_path(account)
    args = ["/usr/bin/pass", "show", f"{path}/app_password"]
    sub = subprocess.run(args, check=True, capture_output=True)
    return sub.stdout.decode("utf-8")

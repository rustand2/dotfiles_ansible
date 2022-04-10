#!/usr/bin/python
import subprocess


def get_client_id(account):
    args = ["/usr/bin/pass", "show", f"mutt/{account}/client_id"]
    sub = subprocess.run(args, check=True, capture_output=True)
    return sub.stdout.decode("utf-8")


def get_client_secret(account):
    args = ["/usr/bin/pass", "show", f"mutt/{account}/client_secret"]
    sub = subprocess.run(args, check=True, capture_output=True)
    return sub.stdout.decode("utf-8")


def get_refresh_token(account):
    args = ["/usr/bin/pass", "show", f"mutt/{account}/refresh_token"]
    sub = subprocess.run(args, check=True, capture_output=True)
    return sub.stdout.decode("utf-8")

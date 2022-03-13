# My dotfiles
These are my dotfiles, managed with Ansible. Included is also a Vagrantfile to quickly spawn a vm with the Ansible configuration applied.

## Configuration variables
Copy vars.yml.defaults to vars.yml and fill in your details.

## Email configuration
Create a pass entry for mutt named `mutt/token` with the following content and insert your Gmail API credentials:
 
```
{
    "registration": "google",
    "authflow": "authcode",
    "email": "<YOUR GMAIL ADDRESS",
    "client_id": "<YOUR CLIENT ID>",
    "client_secret": "<YOUR CLIENT SECRET>"
}
```

The first time you open mutt you will need to log in to your Gmail account.

## Local installation
To install locally run `sudo ansible-playbook -i 127.0.0.1, -c local playbook.yml`

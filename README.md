# My dotfiles
These are my dotfiles, managed with Ansible. Included are also a Vagrantfile and a docker-compose.yml to quickly spawn a vm or container with the Ansible configuration applied.


## Configuration
Copy vars.yml.defaults to vars.yml and fill in your details.


## Installation

### Local installation
To install locally run `sudo ansible-playbook -i 127.0.0.1, -c local playbook.yml`.

### Virtual machine
To instead quickly spin up a fresh vm to test the playbook you can use the supplied Vagrantfile. It contains a few different distros, check the contents of this file to see the currently configured distros. To start an Archlinux vm simply run `vagrant up arch`, then `vagrant ssh arch` to log in to it.

### Docker container
There are also a few docker targets available in the supplied `docker-compose.yml`. To start a Debian container you could run `docker-compose up debian --build` to build it and `docker-compose run debian`.


## Post installation

### Email configuration
Create a Gmail app password and insert it into pass in an entry called `mutt/<EMAIL ADDRESS>/app_password`, optionally in a subdirectory if needed.


## Advanced usage

#### Running parts of the playbook
There is extensive use of tags in the Ansible roles to allow running only certain parts of the playbook. Adding the parameter `-t TAGNAME` will run only the tasks that are tagged with `TAGNAME`. Likewise it is also possible to skip all tasks with a certain tag by instead using the `--skip-tags` parameter. It is also possible to specify a list of tags separated by comma.

One important thing to remember is that Ansible treats multiple tags as an OR operation not an AND operation, so specifying e.g. `-t neovim,configure` will run all tasks tagged with `neovim` and all tasks tagged with `configure`, it is currently not possible to make Ansible treat it as an AND operation. Therefore I have included a few helpful combined tags, e.g. the `neovim.configure` tag which runs only configuration tasks of neovim role.

To get a list of all the currently available tags run `ansible-playbook playbook.yml --list-tags`.


## Hacking

### Adding a new distro
Add it to the `Vagrantfile` or create a `Dockerfile` and add it to the `docker-compose.yml`, then spin it up as normal. You will most likely encounter some errors because things are named differently from the other distros. You can fix this by going to the failing role and creating a vars file for your distro with the correct values.

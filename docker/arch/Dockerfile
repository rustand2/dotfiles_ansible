FROM archlinux:latest

RUN pacman -Syu --noconfirm

RUN pacman -S --noconfirm ansible

COPY . /provisioning

WORKDIR /provisioning

RUN ansible-playbook -i 127.0.0.1, -c local plays/docker.yml \
    --skip-tags hardware \
    --skip-tags update \
    --skip-tags gui

USER 1000:1000

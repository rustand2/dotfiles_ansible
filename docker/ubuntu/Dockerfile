FROM ubuntu:21.10

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
  apt-get install -y python3-pip software-properties-common && \
  pip3 install --upgrade pip && \
  pip3 install ansible

COPY . /provisioning

WORKDIR /provisioning

RUN ansible-playbook -i 127.0.0.1, -c local plays/docker.yml \
    --skip-tags hardware \
    --skip-tags update \
    --skip-tags gui

USER 1000:1000

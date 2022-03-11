# # -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.define "skole" do |skole|
    skole.vm.box = "archlinux/archlinux"
    skole.vm.synced_folder '/home/lars/c4v', '/c4v', owner: 'lars'
  end
  config.vm.define "arch" do |arch|
    arch.vm.box = "archlinux/archlinux"
  end
  config.vm.define "ubuntu" do |ubuntu|
    ubuntu.vm.box = "ubuntu/impish64"
  end
  config.vm.define "debian" do |debian|
    debian.vm.box = "debian/buster64"
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "playbook.yml"
  end

  config.vm.provider "libvirt" do |v|
    v.memory = 8192
    v.cpus = 4
    v.video_type = "virtio"
    v.machine_virtual_size = 60
  end
end

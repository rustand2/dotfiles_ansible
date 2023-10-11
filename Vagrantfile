# # -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.define "arch" do |arch|
    arch.vm.box = "archlinux/archlinux"
  end
  config.vm.define "ubuntu" do |ubuntu|
    ubuntu.vm.box = "generic/ubuntu2110"
  end
  config.vm.define "debian" do |debian|
    debian.vm.box = "debian/bullseye64"
  end
  config.vm.define "debian-testing" do |debian_testing|
    debian_testing.vm.box = "debian/testing64"
  end
  config.vm.define "freebsd" do |freebsd|
    freebsd.vm.box = "freebsd/FreeBSD-14.0-CURRENT"
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true

  # Bootstrap python on BSD
  config.vm.provision "bootstrap_python", type: "ansible"  do |ansible|
    ansible.playbook = "plays/vagrant/bootstrap_python.yml"
  end

  # Enable truecolor over ssh
  config.ssh.forward_env = ['COLORTERM']
  config.vm.provision "colorterm", type: "ansible"  do |ansible|
    ansible.playbook = "plays/vagrant/enable_colorterm.yml"
  end

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "private/plays/vagrant.yml"
  end


  config.vm.provider "libvirt" do |v|
    v.memory = 8192
    v.cpus = 4
    v.video_type = "virtio"
    v.machine_virtual_size = 60
  end
end

(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/Detroit")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "Voyager")
  (users (cons* (user-account
                  (name "Que")
                  (comment "Que")
                  (group "users")
                  (home-directory "/home/Que")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "1a274421-89f0-4864-9a64-91b204ab816d")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "783D-C67E" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "dfe814c2-fdda-4efd-8673-972aa78ab4ed"
                     'ext4))
             (type "ext4"))
           (file-system
             (mount-point "/home")
             (device
               (uuid "64ed9c27-0c03-4ba5-b34c-927452b0defd"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
